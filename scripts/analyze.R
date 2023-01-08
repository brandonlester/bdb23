########################################
# analyze results
#
#
########################################

# IDEAS -------------------------------------------------------------------

#biggest swings in sack prob from frame to frame, start to end of play
#prob from snap to result - players overcoming situation on either off or def side
#probs by defenders in box, num rushers, coverage, formation, down linemen, by features

#IDEA*****
#diff of overall play prob and sum/avg/etc of individual player starting probs?
#this diff at each frame?
#measure of default prob in a situation and the players/team that increase/decrease chances due to skill/play/technique

#IDEA****
#Model predicting probab change to get most important to win/recover

#IDEA****
#animate prob diff battles for example play





# Libraries ---------------------------------------------------------------

library(tidyverse)
library(kableExtra)
library(gganimate)
library(ggnewscale)
library(magick)

#source("scripts/dots.R")
#^functions/constants from dots.R are duplicated in this script (analyze.R)


# Constants ---------------------------------------------------------------

#folders - diff in kaggle notebook
data_folder <- "data"
output_folder <- "output"

hex_points <- c("QB", "LT", "LG", "C", "RG", "RT")
ol_positions <- hex_points[-1]


# Metric functions --------------------------------------------------------

summarise_results <- function(df, target) {
  names(df)[names(df) == target] <- "actual"
  names(df)[names(df) == paste0("pred_", target)] <- "prob"
  
  df %>% 
    arrange(gameId, playId, frameId, nflId) %>% 
    group_by(gameId, playId, nflId) %>% 
    mutate(
      prev_prob = lag(prob),
      start_prob = prob[frameId == min(frameId)]
    ) %>% 
    ungroup() %>% 
    mutate(
      start_probchange = prob - start_prob,
      frame_probchange = prob - prev_prob
    )
}

calc_metric <- function(df, target) {
  names(df)[names(df) == target] <- "actual"
  names(df)[names(df) == paste0("pred_", target)] <- "prob"
  
  temp_df <- df %>% 
    group_by(gameId, playId, nflId) %>% 
    summarise(
      prob = prob[frameId==min(frameId)],
      actual = unique(actual)
    ) %>% 
    ungroup()
  
  if(target == "pressure_allowed") {
    temp_df %>% mutate(metric = prob - actual)
  } else {
    temp_df %>% mutate(metric = actual - prob)
  }
    
}

metric_summary <- function(df, target) {
  df %>% 
    calc_metric(target = target) %>% 
    group_by(nflId) %>% 
    summarise(
      snaps = n(),
      total_metric = sum(metric),
      avg_metric = mean(metric)
    ) %>% 
    arrange(desc(avg_metric)) %>% 
    inner_join(df_players, by = "nflId") %>% 
    filter(snaps >= 100)
}

adj_metric_summary <- function(df, metric_col, id_col) {
  temp_df <- pbp_edges %>% 
    group_by(!!as.name(id_col)) %>% 
    summarise(
      snaps = n(),
      total_metric = sum(!!as.name(metric_col)),
      avg_metric = mean(!!as.name(metric_col))
    ) %>% 
    filter(snaps >= 100) %>% 
    arrange(desc(avg_metric))
  
  players <- df_players
  names(players)[names(players)=="nflId"] <- id_col
  
  temp_df <- temp_df %>% 
    inner_join(players, by = id_col)
  
  names(temp_df)[names(temp_df) == id_col] <- "nflId"
  
  return(temp_df)
}

metric_table <- function(summary_metric, target) {
  # block_name <- "PAUE"
  # rush_name <- "PDOE"
  
  temp_df <- summary_metric %>% 
    #head(10) %>% 
    mutate(
      avg_metric = round(avg_metric, 3),
      total_metric = round(total_metric, 3)
    ) %>% 
    select(
      nflId,
      Name = displayName,
      Position = officialPosition,
      Snaps = snaps,
      avg_metric,
      total_metric
    ) 
  
  # if(target == "pressure_allowed") {
  #   names(temp_df)[names(temp_df) == "avg_metric"] <- paste("Average", block_name)
  #   names(temp_df)[names(temp_df) == "total_metric"] <- paste("Total", block_name)
  # } else {
  #   names(temp_df)[names(temp_df) == "avg_metric"] <- paste("Average", rush_name)
  #   names(temp_df)[names(temp_df) == "total_metric"] <- paste("Total", rush_name)
  # }
  
  return(temp_df)
}

combined_metrics <- function(metric_table, adj_metric_table) {
  metric_table %>% 
    inner_join(select(adj_metric_table, -Name, -Position), by = "nflId", suffix = c("", "_adj")) %>% 
    mutate(
      metric_rank = dplyr::min_rank(avg_metric),
      adj_metric_rank = dplyr::min_rank(avg_metric_adj),
      rank_diff = adj_metric_rank - metric_rank
    )
}

compare_metrics <- function(df, xmet, ymet, lab_x, lab_y) {
  compare_cor <- cor(df[[xmet]], df[[ymet]])
  compare_cor <- paste("R2:", round(compare_cor,2))
  
  
  ggplot(df, aes(x = !!as.name(xmet), y = !!as.name(ymet))) + 
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) + 
    geom_text(x = min(df[[xmet]]), y = max(df[[ymet]]), label = compare_cor) +
    ggtitle("Metric Comparison") +
    ylab(lab_y) +
    xlab(lab_x) +
    theme_minimal()
}

# Plotting functions ------------------------------------------------------

std_coords <- function(tracking_df) {
  #Standardizing tracking data so its always in direction of offensive team.
  tracking_df %>% 
    mutate(x = ifelse(playDirection == "left", 120-x, x),
           y = ifelse(playDirection == "left", 160/3 - y, y))
}

prep_example_for_plot <- function(tracking_df, game_id, play_id) {
  tracking_df %>% 
    filter(gameId == game_id & playId == play_id) %>%
    inner_join(select(df_plays, gameId, playId, playDescription, possessionTeam)) %>% 
    std_coords()
}

prep_for_probplot <- function(df) {
  df %>% 
    filter(gameId == ex_gameId & playId == ex_playId) %>% 
    inner_join(select(df_players, nflId, displayName), by = "nflId") %>% 
    mutate(nflId = as.character(nflId)) 
}

create_pocket_plot <- function(df_for_plot) {
  df_for_plot_pocket <- df_for_plot %>% 
    inner_join(
      select(df_pff_pocket, gameId, playId, nflId, pff_positionLinedUp), 
      by = c("gameId", "playId", "nflId")
    ) %>% 
    arrange(factor(pff_positionLinedUp, levels = hex_points))
  
  ggplot(data = filter(df_for_plot, side_of_ball == "off"), aes(x = x, y = y)) + 
    geom_polygon(data = df_for_plot_pocket, aes(x = x, y = y), fill = "blue", alpha = 0.1) +
    geom_point(aes(fill = pred_pressure_allowed, size = size_of_point), shape = 21) +
    scale_fill_gradient("off", low = "white", high = "blue") +
    new_scale("fill") +
    geom_point(data = filter(df_for_plot, side_of_ball == "def"), aes(fill = pred_pressure_delivered, size = size_of_point), shape = 21) +
    scale_fill_gradient("def", low = "white", high = "orange") +
    geom_text(data = df_for_plot, aes(x = x, y = y, label = jerseyNumber, size = size_of_point*0.5), color = "black", vjust = 0.35) +
    scale_radius() +
    ggtitle(df_for_plot$playDescription) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major.x = element_line(size = 1),
      panel.grid.minor.x = element_line(size = 1),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()#,
      #plot.title = element_text(size = 6)
    )
}

create_prob_plot <- function(df, target_pred) {
  
  df <- prep_for_probplot(df)
  #legend_name <- if(target_pred == "pred_pressure_allowed") "Blocker" else "Rusher"
  
  ggplot(df, aes(x = frames_from_start, y = !!as.name(target_pred), group = displayName, color = displayName)) +
    geom_line() +
    geom_point(pch=19, size=3) +
    #scale_color_manual(legend_name, values = c("black", "orange", "blue")) +
    scale_y_continuous("Pressure Probability", labels = scales::percent) +
    scale_x_continuous("Time(s)", 
                       breaks = seq(0, max(df$frames_from_start), 5),
                       labels = ~.x/10) +
    ggtitle("Frame-by-Frame Pressure Probability") +
    theme_minimal() +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    )
}

# Read data ---------------------------------------------------------------
### THIS SECTION DIFFERENT IN KAGGLE NOTEBOOK


##source data
df_pff <- read_csv(file.path(data_folder, "pffScoutingData.csv"))
df_plays <- read_csv(file.path(data_folder, "plays.csv"))
df_players <- read_csv(file.path(data_folder, "players.csv"))

#this can be original tracking data as I filter to frame needing a prediction in df_results
df_tracking  <- read_rds(file.path(data_folder, "df_tracking_foi.rds")) 


##model results data
model_results <- readr::read_rds("data/model_results.rds")
block_results <- model_results$block$df_all
rush_results <- model_results$rush$df_all
rm(model_results)
gc()


#TODO - save off versions of this that can be uploaded to kaggle and not changed when using
##additional data
pff_folder <- file.path(data_folder, "pff")
passblockgrades <- read_csv(file.path(pff_folder, "passblockgrades.csv")) %>% select(player_id, player, pbe, grade = grades_pass_block)
passrushgrades <- read_csv(file.path(pff_folder, "passrushgrades.csv")) %>% select(player_id, player, prp, grade = grades_pass_rush_defense)


# #PUSH BACK TO MODEL SCRIPT ----------------------------------------------

df_results <- df_tracking %>% 
  left_join(select(block_results, gameId, playId, frameId, nflId, pred_pressure_allowed), by = c("gameId", "playId", "frameId", "nflId")) %>%
  left_join(select(rush_results, gameId, playId, frameId, nflId, pred_pressure_delivered), by = c("gameId", "playId", "frameId", "nflId")) %>% 
  group_by(gameId, playId, frameId) %>% 
  filter(any(!is.na(pred_pressure_allowed))) %>% 
  filter(any(!is.na(pred_pressure_delivered))) %>% 
  ungroup()


# Visualize pressure allowed/delivered probabilities ----------------------

df_pff_pocket <- df_pff %>% 
  filter(
    (pff_positionLinedUp %in% ol_positions & pff_role == "Pass Block") |
      pff_positionLinedUp == "QB" & pff_role == "Pass"
  )

# #find best over expected play from Bengals DE's
# rush_mets <- calc_metric(rush_results, target = "pressure_delivered")
# 
# bengals_example <- df_pff %>% 
#   inner_join(df_players, by = "nflId") %>% 
#   filter(pff_sack==1) %>% 
#   filter(displayName == "Trey Hendrickson" | displayName == "Sam Hubbard") %>% 
#   select(contains("Id"), displayName) %>% 
#   select(-pff_nflIdBlockedPlayer) %>% 
#   inner_join(df_results, by = c("gameId", "playId", "nflId")) %>% 
#   inner_join(rush_mets, by = c("gameId", "playId", "nflId")) %>% 
#   filter(metric == max(metric)) %>% 
#   select(gameId, playId) %>% 
#   distinct() %>% 
#   as.list()

ex_gameId <- 2021093000
ex_playId <- 3206

#play chosen from Hendrickson's highlights
df_for_plot <- prep_example_for_plot(df_results, ex_gameId, ex_playId) %>% 
  mutate(size_of_point = ifelse(team == "football", 1, 10)) %>% 
  mutate(prob = ifelse(!is.na(pred_pressure_allowed), pred_pressure_allowed, pred_pressure_delivered)) %>% 
  mutate(side_of_ball = ifelse(team == possessionTeam | team == "football", "off", "def"))

pause_frame <- rush_results %>% 
  inner_join(select(df_for_plot, ends_with("Id")), by = c("gameId", "playId", "nflId", "frameId")) %>% 
  filter(dist_to_qb == min(dist_to_qb)) %>% 
  pull(frameId)

# #running this will make plot 1 frame where max probability was
# df_for_plot %>%
#   group_by(gameId, playId, frameId) %>%
#   mutate(max_prob = max(pred_pressure_delivered, na.rm = TRUE)) %>%
#   ungroup() %>%
#   filter(max_prob == max(max_prob,na.rm=TRUE)) %>% 
#   create_pocket_plot()
# 
# create_pocket_plot(filter(df_for_plot, frameId == pause_frame))

full_play_plot <- create_pocket_plot(df_for_plot)
block_prob_plot <- create_prob_plot(block_results, "pred_pressure_allowed")
rush_prob_plot <- create_prob_plot(rush_results, "pred_pressure_delivered")













# Calculate pressure allowed/delivered over expected and aggregate --------

#not used anywhere
# block_results_summary <- summarise_results(block_results, "pressure_allowed")
# rush_results_summary <- summarise_results(rush_results, "pressure_delivered")


# block_metric <- metric_summary(block_results, "pressure_allowed")
# rush_metric <- metric_summary(rush_results, "pressure_delivered")
# 
# block_metric_table <- metric_table(block_metric, "pressure_allowed")
# rush_metric_table <- metric_table(rush_metric, "pressure_delivered")

block_metric_table <- block_results %>% 
  metric_summary("pressure_allowed") %>% 
  metric_table("pressure_allowed")

rush_metric_table <- rush_results %>% 
  metric_summary("pressure_delivered") %>% 
  metric_table("pressure_delivered")




# Compare blockers and blocked players ------------------------------------

fbf_rush2join <- rush_results %>% 
  select(ends_with("Id"), jerseyNumber, team, x, y, s, a, dis, o, dir,
         side_of_ball, contains("delivered"), pff_positionLinedUp, pff_role, contains("near_oppo"),
         dist_to_qb, data_split)

fbf_results_joined <- block_results %>% 
  inner_join(
    fbf_rush2join, 
    by = c("gameId", "playId", "frameId", "pff_nflIdBlockedPlayer" = "nflId"),
    suffix = c(".block", ".rush")
  )

pbp_results_joined <- block_results %>% 
  calc_metric(target = "pressure_allowed") %>% 
  inner_join(select(df_pff, contains("Id")), by = c("gameId", "playId", "nflId")) %>% 
  inner_join(calc_metric(rush_results, target = "pressure_delivered"), 
             by = c("gameId", "playId", "pff_nflIdBlockedPlayer" = "nflId"),
             suffix = c(".block", ".rush"))

#TODO - needs attention
pbp_edges <- pbp_results_joined %>% 
  filter(actual.block == actual.rush) %>% 
  mutate(
    #if prob of allowing pressure > 50%, rusher is favored
    favorite.block = ifelse(prob.block > 0.5, "rusher", "blocker"),
    #if prob of delivering pressure > 50%, rusher is favored
    favorite.rush = ifelse(prob.rush > 0.5, "rusher", "blocker"),
    favorite_conflicts = favorite.block != favorite.rush,
    favorite_edge = abs(prob.block - prob.rush),
    metric_adj.block = ifelse(actual.block == 1, metric.block - favorite_edge, metric.block + favorite_edge),
    metric_adj.rush = ifelse(actual.rush == 1, metric.rush + favorite_edge, metric.rush - favorite_edge),
    actual_conflicts = actual.block != actual.rush
  )

# pbp_edges %>% count(favorite.block, favorite.rush, favorite_conflicts, sort = TRUE)
# pbp_edges %>% group_by(pressure_recorded = actual.block) %>% summarise(avg_edge = mean(favorite_edge))


# block_adj_metrics <- adj_metric_summary(pdp_edges, "metric_adj.block", "nflId")
# rush_adj_metrics <- adj_metric_summary(pdp_edges, "metric_adj.rush", "pff_nflIdBlockedPlayer")
# 
# block_adj_metric_table <- metric_table(block_adj_metrics, "pressure_allowed")
# rush_adj_metric_table <-metric_table(rush_adj_metrics, "pressure_delivered")


block_adj_metric_table <- pdp_edges %>% 
  adj_metric_summary("metric_adj.block", "nflId") %>% 
  metric_table("pressure_allowed")

rush_adj_metric_table <- pdp_edges %>% 
  adj_metric_summary("metric_adj.rush", "pff_nflIdBlockedPlayer") %>% 
  metric_table("pressure_delivered")



# Join base metric with adj metric ----------------------------------------

block_both_mets <- combined_metrics(block_metric_table, block_adj_metric_table)
rush_both_mets <- combined_metrics(rush_metric_table, rush_adj_metric_table)

block_compare_metrics <- block_both_mets %>% 
  inner_join(passblockgrades, by = c("Name" = "player"))

rush_compare_metrics <- rush_both_mets %>% 
  inner_join(passrushgrades, by = c("Name" = "player"))


#likely not/less available - sticking to grades for comparison
# compare_metrics(block_compare_metrics, "pbe", "avg_metric", "PFF Pass Block Efficiency", "PAUE")
# compare_metrics(block_compare_metrics, "pbe", "avg_metric_adj", "PFF Pass Block Efficiency", "Adj PAUE")
# compare_metrics(rush_compare_metrics, "prp", "avg_metric", "PFF Pass Rush Productvity", "PDOE")
# compare_metrics(rush_compare_metrics, "prp", "avg_metric_adj", "PFF Pass Rush Productvity", "Adj PDOE")




# actually output ---------------------------------------------------------


# full_play_plot + transition_time(frameId)
# block_prob_plot + transition_reveal(frameId)
# rush_prob_plot + transition_reveal(frameId)
# 
# animate(
#   p + transition_time(frameId),
#   nframes = max(df_for_plot$frameId),
#   renderer = magick_renderer(),
#   width = 720,
#   height = 440,
#   res = 150
# )


compare_metrics(block_compare_metrics, "grade", "avg_metric", "PFF Grade", "PAUE")
compare_metrics(block_compare_metrics, "grade", "avg_metric_adj", "PFF Grade", "Adj PAUE")

compare_metrics(rush_compare_metrics, "grade", "avg_metric", "PFF Grade", "PDOE")
compare_metrics(rush_compare_metrics, "grade", "avg_metric_adj", "PFF Grade", "Adj PDOE")


block_metric_table %>%
  select(-nflId) %>% 
  rename(`Average PAUE` = avg_metric, `Total PAUE` = total_metric) %>% 
  kbl(caption = "Min 100 snaps") %>%
  kable_paper("hover", full_width = FALSE)

rush_metric_table %>%
  rename(`Average PDOE` = avg_metric, `Total PDOE` = total_metric) %>% 
  kbl(caption = "Min 100 snaps") %>%
  kable_paper("hover", full_width = FALSE)
