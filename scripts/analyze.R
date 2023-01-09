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
#library(plyr)
library(sportyR)

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
    ungroup() %>% 
    inner_join(df_players, by = "nflId") %>% 
    filter(snaps >= 125)
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
    arrange(desc(avg_metric)) %>% 
    mutate(
      Rank = dplyr::row_number(),
      avg_metric = round(avg_metric, 3),
      total_metric = round(total_metric, 3)
    ) %>% 
    select(
      nflId,
      Rank,
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
  
  title_x <- if(lab_y == "PAUE") "PFF Pass Block Grade" else "PFF Pass Rush Grade"
  plot_title <- paste(lab_y, "vs", title_x)
  
  
  ggplot(df, aes(x = !!as.name(xmet), y = !!as.name(ymet))) + 
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) + 
    geom_text(x = min(df[[xmet]]), y = max(df[[ymet]]), label = compare_cor) +
    ggtitle(plot_title) +
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
    std_coords() %>% 
    mutate(size_of_point = ifelse(team == "football", 1, 7)) %>% 
    mutate(prob = ifelse(!is.na(pred_pressure_allowed), pred_pressure_allowed, pred_pressure_delivered)) %>% 
    mutate(side_of_ball = ifelse(team == possessionTeam | team == "football", "off", "def"))
}

prep_for_probplot <- function(df, gid, pid) {
  df %>% 
    filter(gameId == gid & playId == pid) %>% 
    inner_join(select(df_players, nflId, displayName), by = "nflId") %>% 
    mutate(nflId = as.character(nflId)) 
}

create_pocket_plot <- function(df, gid, pid) {
  df_for_plot <- prep_example_for_plot(df, gid, pid)
  
  df_for_plot_pocket <- df_for_plot %>% 
    inner_join(
      select(df_pff_pocket, gameId, playId, nflId, pff_positionLinedUp), 
      by = c("gameId", "playId", "nflId")
    ) %>% 
    arrange(factor(pff_positionLinedUp, levels = hex_points))
  
  start_yardline <- plyr::round_any(min(df_for_plot$x), 10, f = floor)
  end_yardline <- plyr::round_any(max(df_for_plot$x), 10, f = ceiling)
  x_breaks <- seq(start_yardline, end_yardline, 10)
  x_labels <- x_breaks
  x_labels[x_labels > 50] <- 100 - x_labels[x_labels > 50]
  
  
  ggplot(data = filter(df_for_plot, side_of_ball == "off"), aes(x = x, y = y)) + 
    geom_polygon(data = df_for_plot_pocket, aes(x = x, y = y), fill = "blue", alpha = 0.1) +
    geom_point(aes(fill = pred_pressure_allowed, size = size_of_point), shape = 21) +
    scale_fill_gradient("off", low = "white", high = "blue") +
    new_scale("fill") +
    geom_point(data = filter(df_for_plot, side_of_ball == "def"), aes(fill = pred_pressure_delivered, size = size_of_point), shape = 21) +
    scale_fill_gradient("def", low = "white", high = "orange") +
    geom_text(data = df_for_plot, aes(x = x, y = y, label = jerseyNumber, size = size_of_point*0.5), color = "black", vjust = 0.35) +
    scale_radius() +
    scale_x_continuous(breaks = x_breaks, labels = x_labels) +
    ggtitle(df_for_plot$playDescription) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(color = "darkgray"),
      panel.grid.major.x = element_line(size = 1, color = "darkgray"),
      panel.grid.minor.x = element_line(size = 1, color = "darkgray"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      plot.title = element_text(size = 8)
    )
}

create_prob_plot <- function(df, target_pred, gid, pid) {
  
  df <- prep_for_probplot(df, gid, pid)
  title_str <- if(target_pred == "pred_pressure_allowed") "Allowed" else "Delivered"
  
  ggplot(df, aes(x = frames_from_start, y = !!as.name(target_pred), group = displayName, color = displayName)) +
    geom_line() +
    geom_point(pch=19, size=3) +
    #scale_color_manual(legend_name, values = c("black", "orange", "blue")) +
    scale_y_continuous("Pressure Probability", labels = scales::percent) +
    scale_x_continuous("Time(s)", 
                       breaks = seq(0, max(df$frames_from_start), 5),
                       labels = ~.x/10) +
    ggtitle(paste("Frame-by-Frame Pressure", title_str, "Probability")) +
    theme_minimal() +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.title = element_blank()
    )
}

# Read data ---------------------------------------------------------------
### THIS SECTION DIFFERENT IN KAGGLE NOTEBOOK


##source data
df_pff <- read_csv(file.path(data_folder, "pffScoutingData.csv"))
df_plays <- read_csv(file.path(data_folder, "plays.csv"))
df_players <- read_csv(file.path(data_folder, "players.csv"))

##model results data
block_results <- readr::read_rds(file.path(data_folder, "block_results.rds"))
rush_results <- readr::read_rds(file.path(data_folder, "rush_results.rds"))
df_results <- readr::read_rds(file.path(data_folder, "df_results.rds"))


#TODO - save off versions of this that can be uploaded to kaggle and not changed when using
##additional data
pff_folder <- file.path(data_folder, "pff")
kaggle_folder <- file.path(data_folder, "for_kaggle")

passblockgrades <- read_csv(file.path(pff_folder, "pass_blocking_efficiency.csv")) %>% select(player_id, player, pbe, grade = grades_pass_block)
passrushgrades <- read_csv(file.path(pff_folder, "pass_rush_grades.csv")) %>% select(player_id, player, prp, grade = grades_pass_rush_defense)

# readr::write_rds(passblockgrades, file.path(kaggle_folder, "passblockgrades.csv"))
# readr::write_rds(passrushgrades, file.path(kaggle_folder, "passrushgrades.csv"))



# Visualize pressure allowed/delivered probabilities ----------------------

df_pff_pocket <- df_pff %>% 
  filter(
    (pff_positionLinedUp %in% ol_positions & pff_role == "Pass Block") |
      pff_positionLinedUp == "QB" & pff_role == "Pass"
  )


###good first frame example with LB and DT over center
frame1_ex_gameId <- 2021091901
frame1_ex_playId <- 2509

frame1_df <- df_results %>% 
  filter(gameId == frame1_ex_gameId, playId == frame1_ex_playId) %>% 
  filter(frameId == min(frameId))

# frame1_for_plot <- prep_example_for_plot(frame1_df, frame1_ex_gameId, frame1_ex_playId)
# 
# frame1_for_plot_pocket <- frame1_for_plot %>% 
#   inner_join(
#     select(df_pff_pocket, gameId, playId, nflId, pff_positionLinedUp), 
#     by = c("gameId", "playId", "nflId")
#   ) %>% 
#   arrange(factor(pff_positionLinedUp, levels = hex_points))
# 
# frame1_fieldplot <- geom_football("nfl", display_range = "full", x_trans = 59, y_trans = 26.5, xlims = c(40, 80), ylims = c(0, 60)) + 
#   geom_polygon(data = frame1_for_plot_pocket, aes(x = x, y = y), fill = "blue", alpha = 0.3) +
#   geom_point(data = filter(frame1_for_plot, side_of_ball == "off"), aes(x = x, y = y, fill = pred_pressure_allowed, size = size_of_point), shape = 21) +
#   scale_fill_gradient("off", low = "white", high = "blue") +
#   new_scale("fill") +
#   geom_point(data = filter(frame1_for_plot, side_of_ball == "def"), aes(x = x, y = y, fill = pred_pressure_delivered, size = size_of_point), shape = 21) +
#   scale_fill_gradient("def", low = "white", high = "orange") +
#   geom_text(data = frame1_for_plot, aes(x = x, y = y, label = jerseyNumber, size = size_of_point*0.5), color = "black", vjust = 0.35) +
#   scale_radius() +
#   theme(legend.position = "none")

frame1_pocket_plot <- create_pocket_plot(frame1_df, frame1_ex_gameId, frame1_ex_playId)

# #find example plays to animate
# rush_mets <- calc_metric(rush_results, target = "pressure_delivered")
# 
# hendrickson_examples <- df_pff %>%
#   inner_join(df_players, by = "nflId") %>%
#   filter(pff_sack==1) %>%
#   filter(displayName == "Trey Hendrickson") %>% # | displayName == "Sam Hubbard") %>%
#   select(contains("Id"), displayName) %>%
#   select(-pff_nflIdBlockedPlayer) %>%
#   inner_join(df_results, by = c("gameId", "playId", "nflId")) %>%
#   inner_join(rush_mets, by = c("gameId", "playId", "nflId")) %>% 
#   arrange(desc(pred_pressure_delivered)) %>% 
#   select(gameId, playId) %>% 
#   distinct()

#leading contenders
# ex_gameId <- 2021091901
# ex_playId <- 1796
# ex_gameId <- 2021101002
# ex_playId <- 4007

#one of Trey Hendrickson's sacks
ex_gameId <- 2021101002
ex_playId <- 1792

full_play_plot <- create_pocket_plot(df_results, ex_gameId, ex_playId)

block_prob_plot <- create_prob_plot(block_results, "pred_pressure_allowed", ex_gameId, ex_playId)
rush_prob_plot <- create_prob_plot(rush_results, "pred_pressure_delivered", ex_gameId, ex_playId)

max_frame <- df_results %>% 
  filter(gameId == ex_gameId, playId == ex_playId) %>% 
  pull(frameId) %>% 
  max()










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
         dist_to_qb)

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

play_anim <- animate(
  full_play_plot + transition_time(frameId),
  nframes = max_frame,
  renderer = magick_renderer(),
  width = 720,
  height = 440,
  res = 100
)


compare_p1 <- compare_metrics(block_compare_metrics, "grade", "avg_metric", "PFF Grade", "PAUE")
#compare_metrics(block_compare_metrics, "grade", "avg_metric_adj", "PFF Grade", "Adj PAUE")

compare_p2 <- compare_metrics(rush_compare_metrics, "grade", "avg_metric", "PFF Grade", "PDOE")
#compare_metrics(rush_compare_metrics, "grade", "avg_metric_adj", "PFF Grade", "Adj PDOE")


block_metric_table %>%
  head(25) %>% 
  select(-nflId) %>% 
  rename(`Average PAUE` = avg_metric, `Total PAUE` = total_metric) %>% 
  kbl(caption = "Min 100 snaps") %>%
  kable_paper("hover", full_width = FALSE)

rush_metric_table %>%
  head(25) %>% 
  select(-nflId) %>% 
  rename(`Average PDOE` = avg_metric, `Total PDOE` = total_metric) %>% 
  kbl(caption = "Min 100 snaps") %>%
  kable_paper("hover", full_width = FALSE)

block_both_mets %>% 
  arrange(desc(avg_metric_adj)) %>% 
  mutate(Rank = dplyr::row_number()) %>% 
  head(25) %>%  
  select(
    Name, Position, 
    Snaps, `Average PAUE` = avg_metric, `Total PAUE` = total_metric,
    `Adj Eligible Snaps` = Snaps_adj,`Average Adj PAUE` = avg_metric_adj, `Total Adj PAUE` = total_metric_adj
  )

rush_both_mets %>% 
  arrange(desc(avg_metric_adj)) %>% 
  mutate(Rank = dplyr::row_number()) %>% 
  head(25) %>% 
  select(
    Name, Position, 
    Snaps, `Average PDOE` = avg_metric, `Total PDOE` = total_metric,
    `Adj Eligible Snaps` = Snaps_adj,`Average Adj PDOE` = avg_metric_adj, `Total Adj PDOE` = total_metric_adj
  )

