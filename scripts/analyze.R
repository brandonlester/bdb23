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
#frame by frame pressure allowed vs delivered probabilities for blockers and attached rushers

#IDEA****
#Model predicting probab change to get most important to win/recover

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(kableExtra)
library(gganimate)
library(ggnewscale)
library(magick)

source("scripts/dots.R")



# Constants ---------------------------------------------------------------

#folders
data_folder <- "data"
output_folder <- "output"
nb_folder <- "notebook"



hex_points <- c("QB", "LT", "LG", "C", "RG", "RT")
ol_positions <- hex_points[-1]

# Functions ---------------------------------------------------------------

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
  
  df %>% 
    group_by(gameId, playId, nflId) %>% 
    summarise(
      prob = prob[frameId==min(frameId)],
      actual = unique(actual)
    ) %>% 
    ungroup() %>% 
    mutate(metric = actual - prob)
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


metric_table <- function(summary_metric, target) {
  temp_df <- summary_metric %>% 
    #head(10) %>% 
    mutate(
      avg_metric = round(avg_metric, 3),
      total_metric = round(total_metric, 3)
    ) %>% 
    select(
      Name = displayName,
      Position = officialPosition,
      Snaps = snaps,
      avg_metric,
      total_metric
    ) 
  
  if(target == "pressure_allowed") {
    names(temp_df)[names(temp_df) == "avg_metric"] <- "Average PAOE"
    names(temp_df)[names(temp_df) == "total_metric"] <- "Total PAOE"
  } else {
    names(temp_df)[names(temp_df) == "avg_metric"] <- "Average PDOE"
    names(temp_df)[names(temp_df) == "total_metric"] <- "Total PDOE"
  }
  
  return(temp_df)
}

# Read data ---------------------------------------------------------------

#source data
df_pff <- read_csv(file.path(data_folder, "pffScoutingData.csv"))
df_plays <- read_csv(file.path(data_folder, "plays.csv"))
df_players <- read_csv(file.path(data_folder, "players.csv"))

model_results <- readr::read_rds("data/model_results.rds")
block_results <- model_results$block$df_all
rush_results <- model_results$rush$df_all
rm(model_results)
gc()


df_tracking_foi  <- read_rds(file.path(data_folder, "df_tracking_foi.rds"))


# Analyze model results ---------------------------------------------------

#skimr::skim(df_results)


block_results_summary <- summarise_results(block_results, "pressure_allowed")
rush_results_summary <- summarise_results(rush_results, "pressure_delivered")


df_results <- df_tracking_foi %>% 
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

#play chosen from Hendrickson's highlights
df_for_plot <- prep_example_for_plot(df_results, 2021093000, 3206) %>% 
  mutate(size_of_point = ifelse(team == "football", 1, 10)) %>% 
  mutate(prob = ifelse(!is.na(pred_pressure_allowed), pred_pressure_allowed, pred_pressure_delivered)) %>% 
  mutate(side_of_ball = ifelse(team == possessionTeam | team == "football", "off", "def"))

pause_frame <- rush_results %>% 
  inner_join(select(df_for_plot, ends_with("Id")), by = c("gameId", "playId", "nflId", "frameId")) %>% 
  filter(dist_to_qb == min(dist_to_qb)) %>% 
  pull(frameId)

#running this will make plot 1 frame where max probability was
df_for_plot %>%
  group_by(gameId, playId, frameId) %>%
  mutate(max_prob = max(pred_pressure_delivered, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(max_prob == max(max_prob,na.rm=TRUE)) %>% 
  create_pocket_plot()

create_pocket_plot(filter(df_for_plot, frameId == pause_frame))


full_play_plot <- create_pocket_plot(df_for_plot)

animate(
  full_play_plot + transition_time(frameId),
  nframes = max(df_for_plot$frameId),
  renderer = magick_renderer(),
  width = 720,
  height = 440,
  res = 150
)

# Calculate pressure allowed/delivered over expected and aggregate --------

block_metric <- metric_summary(block_results, "pressure_allowed")
rush_metric <- metric_summary(rush_results, "pressure_delivered")

block_metric_table <- metric_table(block_metric, "pressure_allowed")
rush_metric_table <- metric_table(rush_metric, "pressure_delivered")


block_metric_table %>%
  kbl(caption = "Min 100 snaps") %>%
  kable_paper("hover", full_width = FALSE)

rush_metric_table %>%
  kbl(caption = "Min 100 snaps") %>%
  kable_paper("hover", full_width = FALSE)



# Compare blockers and blocked players ------------------------------------

rush2join <- rush_results %>% 
  select(ends_with("Id"), jerseyNumber, team, x, y, s, a, dis, o, dir,
         side_of_ball, contains("delivered"), , pff_positionLinedUp, pff_role, contains("near_oppo"),
         dist_to_qb, data_split)

results_joined <- block_results %>% 
  inner_join(
    rush2join, 
    by = c("gameId", "playId", "frameId", "pff_nflIdBlockedPlayer" = "nflId"),
    suffix = c(".block", ".rush")
  )

results_joined %>% 
  select(ends_with("Id"), pff_nflIdBlockedPlayer, contains("pressure"))

ggplot(results_joined, aes(x=pred_pressure_allowed, y=pred_pressure_delivered)) + 
  geom_point()
