########################################
# analyze results
#
#
########################################


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(skimr)
source("scripts/dots.R")



# Constants ---------------------------------------------------------------

data_folder <- "data"
output_folder <- "output"

ol_positions <- hex_points[-1]

# Functions ---------------------------------------------------------------


# Read data ---------------------------------------------------------------

#source data
df_games <- read_csv(file.path(data_folder, "games.csv"))
df_plays <- read_csv(file.path(data_folder, "plays.csv"))
df_players <- read_csv(file.path(data_folder, "players.csv"))
df_pff <- read_csv(file.path(data_folder, "pffScoutingData.csv"))

list_tracking <- readr::read_rds(file.path(data_folder, "list_tracking.rds"))
df_tracking  <- bind_rows(list_tracking)
rm(list_tracking)


#results from model.R
df_results_import <- readr::read_rds(file.path(output_folder, "df_results.rds"))
df_results_player_import <- readr::read_rds(file.path(output_folder, "df_results_player.rds"))

# Analyze model results ---------------------------------------------------

#skimr::skim(df_results)


df_results <- df_results_import %>% 
  arrange(gameId, playId, frameId) %>% 
  group_by(gameId, playId) %>% 
  mutate(
    prev_pred_sack = lag(pred_sack),
    start_sack_prob = pred_sack[frameId == min(frameId)]
  ) %>% 
  ungroup() %>% 
  mutate(
    start_probchange = pred_sack - start_sack_prob,
    frame_probchange = pred_sack - prev_pred_sack,
    spoe = sack - pred_sack
  )



df_results_player <- df_results_player_import %>% 
  arrange(gameId, playId, frameId, nflId) %>% 
  group_by(gameId, playId, nflId) %>% 
  mutate(
    prev_pred_sack = lag(pred_sack),
    start_sack_prob = pred_sack[frameId == min(frameId)]
  ) %>% 
  ungroup() %>% 
  mutate(
    start_probchange = pred_sack - start_sack_prob,
    frame_probchange = pred_sack - prev_pred_sack,
    spoe = pff_sack - pred_sack
  )


df_trackwpred <- df_tracking %>% 
  inner_join(df_results, by = c("gameId", "playId", "frameId")) #%>% 
  #inner_join(df_results_player, by = c("gameId", "playId", "frameId", "nflId"), suffix = c(".play", ".def_player"))



df_plays_wpreds <- df_results %>%
  group_by(gameId, playId) %>% 
  summarise(
    start_sack_prob = unique(start_sack_prob),
    end_sack_prob = pred_sack[frameId == max(frameId)],
    sack = unique(sack),
    across(
      .cols = where(is.numeric), 
      .fns = list(avg = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)), 
      .names = "{.fn}_{.col}"
    ),
    across(
      .cols = c(pred_sack, pred_no_sack, spoe),
      .fns = sum,
      .names = "sum_{.col}"
    )
  ) %>% 
  select(
    -contains("frameId"), 
    -c(
      avg_sack, sd_sack, 
      avg_start_sack_prob, sd_start_sack_prob,
      avg_end_sack_prob, sd_end_sack_prob
    )
  ) %>% 
  inner_join(df_plays, by = c("gameId", "playId"))


# Visualize probability predictions ---------------------------------------


#new/adjust plot function to do pocket for only pcoket position even with all players
df_for_plot <- prep_example_for_plot(df_trackwpred, 2021090900, 97) 

pocket_plot <- create_pocket_plot(df_for_plot) +
  geom_text(data = df_for_plot, aes(x = mean(x), y = max(y)+ 5, label = paste("Sack Probability", scales::percent(pred_sack, accuracy = 0.01), sep = ": ")))



df_trackwpred_player <- df_tracking %>% inner_join(df_results_player, by = c("gameId", "playId", "frameId", "nflId"))

pocket_plot +
  geom_point(data = df_trackwpred, aes(x = x, y = y, size =))


# df4plot %>% 
#   filter(pred_sack == max(pred_sack)) %>% 
#   create_pocket_plot() + 
#     geom_text(data = df4plot, aes(x = mean(x), y = max(y)+ 5, label = paste("Sack Probability", scales::percent(pred_sack, accuracy = 0.01), sep = ": ")))

# TODO - why is above showing multiple sack probabilities on the plot?
max_prob <- max(df4plot$pred_sack)

example_frame_plot <- df4plot %>% 
  filter(pred_sack == max(pred_sack)) %>% 
  create_pocket_plot() + 
  geom_text(data = df4plot, aes(x = mean(x), y = max(y)+ 5, label = paste("Sack Probability", scales::percent(max_prob, accuracy = 0.01), sep = ": ")))

ggsave(
  path = "output/",
  filename = "prob_and_pocket_plot.png", 
  plot = example_frame_plot,
  device = png
)

pocket_anim <- create_pocket_animation(pocket_plot, max(df_for_plot$frameId))
