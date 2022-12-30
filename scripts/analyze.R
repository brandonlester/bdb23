########################################
# analyze results
#
#
########################################


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(skimr)
source("scripts/dots.R")
library(kableExtra)



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


df_trackwpreds <- df_tracking %>% 
  inner_join(df_results, by = c("gameId", "playId", "frameId")) %>% 
  left_join(df_results_player, by = c("gameId", "playId", "frameId", "nflId"), suffix = c(".play", ".def_player"))

#df_trackwpred_player <- df_tracking %>% inner_join(df_results_player, by = c("gameId", "playId", "frameId", "nflId"))


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


#df_for_plot <- prep_example_for_plot(df_trackwpreds, 2021090900, 97) 
df_for_plot <- prep_example_for_plot(df_trackwpreds, 2021091201, 63) %>% 
  mutate(not_ball = team!="football")

pocket_plot <- ggplot(df_for_plot, aes(x = x, y = y)) + 
  geom_point(aes(fill = pred_sack.def_player, size = not_ball), pch = 21) +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(x = x, y = y, label = jerseyNumber), color = "black", size = 3.5, vjust = 0.35) +
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
    panel.grid.minor.y = element_blank()
  )

pocket_anim <- create_pocket_animation(pocket_plot, max(df_for_plot$frameId))


# pocket_plot <- create_pocket_plot(df_for_plot) +
#   geom_text(data = df_for_plot, aes(x = mean(x), y = max(y)+ 5, label = paste("Sack Probability", scales::percent(pred_sack.play, accuracy = 0.01), sep = ": ")))

# df4plot %>% 
#   filter(pred_sack == max(pred_sack)) %>% 
#   create_pocket_plot() + 
#     geom_text(data = df4plot, aes(x = mean(x), y = max(y)+ 5, label = paste("Sack Probability", scales::percent(pred_sack, accuracy = 0.01), sep = ": ")))

# # TODO - why is above showing multiple sack probabilities on the plot?
# max_prob <- max(df4plot$pred_sack)
# 
# example_frame_plot <- df4plot %>% 
#   filter(pred_sack == max(pred_sack)) %>% 
#   create_pocket_plot() + 
#   geom_text(data = df4plot, aes(x = mean(x), y = max(y)+ 5, label = paste("Sack Probability", scales::percent(max_prob, accuracy = 0.01), sep = ": ")))

# ggsave(
#   path = "output/",
#   filename = "prob_and_pocket_plot.png", 
#   plot = example_frame_plot,
#   device = png
# )
# 
# pocket_anim <- create_pocket_animation(pocket_plot, max(df_for_plot$frameId))


# Final metrics -----------------------------------------------------------

spoe_rankings <- df_results_player %>% 
  group_by(gameId, playId, nflId) %>% 
  summarise(
    sack_prob = unique(start_sack_prob),
    sack = unique(pff_sack)
  ) %>% 
  ungroup() %>% 
  mutate(spoe = sack - sack_prob) %>% 
  group_by(nflId) %>% 
  summarise(
    snaps = n(),
    total_spoe = sum(spoe),
    avg_spoe = mean(spoe)
  ) %>% 
  arrange(desc(avg_spoe)) %>% 
  inner_join(df_players, by = "nflId") %>% 
  filter(snaps >= 100)

summary(spoe_rankings$snaps)

spoe_table <- spoe_rankings %>% 
  head(10) %>% 
  mutate(
    avg_spoe = round(avg_spoe, 3),
    total_spoe = round(total_spoe, 3)
  ) %>% 
  select(
    Name = displayName,
    Position = officialPosition,
    Snaps = snaps,
    `Average SPOE` = avg_spoe,
    `Total SPOE` = total_spoe
  ) %>% 
  kbl(caption = "Min 100 snaps") %>%
  kable_paper("hover", full_width = FALSE)
