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

model_results <- readr::read_rds("data/model_results.rds")
block_results <- model_results$block$df_all
rush_results <- model_results$rush$df_all
rm(model_results)
gc()

df_plays <- readr::read_csv(file.path(data_folder, "plays.csv"))
df_players <- read_csv(file.path(data_folder, "players.csv"))

list_tracking <- readr::read_rds(file.path(data_folder, "list_tracking.rds"))
df_tracking  <- bind_rows(list_tracking)
rm(list_tracking)
gc()

# Analyze model results ---------------------------------------------------

#skimr::skim(df_results)

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
      frame_probchange = prob - prev_prob,
      ppoe = actual - prob
    )
}

block_results_summary <- summarise_results(block_results, "pressure_allowed")
rush_results_summary <- summarise_results(rush_results, "pressure_delivered")

df_tracking %>% 
  inner_join(select(block_results, gameId, playId, nflId, frameId), by = c("gameId", "playId", "frameId", "nflId")) %>% count(gameId, playId, frameId)


df_results <- df_tracking %>% 
  left_join(select(block_results, gameId, playId, frameId, nflId, pred_pressure_allowed), by = c("gameId", "playId", "frameId", "nflId")) %>%
  left_join(select(rush_results, gameId, playId, frameId, nflId, pred_pressure_delivered), by = c("gameId", "playId", "frameId", "nflId")) %>% 
  group_by(gameId, playId, frameId) %>% 
  filter(any(!is.na(pred_pressure_allowed))) %>% 
  filter(any(!is.na(pred_pressure_delivered))) %>% 
  ungroup()

# Visualize probability predictions ---------------------------------------


#df_for_plot <- prep_example_for_plot(df_trackwpreds, 2021090900, 97) 
df_for_plot <- prep_example_for_plot(df_results, 2021091201, 63) %>% 
  mutate(not_ball = team!="football") %>% 
  mutate(prob = ifelse(!is.na(pred_pressure_allowed), pred_pressure_allowed, pred_pressure_delivered))

#TODO add pocket size polygon
pocket_plot <- ggplot(df_for_plot, aes(x = x, y = y)) + 
  geom_point(aes(fill = prob, size = not_ball, color = team), pch = 21) +
  scale_fill_gradient(low = "white", high = "red") +
  scale_color_manual(values = c("white", "grey", "black")) +
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

ggsave(
  path = paste0(output_folder,"/"),
  filename = "draft_dots_plot.jpeg",
  plot = pocket_plot,
  device = jpeg,
  units = "in",
  width = 10,
  height = 6,
  dpi = 300
)

anim_save(animation = pocket_anim, filename = file.path(output_folder, "draft_dots_anim.gif"))

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

ppoe_summary <- function(df, target) {
  names(df)[names(df) == target] <- "actual"
  names(df)[names(df) == paste0("pred_", target)] <- "prob"
  
  df %>% 
    group_by(gameId, playId, nflId) %>% 
    summarise(
      prob = prob[frameId==min(frameId)],
      actual = unique(actual)
    ) %>% 
    ungroup() %>% 
    mutate(ppoe = actual - prob) %>% 
    group_by(nflId) %>% 
    summarise(
      snaps = n(),
      total_ppoe = sum(ppoe),
      avg_ppoe = mean(ppoe)
    ) %>% 
    arrange(desc(avg_ppoe)) %>% 
    inner_join(df_players, by = "nflId") %>% 
    filter(snaps >= 100)
}

block_ppoe <- ppoe_summary(block_results, "pressure_allowed")
rush_ppoe <- ppoe_summary(rush_results, "pressure_delivered")

ppoe_table <- function(summary_ppoe) {
  summary_ppoe %>% 
    head(10) %>% 
    mutate(
      avg_ppoe = round(avg_ppoe, 3),
      total_ppoe = round(total_ppoe, 3)
    ) %>% 
    select(
      Name = displayName,
      Position = officialPosition,
      Snaps = snaps,
      `Average PPOE` = avg_ppoe,
      `Total PPOE` = total_ppoe
    ) %>% 
    kbl(caption = "Min 100 snaps") %>%
    kable_paper("hover", full_width = FALSE)
}

block_ppoe_table <- ppoe_table(block_ppoe)
rush_ppoe_table <- ppoe_table(rush_ppoe)

#TODO - may not need to save to files, just display properly in kaggle notebook
#save_kable(block_ppoe, "block_ppoe.jpeg")
#webshot::install_phantomjs() #this got it to run
#install.packages("magick") #suggest this but still html in jpeg as output
