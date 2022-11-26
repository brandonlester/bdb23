#wrangle
library(tidyverse)
library(skimr)
source("dots.R")

data_folder <- "data"
data_fp <- list.files(data_folder)

df_games <- read_csv(file.path(data_folder, "games.csv"))
df_plays <- read_csv(file.path(data_folder, "plays.csv"))
df_players <- read_csv(file.path(data_folder, "players.csv"))
df_pff <- read_csv(file.path(data_folder, "pffScoutingData.csv"))

fp_tracking <- paste0(data_folder, "/",data_fp[str_detect(data_fp, "week")])
list_tracking <- map(fp_tracking, ~read_csv(.x))
df_tracking  <- bind_rows(list_tracking)
rm(tracking_list, data_fp, fp_tracking)

skim_tracking  <- skimr::skim(df_tracking)
skim_tracking %>% select(-numeric.hist)


df_plays %>% 
  filter(dropBackType == "TRADITIONAL") %>%
  filter(is.na(penaltyYards))

start_events <- c("ball_snap", "autoevent_ballsnap")
end_events <- c("pass_forward", "autoevent_passforward", "run", "qb_sack", "qb_strip_sack", "handoff")

ol_positions <- c("RT", "RG", "C", "LG", "LT")

#target variable
df_plays %>% count(passResult, sort = T)

df_pff %>% 
  filter(pff_positionLinedUp %in% ol_positions) %>% 
  select(pff_beatenByDefender, pff_hitAllowed, pff_hurryAllowed, pff_sackAllowed) %>% 
  summary()


#frams of interest
df_tracking_foi <- df_tracking %>% 
  #head(130) %>% 
  group_by(gameId, playId) %>% 
  mutate(
    start_frame = min(frameId[event %in% start_events]),
    end_frame = min(frameId[event %in% end_events])
  ) %>% 
  filter(frameId >= start_frame & frameId <= end_frame) %>% #TODO - confirm Infs from mutate above filtered out fine
  ungroup()

#TODO - remove df_tracking once above confirmed
