########################################
# Wrangle source data
#
#
########################################


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(skimr)
source("scripts/dots.R")


# Constants ---------------------------------------------------------------

data_folder <- "data"
data_fp <- list.files(data_folder)

start_events <- c("ball_snap", "autoevent_ballsnap")
end_events <- c("pass_forward", "autoevent_passforward", "run", "qb_sack", "qb_strip_sack", "handoff")

sorted_pocket_positions <- "C,LG,LT,QB,RG,RT"
hex_points <- c("QB", "LT", "LG", "C", "RG", "RT")
ol_positions <- hex_points[-1]
frame_group_vars <- c("gameId", "playId", "frameId")
def_roles <- c("Coverage", "Pass Rush")

# Read data ---------------------------------------------------------------


df_games <- read_csv(file.path(data_folder, "games.csv"))
df_plays <- read_csv(file.path(data_folder, "plays.csv"))
df_players <- read_csv(file.path(data_folder, "players.csv"))
df_pff <- read_csv(file.path(data_folder, "pffScoutingData.csv"))

fp_tracking <- paste0(data_folder, "/",data_fp[str_detect(data_fp, "week")])

# #ran once
# list_tracking <- map(fp_tracking, ~read_csv(.x)) 
# readr::write_rds(list_tracking, file.path(data_folder, "list_tracking.rds"))

list_tracking <- readr::read_rds(file.path(data_folder, "list_tracking.rds"))
df_tracking  <- bind_rows(list_tracking)
rm(list_tracking)


# Target variable ---------------------------------------------------------

df_plays %>% count(passResult, sort = T)
df_plays <- df_plays %>% mutate(sack = ifelse(passResult == "S", 1, 0))

df_pff %>% 
  filter(pff_positionLinedUp %in% ol_positions) %>% 
  select(pff_beatenByDefender, pff_hitAllowed, pff_hurryAllowed, pff_sackAllowed) %>% 
  summary()

df_pff <- df_pff %>% 
  mutate(
    pressure_allowed = pff_hurryAllowed + pff_hitAllowed + pff_sackAllowed,
    pressure_delivered = pff_hurry + pff_hit + pff_sack
  )

# Frames of interest ------------------------------------------------------

df_tracking <- df_tracking %>% 
  group_by(gameId, playId) %>% 
  mutate(
    start_frame = min(frameId[event %in% start_events]),
    end_frame = min(frameId[event %in% end_events])
  ) %>% 
  filter(frameId >= start_frame & frameId <= end_frame) %>%
  ungroup()

#skim_tracking <- skimr::skim(df_tracking)


# Calculate pocket size ---------------------------------------------------

#https://www.wikihow.com/Calculate-the-Area-of-a-Hexagon

#collect pass blocking offensive linemen and passing quarterbacks
df_pff_pocket <- df_pff %>% 
  filter(
    (pff_positionLinedUp %in% ol_positions & pff_role == "Pass Block") |
      pff_positionLinedUp == "QB" & pff_role == "Pass"
  )

#write_rds(df_pff_pocket, "data/df_pff_pocket.rds")


#filter to blocking OL and passing QB

# df_pocket <- df_tracking %>% 
#   inner_join(
#     select(df_pff_pocket, gameId, playId, nflId, pff_positionLinedUp), 
#     by = c("gameId", "playId", "nflId")
#   )

df_pocket <- filter_to_pocket(df_tracking, df_pff_pocket)


#filter to only plays with 5 blocking OL and 1 passing QB
df_pocket <- df_pocket %>% 
  group_by(gameId, playId) %>%
  mutate(positions = paste(sort(unique(pff_positionLinedUp)), collapse = ",")) %>% 
  filter(positions == sorted_pocket_positions) %>% 
  ungroup() %>% 
  select(-positions) %>% 
  rename(position = pff_positionLinedUp)

#add columns for x and y positions of OL and QB
df_pocket <- df_pocket %>% 
  group_by(gameId, playId, frameId) %>% 
  mutate(
    across(.cols = c(x,y), .fns = function(z) z[position == "QB"], .names = "{.col}_QB"),
    across(.cols = c(x,y), .fns = function(z) z[position == "LT"], .names = "{.col}_LT"),
    across(.cols = c(x,y), .fns = function(z) z[position == "LG"], .names = "{.col}_LG"),
    across(.cols = c(x,y), .fns = function(z) z[position == "C"], .names = "{.col}_C"),
    across(.cols = c(x,y), .fns = function(z) z[position == "RG"], .names = "{.col}_RG"),
    across(.cols = c(x,y), .fns = function(z) z[position == "RT"], .names = "{.col}_RT")
  ) %>% 
  ungroup()


#add columns of next and starting coordinates of hexagon
df_pocket <- df_pocket %>% 
  #select(gameId, playId, frameId, position, x, y) %>% 
  arrange(gameId, playId, frameId, factor(position, levels = hex_points)) %>% 
  group_by(gameId, playId, frameId) %>% 
  mutate(x_next = lead(x), y_next = lead(y)) %>% #print(n=100)
  mutate(x_start = x[position == hex_points[1]], y_start = y[position == hex_points[1]]) %>% 
  ungroup() %>% 
  mutate(
    x_next = ifelse(is.na(x_next), x_start, x_next),
    y_next = ifelse(is.na(y_next), y_start, y_next)
  ) %>% 
  select(-x_start, -y_start) 

#calculate area of hexagon / pocket size
df_pocket_sizes <- df_pocket %>% 
  mutate(
    x_times_next_y = x * y_next,
    y_times_next_x = y * x_next
  ) %>% #print(n=100)
  group_by(gameId, playId, frameId) %>% 
  summarise(
    sum_a = sum(x_times_next_y), 
    sum_b = sum(y_times_next_x)
  ) %>% 
  mutate(pocket_size = abs(sum_a - sum_b)/2) %>% #square yards
  select(-sum_a, -sum_b) %>% 
  ungroup()

#join pocket sizes onto OL/QB tracking data
df_pocket <- df_pocket %>% 
  inner_join(df_pocket_sizes, by = c("gameId", "playId", "frameId"))


# Calculate distances -----------------------------------------------------

df_tracking_dists <- df_tracking %>% 
  inner_join(
    select(df_pff, gameId, playId, nflId, pff_positionLinedUp, pff_role, pressure_allowed, pressure_delivered), 
    by = c("gameId", "playId", "nflId")
  ) %>% 
  group_by(gameId, playId, frameId) %>% 
  mutate(across(.cols = c(x,y), .fns = function(z) z[pff_positionLinedUp == "QB"], .names = "{.col}_QB")) %>% 
  ungroup()


df_tracking_dists$dist_to_qb <- calc_dist(
  x1 = df_tracking_dists$x,
  y1 = df_tracking_dists$y,
  x2 = df_tracking_dists$x_QB,
  y2 = df_tracking_dists$y_QB
)


# create additional features ----------------------------------------------
#summarize at gameId, playId, frameId level

###from tracking data
#distance from each defender to QB, to each other
#distance from each pass blocker to QB, to each other
#s, a, dis, o of QB, rushers, blockers
#QB distance from sideline
#defenders to receivers - maybe just most open receiver
#number of rushers, blockers
#position of rusher, blockers - DL v LB, etc. OL vs RB vs TE, etc.



# Prep plays data ---------------------------------------------------------

off_personnels <- keep_cats(df_plays, "personnelO", 0.9)
def_personnels <- keep_cats(df_plays, "personnelD", 0.9)



df_plays_wrangled <- df_plays %>% 
  filter(is.na(foulName1)) %>% 
  filter(dropBackType != "UNKNOWN" & dropBackType != "DESIGNED_RUN") %>% 
  filter(offenseFormation != "WILDCAT") %>% 
  #filter(quarter <= xxx) %>% 
  filter(down != 0) %>% 
  #filter(yardsToGo <= xxx) %>% 
  mutate(
    off_personnel = ifelse(personnelO %in% off_personnels, personnelO, "Other"),
    def_personnel = ifelse(personnelD %in% def_personnels, personnelD, "Other")
  ) %>% 
  mutate(
    dropBackCategory = case_when(
      str_detect(dropBackType, "DESIGNED_ROLLOUT") ~ "rollout",
      str_detect(dropBackType, "SCRAMBLE") ~ "scramble",
      TRUE ~ "traditional"
    )
  ) %>% 
  mutate(coverage = ifelse(pff_passCoverageType == "Other", "Other", pff_passCoverage)) %>% 
  mutate(yards_to_endzone = absoluteYardlineNumber - 10) %>% 
  select(
    gameId, playId,
    offenseFormation, personnelO, personnelD, dropBackType, pff_passCoverage, pff_passCoverageType,
    quarter, down, yardsToGo, absoluteYardlineNumber, defendersInBox, pff_playAction
  )


  
  
# join model data set -----------------------------------------------------


df_2model_player <- df_tracking_dists %>% 
  filter(pff_role %in% def_roles) %>% 
  select(contains("Id"), dist_to_qb, pff_sack)


df_2model <- df_pocket_sizes %>% 
  inner_join(select(df_plays, gameId, playId, sack), by = c("gameId", "playId"))

#readr::write_rds(df_2model, file.path(data_folder, "df_2model.rds"))
#readr::write_rds(df_2model_player, file.path(data_folder, "df_2model_player.rds"))
