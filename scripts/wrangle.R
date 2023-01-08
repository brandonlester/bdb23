########################################
# Wrangle source data
#
#
########################################


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(skimr)
library(distances)
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
df_tracking_import  <- bind_rows(list_tracking)
rm(list_tracking)

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
  # mutate(
  #   off_personnel = ifelse(personnelO %in% off_personnels, personnelO, "Other"),
  #   def_personnel = ifelse(personnelD %in% def_personnels, personnelD, "Other")
  # ) %>% 
  mutate(
    num_rb = str_remove(str_extract(personnelO, "[0-9] RB"), " RB"),
    num_te = str_remove(str_extract(personnelO, "[0-9] TE"), " TE"),
    num_wr = str_remove(str_extract(personnelO, "[0-9] WR"), " WR"),
  ) %>% 
  mutate(
    num_dl = str_remove(str_extract(personnelD, "[0-9] DL"), " DL"),
    num_lb = str_remove(str_extract(personnelD, "[0-9] LB"), " LB"),
    num_db = str_remove(str_extract(personnelD, "[0-9] DB"), " DB"),
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
    offenseFormation, num_rb, num_te, num_wr, num_dl, num_lb, num_db, dropBackCategory, pff_passCoverageType,
    quarter, down, yardsToGo, yards_to_endzone, defendersInBox, pff_playAction
  )


# Target variable ---------------------------------------------------------

#df_plays %>% count(passResult, sort = T)
df_plays <- df_plays %>% mutate(sack = ifelse(passResult == "S", 1, 0))

# df_pff %>% 
#   filter(pff_positionLinedUp %in% ol_positions) %>% 
#   select(pff_beatenByDefender, pff_hitAllowed, pff_hurryAllowed, pff_sackAllowed) %>% 
#   summary()

df_pff <- df_pff %>% 
  mutate(
    pressure_allowed = pff_hurryAllowed + pff_hitAllowed + pff_sackAllowed,
    pressure_delivered = pff_hurry + pff_hit + pff_sack
  )

# Frames of interest ------------------------------------------------------

df_tracking <- df_tracking_import %>% 
  #filter to relevant plays
  inner_join(select(df_plays_wrangled, gameId, playId), by = c("gameId", "playId")) %>% 
  group_by(gameId, playId) %>% 
  mutate(
    start_frame = min(frameId[event %in% start_events]),
    end_frame = min(frameId[event %in% end_events])
  ) %>% 
  #filter to relevant frames
  filter(frameId >= start_frame & frameId <= end_frame) %>%
  ungroup()

readr::write_rds(df_tracking, "data/df_tracking_foi.rds")
#skim_tracking <- skimr::skim(df_tracking)


# Calculate pocket size ---------------------------------------------------

#https://www.wikihow.com/Calculate-the-Area-of-a-Hexagon

#collect pass blocking offensive linemen and passing quarterbacks
df_pff_pocket <- df_pff %>% 
  filter(
    (pff_positionLinedUp %in% ol_positions & pff_role == "Pass Block") |
      pff_positionLinedUp == "QB" & pff_role == "Pass"
  )

#filter to blocking OL and passing QB
df_pocket <- df_tracking %>%
  inner_join(
    select(df_pff_pocket, gameId, playId, nflId, pff_positionLinedUp),
    by = c("gameId", "playId", "nflId")
  )


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
df_pocket <- df_pocket %>% 
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



# Archive previous distance calc method -----------------------------------

# df_tracking_dists <- df_tracking_dists %>% 
#   group_by(gameId, playId, frameId) %>% 
#   mutate(playerId = dplyr::row_number()) %>% 
#   ungroup()
# 
# system.time(
#   df_tracking_dists <- get_data_on_pid(df_tracking_dists, c("x", "y"))
# )
# 
# # coord_names <- names(df_tracking_dists)[grep("[x_y_][0-9]{1,2}", names(df_tracking_dists))]
# # 
# # df_tracking_dists %>% 
# #   tidyr::nest(data = any_of(coord_names))
# 
# #get all 44 coordinates onto every row
# system.time(
#   df_coords <- get_player_coords_1(df_tracking_dists)
# )
# readr::write_rds(df_coords, file.path(data_folder, "df_coords.rds"))
# 
# df_coords <- readr::read_rds(file.path(data_folder, "df_coords.rds"))
# 
# df_tracking_dists %>% 
#   inner_join(df_coords, by = c("gameId", ""))
# 
# #using all coordinates to calculate distances from every player
# for(i in 1:22) {
#   i_dists <- calc_dist(
#     x1 = df_coords[["x"]],
#     y1 = df_coords[["y"]],
#     x2 = df_coords[[paste0("x",i)]],
#     y2 = df_coords[[paste0("y",i)]]
#   )
#   
#   df_coords[[paste0("dist_from_", i)]] <- i_dists
# }
# 
# xc_names <- names(df_coords)[grep("x[0-9]{1,2}", names(df_coords))]
# yc_names <- names(df_coords)[grep("y[0-9]{1,2}", names(df_coords))]
# 
# df_tracking_dists <- df_coords %>% 
#   select(-all_of(c(xc_names, yc_names)))
# 
# df_coords <- df_coords %>% 
#   select(contains("Id"), all_of(c(xc_names, yc_names)))



# df_tracking_sob <- df_tracking_dists %>%
#   inner_join(select(df_plays, gameId, playId, possessionTeam), by = c("gameId", "playId")) %>% 
#   mutate(side_of_ball = ifelse(team == possessionTeam, "off", "def"))
# 
# 
# df_tracking_multisob <- get_data_on_pid(df_tracking_sob, "side_of_ball")
# 
# 
# # df_sob_ids <- df_tracking_sob %>% 
# #   group_by(gameId, playId, frameId) %>% 
# #   summarise(
# #     off_pids = list(playerId[side_of_ball == "off"]),
# #     def_pids = list(playerId[side_of_ball == "def"])
# #   ) %>% 
# #   ungroup()





# Calculate distances -----------------------------------------------------

# #create and write nested tracking with distances data frame
# df_tracknest <- df_tracking %>% 
#   select(contains("Id"), x, y) %>% 
#   arrange(gameId, playId, frameId, nflId) %>% 
#   group_by(gameId, playId, frameId) %>% 
#   mutate(playerId = dplyr::row_number()) %>% 
#   tidyr::nest() %>% 
#   ungroup()
# 
# system.time(
#   dist_list <- furrr::future_map(df_tracknest$data, calc_frame_dists)
# ) 
# #elapsed 622.52 when df_tracknest was 274,919 rows
# #elapsed 559.45 when df_tracknest was 246,321 rows
# 
# system.time(
#   dist_list_tidy <- furrr::future_map(dist_list, tidy_frame_dists)
# )
# #elapsed 1757.97 when df_tracknest was 246,321 rows
# 
# df_tracknest$dists <- dist_list_tidy
# 
# readr::write_rds(df_tracknest, "data/df_tracknest.rds")

# ready nested tracking with distances data frame
df_tracknest <- readr::read_rds("data/df_tracknest.rds")
#df_tracknest_2frames <- head(df_tracknest, 2)

#add player Ids to main tracking data
df_pids <- unnest(df_tracknest, cols = c(data))
df_pids <- df_pids %>% select(-dists, -x, -y)
df_tracking <- df_tracking %>% inner_join(df_pids, by = c("gameId", "playId", "frameId", "nflId"))
rm(df_pids)


df_tracking <- df_tracking %>% 
  inner_join(select(df_plays, gameId, playId, possessionTeam), by = c("gameId", "playId")) %>% 
  mutate(side_of_ball = ifelse(team == possessionTeam, "o", "d")) %>% 
  #removes ball
  inner_join(select(df_pff, contains("Id"), contains("pressure"), pff_positionLinedUp, pff_role), by = c("gameId", "playId", "nflId"))


# df_track_sob <- df_tracking %>% select(contains("Id"), side_of_ball) %>% select(-pff_nflIdBlockedPlayer)
# #run on the whole df
# system.time(
#   df_nearoppo <- df_tracknest %>% 
#     unnest(cols = dists) %>% 
#     select(-data) %>% 
#     filter(playerId != otherplayerId) %>% 
#     inner_join(
#       df_track_sob, 
#       by = c("gameId", "playId", "frameId", "playerId")
#     ) %>% 
#     inner_join(
#       df_track_sob, 
#       by = c("gameId", "playId", "frameId", "otherplayerId" = "playerId"), 
#       suffix = c("", "_other")
#     ) %>% 
#     filter(side_of_ball != side_of_ball_other) %>% 
#     group_by(gameId, playId, frameId, playerId) %>% 
#     summarise(
#       near_oppo_pid = otherplayerId[dist == min(dist)],
#       near_oppo_nflId = nflId_other[dist == min(dist)],
#       near_oppo_dist = min(dist)
#     ) %>% 
#     ungroup()
# ) #elapsed 306.88
# 
# system.time(readr::write_rds(df_nearoppo, "data/df_nearoppo.rds")) #elapsed 0.81

df_nearoppo <- readr::read_rds("data/df_nearoppo.rds")

df_tracking <- df_tracking %>% inner_join(df_nearoppo, by = c("gameId", "playId", "frameId", "playerId"))

#create column for few instances where player same distance from 2 opposing players
#then filter to lower nflId number - arbitrary
df_tracking <- df_tracking %>% 
  group_by(gameId, playId, frameId, playerId) %>% 
  mutate(other_near_oppo_nflId = ifelse(n() > 1, max(near_oppo_nflId), NA)) %>% 
  filter(near_oppo_nflId == min(near_oppo_nflId)) %>% 
  ungroup()


#add QB coordinate columns
df_tracking <- df_tracking %>% 
  group_by(gameId, playId, frameId) %>% 
  mutate(across(.cols = c(x,y), .fns = function(z) z[pff_positionLinedUp == "QB"], .names = "{.col}_QB")) %>% 
  ungroup()

#calc distance from QB for all players in every frame
df_tracking$dist_to_qb <- calc_dist(
  x1 = df_tracking$x,
  y1 = df_tracking$y,
  x2 = df_tracking$x_QB,
  y2 = df_tracking$y_QB
)

y_max <- 53.3

#additional features
df_tracking <- df_tracking %>% 
  group_by(gameId, playId, frameId) %>% 
  mutate(
    most_open_rec = min(near_oppo_dist[pff_role == "Pass Route"]),
    num_blockers = length(nflId[pff_role == "Pass Block"]),
    num_rushers = length(nflId[pff_role == "Pass Rush"])
  ) %>% 
  ungroup() %>% 
  mutate(qb_to_sideline = ifelse(y_QB > y_max/2, y_max - y_QB, y_QB)) %>% 
  mutate(frames_from_start = frameId - start_frame)

#save distances
df_dists <- df_tracknest %>%
  unnest(cols = dists) %>%
  select(-data) %>%
  filter(playerId != otherplayerId) %>% 
  mutate(across(-dist, as.integer))

readr::write_rds(df_dists, file.path(data_folder, "df_unnested_dists.rds"))

readr::write_rds(df_tracking, file.path(data_folder, "df_tracking_origvars.rds"))
readr::write_rds(df_pocket, file.path(data_folder, "df_pocket_sizes.rds"))
readr::write_rds(df_plays_wrangled, file.path(data_folder, "df_plays_wrangled.rds"))
