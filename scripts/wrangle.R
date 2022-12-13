#wrangle
library(tidyverse)
library(skimr)
source("scripts/dots.R")

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


#frames of interest
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
rm(df_tracking)
rm(skim_tracking)

skim_tracking_foi <- skimr::skim(df_tracking_foi)


#pocket size

df_pass_pb <- df_pff %>% 
  filter(pff_role == "Pass Block") %>% 
  select(contains("Id"), pff_positionLinedUp, pff_blockType, pff_backFieldBlock)

df_pass_pb %>% count(pff_blockType)

df_pff_pocket_positions <- df_pff %>% 
  filter(
    (pff_positionLinedUp %in% ol_positions & pff_role == "Pass Block") |
      pff_positionLinedUp == "QB" & pff_role == "Pass"
  )

df_tracking_pocket_positions <- df_tracking_foi %>% 
  inner_join(
    select(df_pff_pocket_positions, gameId, playId, nflId, pff_positionLinedUp), 
    by = c("gameId", "playId", "nflId")
  )

sorted_pocket_positions <- "C,LG,LT,QB,RG,RT"

df_tracking_pocket_plays <- df_tracking_pocket_positions %>% 
  group_by(gameId, playId) %>%
  mutate(positions = paste(sort(unique(pff_positionLinedUp)), collapse = ",")) %>% 
  filter(positions == sorted_pocket_positions) %>% 
  ungroup() %>% 
  select(-positions) %>% 
  rename(position = pff_positionLinedUp)

df_tracking_pocket <- df_tracking_pocket_plays %>% 
  group_by(gameId, playId, frameId) %>% 
  summarise(
    across(.cols = c(x,y), .fns = function(z) z[position == "QB"], .names = "{.col}_QB"),
    across(.cols = c(x,y), .fns = function(z) z[position == "LT"], .names = "{.col}_LT"),
    across(.cols = c(x,y), .fns = function(z) z[position == "LG"], .names = "{.col}_LG"),
    across(.cols = c(x,y), .fns = function(z) z[position == "C"], .names = "{.col}_C"),
    across(.cols = c(x,y), .fns = function(z) z[position == "RG"], .names = "{.col}_RG"),
    across(.cols = c(x,y), .fns = function(z) z[position == "RT"], .names = "{.col}_RT")
  )


# df_tracking_pocket %>% 
#   pivot_longer(cols = -c(gameId, playId, frameId)) %>% 
#   mutate(
#     axis = str_sub(name, end = 1),
#     position = str_sub(name, start = 3)
#   ) %>% 
#   select(-name) %>% 
#   pivot_wider(id_cols = c(gameId, playId, frameId, position), names_from = axis, values_from = value)


hex_points <- c("QB", "LT", "LG", "C", "RG", "RT")

example_play_hex <- df_tracking_pocket_plays %>%
  select(gameId, playId, frameId, position, x, y) %>% 
  arrange(gameId, playId, frameId) %>% head(6) #%>% 
  #ggplot(aes(x, y)) + geom_point()

frame_group_vars <- c("gameId", "playId", "frameId")

#https://www.wikihow.com/Calculate-the-Area-of-a-Hexagon
df_frame_pocket_size <- df_tracking_pocket_plays %>% 
  select(gameId, playId, frameId, position, x, y) %>% 
  arrange(gameId, playId, frameId, factor(position, levels = hex_points)) %>% 
  group_by(gameId, playId, frameId) %>% 
  mutate(x_next = lead(x), y_next = lead(y)) %>% #print(n=100)
  mutate(
    x_start = x[position == hex_points[1]], 
    y_start = y[position == hex_points[1]],
    x_next = ifelse(is.na(x_next), x_start, x_next),
    y_next = ifelse(is.na(y_next), y_start, y_next)
  ) %>% 
  select(-x_start, -y_start) %>% 
  mutate(
    x_times_next_y = x * y_next,
    y_times_next_x = y * x_next
  ) %>% #print(n=100)
  summarise(
    sum_a = sum(x_times_next_y), 
    sum_b = sum(y_times_next_x)
  ) %>% 
  mutate(pocket_size = abs(sum_a - sum_b)/2) %>% #square yards
  select(-sum_a, -sum_b) %>% 
  ungroup()

  

df_pocket_w_size <- df_tracking_pocket_positions %>% 
  inner_join(df_frame_pocket_size, by = c("gameId", "playId", "frameId"))


xtemp <- df_pocket_w_size %>% 
  arrange(gameId, playId, frameId, factor(pff_positionLinedUp, levels = hex_points)) %>% 
  filter(gameId == 2021090900 & playId == 97) %>% #& frameId == 12) %>% 
  #filter(gameId == 2021091201 & playId == 2126) %>% #smallest pocket size
  inner_join(select(df_plays, gameId, playId, playDescription)) %>% 
  std_coords()

bfp <- base_field_plot(min(xtemp$x)-10, max(xtemp$x)+10)

pocket_plot <- bfp +
  geom_polygon(data = xtemp, aes(x = x, y = y)) +
  geom_text(data = xtemp, aes(x = mean(x), y = max(y)+ 3, label = round(pocket_size,2))) +
  #adding players
  geom_point(
    data = xtemp, 
    aes(x=x, y=y, shape=team, fill=team, group=nflId, size=team, colour=team), 
    alpha = 0.7
  ) +  
  #adding jersey numbers
  geom_text(
    data = xtemp,
    aes(x = x, y = y, label = jerseyNumber),
    colour = "white", vjust = 0.36, size = 3.5
  ) +
  ggtitle(xtemp$playDescription)

pocket_anim <- pocket_plot +
  transition_time(frameId)  +
  ease_aes("linear") + 
  NULL


play_pocket_anim <- animate(pocket_anim, width = 720, height = 440,
          fps = 10, nframes = max(xtemp$frameId),
          renderer = gifski_renderer())
  
anim_save("output/pocket_size.gif", play_pocket_anim)   
