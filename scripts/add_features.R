library(tidyverse)
source("scripts/dots.R")

data_folder <- "data"

df_tracking <- readr::read_rds(file.path(data_folder, "df_tracking_origvars.rds"))
df_pocket <- readr::read_rds(file.path(data_folder, "df_pocket_sizes.rds"))
df_plays_wrangled <- readr::read_rds(file.path(data_folder, "df_plays_wrangled.rds"))
df_dists <- readr::read_rds(file.path(data_folder, "df_unnested_dists.rds"))


# create additional features ----------------------------------------------
#summarize at gameId, playId, frameId level

###from tracking data
#distance from each defender to QB, to each other
#distance from each pass blocker to QB, to each other
#s, a, dis, o of QB, rushers, blockers

#defenders to receivers - maybe just most open receiver
#number of rushers, blockers
#position of rusher, blockers - DL v LB, etc. OL vs RB vs TE, etc.


#multiple blockers blocking 1 rusher causes adding rows to tracking data in next step
rushed_ids <- df_tracking %>% 
  filter(!is.na(pff_nflIdBlockedPlayer)) %>% 
  group_by(gameId, playId, nflId) %>% 
  summarise(pff_nflIdBlockedPlayer = unique(pff_nflIdBlockedPlayer)) %>% 
  ungroup() %>% 
  select(gameId, playId, rushed_nflId = nflId, nflId = pff_nflIdBlockedPlayer) %>% 
  group_by(gameId, playId, nflId) %>% 
  summarise(rushed_nflId_list = list(rushed_nflId)) %>% 
  ungroup() %>% 
  mutate(
    rushed_nflId1 = map_dbl(rushed_nflId_list, 1),
    rushed_nflId2 = map_dbl(rushed_nflId_list, 2, .null = NA),
    rushed_nflId3 = map_dbl(rushed_nflId_list, 3, .null = NA),
    rushed_nflId4 = map_dbl(rushed_nflId_list, 4, .null = NA)
  ) %>% 
  select(-rushed_nflId_list)

#example of 3 blockers first block being on same rusher
rushed_ids %>% filter(!is.na(rushed_nflId3))
df_tracking %>% filter(gameId==2021090900, playId==583, nflId%in%c(35481, 42404, 44816)) %>% select(playId, frameId, nflId, pff_nflIdBlockedPlayer) %>% select(-frameId) %>% distinct()


df_tracking_wrushers <- df_tracking %>% 
  left_join(rushed_ids, by = c("gameId", "playId", "nflId"))


df_tracking_roles <- df_tracking_wrushers %>% 
  select(gameId, playId, frameId, nflId, playerId, 
         pff_role, pff_nflIdBlockedPlayer, starts_with("rushed_nflId"), 
         pff_positionLinedUp, event) %>% 
  mutate(across(where(is.numeric), as.integer)) %>% 
  filter(pff_role == "Pass Block" | pff_role == "Pass Rush")

#join tracking subset to dists
system.time(
  df_dists_joined <- df_dists %>%
    inner_join(
      df_tracking_roles,
      by = c("gameId", "playId", "frameId", "playerId")
    ) %>%
    inner_join(
      df_tracking_roles,
      by = c("gameId", "playId", "frameId", "otherplayerId" = "playerId"),
      suffix = c("", "_other")
    )
)#elapsed 15.97

gc()

oneframe <- df_dists_joined %>% 
  filter(gameId == min(gameId)) %>% 
  filter(playId == min(playId)) %>% 
  filter(frameId == min(frameId))

# oneframe %>%
#   filter(pff_role != pff_role_other) %>%
#   filter(pff_role == "Pass Block") %>%
#   group_by(gameId, playId, frameId, playerId) %>%
#   summarise(
#     origtrench_dist = dist[pff_nflIdBlockedPlayer == nflId_other],
#     nearest_trench_playerId = otherplayerId[dist == min(dist)],
#     nearest_trench_nflId = nflId_other[dist == min(dist)],
#     nearest_trench_position = pff_positionLinedUp_other[dist == min(dist)],
#     nearest_trench_dist = min(dist)
#   ) %>%
#   ungroup() %>% view("blockers")


# oneframe %>%
#   filter(pff_role != pff_role_other) %>%
#   filter(pff_role == "Pass Rush") %>%
#   group_by(gameId, playId, frameId, playerId) %>%
#   summarise(
#     across((starts_with("rushed_nflId") & !contains("_other")), ~unique(dist[.x==nflId_other])),
#     nearest_trench_playerId = otherplayerId[dist == min(dist)],
#     nearest_trench_nflId = nflId_other[dist == min(dist)],
#     nearest_trench_position = pff_positionLinedUp_other[dist == min(dist)],
#     nearest_trench_dist = min(dist)
#   ) %>%
#   ungroup() %>% view("rushers")

df_dists_joined <- df_dists_joined %>% filter(pff_role != pff_role_other)
#TODO - get dist from same role?

blockers_summary <- df_dists_joined %>%
  filter(pff_role == "Pass Block") %>%
  group_by(gameId, playId, frameId, playerId) %>%
  summarise(
    #origtrench_dist = dist[pff_nflIdBlockedPlayer == nflId_other], #issues with NA and missing the blocked player in other
    nearest_trench_dist = min(dist),
    #taking first Id for simplicity and few cases of 2 players same dist, nearest_trench_dist same
    nearest_trench_playerId = first(otherplayerId[dist == min(dist)]),
    nearest_trench_nflId = first(nflId_other[dist == min(dist)]), 
    nearest_trench_position = first(pff_positionLinedUp_other[dist == min(dist)])
  ) %>%
  ungroup()

rushers_summary <- df_dists_joined %>%
  filter(pff_role == "Pass Rush") %>%
  group_by(gameId, playId, frameId, playerId) %>%
  summarise(
    #across((starts_with("rushed_nflId") & !contains("_other")), ~unique(dist[.x==nflId_other])), #issues with NA and missing the blocked player in other
    nearest_trench_dist = min(dist),
    #taking first Id for simplicity and few cases of 2 players same dist, nearest_trench_dist same
    nearest_trench_playerId = first(otherplayerId[dist == min(dist)]),
    nearest_trench_nflId = first(nflId_other[dist == min(dist)]),
    nearest_trench_position = first(pff_positionLinedUp_other[dist == min(dist)])
  ) %>%
  ungroup()


# system.time(
#   rushers_summary <- rushers_summary %>% 
#     rowwise() %>% 
#     mutate(min_rushed_dist = min(rushed_nflId1, rushed_nflId2, rushed_nflId3, rushed_nflId4, na.rm=TRUE)) %>% 
#     ungroup()
# )
# system.time(
#   rushers_split <- rushers_summary %>% 
#     select(starts_with("rushed_nflId")) %>% 
#     split(seq(nrow(.)))
# ) #elapsed 52.13
# 
# system.time(
#   rushers_mins <- rushers_split %>% 
#     furrr::future_map(~min(.x, na.rm=TRUE))
# )

trench_dists <- bind_rows(blockers_summary,rushers_summary)

df_tracking_trench_dists <- df_tracking_wrushers %>% 
  left_join(trench_dists, by = c("gameId", "playId", "frameId", "playerId"))



#distance between LT and RT (TEs??)
#distance between LE, RE, LEO, REO

readr::write_rds(df_tracking_trench_dists, file.path(data_folder, "df_tracking_trench_dists.rds"))


# assemble data to model --------------------------------------------------


skim_tracking <- skimr::skim(df_tracking)
skim_plays <- skimr::skim(df_plays_wrangled)

df_joined <- df_tracking_trench_dists %>% 
  inner_join(df_plays_wrangled, by = c("gameId", "playId")) %>%
  inner_join(df_pocket, by = c("gameId", "playId", "frameId"))

df_block_2model <- df_joined %>% filter(pff_role == "Pass Block")

all(sum(is.na(df_block_2model$pressure_allowed)) == 0,
    sum(!is.na(df_block_2model$pressure_delivered)) == 0)


df_rush_2model <- df_joined %>% filter(pff_role == "Pass Rush")

all(sum(!is.na(df_rush_2model$pressure_allowed)) == 0,
    sum(is.na(df_rush_2model$pressure_delivered)) == 0)


readr::write_rds(df_block_2model, "data/df_block_2model.rds")
readr::write_rds(df_rush_2model, "data/df_rush_2model.rds")
