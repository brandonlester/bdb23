#loading command to make NFL field in ggplot (credit to Marschall Furman)
source("https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R")
library(gganimate)


hex_points <- c("QB", "LT", "LG", "C", "RG", "RT")

calc_dist <- function(x1, x2, y1, y2) sqrt( (x2 - x1)^2 + (y2 - y1)^2 )

keep_cats <- function(df, cat, pct) {
  df %>% 
    count(!!as.name(cat), sort = TRUE) %>% 
    mutate(total_plays = nrow(df)) %>% 
    mutate(npct = n / total_plays) %>% 
    mutate(npct_cumsum = cumsum(npct)) %>% 
    filter(npct_cumsum <= pct) %>% 
    pull(!!as.name(cat))
}

get_player_coords_1 <- function(df) {
  #elapsed 46.95
  df %>% 
    select(contains("Id"), x, y) %>% 
    group_by(gameId, playId, frameId) %>%
    mutate(
      x1 = x[playerId == 1],
      x2 = x[playerId == 2],
      x3 = x[playerId == 3],
      x4 = x[playerId == 4],
      x5 = x[playerId == 5],
      x6 = x[playerId == 6],
      x7 = x[playerId == 7],
      x8 = x[playerId == 8],
      x9 = x[playerId == 9],
      x10 = x[playerId == 10],
      x11 = x[playerId == 11],
      x12 = x[playerId == 12],
      x13 = x[playerId == 13],
      x14 = x[playerId == 14],
      x15 = x[playerId == 15],
      x16 = x[playerId == 16],
      x17 = x[playerId == 17],
      x18 = x[playerId == 18],
      x19 = x[playerId == 19],
      x20 = x[playerId == 20],
      x21 = x[playerId == 21],
      x22 = x[playerId == 22],
    ) %>% 
    mutate(
      y1 = y[playerId == 1],
      y2 = y[playerId == 2],
      y3 = y[playerId == 3],
      y4 = y[playerId == 4],
      y5 = y[playerId == 5],
      y6 = y[playerId == 6],
      y7 = y[playerId == 7],
      y8 = y[playerId == 8],
      y9 = y[playerId == 9],
      y10 = y[playerId == 10],
      y11 = y[playerId == 11],
      y12 = y[playerId == 12],
      y13 = y[playerId == 13],
      y14 = y[playerId == 14],
      y15 = y[playerId == 15],
      y16 = y[playerId == 16],
      y17 = y[playerId == 17],
      y18 = y[playerId == 18],
      y19 = y[playerId == 19],
      y20 = y[playerId == 20],
      y21 = y[playerId == 21],
      y22 = y[playerId == 22],
    ) %>% 
    ungroup()
}

get_player_coords_2 <- function(df) {
  #elapsed 54.68
  df_grp <- df %>% 
    select(contains("Id"), x, y) %>% 
    group_by(gameId, playId, frameId) %>% 
    mutate(playerId = dplyr::row_number())
  
  for(i in 1:22) df_grp <- df_grp %>% mutate("x{i}" := x[playerId == i])
  for(i in 1:22) df_grp <- df_grp %>% mutate("y{i}" := y[playerId == i])
  
  return(ungroup(df_grp))
}


get_data_on_pid <- function(df, get_vars) {
  
  df_grp <- df %>% group_by(gameId, playId, frameId)
  
  for(i in 1:22) {
    df_grp <- df_grp %>% 
      mutate(
        across(
          .cols = all_of(get_vars), 
          .fns = ~.x[playerId == i], 
          .names = paste("{col}", i, sep = "_")
        )
      )
  }
  
  return(ungroup(df_grp))
}



# distance functions ------------------------------------------------------

calc_frame_dists <- function(iframe) {
  require(distances)
  iframe %>% 
    select(x,y) %>% 
    as.matrix() %>% 
    distances::distances() 
}

tidy_frame_dists <- function(iframedists) {
  iframedists %>%
    distances::distance_columns(1:23) %>%  
    as.data.frame() %>%
    rownames_to_column(var = "playerId") %>%
    mutate(playerId = as.integer(playerId)) %>%
    pivot_longer(-playerId, names_to = "otherplayerId", values_to = "dist") %>%
    mutate(otherplayerId = as.integer(otherplayerId)) %>% 
    as.data.frame()
}

get_dist <- function(frame_dists, pid1, pid2) as.numeric(distances::distance_columns(frame_dists, pid1, pid2))




# foundation of tracking animations ---------------------------------------

base_field_plot <- function(yard_start=0, yard_end=122) {
  #attributes used for plot. first is away, second is football, third is home.
  cols_fill <- c("dodgerblue1", "#663300", "firebrick1")
  cols_col <- c("#000000", "#663300", "#000000")
  size_vals <- c(6, 4, 6)
  shape_vals <- c(21, 16, 21)
  
  #plotting
  ggplot() +
    
    
    #creating field underlay
    gg_field(yardmin = yard_start, yardmax = yard_end) +
    
    #filling forest green for behind back of endzone
    theme(panel.background = element_rect(fill = "forestgreen",
                                          color = "forestgreen"),
          panel.grid = element_blank()) +
    
    
    #setting size and color parameters
    scale_size_manual(values = size_vals, guide = "none") +
    scale_shape_manual(values = shape_vals, guide = "none") +
    scale_fill_manual(values = cols_fill, guide = "none") +
    scale_colour_manual(values = cols_col, guide = "none")
  
}

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
    std_coords()
}

create_pocket_plot <- function(df_for_plot) {
  df_for_plot_pocket <- df_for_plot %>% 
    inner_join(
      select(df_pff_pocket, gameId, playId, nflId, pff_positionLinedUp), 
      by = c("gameId", "playId", "nflId")
    ) %>% 
    arrange(factor(pff_positionLinedUp, levels = hex_points))
  
  ggplot(data = filter(df_for_plot, side_of_ball == "off"), aes(x = x, y = y)) + 
    geom_polygon(data = df_for_plot_pocket, aes(x = x, y = y), fill = "blue", alpha = 0.1) +
    geom_point(aes(fill = pred_pressure_allowed, size = size_of_point), shape = 21) +
    scale_fill_gradient("off", low = "white", high = "blue") +
    new_scale("fill") +
    geom_point(data = filter(df_for_plot, side_of_ball == "def"), aes(fill = pred_pressure_delivered, size = size_of_point), shape = 21) +
    scale_fill_gradient("def", low = "white", high = "orange") +
    geom_text(data = df_for_plot, aes(x = x, y = y, label = jerseyNumber, size = size_of_point*0.5), color = "black", vjust = 0.35) +
    scale_radius() +
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
      panel.grid.minor.y = element_blank()#,
      #plot.title = element_text(size = 6)
    )
}