#loading command to make NFL field in ggplot (credit to Marschall Furman)
source("https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R")
library(gganimate)


hex_points <- c("QB", "LT", "LG", "C", "RG", "RT")

df_pff_pocket <- readr::read_rds("data/df_pff_pocket.rds")

filter_to_pocket <- function(tracking_df, pff_pocket_df) {
  #filter to blocking OL and passing QB
  tracking_df %>% 
    inner_join(
      select(pff_pocket_df, gameId, playId, nflId, pff_positionLinedUp), 
      by = c("gameId", "playId", "nflId")
    )
}

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

# frame_coords <- function(df, x_or_y) {
#   df_temp <- df %>%
#     select(playerId, !!as.name(x_or_y)) %>%
#     mutate(playerId = paste0(x_or_y, as.character(playerId))) %>%
#     t() %>%
#     as.data.frame()
# 
#   names(df_temp) <- df_temp[1,]
#   df_temp[2,]
# }


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


# base play animation function --------------------------------------------

animate_play <- function(tracking_df, plays_df, ex_gameId, ex_playId) {
  
  #print(head(tracking_df))
  
  print('filtering to play')
  
  df_examplePlay <- plays_df %>%
    filter(gameId == ex_gameId & playId == ex_playId) %>% 
    select(gameId, playId, playDescription)

  stopifnot(nrow(df_examplePlay) == 1)
  
  #print(head(df_examplePlay))
  
  print('joining data')
  
  #merging tracking data to play
  df_examplePlayTracking <- inner_join(
    df_examplePlay, 
    tracking_df, 
    by = c("gameId", "playId")
  ) %>% std_coords()
  
  print('setting plotting vars')
  
  plot_title <- df_examplePlay$playDescription
  num_frames <- max(df_examplePlayTracking$frameId)
  
  #print(head(df_examplePlayTracking))
  #print("num_frames: ")
  #print(num_frames)
  
  print('creating plot')
  
  bfp <- base_field_plot()
  
  #plotting
  anim <- bfp +
    
    
    #adding players
    geom_point(data = df_examplePlayTracking, aes(x = x,
                                                  y = y, 
                                                  shape = team,
                                                  fill = team,
                                                  group = nflId,
                                                  size = team,
                                                  colour = team), 
               alpha = 0.7) +  
    
    #adding jersey numbers
    geom_text(data = df_examplePlayTracking,
              aes(x = x, y = y, label = jerseyNumber),
              colour = "white", 
              vjust = 0.36, size = 3.5) + 
    
    
    #titling plot with play description
    labs(title = plot_title) +
    
    #setting animation parameters
    transition_time(frameId)  +
    ease_aes("linear") + 
    NULL
  
  print('play animation')
  
  #saving animation to display in markdown cell below:
  animate(anim, width = 720, height = 440,
          fps = 10, nframes = num_frames,
          renderer = gifski_renderer())
  
  #anim_save("output.gif")
  
}



# pocket size plot and animation ------------------------------------------

prep_example_for_plot <- function(tracking_df, game_id, play_id) {
  tracking_df %>% 
    filter(gameId == game_id & playId == play_id) %>%
    inner_join(select(df_plays, gameId, playId, playDescription)) %>% 
    std_coords()
}

create_pocket_plot <- function(df4plot) {
  #df4plot must be 1 play of tracking data ran through prep_example_for_plot
  df4plot_pocket <- filter_to_pocket(df4plot, df_pff_pocket) %>% 
    arrange(factor(pff_positionLinedUp, levels = hex_points))
  
  #create base field plot
  bfp <- base_field_plot(min(df4plot$x)-10, max(df4plot$x)+10)
  
  #add OL/QB tracking data and pocket size to plot for the whole play
  bfp +
    geom_polygon(data = df4plot_pocket, aes(x = x, y = y)) +
    geom_text(data = df4plot, aes(x = mean(x), y = max(y)+ 3, label = paste("Pocket Size", round(pocket_size,2), sep = ": "))) +
    #adding players
    geom_point(
      data = df4plot, 
      aes(x=x, y=y, shape=team, fill=team, group=nflId, size=team, colour=team), 
      alpha = 0.7
    ) +  
    #adding jersey numbers
    geom_text(
      data = df4plot,
      aes(x = x, y = y, label = jerseyNumber),
      colour = "white", vjust = 0.36, size = 3.5
    ) +
    ggtitle(df4plot$playDescription)
}


create_pocket_animation <- function(pocket_plot, num_frames) {
  #pocket_plot created by create_pocket_plot
  #num frames should be max frameId of given play
  
  #add animation attributes to view frame by frame
  pocket_anim <- pocket_plot +
    transition_time(frameId)  +
    ease_aes("linear") + 
    NULL
  
  #create animation to view pocket size frame by frame
  animate(pocket_anim, width = 720, height = 440,
          fps = 10, nframes = num_frames,
          renderer = gifski_renderer())
}