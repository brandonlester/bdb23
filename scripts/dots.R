#loading command to make NFL field in ggplot (credit to Marschall Furman)
source("https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R")
library(gganimate)

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

