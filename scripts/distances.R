df_presnap <- data.frame()
df_for_loop <- df_tracking_dists %>% 
  select(contains("Id"), x, y)



games <- unique(df_for_loop$gameId)


for(gid in games) {
  #subset to 1 game
  igame <- df_for_loop %>% filter(gameId == gid)
  plays <- unique(igame$playId)
  
  #loop through each play
  for(pid in plays) {
    
    iplay <- igame %>% filter(playId == pid)
    
    #subset to first fame
    frame1 <- iplay %>% 
      filter(frameId == min(frameId)) %>% 
      mutate(playerId = dplyr::row_number())
    
    x_coords <- frame_coords(frame1, "x")
    y_coords <- frame_coords(frame1, "y")
    all_cords <- bind_cols(x_coords, y_coords)
    
    for(i in 1:nrow(frame1)) all_coords <- bind_rows(all_coords, all_coords)
    
  }
}



frame_coords <- function(df, x_or_y) {
  df_temp <- df %>% 
    select(playerId, !!as.name(x_or_y)) %>% 
    mutate(playerId = paste0(x_or_y, as.character(playerId))) %>% 
    t() %>% 
    as.data.frame()
  
  names(df_temp) <- df_temp[1,]
  df_temp[2,]
}




