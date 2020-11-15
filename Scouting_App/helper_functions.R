#Not in Function

'%!in%' <- function(x,y)!('%in%'(x,y))

#Basic Stats Function

basic_table <- function(df){
  
  pa_types <- df %>% 
    count(events) %>% 
    filter(events %!in% c("caught_stealing_2b", "caught_stealing_3b", "caught_stealing_home", "catcher_interference", "sac_bunt", NA, "null"))
  
  error <- pa_types %>% 
    filter(events == "field_error")
  
  ab_types <- pa_types %>% 
    filter(events %!in% c("walk", "intentional_walk", "hit_by_pitch", "sac_fly"))
  
  hit_types <- ab_types %>% 
    filter(events %in% c("single", "double", "triple", "home_run"))
  
  walk_types <- pa_types %>% 
    filter(events %in% c("walk", "intentional_walk", "hit_by_pitch"))
  
  ba <- round(sum(hit_types$n)/sum(ab_types$n),3)
  obp <- round((sum(hit_types$n)+ sum(walk_types$n))/sum(pa_types$n,-1*error$n),3)
  
  sing <- hit_types %>% 
    filter(events == "single") %>% 
    select(n) %>% 
    as.numeric()
  
  doub <- hit_types %>% 
    filter(events == "double")%>% 
    select(n) %>% 
    as.numeric()
  
  trip <- hit_types %>% 
    filter(events == "triple")%>% 
    select(n) %>% 
    as.numeric()
  
  hr <- hit_types %>% 
    filter(events == "home_run")%>% 
    select(n) %>% 
    as.numeric()
  
  slg <- round(sum(sing,2*doub,3*trip,4*hr, na.rm = TRUE)/sum(ab_types$n),3)
  ops <- obp+slg
  
  data.frame("BA" = as.character(format(ba,nsmall=3)), "OBP" = as.character(format(obp, nsmall =3)), "SLG" = as.character(format(slg,nsmall=3)), "OPS" = as.character(format(ops,nsmall=3)), "HR" = as.character(hr))
}

#Hot Zones Functions
hotzones <- function(df){
  
  str_z_top <- mean(df$sz_top, na.rm = TRUE)
  str_z_bot <- mean(df$sz_bot, na.rm = TRUE)
  str_z_height <- str_z_top-str_z_bot
  
  slg_zone <- function(pzone, df_full){
    df_zone <- df_full %>% 
      filter(zone == pzone)
    pa_types <- df_zone %>% 
      count(events) %>% 
      filter(events %!in% c("caught_stealing_2b", "caught_stealing_3b", "caught_stealing_home", "catcher_interference", NA, "null"))
    
    ab_types <- pa_types %>% 
      filter(events %!in% c("walk", "intentional_walk", "hit_by_pitch", "sac_bunt", "sac_fly"))
    
    hit_types <- ab_types %>% 
      filter(events %in% c("single", "double", "triple", "home_run"))
    
    sing <- hit_types %>% 
      filter(events == "single") %>% 
      select(n) %>% 
      as.numeric()
    
    doub <- hit_types %>% 
      filter(events == "double")%>% 
      select(n) %>% 
      as.numeric()
    
    trip <- hit_types %>% 
      filter(events == "triple")%>% 
      select(n) %>% 
      as.numeric()
    
    hr <- hit_types %>% 
      filter(events == "home_run")%>% 
      select(n) %>% 
      as.numeric()
    
    slg <- round(sum(sing,2*doub,3*trip,4*hr, na.rm = TRUE)/sum(ab_types$n),3)
  }
  
  df_zones <- sapply(1:9, function(x) slg_zone(pzone=x, df_full = df))
  
  ggplot() +
    geom_polygon(aes(x=c(-.85,-.85,-.283,-.283),
                     y=c(str_z_top, str_z_top-str_z_height/3, str_z_top-str_z_height/3, str_z_top), fill = df_zones[1]), col = "black",alpha = .6) +
    geom_polygon(aes(x=c(-.85,-.85,-.283,-.283),
                     y=c(str_z_top-str_z_height/3, str_z_bot+str_z_height/3, str_z_bot+str_z_height/3, str_z_top-str_z_height/3), fill = df_zones[4]), col = "black",alpha = .6) +
    geom_polygon(aes(x=c(-.85,-.85,-.283,-.283),
                     y=c(str_z_bot+str_z_height/3, str_z_bot, str_z_bot, str_z_bot+str_z_height/3), fill = df_zones[7]), col = "black",alpha = .6) +
    geom_polygon(aes(x=c(-.283,-.283,.283,.283),
                     y=c(str_z_top, str_z_top-str_z_height/3, str_z_top-str_z_height/3, str_z_top), fill = df_zones[2]), col = "black",alpha = .6) +
    geom_polygon(aes(x=c(-.283,-.283,.283,.283),
                     y=c(str_z_top-str_z_height/3, str_z_bot+str_z_height/3, str_z_bot+str_z_height/3, str_z_top-str_z_height/3), fill = df_zones[5]), col = "black",alpha = .6) +
    geom_polygon(aes(x=c(-.283,-.283,.283,.283),
                     y=c(str_z_bot+str_z_height/3, str_z_bot, str_z_bot, str_z_bot+str_z_height/3), fill = df_zones[8]), col = "black",alpha = .6) +
    geom_polygon(aes(x=c(.283,.283,.85,.85),
                     y=c(str_z_top, str_z_top-str_z_height/3, str_z_top-str_z_height/3, str_z_top), fill = df_zones[3]), col = "black",alpha = .6) +
    geom_polygon(aes(x=c(.283,.283,.85,.85),
                     y=c(str_z_top-str_z_height/3, str_z_bot+str_z_height/3, str_z_bot+str_z_height/3, str_z_top-str_z_height/3), fill = df_zones[6]), col = "black",alpha = .6) +
    geom_polygon(aes(x=c(.283,.283,.85,.85),
                     y=c(str_z_bot+str_z_height/3, str_z_bot, str_z_bot, str_z_bot+str_z_height/3), fill = df_zones[9]), col = "black",alpha = .6) +
    scale_fill_gradientn(colors = c("darkblue","blue","white","red","darkred"), values = c(0,.3/4,.45/4,.6/4,4/4), limits = c(0,4)) +
    coord_fixed() + theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),axis.title.y=element_blank(),
                          legend.position="none", panel.background=element_blank(),
                          panel.border=element_blank(),panel.grid.minor=element_blank(),
                          plot.background=element_blank())
}  

hotzones_ev <- function(df){
  
  str_z_top <- mean(df$sz_top, na.rm = TRUE)
  str_z_bot <- mean(df$sz_bot, na.rm = TRUE)
  str_z_height <- str_z_top-str_z_bot
  
  ev_zone <- function(pzone, df_full){
    df_zone <- df_full %>% 
      filter(zone == pzone)
    median(df_zone$launch_speed, na.rm = TRUE)
  }
  
  df_zones <- sapply(1:9, function(x) ev_zone(pzone=x, df_full = df))
  
  ggplot() +
    geom_polygon(aes(x=c(-.85,-.85,-.283,-.283),
                     y=c(str_z_top, str_z_top-str_z_height/3, str_z_top-str_z_height/3, str_z_top), fill = df_zones[1]), col = "black",alpha = .6) +
    geom_polygon(aes(x=c(-.85,-.85,-.283,-.283),
                     y=c(str_z_top-str_z_height/3, str_z_bot+str_z_height/3, str_z_bot+str_z_height/3, str_z_top-str_z_height/3), fill = df_zones[4]), col = "black",alpha = .6) +
    geom_polygon(aes(x=c(-.85,-.85,-.283,-.283),
                     y=c(str_z_bot+str_z_height/3, str_z_bot, str_z_bot, str_z_bot+str_z_height/3), fill = df_zones[7]), col = "black",alpha = .6) +
    geom_polygon(aes(x=c(-.283,-.283,.283,.283),
                     y=c(str_z_top, str_z_top-str_z_height/3, str_z_top-str_z_height/3, str_z_top), fill = df_zones[2]), col = "black",alpha = .6) +
    geom_polygon(aes(x=c(-.283,-.283,.283,.283),
                     y=c(str_z_top-str_z_height/3, str_z_bot+str_z_height/3, str_z_bot+str_z_height/3, str_z_top-str_z_height/3), fill = df_zones[5]), col = "black",alpha = .6) +
    geom_polygon(aes(x=c(-.283,-.283,.283,.283),
                     y=c(str_z_bot+str_z_height/3, str_z_bot, str_z_bot, str_z_bot+str_z_height/3), fill = df_zones[8]), col = "black",alpha = .6) +
    geom_polygon(aes(x=c(.283,.283,.85,.85),
                     y=c(str_z_top, str_z_top-str_z_height/3, str_z_top-str_z_height/3, str_z_top), fill = df_zones[3]), col = "black",alpha = .6) +
    geom_polygon(aes(x=c(.283,.283,.85,.85),
                     y=c(str_z_top-str_z_height/3, str_z_bot+str_z_height/3, str_z_bot+str_z_height/3, str_z_top-str_z_height/3), fill = df_zones[6]), col = "black",alpha = .6) +
    geom_polygon(aes(x=c(.283,.283,.85,.85),
                     y=c(str_z_bot+str_z_height/3, str_z_bot, str_z_bot, str_z_bot+str_z_height/3), fill = df_zones[9]), col = "black",alpha = .6) +
    scale_fill_gradientn(colors = c("darkblue","blue","white","red","darkred"), values = c(0,80/130,90/130,100/130,1), limits = c(0,130)) +
    coord_fixed() + theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),axis.title.y=element_blank(),
                          legend.position="none", panel.background=element_blank(),
                          panel.border=element_blank(),panel.grid.minor=element_blank(),
                          plot.background=element_blank())
}

#Radial Chart
radial_chart <- function(df){
  df2 <- filter(df, description %in% c("hit_into_play", "hit_into_play_no_out", "hit_into_play_score"))
  df3 <- count(df2, launch_speed_angle)
  ggplot() +
    geom_polygon(mapping=aes(x=c(-90,90,90,-90,-90), y=c(0,0,60,60,0)), alpha = .2, fill = "gold") +
    geom_polygon(mapping=aes(x=c(-90,-90,-10,2,4,13,22,-90),y=c(60,120,120,93,85,72,60,60)), alpha = .2, fill = "darkgreen")+
    geom_polygon(mapping=aes(x=c(4,26,30,50,50,4), y=c(120,98,98,334/3,120,120)), alpha = .8, fill = "red")+
    geom_polygon(mapping=aes(x=c(0,24,63/2,52,52,50,50,30,26,4,0,0), y=c(119,95,95,326/3,120,120,334/3,98,98,120,120,119)), fill = "pink", alpha=.5)+
    geom_polygon(mapping=aes(x=c(-10,2,4,13,22,34,41,41,32,26,26,20,20,22,0,0,-10), y =c(120,93,85,72,60,60,64,68,72,77,79,85,96,97,119,120,120)), alpha = .3, fill = "darkorange") +
    geom_polygon(mapping=aes(x=c(52,90,90,34,41,41,32,26,26,20,20,22,24,63/2,52,52),y=c(120,120,60,60,64,68,72,77,79,85,96,97,95,95,326/3,120)), fill="cornflowerblue", alpha=.2)+
    geom_point(data=df2, mapping = aes(x=launch_angle, y=launch_speed))+
    scale_x_continuous(limits = c(-180,180), breaks = seq(-180,180, by = 30))+
    coord_polar(theta = "x", start = pi/2, direction = -1)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                  axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                  axis.title.x=element_blank(),
                                                                  axis.title.y=element_blank(),legend.position="none",
                                                                  panel.background=element_blank(),panel.border=element_blank(),
                                                                  panel.grid.minor=element_blank(),plot.background=element_blank()) +
    annotate("text", x = 142, y = 111.6, label = paste("Barrels:",df3 %>% filter(launch_speed_angle == "6") %>% select(n)), col = "red", size = 3.2) +
    annotate("text", x = 155, y = 97, label = paste("Solid Contact:",df3 %>% filter(launch_speed_angle == "5") %>% select(n)), col = "pink", size = 3.2) +
    annotate("text", x = 171, y = 89, label = paste("Flares & Burners:",df3 %>% filter(launch_speed_angle == "4") %>% select(n)), col = "orange", size = 3.2) +
    annotate("text", x = -171, y = 89, label = paste("Topped:",df3 %>% filter(launch_speed_angle == "3") %>% select(n)), col = "darkgreen", size = 3.2) +
    annotate("text", x = -155, y = 97, label = paste("Hit Under:",df3 %>% filter(launch_speed_angle == "2") %>% select(n)), col = "cornflowerblue", size = 3.2) +
    annotate("text", x = -142, y = 111.6, label = paste("Weak Contact:",df3 %>% filter(launch_speed_angle == "1") %>% select(n)), col = "gold", size = 3.2)
}