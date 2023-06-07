
#' Purpose: This function will accomplish two tasks 
#' 1. Create a panel by merging the enviro data and the catch data 
#' 2. Make a map for each month/yr that is size coded by catch and 
#'    color coded by enviro value

  # Creating the panel by joining the catch data with environmental data 
  creating_enviro_panel.f <- function(f){
    
    print(f)
    
    one_ym <- YM.v[f]
    
    year <- str_sub(one_ym, 1, 4)
    
    one_ym_lab <- paste0(substr(one_ym, 5,6), "/", year)
    
    EV_ym.df <- readRDS(file.path("Data",
                                  enviro_var,
                                  paste0(enviro_var, "_",
                                         one_ym,
                                         ".RDS"))) %>% 
      rename(enviro_col = enviro_var) %>% 
      filter(!is.na(enviro_col)) %>% 
      mutate(latitude = round(as.numeric(latitude)),
             longitude = round(as.numeric(longitude))) %>% 
      group_by(yy, mm, latitude, longitude) %>% 
      summarize(enviro_col = mean(enviro_col, na.rm = TRUE))
    
    # if no data was pulled from ERDDAP for that month/yr, then bypass
    # that month/yr
    if(nrow(EV_ym.df) != 0){
      
      SKPJ_ym.df <- SKPJ.df %>% 
        filter(yy == as.numeric(unique(EV_ym.df$yy)) & 
                 mm == as.numeric(unique(EV_ym.df$mm)))
      
      # Match the enviro and fishing data sets based on lat/lon coordinates
      PS_EV.df <- inner_join(SKPJ_ym.df, 
                             EV_ym.df %>% 
                               ungroup() %>% 
                               select(latitude, longitude, enviro_col), 
                             by = c("latitude", "longitude"))
      
      PS_EV.df %>% 
        doBy::renameCol("enviro_col", enviro_var) %>% 
        saveRDS(file.path("Data", enviro_var, "Set Measurements", 
                          paste0(enviro_var, "_SET_", one_ym, ".RDS")))
      
      return(PS_EV.df)
      
    } #end of if/else condition that bypasses missing month/yr data
    
  } #end of creating_enviro_panel function
  
  
  
  
  #--------------
  # Creating maps of the panels 
  create_enviro_maps.f <- function(f){
    
    PS_EV.df <- enviro_panels.l[[f]]
    
    one_ym_lab <- paste0(unique(PS_EV.df$mm) %>% 
                           str_pad(side = "left", pad = 0, width = 2), 
                         "/",
                         unique(PS_EV.df$yy))
    
    # Find a map of hawaii 
    library("rnaturalearth")
    library("rnaturalearthdata")
    world <- ne_countries(scale='medium',returnclass = 'sf')
    usa <- subset(world, admin == "United States of America")
    #hawaii  <- ggplot(data = usa) +
    #  geom_sf(fill = "cornsilk") +
    #  coord_sf(crs = st_crs(4135), 
    #           xlim = c(-161, -154), ylim = c(18, 23), 
    #           expand = FALSE, datum = NA)
    
    
    # Transform catch data to a shapefile 
    PS_EV.sf <- PS_EV.df %>% 
      mutate(longitude = longitude * -1) %>% 
      st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4135))
    
    # Creating a map 
    ggplot() + 
      geom_sf(data = PS_EV.sf, 
              aes(color = enviro_col, size = SKJ_Catch)) +
      scale_color_gradient(low = "red",  
                           high = "green") +
      geom_sf(data = usa, fill = "cornsilk") + 
      coord_sf(crs = st_crs(4135), xlim = c(-120, -180), 
               ylim = c(0, 50), 
               expand = FALSE, datum = NA) + 
      labs(title = paste0(enviro_var, " Plotted with the Purse Seine Fleet (", one_ym_lab, ")"),
           subtitle = paste0(enviro_var, " measurements are distingushed by color while catch measurements",
                             " for skipjack \n tuna vary by shape."),
           size = "SKJ Tuna Catch",
           color = enviro_var,
           caption = paste0("https://oceanwatch.pifsc.noaa.gov/index.html \n ",
                            "https://www.wcpfc.int/wcpfc-public-domain-aggregated-catcheffort-data-download-pag")) + 
      guides(color = guide_legend(order = 1),
             size  = guide_legend(order = 2)) +
      theme(title = element_text(size = 5))
    
    ggsave(file.path("Results", enviro_var, paste0(enviro_var, "_", one_ym, ".png")))
    
  }
  
  
  #--------------
  # Run the lapply to bind the set level data to the enviro and save a map of the 
  # two data sets together 
  creating_panel_map.f <- function(enviro_var){
    
    # Create the directories
    dir.create(file.path("Results", enviro_var))
    dir.create(file.path("Data", enviro_var, "Set Measurements"))
    
    enviro_panels.l <- lapply(1:length(YM.v), FUN = creating_enviro_panel.f)
    
    lapply(1:length(enviro_panels.l), FUN = create_enviro_maps.f)
    
  } #end of function
  
  
  
  
  
  
  