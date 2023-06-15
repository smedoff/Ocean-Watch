
#' Title: Creating final map
#' Author: Sarah Medoff
#' 
#' Purpose: This script will take the regression coef found in 04 and 
#' apply the linear transformation on the full spatial extent of the 
#' environemntal data sets to get a prediction of catch rates

library(stringr)
library(dplyr)
library(tidyverse)
library(sf)
library(usmap)
library(fixest)

rm(list=ls())

coef.v <- readRDS(file.path("Results", "Coefficients.RDS"))
  
  enviro_vars.v <- c("Chlorophyll",
                     "Salinity",
                     "SST")
  
  # create the year/month vector from file names. you can use any enviro
  # var because all folders have 25 files 
  YM.v <- list.files(file.path("Data", "SST")) %>% 
    setdiff("Set Measurements") %>% 
    str_replace("SST_", "") %>% 
    str_replace(".RDS", "")
  
  # Lets create a map for each month/year that uses the regression values to 
  # predict catch for the entire spatial extent of the enviro vars data sets
  
  #-----
  # First we will create a data frame of predicted values for out sample 
  # by looping over every ym. We need to separate this task from the actual map making because 
  # we need a set range for the legends in the final maps. The legend will be set to the 
  predicted.l <- lapply(1:length(YM.v), FUN = function(ym){
    
    print(ym)
    
    one_ym <- YM.v[ym]
    one_ym_lab <- paste0(substr(one_ym, 5, 6), "/", substr(one_ym, 1, 4))
    
    # pull in all ev data for one month/yr
    enviro_vars.l <- lapply(1:length(enviro_vars.v), FUN = function(v){
      
      one_ev <- enviro_vars.v[v]
      
      one_ev.df <- readRDS(file.path("Data",
                                     one_ev,
                                     paste0(one_ev, "_", one_ym, ".RDS"))) %>% 
        rename(EV = one_ev) %>% 
        mutate(latitude = round(as.numeric(latitude)),
               longitude = round(as.numeric(longitude)),
               EV = ifelse(EV == "NaN", NA, EV),
               mm = as.numeric(mm),
               yy = as.numeric(yy)) %>% 
        group_by(yy, mm, latitude, longitude) %>% 
        summarize(EV = mean(EV, na.rm = TRUE)) %>% 
        filter(!is.na(EV)) %>% 
        select(mm, yy, latitude, longitude, EV) %>% 
        doBy::renameCol("EV", one_ev)
        
      
    }) #end of pulling in all ev data sets for one month/yr
    
    enviro_vars.df <- purrr::reduce(enviro_vars.l, inner_join) %>% 
      mutate(Predicted = Chlorophyll * coef.v["Chlorophyll"] + 
               SST * coef.v["SST"] + 
               Salinity * coef.v["Salinity"])
    
  })
  
  predicted.df <- do.call(rbind, predicted.l)
  
  
  # Making the maps
  lapply(1:length(YM.v), FUN = function(ym){
    
    print(ym)
    
    one_ym <- YM.v[ym]
    one_ym_lab <- paste0(substr(one_ym, 1, 4), "-", substr(one_ym, 5, 6)) %>% 
      zoo::as.yearmon() %>% 
      print()
    
    enviro_vars.df <- predicted.l[[ym]]
    
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
    PS_EV.sf <- enviro_vars.df %>% 
      mutate(longitude = longitude * -1) %>%
      filter(!(longitude %in% c(-161:-154) & latitude %in% c(18:23))) %>% 
      st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4135)) 
    
    # Creating a map 
    ggplot() + 
      geom_sf(data = PS_EV.sf, 
              aes(color = Predicted), size =1.5) +
      scale_color_gradient(low = "red",  
                           high = "green",
                           limits = range(predicted.df$Predicted)) +
      geom_sf(data = usa, fill = "cornsilk") + 
      coord_sf(crs = st_crs(4135), xlim = c(-120, -180), 
               ylim = c(0, 50), 
               expand = FALSE, datum = NA) + 
      labs(title = one_ym_lab,
           subtitle = "Predicted catch of skipjack tuna using an OLS regression with time fixed effects",
           #subtitle = paste0("Predicted catch of skipjack tuna is estimated using the linear regression: \n", 
           #                  round(coef.v["Chlorophyll"], digit = 2), " * Chlor + ",
           #                  round(coef.v["SST"], digit = 2), " * SST + ",
           #                  round(coef.v["Salinity"], digit = 2), " * Salinity"),
           color = "Predicted Catch",
           caption = paste0("https://oceanwatch.pifsc.noaa.gov/index.html \n ",
                            "https://www.wcpfc.int/wcpfc-public-domain-aggregated-catcheffort-data-download-pag")) + 
      guides(color = guide_legend(order = 1),
             size  = guide_legend(order = 2)) +
      theme(title = element_text(size = 5)) 
    ggsave(file.path("Results", "Predicted Catch", paste0("PV_", one_ym, ".png")),
           height = 4, width = 4)  
    
  }) #end of lapply month/yr

  
  #------------------
  # Create the final animation 
  library(magick)
  ev_pngs.v <- list.files(file.path("Results", "Predicted Catch"))
  
  ev_mpas.l <- lapply(1:length(ev_pngs.v), FUN = function(f){
    
    one_file.png <- map(file.path("Results", "Predicted Catch", 
                                  ev_pngs.v[f]), image_read)
    
    return(one_file.png)
    
  })
  
  images <- image_join(ev_mpas.l)
  animation <- image_animate(images, fps = 2)
  image_write(animation, file.path("Results", "Final.gif"))
  
  
  