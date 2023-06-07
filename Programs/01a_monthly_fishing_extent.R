
#' Title: Fishing grid lat/lon
#' Author: Sarah Medoff
#' 
#' Purpose: Creating a data frame that details the fishing extent based on fishing location
#' location meaning the lat/lon of the bottom left corner of the 1x1 deg grid. We will extract the
#' environmental variables based on this spatial extent.
#' For the sake of this project, we will only run the program on 2 years worth of data 
#' to save computational time. I chose to use the purse seine data because it was captured 
#' at a finer spatial resolution than the long line (1x1 deg grids vs. 5x5 deg grids). Code 
#' commented below will read in long line data if further analysis is needed.


library(stringr)
library(dplyr)
library(tidyverse)

rm(list=ls())

  
  # Read in Purse Seine data 
  PS <- read.csv(file.path("Data", 
                           "WCPFC_S_PUBLIC_BY_1x1_MM.csv")) %>% 
    mutate(lat_short = str_sub(lat_short, 1, nchar(lat_short)-1) %>% as.numeric(),
           lon_short = str_sub(lon_short, 1, nchar(lon_short)-1) %>% as.numeric())


  # Extract coordinates from the year 2014-2015 for analysis
  PS1415 <- PS %>% 
    filter(yy %in% 2014:2015) %>% 
    group_by(yy, mm) %>% 
    summarize(latmin = min(lat_short, na.rm = TRUE),
              latmax = max(lat_short, na.rm = TRUE),
              lonmin = min(lon_short, na.rm = TRUE),
              lonmax = max(lon_short, na.rm = TRUE)) %>% 
    mutate(mm = str_pad(mm, side = "left", pad = 0, width = 2))
  
  saveRDS(PS1415, file.path("Data", "PS1415.RDS"))
  
  #--------------
  # Read in Longline data 
  #  LL <- read.csv(file.path("Data", 
  #                           "WCPFC_L_PUBLIC_BY_FLAG_MON.csv")) %>% 
  #    mutate(lat_short = str_sub(lat_short, 1, nchar(lat_short)-1) %>% as.numeric(),
  #           lon_short = str_sub(lon_short, 1, nchar(lon_short)-1) %>% as.numeric())
  
  #  range(LL$lat_short)
  #  range(LL$lon_short)
  