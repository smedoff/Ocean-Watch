
#' Title: Matching environmental data to fishing data
#' Author: Sarah Medoff
#' 
#' Purpose: Merge (inner_join) environmental data and fishing data by lat/lon of 1x1 degree fishing box
#' location. Up until this point, we have extracted a spatial layer (map) of the environmental data 
#' based on the monthly spatial extent of the fishing data. Now, we will identify point locations of 
#' environmental data based on the point locations of fishing (*note: we do not have point locations 
#' of fishing so this will actually be the left corner of the 1x1 degree grid*). We will use these point
#' measurements for our fixed effects regression. 
#' 
#' Output: This code will produce 3 items
#' 1. A panel of fishing activity and environmental measurements 
#' 2. Maps displaying fishing activity and environmental measurements 
#' 3. Animations of how fishing activity and environmental measurements 
#'    change through out time and space


library(stringr)
library(dplyr)
library(tidyverse)
library(sf)
library(usmap)

rm(list=ls())
source("hlprfunc_creating_panel_map.R")
source("hlprfunc_creating_animation.R")

  # Use the full panel so we have detail catch through space and time by species
  PS1415_FULL <- read.csv(file.path("Data", 
                                    "WCPFC_S_PUBLIC_BY_1x1_MM.csv")) %>% 
    filter(yy %in% 2014:2015) %>% 
    mutate(lat_short = str_sub(lat_short, 1, nchar(lat_short)-1) %>% as.numeric(),
           lon_short = str_sub(lon_short, 1, nchar(lon_short)-1) %>% as.numeric())
  
  SKPJ.df <- PS1415_FULL %>% 
    select(yy, mm, latitude = lat_short, longitude = lon_short, cwp_grid,
           SKJ_Catch = skj_c_una, nSets = sets_una)
  
  # Define files that we want to extract based on our 'years' and 'months'
  YM.v <- expand.grid(unique(SKPJ.df$yy), 
                      unique(SKPJ.df$mm) %>% str_pad(width = 2, pad = 0, side = "left")) %>% 
    mutate(YM = paste0(Var1, Var2)) %>% 
    pull(.) %>% 
    sort()
  
  
  
  #----------
  # Chlorophyll 
  creating_panel_map.f(enviro_var = "Chlorophyll")
  create_animation.f(enviro_var = "Chlorophyll")
  
  #----------
  # SST
  creating_panel_map.f(enviro_var = "SST")
  create_animation.f(enviro_var = "SST")
  
  #----------
  # Salinity 
  creating_panel_map.f(enviro_var = "Salinity")
  create_animation.f(enviro_var = "Salinity")
  