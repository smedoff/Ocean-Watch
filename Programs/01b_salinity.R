
#' Title: Salinity 
#' Author: Sarah Medoff
#' 
#' Purpose: This script will download spatial data sets of salinity data from ERDDAP based 
#' on the spatial extent of the monthly fishing foot print. 

library(stringr)
library(dplyr)
library(tidyverse)

rm(list=ls())

  PS1415 <- readRDS(file.path("Data", "PS1415.RDS"))
  
  # SST is taken at a monthly level. We have 2 years worth of data 
  # so we will end up querying 2*12 = 24 files
  
  # Create a folder for the files so we don't waste R memory reading them 
  # into the R environment 
  dir.create(file.path("Data", "Salinity"))
  
  lapply(1:nrow(PS1415), FUN = function(i){
    
    # Grabbing one set 
    one_obs <- PS1415[i,]
    
    day <- paste0(one_obs$yy, "-",
                  str_pad(one_obs$mm, side = "left", width = 2, pad = 0), "-",
                  "01")
    
    url <- paste0("https://oceanwatch.pifsc.noaa.gov/erddap/griddap/", 
                  "OceanWatch_aquarius_sss_monthly.csv?sea_surface_salinity%5B(", 
                  day, 
                  "T00:00:00Z):1:(", 
                  day,
                  "T00:00:00Z)%5D%5B(", 
                  one_obs$latmin,
                  "):1:(", 
                  one_obs$latmax, 
                  ")%5D%5B(", 
                  one_obs$lonmin, 
                  "):1:(", 
                  one_obs$lonmax, 
                  ")%5D")
    
    # Read the url in as a csv (this is why we love ERDDAP)
    ERDDAP <- try(read_csv(url) %>% 
                          as.data.frame() %>% 
                          slice(-1) %>% 
                          rename(Salinity = sea_surface_salinity) %>% 
                          mutate(time = substr(time, 1, 7) %>% str_replace("-", " "),
                                 Salinity = round(as.numeric(sea_surface_salinity), 2)) %>% 
                          separate(time, c("yy", "mm")),
                        silent = TRUE)
    
    if(class(ERDDAP) == "try-error"){
      ERDDAP <- data.frame(yy = one_obs$yy,
                           mm = one_obs$mm, 
                           latitude = NA,
                           longitude = NA, 
                           Salinity = NA)
    }
    
    # Save it externally (as opposed to pulling it into R) This way you dont
    # waste R memory
    saveRDS(ERDDAP, file.path("Data",
                              "Salinity",
                              paste0("Salinity_", one_obs$yy, 
                                     str_pad(one_obs$mm, pad = 0, 
                                             width = 2, side = "left"),
                                     ".RDS")))
    
    print(i)
    
  }) #end lapply for querying envior var


  