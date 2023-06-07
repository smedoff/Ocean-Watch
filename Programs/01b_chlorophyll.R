
#' Title: Chlorophyl 
#' Author: Sarah Medoff
#' 
#' Purpose: This script will download spatial data sets of chlorophyll data from ERDDAP based 
#' on the spatial extent of the monthly fishing foot print. 

library(stringr)
library(dplyr)
library(tidyverse)

rm(list=ls())

  PS1415 <- readRDS(file.path("Data", "PS1415.RDS"))
  
  # Chlorophyll is taken at a monthly level. We have 2 years worth of data 
  # so we will end up querying 2*12 = 24 files
  
  # Create a folder for the files so we don't waste R memory reading them 
  # into the R environment 
  dir.create(file.path("Data", "Chlor"))
  
  lapply(1:nrow(PS1415), FUN = function(i){
    
    # Grabbing one set 
    one_obs <- PS1415[i,]
    
    day <- paste0(one_obs$yy, "-",
                  str_pad(one_obs$mm, side = "left", width = 2, pad = 0), "-",
                  16)
    
    url <- paste0("https://oceanwatch.pifsc.noaa.gov/erddap/griddap/aqua_chla_monthly_2018_0.csv?chlor_a%5B(", 
                  day, 
                  "T12:00:00Z):1:(", 
                  day, 
                  "T12:00:00Z)%5D%5B(", 
                  one_obs$latmax, 
                  "):1:(", 
                  one_obs$latmin, 
                  ")%5D%5B(", 
                  one_obs$lonmin, 
                  "):1:(", 
                  one_obs$lonmax, 
                  ")%5D")
    
    
    
    # Read the url in as a csv (this is why we love ERDDAP)
    ERDDAP_CHLOR <- try(read_csv(url) %>% 
                          as.data.frame() %>% 
                          slice(-1) %>% 
                          mutate(time = substr(time, 1, 7) %>% str_replace("-", " "),
                                 chlor_a = round(as.numeric(chlor_a), 2)) %>% 
                          separate(time, c("yy", "mm")),
                        silent = TRUE)
    
    if(class(ERDDAP_CHLOR) == "try-error"){
      ERDDAP_CHLOR <- data.frame(yy = one_obs$yy,
                                 mm = one_obs$mm, 
                                 latitude = NA,
                                 longitude = NA, 
                                 chlor_a = NA)
    }
    
    # Save it externally (as opposed to pulling it into R) This way you don't
    # waste R memory
    saveRDS(ERDDAP_CHLOR, file.path("Data",
                                    "Chlor",
                                    paste0("CHLOR_", one_obs$yy, 
                                           str_pad(one_obs$mm, pad = 0, 
                                                   width = 2, side = "left"),
                                           ".RDS")))
    
    print(i)
    
  }) #end lapply for querying chlor

  
  
  # Bring the queried data in to your R environment
  chlor_files.v <- list.files(file.path("Data",
                                      "Chlor"))
  
  CHLOR.l <- lapply(1:length(chlor_files.v), FUN = function(f){
    
    one_file.df <- readRDS(file.path("Data",
                                     "Chlor",
                                     chlor_files.v[f]))
    
    return(one_file.df)
    
  }) #end lapply for reading files back in 
  
  # Compile the list into a df
  CHLOR.df <- do.call(rbind, CHLOR.l)
  
  saveRDS(CHLOR.df, file.path("Data",
                              "CHLOR.RDS"))  
  
  