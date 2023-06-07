
#' Title: Building a predictive model 
#' Author: Sarah Medoff
#' 
#' Purpose: We run our ols regression with month/year fixed effects on the environmental/fisheries 
#' panel. This code will bind each individual environmental/fisheries panel and run it through an
#' feols regression model. 
#' 
#' Output: This code will produce 2 items
#' 1. A regression table saved in Results/Reg_Table.png
#' 2. The coefficients estimated which will be used to produce the predictive maps in the 
#'    subsequent script 

library(stringr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(sf)
library(usmap)
library(fixest)
library(xtable)
#library(gtsummary)
library(gt)

rm(list=ls())

  enviro_vars.v <- c("Chlorophyll",
                     "Salinity",
                     "SST")
  
  # Bring in each environmental month/yr panel
  enviro_vars.l <- lapply(1:length(enviro_vars.v), FUN = function(v){
    
    # Grabbing one enviro var
    one_ev <- enviro_vars.v[v]
    
    # Listing all the files
    ev_files.v <- list.files(file.path("Data",
                                       one_ev,
                                       "Set Measurements"))
    
    # Pulling in the files
    one_ev.l <- lapply(1:length(ev_files.v), FUN = function(f){
      
      readRDS(file.path("Data",
                        one_ev,
                        "Set Measurements",
                        ev_files.v[f]))
      
    })
    
    # rbind all the month/years to get a full panel
    one_ev.df <- do.call(rbind, one_ev.l)
  })

  # left_join so we have a column for each enviro var
  enviro_vars.df <- purrr::reduce(enviro_vars.l, left_join) %>% 
    filter(nSets != 0,
           !is.na(Salinity))
  
  #-------------
  # Running the regression
  
  fe_reg <- feols(SKJ_Catch ~ Chlorophyll + Salinity + SST | yy + mm, data = enviro_vars.df)
  fe_reg.l <- list()
  fe_reg.l[["Estiamte"]] <- fe_reg
  
  # https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html#custom-appearance
  modelsummary(fe_reg.l, 
               estimate = "{estimate}{stars}", 
               statistic = "{std.error}, ({p.value})",
               title = "Regression Results",
               output = file.path("Results", "Reg_Table.png")) 
  
  fe_reg$coefficients %>% 
    saveRDS(file.path("Results", "Coefficients.RDS"))
  

  
  