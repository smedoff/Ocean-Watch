# Ocean-Watch
This R program produces an animation of fishing productivity using a predictive model based on a set of environmental variables

*About*

This R project creates a MP3 animation of the spatial distribution of predictive fishing productivity based on a fixed effects linear regression. 

![GitHub Logo](/Results/Final.gif?raw=true)

*Procedures*

This code executes the following procedures

1.	Extracts spatial environmental data based on the extent of the monthly fishing footprint
2.	Match the environmental data to the point location of fishing activity based on 1x1 degree grid cells
3.	Interpolate the linear regression with year and month fixed effects 
4.	Extrapolate the model and create monthly maps of predictive fishing productivity across the Pacific Ocean
5.	Create animations to show how predicted productivity changes across time and space

*Background*

This program was created as a course project for a 4-week Coastal Watch Course offered through the National Oceanic Atmospheric Administration (NOAA). The purpose of the course was to familiarize the NOAA staff with use of satellite data. 

*Data Source*

- https://www.wcpfc.int/wcpfc-public-domain-aggregated-catcheffort-data-download-page 
  - Aggregated data, grouped by 5°x5° latitude/longitude grids, FLAG,  YEAR and MONTH. (longline)
  - Aggregated data, grouped by 1°x1° latitude/longitude grids, YEAR and MONTH.(Purse seine)
  
- https://coastwatch.pfeg.noaa.gov/erddap/info/index.html?page=1&itemsPerPage=1000 
  - Environmental data (Chlorophyll, Chlor, SST)
  
