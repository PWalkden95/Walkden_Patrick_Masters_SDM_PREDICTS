rm(list = ls())
require(raster)
library(rgdal)
library(leaflet)
setwd("../")
setwd("./Combine")

Bioclim_01 <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_01.tif")
Bioclim_01
Europe_Extent <- extent(-10, 36, 30, 75) 

Shapefiles <- readOGR("Country_Shapefiles/ne_50m_admin_0_countries.shp")
Polygons <- SpatialPolygons(Shapefiles@polygons)



Tunisia <- Polygons[38]
Algeria <- Polygons[236]
Jordan <- Polygons[135]
Egypt <- Polygons[177]
Israel <- Polygons[139]
Palestine <- Polygons[140]
Lebanon <- Polygons[126]
Tunisia <- Polygons[38]
Morocco <- Polygons[104]
Iraq <- Polygons[142]
Libya <- Polygons[123]
Saudi_Arabia <-Polygons[68]

North_Africa_Mask <- bind(Egypt,Palestine,Lebanon,Iraq,Israel,Algeria,Saudi_Arabia,Tunisia,Morocco,Libya,Jordan)

plot(North_Africa_Mask)

save(North_Africa_Mask, file = "RData_North_Africa_Mask.RData")




