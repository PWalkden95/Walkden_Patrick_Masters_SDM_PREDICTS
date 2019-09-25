rm(list= ls())
require(biomod2)    ### to make the projections
require(raster)      ## working with rasters 
require(dplyr)       ## data wrangling
require(magrittr)     ## piping
require(sp)
require(REdaS)
require(dplyr)
require(rgdal)

load("RData_European_Bee_Species_SDM_Evaluation_Model_Outs.RData")
load("RData_European_Bee_Species_OLE_Coord_Limits.RData") 
load("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/Combine/RData_North_Africa_Mask.RData")

raster <- raster("Raw_Data/Bioclimatic_Variables/Current/wc2.0_bio_2.5m_01.tif")
Europe_Extent <- extent(-10, 36, 30, 75) 
alphaMap <- reclassify(subset(raster,1), c(-Inf,Inf,0))
alphaMap <- crop(alphaMap, Europe_Extent)


Dispersal_Scenarios <- function(Date, RCP){

  Assisted_Migration <- alphaMap
  No_Dispersal <- alphaMap
  Limited_Dispersal <- alphaMap
  
  for(i in 1:length(Model_Outs)){
    Current <- raster(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Unbound_Current_2/","/2_Raster_Tif.tif", sep = Model_Outs[[i]]@sp.name))
    ras <- raster(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Future_",paste(paste("/",Model_Outs[[i]]@sp.name, sep = ""),"/2_Raster_Tif.tif",sep = ""),sep = paste(Date,RCP, sep = "_")))
    ras_AM <- extend(ras, Europe_Extent)
    map_AM <- reclassify(ras_AM, c(NA,NA,0))
    Assisted_Migration <- Assisted_Migration + map_AM
    writeRaster(Assisted_Migration,paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Future_","/All_bee_Raster_AM_Tif.tif",sep = paste(Date,RCP, sep = "_")),format = "GTiff", overwrite = TRUE)
    
    if(OLE_Coord_Limits[i,2] < -25 | OLE_Coord_Limits[i,2] == "NaN" ){
      xmin <- -25
    } else {
      xmin <- OLE_Coord_Limits[i, 2]
    }
    if(OLE_Coord_Limits[i,4] > 50 | OLE_Coord_Limits[i,4] == "NaN"){
      xmax <- 50
    } else {
      xmax <- OLE_Coord_Limits[i, 4]
    }
    if(OLE_Coord_Limits[i,6] < 30 | OLE_Coord_Limits[i,6] == "NaN"){
      ymin <- 30
    } else {
      ymin <- OLE_Coord_Limits[i, 6]
    }
    if(OLE_Coord_Limits[i,8] > 75 | OLE_Coord_Limits[i,8] == "NaN"){
      ymax <- 75
    } else {
      ymax <- OLE_Coord_Limits[i, 8]
    }
    
    No_Dispersal_extent <- extent(xmin, xmax, ymin, ymax)
    
    
    ras_ND <- crop(ras, No_Dispersal_extent)
    ras_ND <- (ras_ND + Current)
    ras_ND <- extend(ras_ND, Europe_Extent)
    map_ND <- reclassify(ras_ND, c(NA,NA,0))
    map_ND <- (map_ND == 2)
    No_Dispersal <- No_Dispersal + map_ND
    writeRaster(No_Dispersal,paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Future_","/All_bee_Raster_ND_Tif.tif",sep = paste(Date,RCP, sep = "_")),format = "GTiff", overwrite = TRUE)
    plot(No_Dispersal)
    if(Date == "2050"){
    Limited_Dispesal_extent <- extent(xmin-(350/(111.320*cos(deg2rad(mean(ymin,ymax)/pi/180)))), 
                                      xmax + (350/(111.320*cos(deg2rad(mean(ymin,ymax)/pi/180)))),
                                      ymin - 350/110.574,
                                      ymax + 350/110.574)
    } else {
      Limited_Dispesal_extent <- extent(xmin-(550/(111.320*cos(deg2rad(mean(ymin,ymax)/pi/180)))), 
                                        xmax + (550/(111.320*cos(deg2rad(mean(ymin,ymax)/pi/180)))),
                                        ymin - 550/110.574,
                                        ymax + 550/110.574)
    }
    
    
    ras_LD <- crop(ras, Limited_Dispesal_extent)
    ras_LD <- extend(ras_LD, Europe_Extent)
    map_LD <- reclassify(ras_LD, c(NA,NA,0))
    Limited_Dispersal <- Limited_Dispersal + map_LD
    writeRaster(Limited_Dispersal,paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Future_","/All_bee_Raster_LD_Tif.tif",sep = paste(Date,RCP, sep = "_")),format = "GTiff", overwrite = TRUE)
  }
}
 
Dispersal_Scenarios("2050", "2.6")
Dispersal_Scenarios("2050", "4.5")
Dispersal_Scenarios("2050", "6.0")
Dispersal_Scenarios("2050", "8.5")

Dispersal_Scenarios("2070", "2.6")
Dispersal_Scenarios("2070", "4.5")
Dispersal_Scenarios("2070", "6.0")
Dispersal_Scenarios("2070", "8.5")


  Current <- alphaMap
  for(i in 1:length(Model_Outs)){
  bee_ras <- raster(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Current_2/","/2_Raster_Tif.tif", sep = Model_Outs[[i]]@sp.name))
  bee_ras <- extend(bee_ras, Europe_Extent)
  bee_ras <- reclassify(bee_ras, c(NA,NA,0))
  Current <- Current + bee_ras
  }
  writeRaster(Current, "C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Current_2/All_bee_Raster_Tif", format = "GTiff", overwrite = T)
  

  
  ## create spatial maps just showing whether we expect species richness to go up or down due to climate change
  Date <- "2050"
  RCP <- "2.6"
  Dispersal <- "LD"
  
  
  ### to calculate log response ratio
  log_repsonse_map <- function(Date, RCP, Dispersal){
    Current <- raster("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Current_2/All_bee_Raster_Tif.tif")
    richness <- raster(paste(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Future_","/All_bee_Raster_", sep = paste(Date,RCP,sep = "_")),"_Tif.tif", sep = paste(Dispersal, sep = "_")))
    log_response_ratio <- log(richness/Current)
    log_response_ratio <- mask(log_response_ratio, mask = North_Africa_Mask, inverse = TRUE)
    log_response_ratio <- crop(log_response_ratio, Europe_Extent)
    plot(log_response_ratio, zlim = c(-6, 6))
    log_response_ratio
  }
  
  
  ## to show a binary response of positive spp richness change or negative species richness change
  Binary_Richness_Map <- function(Date, RCP, Dispersal){
    Current <- raster("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Current_2/All_bee_Raster_Tif.tif")
    richness <- raster(paste(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Future_","/All_bee_Raster_", sep = paste(Date,RCP,sep = "_")),"_Tif.tif", sep = paste(Dispersal, sep = "_")))
    log_richness <- log(richness/Current)
    bin_richness_low <- reclassify(log_richness, c(-Inf, 0, 100))
    bin_richness_low <- bin_richness_low == 100
    bin_richness_high <- reclassify(log_richness, c(0,Inf,100))
    bin_richness_high <- bin_richness_high == 100
    bin_richness <- ((bin_richness_low*-1)+ bin_richness_high)
    bin_richness <- mask(bin_richness, mask = North_Africa_Mask, inverse = TRUE)
    bin_richness <- crop(bin_richness, Europe_Extent)
    plot(bin_richness, zlim = c(-1.3, 1.3))
    bin_richness
    }
  
  
  ### Then Finally make the binary and log-response ratio maps and dataframes for analysis
  
Log_Binary_Stack <- function(Date, Dispersal){
  
 Bin_2.6 <- Binary_Richness_Map(Date, "2.6", Dispersal)
 
 Bin_4.5 <- Binary_Richness_Map(Date, "4.5", Dispersal)
 
 Bin_6.0 <- Binary_Richness_Map(Date, "6.0", Dispersal)
 
 Bin_8.5 <- Binary_Richness_Map(Date, "8.5", Dispersal)
 
 bin_CC <- stack(Bin_2.6,
                         Bin_4.5,
                         Bin_6.0,
                         Bin_8.5)
 
 names(bin_CC) <- c("CC_Binary_Log_Response_Ratio_RCP2.6",
                            "CC_Binary_Log_Response_Ratio_RCP4.5",
                            "CC_Binary_Log_Response_Ratio_RCP6.0",
                            "CC_Binary_Log_Response_Ratio_RCP8.5")
 
 
 
 writeRaster(stack(bin_CC), filename=paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/Combine/Binary_Climate_Change_","_Tif.tif",sep = paste(Date,Dispersal,sep = "_")), options="INTERLEAVE=BAND", overwrite=TRUE)
 
 log_response_2.6 <- log_repsonse_map(Date, "2.6", Dispersal)
 
 log_response_4.5 <- log_repsonse_map(Date, "4.5", Dispersal)
 
 log_response_6.0 <- log_repsonse_map(Date, "6.0", Dispersal)
 
 
 log_response_8.5 <- log_repsonse_map(Date, "8.5", Dispersal)
 
 
 log_response_CC <- stack(log_response_2.6,
                                  log_response_4.5,
                                  log_response_6.0,
                                  log_response_8.5)
 
 names(log_response_CC) <- c(paste(paste("CC", Dispersal,sep = "_"),"Log_Response_Ratio", Date, "RCP2.6", sep = "_"),
                             paste(paste("CC", Dispersal,sep = "_"),"Log_Response_Ratio", Date, "RCP4.5", sep = "_"),
                             paste(paste("CC", Dispersal,sep = "_"),"Log_Response_Ratio", Date, "RCP6.0", sep = "_"),
                             paste(paste("CC", Dispersal,sep = "_"),"Log_Response_Ratio", Date, "RCP8.5", sep = "_"))  
 
 
 CC_frame <- raster::as.data.frame(log_response_CC)
 saveRDS(CC_frame, file = paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/Combine/RData_Climate_Change_Response_Ratio_",".rds", sep = paste(Date,Dispersal, sep = "_")))
 }
 
 Log_Binary_Stack("2050", "LD")
 Log_Binary_Stack("2050", "ND")
 Log_Binary_Stack("2050", "AM")

 Log_Binary_Stack("2070", "LD")
 Log_Binary_Stack("2070", "ND")
 Log_Binary_Stack("2070", "AM") 
 
 
 