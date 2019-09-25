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

United_Kingdom <- Polygons[32]
Turkey <- Polygons[37]
Switzerland <- Polygons[48]
Sweden <- Polygons[49]
Spain <- Polygons[55]
Slovakia <- Polygons[61]
Slovenia <- Polygons[62]
Serbia <- Polygons[66]
Romania <- Polygons[77]
Portugal <- Polygons[79]
Poland <- Polygons[80]
Norway <- Polygons[89]
Netherlands <- Polygons[97]
Luxembourg <- Polygons[120]
Lithuania <- Polygons[121]
Liechtenstein <- Polygons[122]
Latvia <- Polygons[126]
Kosovo <- Polygons[131]
Italy <- Polygons[138]
Ireland <-Polygons[141]
Hungary <- Polygons[147]
Greece <- Polygons[155]
Germany <- Polygons[157]
Georgia <- Polygons[158]
France <- Polygons[161]
Finland <- Polygons[170]
Estonia <- Polygons[173]
Denmark <- Polygons[184]
Czech_Republic <- Polygons[185]
Cyprus <- bind(Polygons[186], Polygons[187])
Croatia <- Polygons[189]
Bulgaria <- Polygons[209]
Bosnia_and_Herz <- Polygons[213]
Belgium <- Polygons[218]
Belarus <- Polygons[219]
Austria <- Polygons[224]
Andorra <- Polygons[235]
Albania <- Polygons[237]
Russia <- Polygons[76]

Europe_Shapefiles <- list(United_Kingdom,Turkey,Switzerland,Sweden,Spain,Slovakia,Slovenia,Serbia,Romania,Portugal,Poland,
                          Norway, Netherlands, Luxembourg, Lithuania, Liechtenstein, Latvia, Italy, Ireland, Iceland, Hungary, Greece,
                          Germany, Georgia, France, Finland, Estonia, Denmark, Czech_Republic, Cyprus, Croatia, Bulgaria, Bosnia_and_Herz,
                          Belgium, Belarus, Austria, Andorra, Albania, Russia)

Europe_Shape <- bind(United_Kingdom,Turkey,Switzerland,Sweden,Spain,Slovakia,Slovenia,Serbia,Romania,Portugal,Poland,
                     Norway, Netherlands, Luxembourg, Lithuania, Liechtenstein, Latvia, Italy, Ireland, Iceland, Hungary, Greece,
                     Germany, Georgia, France, Finland, Estonia, Denmark, Czech_Republic, Cyprus, Croatia, Bulgaria, Bosnia_and_Herz,
                     Belgium, Belarus, Austria, Andorra, Albania, Russia)


names(Europe_Shapefiles) <- c("United_Kingdom","Turkey","Switzerland","Sweden","Spain","Slovakia","Slovenia","Serbia","Romania","Portugal","Poland",
                              "Norway", "Netherlands", "Luxembourg", "Lithuania", "Liechtenstein", "Latvia", "Italy", "Ireland", "Iceland", "Hungary", "Greece",
                              "Germany", "Georgia", "France", "Finland", "Estonia", "Denmark", "Czech_Republic", "Cyprus", "Croatia", "Bulgaria", "Bosnia_and_Herz",
                              "Belgium", "Belarus", "Austria", "Andorra", "Albania", "Russia")




Proportion_Countries <- c()
for(i in 1:length(Europe_Shapefiles)){
  
  log_repsonse_map <- function(Date, RCP, Dispersal){
    Current <- raster("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Current_2/All_bee_Raster_Tif.tif")
    richness <- raster(paste(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Future_","/All_bee_Raster_", sep = paste(Date,RCP,sep = "_")),"_Tif.tif", sep = paste(Dispersal, sep = "_")))
    log_response_ratio <- log(richness/Current)
    log_response_ratio <- raster::mask(log_response_ratio, mask = Europe_Shapefiles[[i]], inverse = FALSE)
    plot(log_response_ratio, zlim = c(-6, 6))
    log_response_ratio
  }
  
  
  
  
  Log_Stack <- function(Date, Dispersal){
    
    log_response_2.6 <- log_repsonse_map(Date, "2.6", Dispersal)
    log_response_2.6 <- raster::mask(log_response_2.6, mask = Europe_Shapefiles[[i]], inverse = FALSE)
    
    log_response_4.5 <- log_repsonse_map(Date, "4.5", Dispersal)
    log_response_4.5 <- raster::mask(log_response_4.5, mask = Europe_Shapefiles[[i]], inverse = FALSE)
    
    log_response_6.0 <- log_repsonse_map(Date, "6.0", Dispersal)
    log_response_6.0 <- raster::mask(log_response_6.0, mask = Europe_Shapefiles[[i]], inverse = FALSE)
    
    log_response_8.5 <- log_repsonse_map(Date, "8.5", Dispersal)
    log_response_8.5 <- raster::mask(log_response_8.5, mask = Europe_Shapefiles[[i]], inverse = FALSE)
    
    log_response_CC <- stack(log_response_2.6,
                             log_response_4.5,
                             log_response_6.0,
                             log_response_8.5)
    
    names(log_response_CC) <- c(paste(paste("CC", Dispersal,sep = "_"),"Log_Response_Ratio", Date, "RCP2.6", sep = "_"),
                                paste(paste("CC", Dispersal,sep = "_"),"Log_Response_Ratio", Date, "RCP4.5", sep = "_"),
                                paste(paste("CC", Dispersal,sep = "_"),"Log_Response_Ratio", Date, "RCP6.0", sep = "_"),
                                paste(paste("CC", Dispersal,sep = "_"),"Log_Response_Ratio", Date, "RCP8.5", sep = "_"))  
    
    
    CC_frame <- raster::as.data.frame(log_response_CC)
  }
  
  
  
  Europe_Extent <- extent(-25, 50, 30, 75) 
  
  Year <- c("2015","2050","2070")
  
  
  for( j in 1:length(Year)){   ### Extract LU change rasters and upsample them so that they are at the same resolution as Climate change projections
    RCP_2.6 <- raster(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/PREDICTS/LUH2_rasters_PW2/two_PREDICTS_RCP2.6-",".tif", sep = Year[j])) ### Careful to mention that this does not convey any more information
    RCP_4.5 <- raster(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/PREDICTS/LUH2_rasters_PW2/two_PREDICTS_RCP4.5-",".tif", sep = Year[j]))
    RCP_6.0 <- raster(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/PREDICTS/LUH2_rasters_PW2/two_PREDICTS_RCP6.0-",".tif", sep = Year[j]))
    RCP_8.5 <- raster(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/PREDICTS/LUH2_rasters_PW2/two_PREDICTS_RCP8.5-",".tif", sep = Year[j]))
    RCP_2.6 <- disaggregate(RCP_2.6, fact = 0.25/ 0.04166667)
    RCP_2.6 <- raster::mask(RCP_2.6, mask = Europe_Shapefiles[[i]], inverse = FALSE)
    RCP_4.5 <- disaggregate(RCP_4.5, fact = 0.25/ 0.04166667)
    RCP_4.5 <- raster::mask(RCP_4.5, mask = Europe_Shapefiles[[i]], inverse = FALSE)
    RCP_6.0 <- disaggregate(RCP_6.0, fact = 0.25/ 0.04166667)
    RCP_6.0 <- raster::mask(RCP_6.0, mask = Europe_Shapefiles[[i]], inverse = FALSE)
    RCP_8.5 <- disaggregate(RCP_8.5, fact = 0.25/ 0.04166667)
    RCP_8.5 <- raster::mask(RCP_8.5, mask = Europe_Shapefiles[[i]], inverse = FALSE)
    assign(paste("PREDICTS_Stack",Year[j], sep = "_"), crop(stack(RCP_2.6,RCP_4.5,RCP_6.0,RCP_8.5),Europe_Extent))  ## Stack each of the RCP scenarios together             
  }
  
  
  plot(PREDICTS_Stack_2015)
  plot(PREDICTS_Stack_2050)
  plot(PREDICTS_Stack_2070)
  
  
  
  
  LU_2050 <- log(PREDICTS_Stack_2050/PREDICTS_Stack_2015) #### get the log response ratio of relative species richness for each RCP scenario
  names(LU_2050) <- c("Log_Response_Ratio_2050_RCP2.6",
                      "Log_Response_Ratio_2050_RCP4.5",
                      "Log_Response_Ratio_2050_RCP6.0",
                      "Log_Response_Ratio_2050_RCP8.5")
  
  
  
  LU_2070 <- log(PREDICTS_Stack_2070/PREDICTS_Stack_2015)  ## same for 2070
  names(LU_2070) <- c("Log_Response_Ratio_2070_RCP2.6",
                      "Log_Response_Ratio_2070_RCP4.5",
                      "Log_Response_Ratio_2070_RCP6.0",
                      "Log_Response_Ratio_2070_RCP8.5")
  
  
  
  LU_2050_frame <- as.data.frame(LU_2050)     ### extract the values into a data frame for each time period
  LU_2070_frame <- as.data.frame(LU_2070)
  
  LU_frame <- cbind(LU_2050_frame,LU_2070_frame)
  
  
  
  
  
  
  LU_Log_CC_func <- function(Date,Dispersal){
    RCP_2<- c("2.6", "4.5", "6.0", "8.5")  
    LU_CC_Log_frame <- data.frame(Country = as.character(names(Europe_Shapefiles[i])))  
    
    
    for(k in 1:length(RCP_2)){
      data_1 <- Log_Stack(Date, Dispersal)
      data_1 <- data_1 %>%
        dplyr::select(grep(pattern = as.character(RCP_2[k]), colnames(data_1)))
      
      
      data_2 <- LU_frame
      
      data_2 <- data_2 %>%
        dplyr::select(grep(pattern = paste(Date,paste("RCP",RCP_2[k],sep = ""),sep = "_"), colnames(data_2)))
      
      
      data <- cbind(data_2, data_1)
      
      data <- drop_na(data)    ### remove NAs
      colnames(data) <- c("Land_Use_Log","Climate_Change_Log")
      
      
      data <- data %>%     ##### remove Infinite values
        filter(Climate_Change_Log != Inf & Land_Use_Log != -Inf & Climate_Change_Log != -Inf & Land_Use_Log != Inf) 
      
      ### summary show Na and Infs have been removed
      
      
      
      data <- data %>%                  #### create a new column for each scenario for what we estimate the sensitivity of eacg grid cell to climate and land-use change 
        mutate(correlation = ifelse(Climate_Change_Log > 0 & Land_Use_Log > 0  | Climate_Change_Log > 0 & Land_Use_Log == 0 | Climate_Change_Log == 0 & Land_Use_Log > 0,
                                    "Both Positive", ":)"),
               correlation = ifelse(Land_Use_Log < 0 | Land_Use_Log < 0 & Climate_Change_Log == 0 ,
                                    "Land-Use Negative", correlation),
               correlation = ifelse(Climate_Change_Log < 0 & Land_Use_Log > 0  | Climate_Change_Log < 0 & Land_Use_Log ==0,
                                    "Climate-Change Negative", correlation),
               correlation = ifelse(Climate_Change_Log < 0 & Land_Use_Log < 0 ,
                                    "Both Negative", correlation),
               correlation = ifelse(Climate_Change_Log == 0 & Land_Use_Log == 0 ,
                                    "Both Positive", correlation))
      
      
      Country_Prop <- data.frame(as.character(names(Europe_Shapefiles[i])),
                                 length(which(data$correlation == "Both Positive"))/nrow(data),
                                 length(which(data$correlation == "Both Negative"))/nrow(data),
                                 length(which(data$correlation == "Climate-Change Negative"))/nrow(data),
                                 length(which(data$correlation == "Land-Use Negative"))/nrow(data))
      
      colnames(Country_Prop) <- c("Country",paste("N_Both_Positive",RCP_2[k], Date,Dispersal, sep = "_"),paste("N_Both_Negative", RCP_2[k], Date,Dispersal, sep = "_"),
                                  paste("N_Climate_Change_Negative", RCP_2[k], Date,Dispersal, sep = "_"), paste("N_Land_Use_Negative", RCP_2[k],Date,Dispersal,sep = "_"))
      
      
      
      LU_CC_Log_frame <- right_join(LU_CC_Log_frame, Country_Prop, by = "Country")
    }
    LU_CC_Log_frame
  }
  
  
  LD_2050 <- LU_Log_CC_func("2050", "LD")
  ND_2050 <- LU_Log_CC_func("2050", "ND")
  AM_2050 <- LU_Log_CC_func("2050", "AM")
  
  LD_2070 <- LU_Log_CC_func("2070", "LD")
  ND_2070 <- LU_Log_CC_func("2070", "ND")
  AM_2070 <- LU_Log_CC_func("2070", "AM")
  
  Country_frame <- right_join(LD_2050, ND_2050, by = "Country")
  Country_frame <- right_join(Country_frame, AM_2050, by = "Country")
  Country_frame <- right_join(Country_frame, LD_2070, by = "Country")
  Country_frame <- right_join(Country_frame, ND_2070, by = "Country")
  Country_frame <- right_join(Country_frame, AM_2070, by = "Country")
  
  Proportion_Countries <- rbind(Proportion_Countries, Country_frame)
}


saveRDS(Proportion_Countries, file = "Country_Proportions.rds")
