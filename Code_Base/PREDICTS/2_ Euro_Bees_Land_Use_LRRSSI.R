rm(list = ls())
require(lme4)
require(raster)
require(ncdf4)
require(RColorBrewer)
require(tidyr)
require(rgdal)
require(dplyr)
require(yarg)
require(roquefort)
require(Hmisc)
require(arm)
require(RColorBrewer)
require(mgcv)

load("RData_European_Bee_LU_SSI.RData") ### Load in European Bee species species sensitivity index
load("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/Combine/RData_North_Africa_Mask.RData")

Europe_Extent <- extent(-25, 50, 30, 75) #### Europe Extent 

Year <- c("2015","2050","2070")  ### Current and projected years 
i <- 1

### load in rasters for `natural` land the being primary and secondary land use types -- croped and upsampled


for( i in 1:length(Year)){   ### Extract LU change rasters and upsample them so that they are at the same resolution as Climate change projections
Primary_RCP_2.6 <- raster(paste("LUH2_rasters_PW/2.6_primary-",".tif", sep = Year[i])) ### Careful to mention that this does not convey any more information
Primary_RCP_4.5 <- raster(paste("LUH2_rasters_PW/4.5_primary-",".tif", sep = Year[i]))
Primary_RCP_6.0 <- raster(paste("LUH2_rasters_PW/6.0_primary-",".tif", sep = Year[i]))
Primary_RCP_8.5 <- raster(paste("LUH2_rasters_PW/8.5_primary-",".tif", sep = Year[i]))
Primary_RCP_2.6 <- disaggregate(Primary_RCP_2.6, fact = 0.25/ 0.04166667)
Primary_RCP_2.6 <- raster::mask(Primary_RCP_2.6, mask = North_Africa_Mask, inverse = TRUE)
Primary_RCP_4.5 <- disaggregate(Primary_RCP_4.5, fact = 0.25/ 0.04166667)
Primary_RCP_4.5 <- raster::mask(Primary_RCP_4.5, mask = North_Africa_Mask, inverse = TRUE)
Primary_RCP_6.0 <- disaggregate(Primary_RCP_6.0, fact = 0.25/ 0.04166667)
Primary_RCP_6.0 <- raster::mask(Primary_RCP_6.0, mask = North_Africa_Mask, inverse = TRUE)
Primary_RCP_8.5 <- disaggregate(Primary_RCP_8.5, fact = 0.25/ 0.04166667)
Primary_RCP_8.5 <- raster::mask(Primary_RCP_8.5, mask = North_Africa_Mask, inverse = TRUE)
assign(paste("Primary_PREDICTS_Stack",Year[i], sep = "_"), crop(stack(Primary_RCP_2.6,Primary_RCP_4.5,Primary_RCP_6.0,Primary_RCP_8.5),Europe_Extent))  ## Stack each of the RCP scenarios together             

Secondary_RCP_2.6 <- raster(paste("LUH2_rasters_PW/2.6_secondary-",".tif", sep = Year[i])) ### Careful to mention that this does not convey any more information
Secondary_RCP_4.5 <- raster(paste("LUH2_rasters_PW/4.5_secondary-",".tif", sep = Year[i]))
Secondary_RCP_6.0 <- raster(paste("LUH2_rasters_PW/6.0_secondary-",".tif", sep = Year[i]))
Secondary_RCP_8.5 <- raster(paste("LUH2_rasters_PW/8.5_secondary-",".tif", sep = Year[i]))
Secondary_RCP_2.6 <- disaggregate(Secondary_RCP_2.6, fact = 0.25/ 0.04166667)
Secondary_RCP_2.6 <- raster::mask(Secondary_RCP_2.6, mask = North_Africa_Mask, inverse = TRUE)
Secondary_RCP_4.5 <- disaggregate(Secondary_RCP_4.5, fact = 0.25/ 0.04166667)
Secondary_RCP_4.5 <- raster::mask(Secondary_RCP_4.5, mask = North_Africa_Mask, inverse = TRUE)
Secondary_RCP_6.0 <- disaggregate(Secondary_RCP_6.0, fact = 0.25/ 0.04166667)
Secondary_RCP_6.0 <- raster::mask(Secondary_RCP_6.0, mask = North_Africa_Mask, inverse = TRUE)
Secondary_RCP_8.5 <- disaggregate(Secondary_RCP_8.5, fact = 0.25/ 0.04166667)
Secondary_RCP_8.5 <- raster::mask(Secondary_RCP_8.5, mask = North_Africa_Mask, inverse = TRUE)
assign(paste("Secondary_PREDICTS_Stack",Year[i], sep = "_"), crop(stack(Secondary_RCP_2.6,Secondary_RCP_4.5,Secondary_RCP_6.0,Secondary_RCP_8.5),Europe_Extent))  ## Stack each of the RCP scenarios together             

}



Current_LRR <- c()
for(i in 1:nrow(European_Bee_SSI)){
Spp_Name <- European_Bee_SSI[i,1]     #### Species Name
SSI_est <- as.numeric(European_Bee_SSI[i,8])   #### SSI estimate

## Load current distribution map - projected
Species <- raster(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Current_2/","/2_Raster_Tif.tif", sep = sub(" ", ".", Spp_Name))) 
Species_extent <- extent(Species@extent)

## add primary and secondary together to get a total proportion of natural land in each grid cell
Natural_2015 <- (Primary_PREDICTS_Stack_2015 + Secondary_PREDICTS_Stack_2015)
## crop to the species extent
Natural_2015_crop <- crop(Natural_2015,Species_extent)
names(Natural_2015_crop) <- c("RCP2.6", "RCP4.5", "RCP6.0", "RCP8.5")

### convert both rasters to data frames -- be careful that they are the same resolution and extent at this point so that the row match up to their respective cells
Species_frame <- as.data.frame(Species)
Natural_2015_frame <- as.data.frame(Natural_2015_crop)


### bind the dataframes
Species_natural <- cbind(Species_frame,Natural_2015_frame)

### drop NAs
Species_natural <- drop_na(Species_natural)


#### calculate exp(LRR) -- the relative value of non-natural 

### (the proportion of natural x 1) + (proportion of non-natural x exp(Spp_SSI))
Species_natural <- Species_natural %>%
  filter(X2_Raster_Tif == 1) %>%     #### only look at the land use values for where the species is present
  summarise(Scientific_Name = Spp_Name, Proportion_natural_2.6 = mean(RCP2.6), Proportion_non_Natural_2.6 = 1 - mean(RCP2.6), ## Calulate the overall proportion of natural vs non-natural
                                        Proportion_natural_4.5 = mean(RCP4.5), Proportion_non_Natural_4.5 = 1 - mean(RCP4.5), ## for 2015 there are still 4 RCP estimates so I calcualted them all
            Proportion_natural_6.0 = mean(RCP6.0), Proportion_non_Natural_6.0 = 1 - mean(RCP6.0),                            ### all very similar just for continuity when comparing them later.
            Proportion_natural_8.5 = mean(RCP8.5), Proportion_non_Natural_8.5 = 1 - mean(RCP8.5)) %>% 
  mutate(SSI = SSI_est, Bdv_2.6 = (Proportion_natural_2.6 + (Proportion_non_Natural_2.6 * exp(SSI_est))),   ### getting the exp(LRR) estimate
         Bdv_4.5 = (Proportion_natural_4.5 + (Proportion_non_Natural_4.5 * exp(SSI_est))),
         Bdv_6.0 = (Proportion_natural_6.0 + (Proportion_non_Natural_6.0 * exp(SSI_est))),
         Bdv_8.5 = (Proportion_natural_8.5 + (Proportion_non_Natural_8.5 * exp(SSI_est)))) ### logging the exp(LRR) -- I dont know whether this is necessary but I've calculated it anyway.
Current_LRR <- rbind(Current_LRR, Species_natural)
}


#### Now we have the exp(LRR) for the current distribution and Land-use we can then do the same for future land-uses over the species distribution
### Pretty much same again



Future_LRR_2050 <- c()
for(i in 1:nrow(European_Bee_SSI)){
Spp_Name <- European_Bee_SSI[i,1]
SSI_est <- as.numeric(European_Bee_SSI[i,8])
Species <- raster(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Current_2/","/2_Raster_Tif.tif", sep = sub(" ", ".", Spp_Name)))
Species_extent <- extent(Species@extent)


#### Add primary, secondary, crop, name and data frame
Natural_2050 <- (Primary_PREDICTS_Stack_2050 + Secondary_PREDICTS_Stack_2050)
Natural_2050_crop <- crop(Natural_2050, Species_extent)
names(Natural_2050_crop) <- c("RCP2.6", "RCP4.5", "RCP6.0", "RCP8.5")
Natural_2050_frame <- as.data.frame(Natural_2050_crop)

## Frame all the 
Species_frame <- as.data.frame(Species)


Species_Natural_Current_2050 <-cbind(Species_frame,Natural_2050_frame)
Species_Natural_Current_2050 <- drop_na(Species_Natural_Current_2050)
Species_Natural_Current_2050 <- Species_Natural_Current_2050 %>%
  filter(X2_Raster_Tif == 1) %>%
  summarise(Scientific_Name = Spp_Name, Proportion_natural_2.6 = mean(RCP2.6), Proportion_non_Natural_2.6 = 1 - mean(RCP2.6),
            Proportion_natural_4.5 = mean(RCP4.5), Proportion_non_Natural_4.5 = 1 - mean(RCP4.5),
            Proportion_natural_6.0 = mean(RCP6.0), Proportion_non_Natural_6.0 = 1 - mean(RCP6.0),
            Proportion_natural_8.5 = mean(RCP8.5), Proportion_non_Natural_8.5 = 1 - mean(RCP8.5)) %>%
  mutate(SSI = SSI_est, Bdv_2.6_2050 = (Proportion_natural_2.6 + (Proportion_non_Natural_2.6 * exp(SSI_est))),
         Bdv_4.5_2050 = (Proportion_natural_4.5 + (Proportion_non_Natural_4.5 * exp(SSI_est))),
         Bdv_6.0_2050 = (Proportion_natural_6.0 + (Proportion_non_Natural_6.0 * exp(SSI_est))),
         Bdv_8.5_2050 = (Proportion_natural_8.5 + (Proportion_non_Natural_8.5 * exp(SSI_est))))
Future_LRR_2050 <- rbind(Future_LRR_2050, Species_Natural_Current_2050)
}


Future_LRR_2070 <- c()
for(i in 1:nrow(European_Bee_SSI)){
  Spp_Name <- European_Bee_SSI[i,1]
  SSI_est <- as.numeric(European_Bee_SSI[i,8])
  Species <- raster(paste("C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/SDMs/Projections/Current_2/","/2_Raster_Tif.tif", sep = sub(" ", ".", Spp_Name)))
  Species_extent <- extent(Species@extent)
  
  
  #### Add primary, secondary, crop, name and data frame
  Natural_2070 <- (Primary_PREDICTS_Stack_2070 + Secondary_PREDICTS_Stack_2070)
  Natural_2070_crop <- crop(Natural_2070, Species_extent)
  names(Natural_2070_crop) <- c("RCP2.6", "RCP4.5", "RCP6.0", "RCP8.5")
  Natural_2070_frame <- as.data.frame(Natural_2070_crop)
  
  ## Frame all the 
  Species_frame <- as.data.frame(Species)
  
  
  Species_Natural_Current_2070 <-cbind(Species_frame,Natural_2070_frame)
  Species_Natural_Current_2070 <- drop_na(Species_Natural_Current_2070)
  Species_Natural_Current_2070 <- Species_Natural_Current_2070 %>%
    filter(X2_Raster_Tif == 1) %>%
    summarise(Scientific_Name = Spp_Name, Proportion_natural_2.6 = mean(RCP2.6), Proportion_non_Natural_2.6 = 1 - mean(RCP2.6),
              Proportion_natural_4.5 = mean(RCP4.5), Proportion_non_Natural_4.5 = 1 - mean(RCP4.5),
              Proportion_natural_6.0 = mean(RCP6.0), Proportion_non_Natural_6.0 = 1 - mean(RCP6.0),
              Proportion_natural_8.5 = mean(RCP8.5), Proportion_non_Natural_8.5 = 1 - mean(RCP8.5)) %>%
    mutate(SSI = SSI_est, Bdv_2.6_2070 = (Proportion_natural_2.6 + (Proportion_non_Natural_2.6 * exp(SSI_est))),
           Bdv_4.5_2070 = (Proportion_natural_4.5 + (Proportion_non_Natural_4.5 * exp(SSI_est))),
           Bdv_6.0_2070 = (Proportion_natural_6.0 + (Proportion_non_Natural_6.0 * exp(SSI_est))),
           Bdv_8.5_2070 = (Proportion_natural_8.5 + (Proportion_non_Natural_8.5 * exp(SSI_est))))
  Future_LRR_2070 <- rbind(Future_LRR_2070, Species_Natural_Current_2070)
}


Land_Use_SSI <- data.frame(Current_LRR[,c(1,10:14)],Future_LRR_2050[,c(11:14)],Future_LRR_2070[,c(11:14)])
Land_Use_SSI <- Land_Use_SSI %>%
  mutate(LU_SSI_2050_2.6 = log(Bdv_2.6_2050/Bdv_2.6),
            LU_SSI_2050_4.5 = log(Bdv_4.5_2050/Bdv_4.5),
            LU_SSI_2050_6.0 = log(Bdv_6.0_2050/Bdv_6.0),
            LU_SSI_2050_8.5 = log(Bdv_8.5_2050/Bdv_8.5),
            LU_SSI_2070_2.6 = log(Bdv_2.6_2070/Bdv_2.6),
            LU_SSI_2070_4.5 = log(Bdv_4.5_2070/Bdv_4.5),
            LU_SSI_2070_6.0 = log(Bdv_6.0_2070/Bdv_6.0),
            LU_SSI_2070_8.5 = log(Bdv_8.5_2070/Bdv_8.5))


save(Land_Use_SSI,file = "C:/Users/patri/OneDrive - Imperial College London/Work/Biology/(2018-2019) Imperial College London/Summer Project/R/Combine/RData_European_Bee_LU_SSI.RData")


