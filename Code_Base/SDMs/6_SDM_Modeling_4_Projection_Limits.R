rm(list = ls())
require(sExtinct)   ### Load the package to perform the optimal linear estimation 
require(dplyr)     #### just for data manipulation

load(file = "RData_European_Bee_Species_Thinned_Data_5km.RData")   ### load presence data 


OLE_Coord_Limits <- c()
for( i in 1:length(Thinned_Data_5km)){

latitude <-Thinned_Data_5km[[i]]["decimallatitude"]               #### Extract decimal latitude

latitude <- latitude %>%            ### Round latitude values and calculate each frequency 
  mutate(round = round(decimallatitude)) %>%
  group_by(round) %>%
  summarise(frequency = length(round)) %>%
  ungroup()

Round_latitude <- data.frame(latitude)                         ## convert to data frame

Transform_latitude <- Round_latitude %>% group_by(round) %>%    #### to be able to deal with negative latitude values transform each latitude by adding 90 to each value
  mutate(Transform_latitude  = 90 + as.numeric(round))

Max_Latitude <- data.frame(Transform_latitude[,c(3:2)])    #### Convert to data frame 

Maximum_Latitude_est <- sExtinct::OLE(Max_Latitude, alpha = 0.05)   ### perform OLE analysis to estimate the likely upper-bound of species ranges 
Maximum_Latitude_est <- Maximum_Latitude_est$Estimate - 90          #### transform value back by minusing 90 again




Min_Latitude <- Transform_latitude %>% group_by(Transform_latitude) %>%    ### now to calculate the minimum likely bound of species ranges  
  mutate(latitude_min = 180 - Transform_latitude)                          ## basically flip the latitudes around as OLE only calculates upper bound 

Min_Latitude <- data.frame(Min_Latitude[,c(4,2)])   ## convert to data frame

Minimum_Latitude_est <- sExtinct::OLE(Min_Latitude, alpha = 0.05)   ## perform OLE
Minimum_Latitude_est <- (180 - Minimum_Latitude_est$Estimate) - 90   #### Flip coorindates back so that you have calculated the lower bound and then transform back


longitude <-Thinned_Data_5km[[i]]["decimallongitude"]            ##### Rinse and repeat for longitude.

longitude <- longitude %>%
  mutate(round = round(decimallongitude)) %>%
  group_by(round) %>%
  summarise(frequency = length(round)) %>%
  ungroup()

Round_longitude <- data.frame(longitude)

Transform_longitude <- Round_longitude %>% group_by(round) %>%
  mutate(Transform_longitude  = 180 + as.numeric(round))


Max_longitude <- data.frame(Transform_longitude[,c(3,2)])


Maximum_Longitude_est <- sExtinct::OLE(Max_longitude, alpha = 0.05)
Maximum_Longitude_est <- Maximum_Longitude_est$Estimate - 180



Min_longitude <- Transform_longitude %>% group_by(Transform_longitude) %>%
  mutate(Min_Trans_Longitude = 360 - Transform_longitude)

Min_longitude <- data.frame(Min_longitude[,c(4,2)])

Minimum_Longitude_est <- sExtinct::OLE(Min_longitude, alpha = 0.05)
Minimum_Longitude_est <- (360 - Minimum_Longitude_est$Estimate) - 180

Spp_name <- Thinned_Data_5km[[i]][["species"]][1]

Coord_Limits <- data.frame(Spp_name,
                           Minimum_Longitude_est, min(Thinned_Data_5km[[i]][["decimallongitude"]]),
                           Maximum_Longitude_est, max(Thinned_Data_5km[[i]][["decimallongitude"]]),
                           Minimum_Latitude_est, min(Thinned_Data_5km[[i]][["decimallatitude"]]),
                           Maximum_Latitude_est, max(Thinned_Data_5km[[i]][["decimallatitude"]]))
colnames(Coord_Limits)[c(3,5,7,9)] <- c("Min_Presence_Longitude", "Max_Presence_Longitude", "Min_Presence_Latitude","Max_Presence_Latitude")


OLE_Coord_Limits <-rbind(OLE_Coord_Limits,Coord_Limits)

}

save(OLE_Coord_Limits, file = "RData_European_Bee_Species_OLE_Coord_Limits.RData")
