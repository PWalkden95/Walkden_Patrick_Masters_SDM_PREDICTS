rm(list = ls())
require(raster)
require(dplyr)
require(ggplot2)
require(tidyr)
require(extrafont)
loadfonts(device="win")

setwd("../")
setwd("./Combine")

load("RData_PREDICTS_Land_Use_Response_Ratio_2050_2.RData")   ### load land-use log response 
CC_2050_LD_frame <- readRDS("RData_Climate_Change_Response_Ratio_2050_LD.rds")   ### load climate change log response
CC_2050_ND_frame <- readRDS("RData_Climate_Change_Response_Ratio_2050_ND.rds")
CC_2050_AM_frame <- readRDS("RData_Climate_Change_Response_Ratio_2050_AM.rds")

load("RData_PREDICTS_Land_Use_Response_Ratio_2070_2.RData")   ### load land-use log response 
CC_2070_LD_frame <- readRDS("RData_Climate_Change_Response_Ratio_2070_LD.rds")   ### load climate change log response
CC_2070_ND_frame <- readRDS("RData_Climate_Change_Response_Ratio_2070_ND.rds")
CC_2070_AM_frame <- readRDS("RData_Climate_Change_Response_Ratio_2070_AM.rds")

Combine_log_frame <- cbind(LU_2050_frame,LU_2070_frame,
                           CC_2050_AM_frame,CC_2050_LD_frame,CC_2050_ND_frame,
                           CC_2070_AM_frame,CC_2070_LD_frame,CC_2070_ND_frame)

Date <- "2070"
RCP <- "8.5"
Dispersal <- "LD"

Spatial_Quadrant <- function(Date,RCP,Dispersal){

data <- Combine_log_frame[,c(paste("Log_Response_Ratio_", RCP, sep = paste(Date,"RCP",sep = "_")),
                             paste("CC",Dispersal,"Log_Response_Ratio",Date, paste("RCP",RCP,sep = ""),sep = "_"))] ### combine data frames gric cell shouls line up
data <- drop_na(data)    ### remove NAs
colnames(data) <- c("Land_Use_Log","Climate_Change_Log")

 
data <- data %>%     ##### remove Infinite values
  filter(Climate_Change_Log != Inf & Land_Use_Log != -Inf & Climate_Change_Log != -Inf & Land_Use_Log != Inf) 

summary(data)  ### summary show Na and Infs have been removed

hist(data$Land_Use_Log)

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

LU_95 <- stats::quantile(data$Land_Use_Log, probs = c(0.01, 0.99))
CC_95 <- stats::quantile(data$Climate_Change_Log, probs = c(0.01, 0.99))

### Create a quadrant graph for each of the scenarios


Spatial_quadrant <- ggplot(data, aes(x = Climate_Change_Log, y = Land_Use_Log, col = correlation)) +   
  geom_point(shape = 20, show.legend = FALSE) +
  geom_point( x = mean(data$Climate_Change_Log), y = mean(data$Land_Use_Log), colour = "black", size = 5, shape = 8) +
  geom_smooth(method = "lm", show.legend = FALSE, color = "black")+
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_abline(intercept = 0, slope = -1, linetype = "dashed") +
  xlim(-max(abs(data[,2])),max(abs(data[,2]))) +
  ylim(-max(abs(data[,1])), max(abs(data[,1]))) +
  theme_classic() +
  theme(text=element_text(family = "Times New Roman", size = 25))+
        xlab("Climate Change LRR")+
        ylab("Land-Use Change LRR")+
  scale_color_manual(values = c("darkblue","lightblue","orange" ,"forestgreen"))

Spatial_quadrant
ggsave(paste("Spatial_Quadrants_2/Spatial_quadrant_",".png",sep = paste(Date, RCP, Dispersal, sep = "_")),Spatial_quadrant,device = "png", height = 10, width = 10,dpi=300)

}


Spatial_Quadrant("2050", "2.6", "LD")
Spatial_Quadrant("2050", "4.5", "LD")
Spatial_Quadrant("2050", "6.0", "LD")
Spatial_Quadrant("2050", "8.5", "LD")

Spatial_Quadrant("2050", "2.6", "ND")
Spatial_Quadrant("2050", "4.5", "ND")
Spatial_Quadrant("2050", "6.0", "ND")
Spatial_Quadrant("2050", "8.5", "ND")

Spatial_Quadrant("2050", "2.6", "AM")
Spatial_Quadrant("2050", "4.5", "AM")
Spatial_Quadrant("2050", "6.0", "AM")
Spatial_Quadrant("2050", "8.5", "AM")

Spatial_Quadrant("2070", "2.6", "LD")
Spatial_Quadrant("2070", "4.5", "LD")
Spatial_Quadrant("2070", "6.0", "LD")
Spatial_Quadrant("2070", "8.5", "LD")

Spatial_Quadrant("2070", "2.6", "ND")
Spatial_Quadrant("2070", "4.5", "ND")
Spatial_Quadrant("2070", "6.0", "ND")
Spatial_Quadrant("2070", "8.5", "ND")

Spatial_Quadrant("2070", "2.6", "AM")
Spatial_Quadrant("2070", "4.5", "AM")
Spatial_Quadrant("2070", "6.0", "AM")
Spatial_Quadrant("2070", "8.5", "AM")
Date <- "2070"
RCP <- "2.6"
Dispersal <- "LD"

Describe_LRR <- function(Date,RCP,Dispersal){
  
  data <- Combine_log_frame[,c(paste("Log_Response_Ratio_", RCP, sep = paste(Date,"RCP",sep = "_")),
                               paste("CC",Dispersal,"Log_Response_Ratio",Date, paste("RCP",RCP,sep = ""),sep = "_"))] ### combine data frames gric cell shouls line up
  data <- drop_na(data)    ### remove NAs
  colnames(data) <- c("Land_Use_Log","Climate_Change_Log")
  
  
  data <- data %>%     ##### remove Infinite values
    filter(Climate_Change_Log != Inf & Land_Use_Log != -Inf & Climate_Change_Log != -Inf & Land_Use_Log != Inf) 
  
  DescrLRR <- data.frame(as.character(paste(Date,Dispersal, RCP, sep = "_")),mean(data$Land_Use_Log),sd(data$Land_Use_Log),min(data$Land_Use_Log), max(data$Land_Use_Log),
                         mean(data$Climate_Change_Log), sd(data$Climate_Change_Log), min(data$Climate_Change_Log), max(data$Climate_Change_Log), length(which(data$Land_Use_Log > 0 & data$Climate_Change_Log > 0)),
                         length(which(data$Land_Use_Log < 0 & data$Climate_Change_Log < 0)),length(which(data$Land_Use_Log > 0 & data$Climate_Change_Log < 0)),length(which(data$Land_Use_Log < 0 & data$Climate_Change_Log > 0)))
  
  
  colnames(DescrLRR) <- c("Scenario",
                          "mean_Land_Use_LRR",
                          "sd_Land_Use_LRR",
                          "min_Land_Use_LRR",
                          "max_Land_Use_LRR",
                          "mean_Climate_Change_LRR",
                          "sd_Climate_Change_LRR",
                          "min_Climate_Change_LRR",
                          "max_Climate_Change_LRR",
                          "Both Positive",
                          "Both Negative",
                          "Land-Use Positive",
                          "Climate-Postive")
  DescrLRR
}

Descriptives <- rbind(Describe_LRR("2070", "2.6", "LD"),
                      Describe_LRR("2070", "4.5", "LD"),
                      Describe_LRR("2070", "6.0", "LD"),
                      Describe_LRR("2070", "8.5", "LD"),
                      
                      Describe_LRR("2070", "2.6", "ND"),
                      Describe_LRR("2070", "4.5", "ND"),
                      Describe_LRR("2070", "6.0", "ND"),
                      Describe_LRR("2070", "8.5", "ND"),
                      
                      Describe_LRR("2070", "2.6", "AM"),
                      Describe_LRR("2070", "4.5", "AM"),
                      Describe_LRR("2070", "6.0", "AM"),
                      Describe_LRR("2070", "8.5", "AM"),
                    
                      Describe_LRR("2050", "2.6", "LD"),
                      Describe_LRR("2050", "4.5", "LD"),
                      Describe_LRR("2050", "6.0", "LD"),
                      Describe_LRR("2050", "8.5", "LD"),
                      
                      Describe_LRR("2050", "2.6", "ND"),
                      Describe_LRR("2050", "4.5", "ND"),
                      Describe_LRR("2050", "6.0", "ND"),
                      Describe_LRR("2050", "8.5", "ND"),
                      
                      Describe_LRR("2050", "2.6", "AM"),
                      Describe_LRR("2050", "4.5", "AM"),
                      Describe_LRR("2050", "6.0", "AM"),
                      Describe_LRR("2050", "8.5", "AM"))
mean(as.numeric(Descriptives[c(9:12),7]))





