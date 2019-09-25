rm(list = ls())
require(dplyr)
require(tidyr)
require(ggplot2)
require(extrafont)
loadfonts(device="win")

setwd("../")
setwd("./Combine")
 
load("RData_European_Bee_CC_SSI.RData")
load("RData_European_Bee_LU_SSI.RData")

mean <- range(Land_Use_SSI$SSI)

Date <- "2050"
RCP <- "8.5"

combine_SSI <- cbind(Climate_SSI, Land_Use_SSI[,c(15:22)])
 
Species_Quadrant <- function(Date, RCP){

Dispersal <- c("LD", "AM", "ND")

i <- 2
for(i in 1:length(Dispersal)){
data <- combine_SSI[,c("Scientific_Name",paste("SSI",Dispersal[i],Date,RCP,sep = "_"), paste("LU_SSI",Date,RCP,sep = "_"))]
data <- subset(data, data[,2] != -Inf & data[,2] != Inf & data[,3] != -Inf & data[,3] != Inf)
colnames(data)[2:3] <- c("Climate_SSI", "Land_Use_SSI")

Mean <- data %>%
  summarise(Mean_LU = mean(Land_Use_SSI),
            Mean_CC = mean(Climate_SSI))

length(which(data$Climate_SSI < 0 & data$Land_Use_SSI  < 0))



quadrant_SSI <- ggplot(data, aes(x = Climate_SSI, y = Land_Use_SSI, col = Scientific_Name)) +   
  geom_point( show.legend = FALSE) +
  geom_point( x = mean(data$Climate_SSI), y = mean(data$Land_Use_SSI), colour = "black", size = 5, shape = 8 ) +
  geom_segment(aes(xend = 0, yend = 0), show.legend = FALSE) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  xlim(-(max(abs(data[,2]))),max(abs(data[,2]))) +
  ylim(-(max(abs(data[,3]))),max(abs(data[,3]))) +
  theme_classic() +
  ggtitle(paste(Dispersal[i],Date,RCP, sep = "_")) +
  xlab("Climate LRR")+
  ylab("Land-Use LRR") +
  theme(text=element_text(family = "Times New Roman", size = 20))


frame <- data.frame("nrow" = nrow(data), "Both Positive" = length(which(data[,2] > 0 & data[,3] > 0)),
                    "Both Negative" = length(which(data[,2] < 0 & data[,3] < 0)),
                    "Climate-change Negative" = length(which(data[,2] < 0 & data[,3] > 0)),
                    "Land-Use Negative" = length(which(data[,2] > 0 & data[,3] < 0)))



assign(paste("Species_quarant_data",Dispersal[i],Date,RCP, sep = "_"), frame)
ggsave(paste("Species_Quadrants/Species_Quadrant_",".png",sep = paste(Dispersal[i],Date,RCP,sep = "_")),quadrant_SSI,device = "png", height = 10, width = 10,dpi=300)
}
}

Species_Quadrant("2050", "2.6")
Species_Quadrant("2050", "4.5")
Species_Quadrant("2050", "6.0")
Species_Quadrant("2050", "8.5")

Species_Quadrant("2070", "2.6")
Species_Quadrant("2070", "4.5")
Species_Quadrant("2070", "6.0")
Species_Quadrant("2070", "8.5")


