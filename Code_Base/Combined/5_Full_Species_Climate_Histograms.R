require(ggplot2)
require(tidyr)
require(dplyr)
setwd("../")
setwd("./Combine")

Date <- "2070"
RCP <- "2.6"
Dispersal <- "LD"


climate_hist <- function(Date,RCP,Dispersal){

data <- Climate_SSI %>%
  dplyr::select(grep(pattern = paste(Dispersal,Date,RCP,sep = "_"),x = colnames(Climate_SSI)))
colnames(data) <- "Climate_LRR"
data$above <- data$Climate_LRR > 0

hist <- ggplot(data = data, aes(x= Climate_LRR, fill = above))+
  geom_histogram(show.legend = FALSE, bins = 40) +
  geom_vline(xintercept = 0)+
  xlab("Climate LRR") +
  ylab("Frequency") +
  labs(title = paste(Dispersal,Date, RCP, sep = "_"),
       caption = paste(paste("N > 0 = ", length(which(data$above))),
                       paste("N < 0 = ", length(which(data$Climate_LRR < 0))))) +
  theme_classic() 

hist
ggsave(paste("Full_Species_Climate_SSI/Full_Species_Hist_",".png",sep = paste(Dispersal,Date,RCP,sep = "_")),hist,device = "png", height = 10, width = 10,dpi=300)
}

climate_hist("2070","2.6", "LD")
climate_hist("2070","4.5", "LD")
climate_hist("2070","6.0", "LD")
climate_hist("2070","8.5", "LD")


climate_hist("2070","2.6", "ND")
climate_hist("2070","4.5", "ND")
climate_hist("2070","6.0", "ND")
climate_hist("2070","8.5", "ND")


climate_hist("2070","2.6", "AM")
climate_hist("2070","4.5", "AM")
climate_hist("2070","6.0", "AM")
climate_hist("2070","8.5", "AM")


climate_hist("2050","2.6", "LD")
climate_hist("2050","4.5", "LD")
climate_hist("2050","6.0", "LD")
climate_hist("2050","8.5", "LD")


climate_hist("2050","2.6", "ND")
climate_hist("2050","4.5", "ND")
climate_hist("2050","6.0", "ND")
climate_hist("2050","8.5", "ND")


climate_hist("2050","2.6", "AM")
climate_hist("2050","4.5", "AM")
climate_hist("2050","6.0", "AM")
climate_hist("2050","8.5", "AM")

