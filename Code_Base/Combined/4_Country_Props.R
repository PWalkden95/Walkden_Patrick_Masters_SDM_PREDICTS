rm(list = ls())
require(dplyr)
require(tidyr)
require(ggplot2)
require(extrafont)
loadfonts(device="win")
setwd("../")
setwd("./Combine")

Country_props <- readRDS("Country_Proportions.rds")

Date <- "2070"
RCP <-"2.6"
Dispersal <- "LD"


Country_Prop_Graph <- function(Date, RCP, Dispersal){

data <- Country_props %>%
  dplyr::select(Country,grep(paste(RCP,Date,Dispersal,sep = "_"), colnames(Country_props)))

colnames(data)[2:5] <- c("1_Both Positive", "3_Both Negative", "2_Climate Change Negative", "4_Land_Use Change Negative")
data <- data %>% gather(Quadrant, Proportion,  -c("Country"), na.rm = TRUE)
data <- data %>% arrange(Country)
data$Country <- sub("_", " ", data$Country)
data <- data %>%
  filter(Country != "Iceland" & Country != "Russia")


stacked <- ggplot(data, aes(x = Country, y = Proportion))+
  geom_col(aes(fill = Quadrant), width = 0.7) +
  scale_fill_manual(values = c("lightblue","orange" ,"darkblue","forestgreen")) +
  theme_classic() +
  theme(text=element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 30, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        legend.position =  "none")

stacked

ggsave(paste("Country_Proportions/Threat_Proportions", ".png", sep = paste(RCP,Date,Dispersal,sep = "_")),stacked,device = "png", height = 10, width = 40,dpi=300)
}

Country_Prop_Graph("2050", "2.6", "LD")
Country_Prop_Graph("2050", "4.5", "LD")
Country_Prop_Graph("2050", "6.0", "LD")
Country_Prop_Graph("2050", "8.5", "LD")

Country_Prop_Graph("2050", "2.6", "ND")
Country_Prop_Graph("2050", "4.5", "ND")
Country_Prop_Graph("2050", "6.0", "ND")
Country_Prop_Graph("2050", "8.5", "ND")

Country_Prop_Graph("2050", "2.6", "AM")
Country_Prop_Graph("2050", "4.5", "AM")
Country_Prop_Graph("2050", "6.0", "AM")
Country_Prop_Graph("2050", "8.5", "AM")

Country_Prop_Graph("2070", "2.6", "LD")
Country_Prop_Graph("2070", "4.5", "LD")
Country_Prop_Graph("2070", "6.0", "LD")
Country_Prop_Graph("2070", "8.5", "LD")

Country_Prop_Graph("2070", "2.6", "ND")
Country_Prop_Graph("2070", "4.5", "ND")
Country_Prop_Graph("2070", "6.0", "ND")
Country_Prop_Graph("2070", "8.5", "ND")

Country_Prop_Graph("2070", "2.6", "AM")
Country_Prop_Graph("2070", "4.5", "AM")
Country_Prop_Graph("2070", "6.0", "AM")
Country_Prop_Graph("2070", "8.5", "AM")
