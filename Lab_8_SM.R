#Lab Week 8
library(tidyverse)
library(sf)
library(tmap)
library(leaflet)
library(spatstat)
library(maptools)
library(corrplot)

####make a column graph of Texas Oil Spills

oil_spills <- read_csv("oil_spills.csv")
View(oil_spills)

df <- oil_spills %>% 
  filter(`Accident State` == "TX" & `Accident Year` < 2017) %>% 
  group_by(`Accident Year`) %>% 
  summarise(Loss = sum(`Net Loss (Barrels)`))
df

colnames(df) <- c("Year", "Loss")

ggplot(df, aes(x = Year, y = Loss)) +
  geom_col()