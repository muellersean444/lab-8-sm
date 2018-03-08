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

###Leaflet plot of spill locations in TX in 2016
df_loc <- oil_spills %>% 
  filter(`Accident State` == "TX" & `Accident Year` == 2016) %>% 
  select(Latitude, Longitude, `Net Loss (Barrels)`)

colnames(df_loc) <- c("latitude", "longitude", "net_loss")

oil_sf <- st_as_sf(df_loc, coords = c("longitude", "latitude"), crs = 4326)

leaflet(oil_sf) %>% 
  addTiles() %>% 
  addMarkers()