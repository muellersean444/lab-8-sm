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

## tmap plot with the Texas state shapefile
states <- st_read(dsn = ".", layer = "states")

tex_border <- states %>% 
  filter(STATE_NAME == "Texas") %>% 
  st_transform(4326)

plot(tex_border)

tm_shape(tex_border) + 
  tm_polygons() +
  tm_shape(oil_sf) +
  tm_dots(size=0.3)

#are these spatially random locations?

##Convert the data to spatial points patterns (combination of point data and the bounding window)

spill_sp <- as(oil_sf, "Spatial")
spill_ppp <- as(spill_sp, "ppp")

tx_sp <- as(tex_border, "Spatial")
tx_owin <- as(tx_sp, "owin")

#point pattern analysis is 

all_ppp <- ppp(spill_ppp$x, spill_ppp$y, window = tx_owin)


###A density plot:
plot(density(all_ppp, sigma = 0.4))
###^ need a meaningful way to determine sigma bc that parameter really affects your graph

###Quadrant test for spatial evenness
#question is are oil spills evenly distributed throughout the state
oil_qt <- quadrat.test(all_ppp, nx = 5, ny = 5)
oil_qt #so not randomly distributed

# data:  all_ppp
#X2 = 589.36, df = 17, p-value < 2.2e-16
#alternative hypothesis: two.sided

plot(all_ppp)
plot(oil_qt, add = TRUE, cex = 0.4)

###G - function for nearest neighbor analysis
r <- seq(0,1, by =0.1)

oil_gfun <- envelope(all_ppp, fun = Gest, r = r, nsim = 100)

ggplot(oil_gfun, aes(x = r, y = obs)) + 
  geom_line(color = "black") +
  geom_line(aes(x = r, y = theo), color = "red")
  
#what this is telling us is that r observed data has higher proportion of pijnt pairs with nearest neighbors at shorter distances compared to CSR data. On average, our data points tend to have a closer nearer neighbor than if it were TRULY randomly distributed.   

###Nearest neighbor using the L-function (Ripley's K, standardized)

r2 <- seq(0,3, by = 0.5)
oil_lfun <- envelope(all_ppp, fun = Lest, r = r2, rsim = 20, global = TRUE)

ggplot(oil_lfun, aes(x = r2, y = obs)) + 
  geom_line(color = "black") +
  geom_line(aes(x = r2, y = theo), color = "blue")