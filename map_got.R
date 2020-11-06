library(sf)
library(ggplot2)
library(dplyr)
library(ggiraph)
#continents = st_read("data/GoTRelease/Continents.shp", crs=4326)
#episodes = st_read("data/episodes.csv", crs=4326)
#appearances = st_read("data/appearances.csv", crs=4326)
#characters = st_read("data/characters.csv", crs=4326)
#populations = st_read("data/populations.csv", crs=4326)
#scenes = read_csv("data/scenes.csv", crs=4326)
continents = st_read("data/GoTRelease/Continents.shp", crs=4326)
islands = st_read("data/GoTRelease/Islands.shp", crs=4326)
lakes = st_read("data/GoTRelease/Lakes.shp", crs=4326)
land = st_read("data/GoTRelease/Land.shp", crs=4326)
landscape = st_read("data/GoTRelease/Landscape.shp", crs=4326)
locations = st_read("data/GoTRelease/Locations.shp", crs=4326)
political = st_read("data/GoTRelease/Political.shp", crs=4326)
regions = st_read("data/GoTRelease/Regions.shp", crs=4326)
rivers = st_read("data/GoTRelease/Rivers.shp", crs=4326)
roads = st_read("data/GoTRelease/Roads.shp", crs=4326)
scenesLocations = st_read("data/GoTRelease/ScenesLocations.shp", crs=4326)
wall = st_read("data/GoTRelease/Wall.shp",crs=4326)

montains = landscape %>%  filter(type=="montain")
forests = landscape %>%  filter(type=="forest")
swamp = landscape %>%  filter(type=="swamp")
stepp = landscape %>%  filter(type=="stepp")

map = ggplot()+geom_sf(data=continents$geometry,fill="ivory",color="ivory3")

map=ggplot(locations)+geom_sf(data=continents$geometry,fill="ivory",color="ivory3")+
  geom_sf(data = islands$geometry, fill="ivory",color="ivory3") +
  geom_sf(data = forests$geometry, fill="green",color="ivory")+   
  geom_sf(data = montains$geometry, fill="#CCCCCC",color="ivory")+ 
  geom_sf(data = swamp$geometry, fill="#669999",color="ivory")+ 
  geom_sf(data = stepp$geometry, fill="#669933",color="ivory")+
  geom_sf(data = lakes$geometry, fill="#33CCFF",color="black")+ 
  geom_sf(data = rivers$geometry, fill="#00CCFF",color="blue")+ 
  geom_sf(data = roads$geometry, fill="#666666",color="#666666")

mapLocation = map +
  geom_sf(data = locations$geometry, fill="#FFCCCC",color="#FFCCCC")

my_gg <- mapLocation + geom_sf_interactive(aes(tooltip = name), size = 2)

ggiraph(code = print(my_gg))




