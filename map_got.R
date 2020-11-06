library(sf)
library(ggplot2)
library(dplyr)
library(ggiraph)

continents = st_read("data/GoTRelease/Continents.shp", crs = 4326)
islands = st_read("data/GoTRelease/Islands.shp", crs = 4326)
lakes = st_read("data/GoTRelease/Lakes.shp", crs = 4326)
landscape = st_read("data/GoTRelease/Landscape.shp", crs = 4326)
locations = st_read("data/GoTRelease/Locations.shp", crs = 4326)
political = st_read("data/GoTRelease/Political.shp", crs = 4326)
regions = st_read("data/GoTRelease/Regions.shp", crs = 4326)
rivers = st_read("data/GoTRelease/Rivers.shp", crs = 4326)
roads = st_read("data/GoTRelease/Roads.shp", crs = 4326)
wall = st_read("data/GoTRelease/Wall.shp", crs = 4326)

locations = locations %>% relocate(type, .after = name)

continentType = data.frame ("type" = rep("continent", length(continents$id)))
islandType = data.frame("type" = rep("island", length(islands$id)))
lakesType = data.frame("type" = rep("lakes", length(lakes$id)))
riverType = data.frame("type" = rep("rivers", length(rivers$id)))
roadsType = data.frame("type" = rep("roads", length(roads$id)))
wallType = data.frame("type" = rep("wall", length(wall$id)))
policalType = data.frame("type" = rep("political", length(political$id)))

allDatas = bind_rows(bind_cols(continents, continentType),
  bind_cols(islands, islandType),
  landscape,
  bind_cols(rivers, riverType),
  bind_cols(lakes, lakesType),
  bind_cols(roads, roadsType),
  bind_cols(wall, wallType),
  locations
)
#bind_cols(continents, continentType),
#bind_cols(continents, continentType),
#regions,
#bind_cols(political, policalType),



spaces = c(
  "continent",
  "forest",
  "mountain",
  "stepp",
  "swamp",
  "lakes",
  "rivers",
  "roads",
  "islands",
  "Castle",
  "city",
  "Other",
  "Ruin",
  "Town",
  "wall",
  "political",
  "desert",
  "land",
  "shore",
  "water"
)

cols = c(
  "ivory",
  "green",
  "#gray88",
  "#669933",
  "#669999",
  "blue",
  "#00CCFF",
  "gray0",
  "lightgoldenrod1",
  "red",
  "black",
  "lightskyblue4",
  "slategray4",
  "lightyellow1",
  "#CCCCCC",
  "orangered3",
  "orange3",
  "moccasin",
  "royalblue",
  "#33CCFF"
)


names(cols) = spaces

levels(allDatas$type) = spaces
map = ggplot(allDatas) + geom_sf(aes(fill=type), size = 0.1)+
  geom_sf(data = locations, fill="black", color = "black")+
  geom_sf_interactive(data=locations, aes(tooltip = name), size = 2)

ggiraph(code = print(map))
