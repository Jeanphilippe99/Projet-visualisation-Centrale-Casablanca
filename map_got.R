library(sf)
library(ggplot2)
library(dplyr)
library(ggiraph)
library(roxygen2)

#' import de la base de données

continents = st_read("data/GoTRelease/Continents.shp", crs = 4326)
islands = st_read("data/GoTRelease/Islands.shp", crs = 4326)
lakes = st_read("data/GoTRelease/Lakes.shp", crs = 4326)
landscape = st_read("data/GoTRelease/Landscape.shp", crs = 4326)
locations = st_read("data/GoTRelease/Locations.shp", crs = 4326)
rivers = st_read("data/GoTRelease/Rivers.shp", crs = 4326)
roads = st_read("data/GoTRelease/Roads.shp", crs = 4326)
wall = st_read("data/GoTRelease/Wall.shp", crs = 4326)


#' creation d'une dataframe contenant toutes les datas utiles.
#' Pour pouvoir reconnaitre les differentes datas, ajout de l'attribut type

#'\code{addTypeAttribut} ajoute à une dataframe une colonne de nom
#'@param nameAttribut avec pour valeur
#'@param valueAttribut dans la data
#'@param dataName et @return la dataframe
addTypeAttribut <-
  function(dataName, valueAttribut, nameAttribut = "type") {
    return (bind_cols(dataName, data.frame (nameAttribut = rep(
      valueAttribut, length(dataName$id)
    ))))
  }


#addTypeAttribut(continents, "type", "continent")
continentType = data.frame ("type" = rep("continent", length(continents$id)))
islandType = data.frame("type" = rep("island", length(islands$id)))
lakesType = data.frame("type" = rep("lakes", length(lakes$id)))
riverType = data.frame("type" = rep("rivers", length(rivers$id)))
roadsType = data.frame("type" = rep("roads", length(roads$id)))
wallType = data.frame("type" = rep("wall", length(wall$id)))
#policalType = data.frame("type" = rep("political", length(political$id)))

allDatas = bind_rows(
  bind_cols(continents, continentType),
  bind_cols(islands, islandType),
  landscape,
  bind_cols(rivers, riverType),
  bind_cols(lakes, lakesType),
  bind_cols(roads, roadsType),
  bind_cols(wall, wallType),
)

spaces = c(
  "continent",
  "forest",
  "mountain",
  "stepp",
  "swamp",
  "lakes",
  "rivers",
  "roads",
  "island",
  "location",
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
  "gray88",
  "#669933",
  "cyan4",
  "blue",
  "cyan3",
  "gray1",
  "gold",
  "black",
  "gold4",
  "orangered3",
  "darkgoldenrod1",
  "gray",
  "yellow",
  "#33CCFF"
)


names(cols) = spaces
levels(allDatas$type) = spaces

#'\code{displayMap} Consruire la map de got en fonction
#'@param layerGeometry la couche donc la géometrie sera ajoutée et
#'@param layerCol pour fixer la couleur de la couche
#'@param layerFill pour fixer la couleur ndu remplissage
#'@return une novelle map
displayMap = function(layerGeometry=locations, layerCol="red", layerFill="red") {
  map = ggplot(allDatas) + geom_sf(aes(fill = type), size = 0.1) +
    geom_sf(data = locations,fill = "black",color = "black") +
    
    scale_fill_manual("Lands category", values = cols) +
    theme_minimal() +
    geom_sf_interactive(data = locations, aes(tooltip = name), size = 2) +
    geom_sf_text(
      data = allDatas %>% filter(type == "continent"),
      aes(label = name),
      color = "red",
      fontface = "bold"
    )
  if(layerGeometry!= locations){map = map + geom_sf(data = layerGeometry, 
                                                fill= layerFill,
                                                color = layerCol) }
  return (ggiraph(code = print(map)))
}
displayMap()
