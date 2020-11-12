#' Coded by @Jeanphilippe99

source("importFilesAndDatas.R")
library(roxygen2)

#' creation d'une dataframe contenant toutes les datas utiles.
#' Pour pouvoir reconnaitre les differentes datas, ajout de l'attribut type

continents$type="continent"
islands$type="island"
lakes$type="lake"
rivers$type="river"
roads$type="road"
wall$type="wall"

allDatas = bind_rows( continents,islands, landscape, rivers,lakes,roads,wall)

spaces = c(
  "continent",
  "forest",
  "mountain",
  "stepp",
  "swamp",
  "lake",
  "river",
  "road",
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
  "darkgray",
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
#'@param layerGeometry la couche donc la g?ometrie sera ajout?e et
#'@param layerCol pour fixer la couleur de la couche
#'@param layerFill pour fixer la couleur du remplissage
#'@return une novelle map
displayMap = function(layerGeometry=NA, layerCol="red", layerFill="red") {
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
  if(!is.na(layerGeometry)){map = map + geom_sf(data = layerGeometry, 
                                                fill= layerFill,
                                                color = layerCol) }
  return (map)
}

