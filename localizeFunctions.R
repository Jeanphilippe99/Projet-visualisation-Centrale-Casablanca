#' Coded by @JosueInitDev and @KOUASSI2297

#**************************************************************
#*C'est la seconde fonction******elle utilise la première******
#**************************************************************
source("importFilesAndDatas.R")


#getPathCharacter : fonction qui prend la saison, l'épisode, le caractère et renvoie la liste des lieux visités par celui-ci
getPathCharacter <- function(theSaison, theEpisode, theCaractere) {
  elt = scenes %>% inner_join(episodes) %>% inner_join(appearances)
  elt = elt[elt$seasonNum == theSaison,] #filtrer par la saison
  elt = elt[elt$episodeId == theEpisode,] %>% filter(name == theCaractere) %>% group_by(location) %>% summarise(nbr_scenes = n()) 
  #NB : pendant le filtrage, on ne repete pas les locations qui se repetent, mais on compte le nombre de fois qu'il a visité chaque location (nbr_scenes)
  return(elt)
}

#plotLieux : fonction qui prend une liste de lieux et renvoie la representation en couleurs
plotLieux<-function(theData){
#' theData : tableau contenant les lieux avec le nombre de fois que ce lieu fu 
#' visité par un caractere bien precis (resultat de lieuVisite())

  A = st_read("data/GoTRelease/ScenesLocations.shp")
  elt = A %>% inner_join(theData) #jointure sur location
  
  plot(st_geometry(elt), col=factor(elt$location), cex=elt$times, lwd=5)
}


#' fonction qui prend la saison et l'épisode et renvoie les lieux où 
#' les scènes ont été tournées
getLocations <- function(theSaison, theEpisode) {
  elt = scenes %>% inner_join(episodes)
  elt = elt[elt$seasonNum == theSaison,]
  elt = elt[elt$episodeNum == theEpisode,] %>% group_by(location) %>% summarise(nbr_scenes =
                                                                                  n()) #times est le nbr de fois que location apparait
  #NB : pendant le filtrage, on ne repete pas les locations qui se repetent, mais on compte le nombre de fois qu'il y a eu des scènes dans ce lieu (nbr_scenes)
  return(elt)
}

#' Coded by @PeggyAdjoumani

#' getDeathLocations : fonction qui prend la saison, l'épisode et renvoie 
#' la liste des lieux où il y'a eu des morts
getDeathLocations <- function(theSaison, theEpisode) {
  elt = scenes %>% inner_join(episodes)
  elt = elt[elt$nbdeath > 0,] #on garde la data où il y'a des morts
  elt = elt[elt$seasonNum == theSaison,] #filtrer par la saison
  elt = elt[elt$episodeNum == theEpisode,] %>% group_by(location) %>% summarise(morts = n()) #morts=nbr de morts par lieu
  #NB : pendant le filtrage, on ne repete pas les locations qui se repetent, mais on compte le nombre de fois qu'il y a eu des morts dans ce lieu au cours de la saison et de l'épisode
  return(elt)
}

#' getEpisodes est une fonction qui retourne les épisodes de la saison avec numSeas
#' comme paramètre correspondant ? la saison en questio
getEpisodes <- function(numSeas) {
  A <- episodes[episodes$seasonNum == numSeas, ]
  return (A)
}

#'\code{getCharacters} '@return list of characters in episode according
#'@param numseas season number and
#'@param numEpisode episode number

getCharacters <- function(x, y) {
  D <- getEpisodes(x)
  D <- D[D$episodeNum == y, ]
  D <- D$episodeId
  liste_scenes <-
    lapply(D, function(z) {
      B <- scenes[scenes$episodeId == z, ]
      B <- B$sceneId
    })
  liste_scenes <- unlist(liste_scenes)
  liste_acteurs <-
    lapply(liste_scenes, function(e) {
      T <- appearances[appearances$sceneId == e, ]
      T <- T$name
    })
  
  liste_acteurs <- unique(unlist(liste_acteurs))
  return(liste_acteurs)
}