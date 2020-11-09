source("importFilesAndDatas.R")

#' getSeason est une fonction qui retourne les épisodes de la saison avec numSeas
#' comme paramètre correspondant � la saison en questio

getSeason <- function(numSeas) {
  A <- episodes[episodes$seasonNum == numSeas, ]
  #A$idAndTitle = past(A$episodeNum, A$episodeTitle, sep=" ")
  #liste_episodes <- A$episodeNum
  #liste_Id <- A$episodeId
  #return (as.data.frame(A))
  return (A)
}
#getSeason(3)$episodeTitle

#'\code{getCharacters} '@return list of characters in episode according
#'@param numseas season number and
#'@param numEpisode episode number

getCharacters <- function(x, y) {
  D <- getSeason(x)
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

#getCharacters(3, 4)
