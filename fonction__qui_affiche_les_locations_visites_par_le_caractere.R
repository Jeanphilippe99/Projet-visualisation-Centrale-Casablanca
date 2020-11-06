#**************************************************************
#*C'est la seconde fonction******elle utilise la première******
#**************************************************************

library(readr)
appearances = read_csv("data/appearances.csv")
episodes = read_csv("data/episodes.csv")
scenes = read_csv("data/scenes.csv")

#lieuVisite : fonction qui prend la saison, l'épisode, le caractère et renvoie la liste des lieux visités par celui-ci
lieuVisite<-function(theSaison, theEpisode, theCaractere){
  elt = scenes %>% inner_join(episodes) %>% inner_join(appearances)
  elt = elt[elt$seasonNum==theSaison,] #filtrer par la saison
  elt = elt[elt$episodeId==theEpisode,] %>% filter(name==theCaractere) %>% group_by(location) %>% summarise(times=n()) #filtrer par l'épisode et nom du caractère
  #NB : pendant le filtrage, on ne repete pas les locations qui se repetent, mais on compte le nombre de fois qu'il a visité chaque location (times)
  
  return(elt)
}

#exemple
a=lieuVisite(1, 1, 'Will')

#plotLieux : fonction qui prend une liste de lieux et renvoie la representation en couleurs
plotLieux<-function(theData){
  #theData : tableau contenant les lieux avec le nombre de fois que ce lieu fu visité par un caractere bien precis (resultat de lieuVisite())
  library(sf)
  A = st_read("data/GoTRelease/ScenesLocations.shp")
  elt = A %>% inner_join(theData) #jointure sur location
  
  plot(st_geometry(elt), col=factor(elt$location), cex=elt$times, lwd=5)
}

#exemple
plotLieux(a)


#lieuMort : fonction qui prend la saison, l'épisode et renvoie la liste des lieux où il y'a eu des morts
lieuMort <- function(theSaison, theEpisode){
  elt = scenes %>% inner_join(episodes)
  elt = elt[elt$nbdeath>0,] #on garde la data où il y'a des morts
  elt = elt[elt$seasonNum==theSaison,] #filtrer par la saison
  elt = elt[elt$episodeNum==theEpisode,] %>% group_by(location) %>% summarise(morts=n()) #morts=nbr de morts par lieu
  #NB : pendant le filtrage, on ne repete pas les locations qui se repetent, mais on compte le nombre de fois qu'il y a eu des morts dans ce lieu au cours de la saison et de l'épisode
  return(elt)
}

#exemple
c=lieuMort(1,1)