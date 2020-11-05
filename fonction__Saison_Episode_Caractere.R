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
a=lieuVisite(1, 1, 'Gared')