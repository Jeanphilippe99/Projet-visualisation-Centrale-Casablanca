library(readr)
library(dplyr)
library(tidyr)
library(shiny)
characters = read_csv("data/characters.csv")
episodes = read_csv("data/episodes.csv")
scenes = read_csv("data/scenes.csv")
appearances = read_csv("data/appearances.csv")

#data frame qui regroupe la saison,l'épisode, la localisation et le nombre de morts
death_localisation = scenes %>% left_join(episodes) %>% 
  group_by(episodeNum,seasonNum,location) %>% 
  summarize(nbre_Morts=sum(nbdeath))

#la fonction death prend en paramètres la saison, l'épisode et renvoie la localisation suivie du nombre de morts
death=function(x,y){
  d=death_localisation[death_localisation$seasonNum==x,] 
  e=d[d$episodeNum==y,] 
  deathLoc=e %>% 
    group_by(location) %>% 
    summarize(nbre_Morts=sum(nbre_Morts))
  return(deathLoc)
}


  