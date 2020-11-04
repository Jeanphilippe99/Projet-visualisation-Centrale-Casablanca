library(readr)
characters = read_csv("data/characters.csv")
episodes = read_csv("data/episodes.csv")
scenes = read_csv("data/scenes.csv")
appearances = read_csv("data/appearances.csv")
#saison est une fonction qui retourne les épisodes de la saison avec x comme paramètre correspondant à la saison en questio

saison<-function(x){
  A<-episodes[episodes$seasonNum==x,]
  liste_episodes<-A$episodeNum
  liste_Id<-A$episodeId
  return (as.data.frame(A))
}
saison(3)$episodeTitle

#retourne les épisodes 
#épisode est une fonction qui retourne la liste des acteurs avec 
epi<-function(x,y){
  D<-saison(x)
  D<-D[D$episodeNum==y,]
  D<-D$episodeId
  liste_scenes<-lapply(D,function(z){B<-scenes[scenes$episodeId==z,]
                                                B<-B$sceneId}
                                                )
  liste_scenes<-unlist(liste_scenes)
  liste_acteurs<-lapply(liste_scenes,function(e){T<-appearances[appearances$sceneId==e,]
                                                                         T<-T$name   })
  
  liste_acteurs<-unique(unlist(liste_acteurs))
  return(liste_acteurs)
}




a<-epi(3,4)

