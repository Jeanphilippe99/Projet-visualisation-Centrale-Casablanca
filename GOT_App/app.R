#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/

library(shiny)
library(sf)
library(readr)
library(ggplot2)
library(dplyr)
library(ggiraph)
library(shinycssloaders)
#lecture des données
#continents = st_read("data/GoTRelease/Continents.shp")
#islands = st_read("data/GoTRelease/Islands.shp")
#lakes = st_read("data/GoTRelease/Lakes.shp")
#rivers = st_read("data/GoTRelease/Rivers.shp")
#landscape = st_read("data/GoTRelease/Landscape.shp")
#regions = st_read("data/GoTRelease/Regions.shp")
#roads = st_read("data/GoTRelease/Roads.shp")
appearances = read_csv("data/appearances.csv")
episodes = read_csv("data/episodes.csv")
scenes = read_csv("data/scenes.csv")
caracteres = read_csv("data/characters.csv")

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

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css") #on utilise un fichier CSS externe (dans le dossier www)
    ),
    
    tags$header(
        tags$img(src="ecc.png", title="Ecole Centrale Casablanca", width="70", height="55", class="logo"),
        tags$b("GoT Data Visualisation Application", class="titre")
    ),

    sidebarLayout(
        sidebarPanel(
            tags$i("Choisir la saison et l'épisode pour voir le nombre de morts par lieu."),
            selectInput("saison","Saison",choices=c(1,2,3,4,5,6,7,8),selected=1),
            #Quand la saison change, le nombre d'épisodes change aussi. Donc c'est "observe" dans la partie server qui s'en charge
            selectInput("episode","Episode",choices=episodes$episodeNum[episodes$seasonNum==1],selected=1),
            radioButtons("mortsOuScenes","Voir les lieux des morts ou des scènes",choices=c("Scènes","Morts")),
            actionButton("btnSaisonEpisode", "Afficher"),
            
            tags$br(),
            tags$i("Vous pouvez choisir un personnage pour voir ses lieux de scènes où l'endroit de sa mort si jamais."),
            #quand la saison et l'épisode change, la liste de personnages change aussi (seul ceux qui ont participé)
            selectInput("caractere","Nom du personnage",choices=caracteres$name),
            actionButton("btnSaisonEpisodeCaract", "Afficher", class="btn"),
            
            #A propos
            tags$br(),
            tags$br(),
            actionLink("aPropos", "A Propos")
        ),

        mainPanel(
            plotOutput("GoTmap") %>% withSpinner(color="#ad1d28", color.background="#CBDFDD", size=2),
            textOutput("alert"), #message d'alerte (ex: s'il n'y a pas de données à afficher on le signale)
            textOutput("aProposText")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    #maj des données du select de episode en fonction de la saison choisie
    observe({
        a = input$saison #on récupère la saison selectionnée afin de trouver les épisodes (toutes les saisons n'ont pas même nbr d'épisodes)
        updateSelectInput(session, "episode", "Choisir l'épisode", choices=episodes$episodeNum[episodes$seasonNum==a])
        #updateSelectInput(episode, "User", choices = as.character(dat5[dat5$email==input$Select, date]))
    })
    
    #saison est une fonction qui retourne les épisodes de la saison avec x comme paramètre correspondant à la saison en questio
    saison<-function(x){
        A<-episodes[episodes$seasonNum==x,]
        liste_episodes<-A$episodeNum
        liste_Id<-A$episodeId
        return (as.data.frame(A))
    }
    
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
    
    #maj des données du select de personnage (car chaque saison et épisode à ses personnages)
    observe({
        a = input$saison #on récupère la saison selectionnée afin de trouver les épisodes (toutes les saisons n'ont pas même nbr d'épisodes)
        b = input$episode
        updateSelectInput(session, "caractere", "Nom du personnage", choices=epi(as.numeric(input$saison), as.numeric(input$episode)))
        #updateSelectInput(episode, "User", choices = as.character(dat5[dat5$email==input$Select, date]))
    })
    
    #fonction qui affiche la MAP de GoT (continents, lacs, îls, routes ...)
    displayMap <- function(){ #la fonction retourne la data sf à ploter, mais ne fait aucun affichage (cela permet après d'ajouter d'autres données vant de plot)
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
        
        #ggiraph(code = print(my_gg))
        return(my_gg)
    }
    
    #lieuVisite : fonction qui prend la saison, l'épisode, le caractère et renvoie la liste des lieux visités par celui-ci
    lieuVisite <- function(theSaison, theEpisode, theCaractere){
        elt = scenes %>% inner_join(episodes) %>% inner_join(appearances)
        elt = elt[elt$seasonNum==theSaison,] #filtrer par la saison
        elt = elt[elt$episodeId==theEpisode,] %>% filter(name==theCaractere) %>% group_by(location) %>% summarise(times=n()) #filtrer par l'épisode et nom du caractère
        #NB : pendant le filtrage, on ne repete pas les locations qui se repetent, mais on compte le nombre de fois qu'il a visité chaque location (times)
        return(elt)
    }
    
    #lieuMort : fonction qui prend la saison, l'épisode et renvoie la liste des lieux où il y'a eu des morts
    lieuMort <- function(theSaison, theEpisode){
        elt = scenes %>% inner_join(episodes)
        elt = elt[elt$nbdeath>0,] #on garde la data où il y'a des morts
        elt = elt[elt$seasonNum==theSaison,] #filtrer par la saison
        elt = elt[elt$episodeNum==theEpisode,] %>% group_by(location) %>% summarise(morts=n()) #morts=nbr de morts par lieu
        #NB : pendant le filtrage, on ne repete pas les locations qui se repetent, mais on compte le nombre de fois qu'il y a eu des morts dans ce lieu au cours de la saison et de l'épisode
        return(elt)
    }
    
    #Bouton qui affiche les lieux des scènes et des morts (1er bouton)
    observeEvent(input$btnSaisonEpisode, {
        output$GoTmap <- renderPlot({
            if (input$mortsOuScenes=="Scènes"){    
                output$alert <- renderText({
                    paste("-----! Affichage des lieux de scènes selon la saison et l'épisode !-----")
                })
            }
            else { #else Morts
                theData = lieuMort(as.numeric(input$saison), as.numeric(input$episode)) #appelle de la fonction lieuMort
                
                A = st_read("data/GoTRelease/ScenesLocations.shp", crs=4326) #lecture des lieux des morts
                elt = A %>% inner_join(theData) #jointure sur location
                
                plot(displayMap() + geom_sf(data=elt, fill="red", color="red", size=5))
                
                output$alert <- renderText({
                    paste("-----! Affichage des lieux des morts selon la saison et l'épisode !-----")
                })
            }
        })
    })
    
    #Bouton qui affiche les lieux des scènes du caractère choisi (2ème bouton)
    observeEvent(input$btnSaisonEpisodeCaract, {
        output$GoTmap <- renderPlot({
            if (input$mortsOuScenes=="Scènes"){
                theData = lieuVisite(as.numeric(input$saison), as.numeric(input$episode), input$caractere) #appelle de la fonction lieuVisite
                if (nrow(theData)<1){ #theData est vide (i.e en fonction de la saison, de l'épisode et du perso choisi, il n'y a pas de lieu à afficher)
                    output$alert <- renderText({
                        paste("-----! Donnée vide, rien à afficher !-----")
                    })
                }
                else {
                    output$alert <- renderText({
                        paste("-----! Affichage des lieux de scènes du personnage selon la saison et l'épisode !-----")
                    })
                }
                A = st_read("data/GoTRelease/ScenesLocations.shp", crs=4326) #lecture des lieux visités par le caractère
                elt = A %>% inner_join(theData) #jointure sur location
            
                plot(displayMap() + geom_sf(data=elt, fill="red", color="red", size=5))
            }
            else { #else Morts
                if (is.na(caracteres[caracteres$name==input$caractere,]$killedBy)){ #personnage non tué
                    output$alert <- renderText({
                        paste("-----! Cette personne n'a pas connu la mort !-----")
                    })
                }
                else { #personnage tué
                    output$alert <- renderText({
                        paste("-----! Personnage tué par", caracteres[caracteres$name==input$caractere,]$killedBy, "!-----", sep=" ")
                    })
                }
                plot(displayMap()) #affichage map de base
            }
        })
    })
    
    #affichage de la carte
    output$GoTmap <- renderPlot({
        plot(displayMap())
    })
    
    #A Propos (affichage)
    observeEvent(input$aPropos, {
        output$aProposText <- renderText({
            read_file("www/a-propos.txt") #texte à propos
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
