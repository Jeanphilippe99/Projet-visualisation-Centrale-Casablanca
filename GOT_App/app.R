#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/

library(shiny)
library(sf)
library(readr)
#lecture des données
continents = st_read("data/GoTRelease/Continents.shp")
islands = st_read("data/GoTRelease/Islands.shp")
lakes = st_read("data/GoTRelease/Lakes.shp")
rivers = st_read("data/GoTRelease/Rivers.shp")
landscape = st_read("data/GoTRelease/Landscape.shp")
regions = st_read("data/GoTRelease/Regions.shp")
roads = st_read("data/GoTRelease/Roads.shp")
appearances = read_csv("data/appearances.csv")
episodes = read_csv("data/episodes.csv")
scenes = read_csv("data/scenes.csv")
caracteres = read_csv("data/characters.csv")

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
            tags$i("Choisir la saison et l'épisode pour voir les lieux des morts ou des scènes."),
            selectInput("saison","Saison",choices=c(1,2,3,4,5,6,7,8),selected=1),
            #Quand la saison change, le nombre d'épisodes change aussi. Donc c'est "observe" dans la partie server qui s'en charge
            selectInput("episode","Episode",choices=episodes$episodeNum[episodes$seasonNum==1],selected=1),
            radioButtons("mortsOuScenes","Voir les lieux des morts ou des scènes",choices=c("Morts","Scènes")),
            actionButton("btnSaisonEpisode", "Afficher"),
            
            tags$br(),
            tags$i("Vous pouvez choisir un personnage pour voir ses lieux de scènes où l'endroit de sa mort si jamais."),
            selectInput("caractere","Nom du personnage",choices=caracteres$name),
            actionButton("btnSaisonEpisodeCaract", "Afficher"),
            
            #A propos
            tags$br(),
            tags$br(),
            actionLink("aPropos", "A Propos")
        ),

        mainPanel(
            plotOutput("GoTmap"),
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
    
    #fonction qui affiche la MAP de GoT (continents, lacs, îls, routes ...)
    displayMap <- function(){
        plot(st_geometry(continents), border=1+continents$id, cex=5)
        plot(st_geometry(islands), add=T, col="#CD887A")
        plot(st_geometry(lakes), add=T, col="#2CD7F9")
        plot(st_geometry(rivers), add=T, col="#2CD7F9", lwd=2)
        plot(st_geometry(landscape), add=T, col="#C4B4A1", lwd=1, border=0)
        #plot(st_geometry(regions), add=T)
        plot(st_geometry(roads), add=T, col="#70512C", lwd=2)
    }
    
    #lieuVisite : fonction qui prend la saison, l'épisode, le caractère et renvoie la liste des lieux visités par celui-ci
    lieuVisite <- function(theSaison, theEpisode, theCaractere){
        elt = scenes %>% inner_join(episodes) %>% inner_join(appearances)
        elt = elt[elt$seasonNum==theSaison,] #filtrer par la saison
        elt = elt[elt$episodeId==theEpisode,] %>% filter(name==theCaractere) %>% group_by(location) %>% summarise(times=n()) #filtrer par l'épisode et nom du caractère
        #NB : pendant le filtrage, on ne repete pas les locations qui se repetent, mais on compte le nombre de fois qu'il a visité chaque location (times)
        return(elt)
    }
    
    #Bouton qui affiche les lieux des scènes du caractère choisi
    observeEvent(input$btnSaisonEpisodeCaract, {
        output$GoTmap <- renderPlot({
            theData = lieuVisite(as.numeric(input$saison), as.numeric(input$episode), input$caractere) #appel de la fonction lieuVisite
            if (nrow(theData)<1){ #theData est vide (i.e en fonction de la saison, de l'épisode et du perso choisi, il n'y a pas de lieu à afficher)
                output$alert <- renderText({
                    paste("---------------! Donnée vide, rien à afficher !---------------")
                })
            }
            A = st_read("data/GoTRelease/ScenesLocations.shp") #lecture des lieux visités par le caractère
            elt = A %>% inner_join(theData) #jointure sur location
        
            displayMap()
            plot(st_geometry(elt), add=T, col=factor(elt$location), cex=elt$times, lwd=5)
        })
    })
    
    #affichage de la carte
    output$GoTmap <- renderPlot({
        displayMap()
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
