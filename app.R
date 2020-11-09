#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/

source("getCharacters.R")
source("displayMap.R")
library(shiny)
library(shinyalert)
library(shinycssloaders)

# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyalert(),
  #alert pour afficher à propos (en cliquant sur le lien (actionLink) ci-dessous)
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css") #on utilise un fichier CSS externe (dans le dossier www)
  ),
  
  tags$header(
    tags$img(
      src = "ecc.png",
      title = "Ecole Centrale Casablanca",
      width = "70",
      height = "55",
      class = "logo"
    ),
    tags$b("GoT Data Visualisation Application", class = "titre")
  ),
  
  sidebarLayout(
    sidebarPanel(
      tags$i(
        "Choisir la saison et l'épisode pour voir le nombre de morts par lieu."
      ),
      selectInput(
        "saison",
        "Saison",
        choices = c(1, 2, 3, 4, 5, 6, 7, 8),
        selected = 1
      ),
      #Quand la saison change, le nombre d'épisodes change aussi. Donc c'est "observe" dans la partie server qui s'en charge
      selectInput(
        "episode",
        "Episode",
        choices = episodes$episodeNum[episodes$seasonNum == 1],
        selected = 1
      ),
      radioButtons(
        "mortsOuScenes",
        "Voir les lieux des morts ou des scènes",
        choices = c("Scènes", "Morts")
      ),
      tags$i("NB : ce bouton prend en compte la saison et l'épisode"),
      tags$br(),
      actionButton("btnSaisonEpisode", "Afficher"),
      
      tags$br(),
      tags$i(
        "Vous pouvez choisir un personnage pour voir ses lieux de scènes où l'endroit de sa mort si jamais."
      ),
      #quand la saison et l'épisode change, la liste de personnages change aussi (seul ceux qui ont participé)
      selectInput("caractere", "Nom du personnage", choices = caracteres$name),
      tags$i("NB : ce bouton prend en compte la saison, l'épisode et le personnage"),
      actionButton("btnSaisonEpisodeCaract", "Afficher pour ce personnage", class =
                     "btn"),
      
      #A propos
      tags$br(),
      tags$br(),
      actionLink("aPropos", "A Propos") #bouton de type link qui affiche le poppup
    ),
    
    mainPanel(
      ggiraphOutput("GoTmap") %>% withSpinner(
        color = "#ad1d28",
        color.background = "#CBDFDD",
        size = 2
      ),
      textOutput("alert"),
      #message d'alerte (ex: s'il n'y a pas de données à afficher on le signale)
      textOutput("alert2"),
      tableOutput("table"),
      textOutput("aProposText")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #maj des données du select de episode en fonction de la saison choisie
  observe({
    a = input$saison #on récupère la saison selectionnée afin de trouver les épisodes (toutes les saisons n'ont pas même nbr d'épisodes)
    updateSelectInput(session,
                      "episode",
                      "Choisir l'épisode",
                      choices = episodes$episodeNum[episodes$seasonNum == a])
    #updateSelectInput(episode, "User", choices = as.character(dat5[dat5$email==input$Select, date]))
  })
  
  
  #maj des données du select de personnage (car chaque saison et épisode à ses personnages)
  observe({
    a = input$saison #on récupère la saison selectionnée afin de trouver les épisodes (toutes les saisons n'ont pas même nbr d'épisodes)
    b = input$episode
    updateSelectInput(session,
                      "caractere",
                      "Nom du personnage",
                      choices = getCharacters(as.numeric(input$saison), as.numeric(input$episode)))
    #updateSelectInput(episode, "User", choices = as.character(dat5[dat5$email==input$Select, date]))
  })
  
  
  #lieuVisite : fonction qui prend la saison, l'épisode, le caractère et renvoie la liste des lieux visités par celui-ci
  lieuVisite <- function(theSaison, theEpisode, theCaractere) {
    elt = scenes %>% inner_join(episodes) %>% inner_join(appearances)
    elt = elt[elt$seasonNum == theSaison, ] #filtrer par la saison
    elt = elt[elt$episodeId == theEpisode, ] %>% filter(name == theCaractere) %>% group_by(location) %>% summarise(nbr_scenes =
                                                                                                                     n()) #filtrer par l'épisode et nom du caractère
    #NB : pendant le filtrage, on ne repete pas les locations qui se repetent, mais on compte le nombre de fois qu'il a visité chaque location (nbr_scenes)
    return(elt)
  }
  
  #lieuMort : fonction qui prend la saison, l'épisode et renvoie la liste des lieux où il y'a eu des morts
  lieuMort <- function(theSaison, theEpisode) {
    elt = scenes %>% inner_join(episodes)
    elt = elt[elt$nbdeath > 0, ] #on garde la data où il y'a des morts
    elt = elt[elt$seasonNum == theSaison, ] #filtrer par la saison
    elt = elt[elt$episodeNum == theEpisode, ] %>% group_by(location) %>% summarise(morts =
                                                                                     n()) #morts=nbr de morts par lieu
    #NB : pendant le filtrage, on ne repete pas les locations qui se repetent, mais on compte le nombre de fois qu'il y a eu des morts dans ce lieu au cours de la saison et de l'épisode
    return(elt)
  }
  
  #fonction qui prend la saison et l'épisode et renvoie les lieux où les scènes ont été tournées
  lieuScene <- function(theSaison, theEpisode) {
    elt = scenes %>% inner_join(episodes)
    elt = elt[elt$seasonNum == theSaison, ]
    elt = elt[elt$episodeNum == theEpisode, ] %>% group_by(location) %>% summarise(nbr_scenes =
                                                                                     n()) #times est le nbr de fois que location apparait
    #NB : pendant le filtrage, on ne repete pas les locations qui se repetent, mais on compte le nombre de fois qu'il y a eu des scènes dans ce lieu (nbr_scenes)
    return(elt)
  }
  
  #Bouton qui affiche les lieux des scènes et des morts (1er bouton)
  observeEvent(input$btnSaisonEpisode, {
    if (input$mortsOuScenes == "Scènes") {
      output$alert <-
        renderText({
          paste(
            "-----! Affichage des lieux de toutes les scènes selon la saison et l'épisode !-----"
          )
        })
      output$alert2 <-
        renderText({
          paste("")
        }) #on efface le contenu de "alert2" car rien à afficher à ce endroit
      
      #Maintenant, ajout d'un geom_sf sur le graphe envoyé par la fonction displayMap puis affichage
      theData = lieuScene(as.numeric(input$saison), as.numeric(input$episode)) #appelle de la fonction lieuMort
      A = st_read("data/GoTRelease/ScenesLocations.shp", crs = 4326) #lecture des lieux des morts
      elt = A %>% inner_join(theData) #jointure sur location
      
      B = displayMap() + geom_sf(
        data = elt,
        fill = "red",
        color = "red",
        size = 5
      )
      output$GoTmap <- renderggiraph(ggiraph(code = print(B)))
      
      output$alert <-
        renderText({
          paste(
            "-----! Affichage des lieux de toutes les morts selon la saison et l'épisode !-----"
          )
        })
      output$alert2 <- renderText({
        paste("")
      })
      output$table <-
        renderTable(theData) #affichage en tableau
    }
    else {
      #Morts
      #Maintenant, ajout d'un geom_sf sur le graphe envoyé par la fonction displayMap puis affichage
      theData = lieuMort(as.numeric(input$saison), as.numeric(input$episode)) #appelle de la fonction lieuMort
      A = st_read("data/GoTRelease/ScenesLocations.shp", crs = 4326) #lecture des lieux des morts
      elt = A %>% inner_join(theData) #jointure sur location
      
      B = displayMap() + geom_sf(
        data = elt,
        fill = "red",
        color = "red",
        size = as.numeric(theData$morts) + 2
      )
      output$GoTmap <- renderggiraph(ggiraph(code = print(B)))
      
      output$alert <-
        renderText({
          paste(
            "-----! Affichage des lieux de toutes les morts selon la saison et l'épisode !-----"
          )
        })
      output$alert2 <-
        renderText({
          ""
        }) #affichage des noms des lieux
      output$table <-
        renderTable(theData) #affichage en tableau
    }
  })
  
  #Bouton qui affiche les lieux des scènes du caractère choisi (2ème bouton)
  observeEvent(input$btnSaisonEpisodeCaract, {
    output$GoTmap <- renderggiraph(if (input$mortsOuScenes == "Scènes") {
      theData = lieuVisite(as.numeric(input$saison),
                           as.numeric(input$episode),
                           input$caractere) #appelle de la fonction lieuVisite
      if (nrow(theData) < 1) {
        #theData est vide (i.e en fonction de la saison, de l'épisode et du perso choisi, il n'y a pas de lieu à afficher)
        output$alert <- renderText({
          paste("-----! Donnée vide, rien à afficher !-----")
        })
        output$alert2 <-
          renderText({
            paste("Il se peut que la base de données ne contient pas d'information à ce sujet")
          })
      }
      else {
        output$alert <- renderText({
          paste(
            "Lieux des scènes de",
            input$caractere,
            "dans la saison",
            input$saison,
            "et épisode",
            input$episode,
            sep = " "
          )
        })
        output$alert2 <- renderText({
          paste("")
        })
        output$table <-
          renderTable(theData) #affichage en tableau
      }
      A = st_read("data/GoTRelease/ScenesLocations.shp", crs =
                    4326) #lecture des lieux visités par le caractère
      elt = A %>% inner_join(theData) #jointure sur location
      
      B = displayMap() + geom_sf(
        data = elt,
        fill = "red",
        color = "red",
        size = 5
      )
      ggiraph(code = print(B))
    }
    else {
      #else Morts
      if (is.na(caracteres[caracteres$name == input$caractere, ]$killedBy)) {
        #personnage non tué
        output$alert <- renderText({
          paste("-----! Cette personne n'a pas connu la mort !-----")
        })
        output$alert2 <- renderText({
          paste("")
        })
        
      }
      else {
        #personnage tué
        output$alert <- renderText({
          paste("-----! Personnage tué par",
                caracteres[caracteres$name == input$caractere, ]$killedBy,
                "!-----",
                sep = " ")
        })
        output$alert2 <-
          renderText({
            paste("NB : impossible d'afficher le lieu de sa mort car information inconnue")
          })
      }
      ggiraph(code = print(displayMap())) #affichage map de base
    })
  })
  
  #affichage de la carte
  output$GoTmap <- renderggiraph(ggiraph(code = print(displayMap())))
  
  #A Propos (affichage mode modal (popup))
  observeEvent(input$aPropos, {
    shinyalert(
      title = "About",
      #text = read_file("www/a-propos_.html"), #texte à propos
      text = readLines("www/a-propos.html"),
      size = "s",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)