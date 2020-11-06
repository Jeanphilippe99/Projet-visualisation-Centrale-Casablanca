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
            selectInput("saison","Saison",choices=c(1,2,3,4,5,6,7,8)),
            #Quand la saison change, le nombre d'épisodes change aussi. Donc c'est "observe" dans la partie server qui s'en charge
            selectInput("episode","Episode",choices=episodes$episodeNum[episodes$seasonNum==1]),
            radioButtons("mortsOuScenes","Voir les lieux des morts ou des scènes",choices=c("Morts","Scènes")),
            actionButton("btnSaisonEpisode", "Afficher"),
            
            tags$br(),
            tags$i("Vous pouvez choisir un personnage pour voir ses lieux de scènes où l'endroit de sa mort si jamais."),
            selectInput("caractere","Nom du personnage",choices=caracteres$name),
            actionButton("btnSaisonEpisode", "Afficher"),
            
            #A propos
            tags$br(),
            tags$br(),
            actionLink("aPropos", "A Propos")
        ),

        mainPanel(
            plotOutput("GoTmap")
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
    
    displayMap <- reactive({
        return("la hessssssss")
    })
    
    #affichage de la carte
    output$GoTmap <- renderPlot({
        continents = st_read("data/GoTRelease/Continents.shp")
        islands = st_read("data/GoTRelease/Islands.shp")
        lakes = st_read("data/GoTRelease/Lakes.shp")
        rivers = st_read("data/GoTRelease/Rivers.shp")
        landscape = st_read("data/GoTRelease/Landscape.shp")
        regions = st_read("data/GoTRelease/Regions.shp")
        roads = st_read("data/GoTRelease/Roads.shp")
        
        plot(st_geometry(continents), border=1+continents$id, cex=5)
        plot(st_geometry(islands), add=T, col="#CD887A")
        plot(st_geometry(lakes), add=T, col="#2CD7F9")
        plot(st_geometry(rivers), add=T, col="#2CD7F9", lwd=2)
        plot(st_geometry(landscape), add=T, col="#C4B4A1", lwd=1, border=0)
        #plot(st_geometry(regions), add=T)
        plot(st_geometry(roads), add=T, col="#70512C", lwd=2)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
