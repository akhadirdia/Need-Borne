#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(shinythemes)
library(DT)
library(dplyr)
library(ggplot2)
library(shinymanager)
library(openxlsx)


inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


# data.frame with credentials info
credentials <- data.frame(
  user = c("abdou", "patrick", "julie", "jamal"),
  password = c("a2023", "p2023", "j2023", "ja2023"),
  # comment = c("alsace", "auvergne", "bretagne"), %>% 
  stringsAsFactors = FALSE
)






df <- read_excel("vehicule_circulation.xlsx", sheet = "elecrique")
Annee <- df$Year
df = as.matrix(df)
row.names(df)<-df[,1]
df <- df[,-1]

df1 <- read_excel("vehicule_circulation.xlsx", sheet = "pourcentage")
df1 = as.matrix(df1)
row.names(df1)<-df1[,1]
df1 <- df1[,-1]
# Define UI for application that draws a histogram
ui <- secure_app(head_auth = tags$script(inactivity),language = "fr",fab_position = "top-right",
  fluidPage(
    
    tags$img(src = "image001.png", width = "206px", height = "55px"),
  theme = shinytheme("cerulean"),
    # Application title
    titlePanel(
      tags$h1("BIENVENUE DANS LA PLATEFORME DE DETERMINATION DU NOMBRE DE BORNES A INSTALLER ", align="center")),
  hr(),

    # Generate a row with a sidebar
    sidebarLayout(      
      
      # Define the sidebar with one input
      sidebarPanel(
        selectInput("region", "Veuillez selectionner la Région où se trouve l'entreprise:", 
                    choices=colnames(df)),
        hr(),
        selectInput("client", "Veuillez selectionner le type de client:", 
                    choices=c('Immeuble', 'Autre client')),
        ##helpText("Data from AT&T (1961) The World's Telephones."),
        hr(),
        sliderInput("bins",
                    "Veuillez choisir le nombre de Stationnement:",
                    min = 1,
                    max = 2000,
                    value = 300),
        
        hr(),
        downloadButton("download_pdf", "Télécharger la methodologie")
      ),
      
      
      
      
      # Create a spot for the barplot
      mainPanel(
        verbatimTextOutput("res_auth"),
        tabPanel("Table", dataTableOutput("countryTable"),
                 value = "table"),
        downloadButton('download',"Telecharger les données"),
        plotOutput("VEplot")  ,
        plotOutput("VEplot2")
      )
      
    ),
  
))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  # output$res_auth <- renderPrint({
  #   reactiveValuesToList(result_auth)
  # })

  # Fill in the spot we created for a plot
  
  observe({
    
    calcul_borne <- function(n1, n2, n3){
      
      df2 <- mutate(as.data.frame(df1), 'Nombre de bornes de 7.2 kW' = (df1[,input$region]*input$bins)/n1,
                    'Nombre de bornes de 9.6 kW' = (df1[,input$region]*input$bins)/n1,
                    'Nombre de bornes de 24 kW' = (df1[,input$region]*input$bins)/n2,
                    'Nombre de bornes de 50 kW' = (df1[,input$region]*input$bins)/n3)
      Nbre_vehicule_electrique <-df[, input$region]
      Pourcentage_vehicule_eletrique <- df1[, input$region]*100
      Nbre_borne7.2 <- (df1[,input$region]*input$bins)/n1
      Nbre_borne9.6 <- (df1[,input$region]*input$bins)/n1
      Nbre_borne24 <- (df1[,input$region]*input$bins)/n2
      Nbre_borne50 <- (df1[,input$region]*input$bins)/n3
      
      datadown <- cbind(Annee, Nbre_vehicule_electrique, Pourcentage_vehicule_eletrique, Nbre_borne7.2, Nbre_borne9.6, Nbre_borne24, Nbre_borne50)
      datadown <- as.data.frame (datadown)
      datadown <- round(datadown)
      datadown[datadown == 0] <- 1
      
      output$countryTable = renderDataTable({
        
        
        databorne = as.data.frame(round(df2)) %>% select("Nombre de bornes de 7.2 kW", 'Nombre de bornes de 9.6 kW', "Nombre de bornes de 24 kW", "Nombre de bornes de 50 kW")
        databorne[databorne == 0] <- 1
        datatable (databorne,  class = "hover cell-border compact", options = list(
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')),
          caption = "Nombre de bornes à installer par année - Trois scenario: Installer des bornes de 7.2 kW, Installer des bornes de 9.6 kW,
      Installer des bornes de 24 kW, Installer des bornes de 50 kW")  %>%
          formatStyle(               # Fonction pour changer le style d'une colonne
            c('Nombre de bornes de 7.2 kW', 'Nombre de bornes de 9.6 kW', 'Nombre de bornes de 24 kW', 'Nombre de bornes de 50 kW'),                 # Nom de la colonne à modifier
            color = 'green',         # Couleur du texte dans la colonne
            backgroundColor = 'white', # Couleur des cases de la colonne
            fontWeight = 'bold'      # Affiche les caractères en gras
          )
        
        
      }
      )
      
      
      output$download <- downloadHandler(
        filename = function(){paste0(input$region, ".xlsx")},
        content = function(file){
          ##write.csv(round(df1[,input$region]*input$bins), file)
          write.xlsx(round(datadown), file)
        }
      )
      
      output$VEplot <- renderPlot({
        
        # Render a barplot
        
        # ggplot(as.data.frame(df1)) + geom_bar(aes(x = round(df1[,input$region]*100)), fill = "green", width = .5,  main= paste0("Pourcentage de vehicule electrique prévu dans la région de  ", input$region),
        #                        ylab="Pourcentage de vehicules electriques",
        #                        xlab="Année")
        
        barplot(round(df1[,input$region]*100), col = "green",
                main= paste0("Pourcentage de vehicule electrique prévu dans la région de  ", input$region),
                ylab="Pourcentage de vehicules electriques",
                xlab="Année")
        
        
      })
      
      # Fill in the spot we created for a plot
      output$VEplot2 <- renderPlot({
        
        # Render a barplot
        
        
        barplot(round(df[,input$region]), col="green", border = "red",
                main=paste0("Nombre de vehicules electriques prévus dans la région de  ", input$region),
                ylab="Nombre de vehicules electriques",
                xlab="Année")
        
      })
      
    }
    
    output$download_pdf <- downloadHandler(
      filename = function() {
        "methodes_calcul.pdf"
      },
      content = function(file) {
        file.copy("www/doc_calcul_de_bornes.pdf", file)
      }
    )
    
    
    if (!is.null(input$client) && input$client =="Immeuble") {
      calcul_borne(1, 2.8, 5.8)
    }else{
      calcul_borne(8, 16, 32)
    }

  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
