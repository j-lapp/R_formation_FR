library(shiny)
library(highcharter)
library(dplyr)

# global - manipulation des donnees
# lisez les donnees HSM
data <- read.csv("donnees/REG1903b_3_Frontiers_HSM_Burkina_Faso_2021-03-16.csv",
                 stringsAsFactors = F,
                 na.strings = c("NA", "N/A",""))

# objet UI
ui <- fluidPage(
  # panneau titre
  titlePanel(
    h1("Application Shiny", style = "color: #58585A")),
  
  # ici on define le contenu au dessous de le titre
  sidebarLayout(
    sidebarPanel(
      h2("REACH Burkina Faso", style = "color: #EE5859"), # couleur comme deuxieme argument
      h3("panneau sidebar"),
      p("ici c'est notre premiere sidebar!"),
      selectInput("admin1", 
                  "Selectionnez une region:",
                  choices = sort(unique(data$admin1)),
                  selected = ""),
      radioButtons("couleur", 
                   "Selectionnez une couleur:",
                   choices = c("rouge" = "#EE5859",  "gris" = "#58585A"),
                   selected = "rouge"),
      sliderInput("bins", 
                  "Selectionnez une nombre de amplitude:",
                  min = 1,
                  max = 20,
                  value = 10)
    ),
    
    # panneau 
    mainPanel(
      plotOutput(outputId = "graphique")
      
    )
  )
)

# fonction de server 
server <- function(input, output) {
  
  output$graphique <- renderPlot({
    
    data_filtree <- data %>% 
      filter(admin1 == input$admin1)
    
    ic_age <- data_filtree$ic_age
    
    bins <- seq(min(ic_age), max(ic_age), length.out = input$bins + 1)
    
    
    hist(ic_age, 
         breaks = bins, 
         col = input$couleur, 
         border = "white",
         xlab = "IC Age",
         main = paste0("Distribution age de IC en la region ", input$admin1))
    
  })
  
}

# lancer l'application
shinyApp(ui, server)