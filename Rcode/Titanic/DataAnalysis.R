library(tidyverse)
library(ggplot2)
library(shiny)

titanic <- read.csv("C:/Users/Jorge/Documents/Programacion/Proyectos/Titanic_Probabilidad/Datasets/Titanic/train.csv", 
                    stringsAsFactors = FALSE)

titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Survived <- as.factor(titanic$Survived)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

ui <- fluidPage(
  
  titlePanel("Simulaciones de Montecarlo"),
  hr(),
  h2("Escuela Superior de Física y Matemáticas",  align = "center"),
  h4("Created by Jaziel David Flores Rodríguez & Jorge Peralta García",  align = "center"),
    
    # Show a plot of the generated distribution
    mainPanel(
      tags$link(rel="stylesheet",href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"),
      tags$div(class="navbar",
               tags$a(href="#titanic","Titanic"),
               tags$a(href="#densidad","Densidades de probabilidad"),
               tags$a(href="#montecarlo","Montecarlo"),
      tags$style(HTML("
      /* The navigation menu */
      .navbar {
        overflow: hidden;
        background-color: #333;
      }
      
      /* Navigation links */
      .navbar a {
        float: left;
        font-size: 16px;
        color: white;
        text-align: center;
        padding: 14px 16px;
        text-decoration: none;
      }
      
      /* Add a red background color to navigation links on hover */
      .navbar a:hover {
        background-color: red;
      }
      "))
      ),
      plotOutput("distPlot")
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    ggplot(titanic, aes(x = Age, fill = Survived)) +
      theme_bw() +
      facet_wrap(Sex ~ Pclass) +
      geom_histogram(binwidth = 5) +
      labs(y = "Edad",
           x = "Sobrevivientes",
           title = "Sobrevivientes del Titanic por Edad, PClase y Sexo")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
