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
  h2("Escuela Superior de F�sica y Matem�ticas",  align = "center"),
  h4("Created by Jaziel David Flores Rodr�guez & Jorge Peralta Garc�a",  align = "center"),
    # Show a plot of the generated distribution

      tabsetPanel(
        tabPanel("Gr�ficas",
                 fluidRow(
                    box(solidHeader = TRUE,
                        collapsible = TRUE, plotOutput("plot1")
                    ),
                    box(plotOutput("plot2")),
                    box(solidHeader = TRUE,
                        collapsible = TRUE, plotOutput("plot3")
                    ),
                    box(plotOutput("plot4"))
                 )
                 
        ), 
        
        tabPanel("Montecarlo", 
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput("bins",
                                 "Number of bins:",
                                 min = 1,
                                 max = 50,
                                 value = 30)
                   ),
                   plotOutput("montecarlo")
                  )
        ),
        
        tabPanel("Tabla",
                  fluidRow(
                    column(12,
                      dataTableOutput('table')
                    )
                  )
        )
        
      )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    ggplot(titanic, aes(x = Sex, fill = Survived)) + 
      scale_fill_discrete(name = "Sexo", labels = c("Mujer", "Hombre"))+
      theme_bw() +
      geom_bar() +
      labs(x = 'Sexo',y = "N�mero de pasajeros",
           title = "Sobrevivencia por sexo")
  })
  
  output$plot2 <- renderPlot({
    ggplot(as.data.frame(prop.table(table(titanic$Sex,titanic$Survived),2)), 
           aes(x=Var2,y=Freq,fill=Var1)) +
      scale_fill_discrete(name = "Sexo", labels = c("Mujer", "Hombre"))+
      theme_bw() +
      geom_col() +
      labs(x = '0=Muertos,1=Vivos',y = "Porcentaje de pasajeros",
           title = "Porcentaje de sobrevivencia por sexo")
  })
  
  output$plot3 <- renderPlot({
    ggplot(titanic, aes(x = Pclass, fill = Survived)) + 
      theme_bw() +
      geom_bar() +
      labs(x = 'Sexo',y = "N�mero de pasajeros",
           title = "Sobrevivencia por clase de pasajero")
  })
  
  output$plot4 <- renderPlot({
    ggplot(as.data.frame(prop.table(table(titanic$Pclass,titanic$Survived),2)), 
           aes(x=Var2,y=Freq,fill=Var1)) +
      scale_fill_discrete(name = "Clase", labels = c("Alta", "Media", "Baja"))+
      theme_bw() +
      geom_col() +
      labs(x = '0=Muertos,1=Vivos',y = "Porcentaje de pasajeros",
           title = "Porcentaje de sobrevivencia por clase de pasajero")
  })
  
  
  output$table <- renderDataTable(titanic)
  
  output$montecarlo <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })

}



# Run the application 
shinyApp(ui = ui, server = server)
