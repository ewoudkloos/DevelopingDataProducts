library(shiny)


shinyUI(fluidPage(
        
  titlePanel("Expected rise if global temperature"),
  
  sidebarLayout(
    sidebarPanel(
        titlePanel("Period to predict over"),
        h4("Due to the emmision of greenhouse gasses the global anual temperature is rising. 
          With this app you can see what a expected the impact (rise of global temperature) will be in the future."),
        p("We have collected measurements of average global temperates from two data scrource: GCAG and GISTEMP respectively (from datahub.io). Furthermore we have collected data of co2 concentratetion in the atmosphere in parts per million (ppm). The data is over the period from 1958 to 1998."),
       sliderInput("years",
                   "How many years ahead do we want tot predict:",
                   min = 1,
                   max = 100,
                   value = 10),
       h4(" "),
       h4(" "),
       h3("Forecast of the increase in co2 concentration (ppm) in the atmosphere:"),
       plotOutput("co2forecast")
       
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       
       plotOutput("GCAGplot"),
       plotOutput("GISTEMPplot")
    )
  )
))
