
library(shiny)

# Define UI for slider demo application
shinyUI(

  navbarPage(
    "SIR model simulation",
    tabPanel(
      "Plots",
      sidebarLayout(
        sidebarPanel(
          h3("Parameters"),
          sliderInput("N", "Total Population:", min=0, max=1000, value=100, step=100),
          sliderInput("Ip", "Initial proportion infected:", min=0, max=1, value=0.1, step=0.01),
          sliderInput("B", "Transmission rate:", min=0, max=1, value=0.5, step=0.1),
          sliderInput("C", "Contact rate:", min=0, max=1, value=0.5, step=0.1),
          sliderInput("V", "Days to recover:", min=2, max=20, value=7, step=1),
          sliderInput("np", "Time periods:", min=10, max=200, value=100, step=10),

          p()
        ),
        mainPanel (
          plotOutput("graph1")

        )
      )
    )
    ,
    tabPanel(
       "Numerical results & download",
       sidebarLayout(
         sidebarPanel(
           h3("tables"),
           downloadButton('downloadData', 'Download')

         ),
         mainPanel(
           tableOutput("datatable")
         )
       )
    )
  )
)
