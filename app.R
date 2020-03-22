library(shiny)
library("ggplot2")

# Simulation of SIR model
# http://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology#The_SIR_model

ui <-   navbarPage(
  "SIR model simulation",
  tabPanel(
    "Plots",
    sidebarLayout(
      sidebarPanel(
        h3("Parameters"),
        sliderInput("N", "Total Population:", min=0, max=1000, value=100, step=100),
        sliderInput("Ip", "Initial proportion infected:", min=0, max=1, value=0.1, step=0.01),
        sliderInput("Rp", "Initial proportion recovered:", min=0, max=1, value=0.1, step=0.01),
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


server <- function(input, output) {
  
  mydata <- reactive({
    # Model Parameters:
    Sp <- 1-Ip-Rp   # Proportion Susceptible
    Ip <- input$Ip  # Proportion Infected
    Rp <- input$Rp  # Proportion Recovered
    N  <- input$N   # Total Population
    np <- input$np  # Time periods
    
    # Infection Parameters:
    B <- input$B # Transmission rate
    C <- input$C # Contact rate
    v <- 1/input$V # Recovery rate days
    
    # Model - Dynamic Change
    StoI <- function() B*C*S*I/N
    ItoR <- function() v*I
    DS <- function() -StoI()
    DI <- function() StoI() - ItoR()
    DR <- function() ItoR()
    
    # Initial populations:
    Sv <- S <- Sp*N # Susceptible population
    Iv <- I <- Ip*N # Infected
    Rv <- R <- Rp*N # Recovered
    
    # Loop through periods
    for (p in 1:np) {
      # Calculate the change values
      # Must calculate before applying changes to S, I and R
      ds = DS()
      di = DI()
      dr = DR()
      
      # Update the total populations
      S = S + ds
      I = I + di
      R = R + dr
      
      # Save the changes in vector form
      Sv = c(Sv, S)
      Iv = c(Iv, I)
      Rv = c(Rv, R)  
    }
    
    # Turn the results into a table
    long <- data.frame(
      Period=rep((1:length(Sv)),3), 
      Population = c(Sv, Iv, Rv), 
      Indicator=rep(c("Susceptible",
                      "Infected", 
                      "Recovered"), 
                    each=length(Sv)))
    wide <- cbind(Sv, Iv, Rv)
    
    list(long=long, wide=wide)
    
  })
  
  output$graph1 <- renderPlot({
    p <- ggplot(mydata()[["long"]], 
                aes(x=Period, 
                    y=Population, 
                    group=Indicator))    
    
    p <- p  + 
      geom_line(aes(colour = Indicator), size=1.5) +  
      ggtitle("Population Totals")
    print(p)
  })
  
  output$datatable <- renderTable(mydata()[["wide"]])
  
  output$downloadData <- downloadHandler(
    filename = function() { 'SIR.csv'},
    content = function(file) {
      write.csv(mydata()[["wide"]], file)
    }
  )
  
}

shinyApp(ui = ui, server = server)
