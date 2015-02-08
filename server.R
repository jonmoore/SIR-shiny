library("shiny")
library("ggplot2")

# Simulation of SIR model
# http://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology#The_SIR_model
shinyServer(function(input, output) {
  
  mydata <- reactive({
    # Model Parameters:
    Ip <- input$Ip  # Proportion Infected
    Sp <- 1-Ip      # Proportion Susceptible
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
    Rv <- R <- 0    # Immune
    
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

})
