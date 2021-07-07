
library(shiny)
library(unmarked)
library(flextable)
library(magrittr)
source("func_simul_Nmix.R")


# *** Define user interface - ui ------------------------------------------

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      helpText("Simulates data collection and analysis with N-mixture models"),
      # True parameter values
      h3("Parameters"),
      numericInput("lambda",strong("Local abundance (λ)"),value=0.3),
      sliderInput("phi",strong("Availability (φ)"),min=0,max=1,value=0.5,step=0.05),
      sliderInput("p",strong("Perception (p)"),min=0,max=1,value=0.7,step=0.05),
      
      # Sampling design settings
      h3("Sampling design"),
      numericInput("S",strong("Number of sites (S)"),value=60),
      numericInput("J",strong("Number of visits (J)"),value=5),
      sliderInput("obs2",strong("Proportion of double obs. protocol"),min=0,max=1,value=0,step=0.05),
      
      width=4
    ), # sidebarPanel
  
    mainPanel(
      h3("Model results:"),
      uiOutput("modResu"),
      textOutput("model"),
      
      width=5
    ) # main
  )
) #page - ui

server <- function(input, output){
  output$modResu <- renderUI({
    resul <- simul_Nmix(S=input$S,J=input$J,obs2=input$obs2,lambda=input$lambda,phi=input$phi,p=input$p)
    resul <- cbind(parameter=rownames(resul),resul)
    flextable(resul) %>% color(j=2, i= ~true<ucl & true>lcl, color="blue") %>%
      color(j=2, i= ~true>ucl | true<lcl, color="red") %>% autofit() %>% 
      htmltools_value()
    })
  
  output$model <- renderText({paste("Model used was a",
                                    ifelse(input$obs2==0,"Binomial","Multinomial"),
                                    "N-mixture model for",
                                    if(input$obs2==0){"single observer"}
                                    else{if(input$obs2==1){"double observer"}
                                         else{"mixed single and double observer"}},
                                    "counts"
                                    )
    })
}

shinyApp(ui=ui, server=server)
