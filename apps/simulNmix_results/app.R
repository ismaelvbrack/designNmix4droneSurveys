
library(shiny)
library(ggplot2)
library(flextable)
library(magrittr)
library(officer)
library(grid)
library(gridExtra)
library(ggpubr)
library(wesanderson)

source("func_plotRMSE.R")

table1obs <- readRDS("table1obs.rds")
table2obs <- readRDS("table2obs.rds")
rel.rmse <- readRDS("rel.rmse.rds")

scenarios <- as.data.frame(do.call(rbind,strsplit(names(rel.rmse),"_")))
names(scenarios) <- c("lambda","phi")


# define user interface - ui ------------------------------------------

ui <- fluidPage(
  fluidRow(
      column(
      helpText("Simulation results on model performance and optimal survey effort allocation for N-mixture models
               under different scenarios of local abundance and availability probability"),
      
      h3("Choose scenario..."),
      
      selectInput("lambda",strong("Local abundance (λ)"),choices=unique(scenarios$lambda),selected=2),
      selectInput("phi",strong("Availability (φ)"),choices=unique(scenarios$phi),selected=0.1),
      br(),
      sliderInput("ylimit","Y-axis",min=0,max=1.2,value=c(0,0.4),step=0.05),
      br(),
      tags$ul(tags$li("Perception probability by observers was fixed at 0.8"),
           tags$li("Total survey effort (budget) was fixed at 2000 flights"),
           tags$li("Performance is based on the relative root mean square error (rel.RMSE)
        calculated from 2000 iterations")
      ),
      br(),
      width=3,style="background-color:#D9D9D9"
      ), # column 1; row 1
      column(
        h3("RMSE curves"),
        plotOutput("fig",width=600,height=500),
        width=6
      )), # column 2; row 1
  
    fluidRow(
      br(),
      h3("Optimal number of visits (J)",align="center"),
      br(),
      h4("Single observer Binomial N-mixture model",align="center"),
      uiOutput("tab1"),
      br(),
      h4("   Double observers Multinomial N-mixture model",align="center"),
      uiOutput("tab2"),
      
      width=10
    ) # row 2
  ) # page - ui


# server ------------------------------------------------------------------
  
server <- function(input, output, session){
  observeEvent(input$lambda,
      {updateSelectInput(inputId="phi",
                        choices=scenarios[which(scenarios$lambda==input$lambda),"phi"])
  })
  
  output$tab1 <- renderUI({
    table1obs %>% bg(i=which(unique(scenarios$phi)==input$phi),
                     j=which(unique(scenarios$lambda)==input$lambda)+2,
                     bg="lawngreen",part="body") %>% 
      bold(i=which(unique(scenarios$phi)==input$phi),
           j=which(unique(scenarios$lambda)==input$lambda)+2,
           part="body") %>% 
      autofit() %>% htmltools_value()
  })
  
  output$tab2 <- renderUI({
    table2obs %>% bg(j=which(unique(scenarios$lambda)==input$lambda)+2,
                     i=which(unique(scenarios$phi)==input$phi),
                     bg="lawngreen",part="body") %>% 
      bold(i=which(unique(scenarios$phi)==input$phi),
           j=which(unique(scenarios$lambda)==input$lambda)+2,
           part="body") %>% 
      autofit() %>% htmltools_value()
  })
  
  output$fig <- renderPlot({
    scen <- paste0(input$lambda,"_",input$phi)
    
    plotRMSE(data=rel.rmse, scen=scen,type="simple",complete=T,
             yax=c(input$ylimit[1],input$ylimit[2]))
    
  })
}

shinyApp(ui=ui, server=server)
