#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggthemes)
library(ggforce)
library(DT)
library(shinydashboard)


# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  # App title ----
  dashboardHeader(title = "Vowel formant plotter", titleWidth = 325),
  
  # Sidebar layout with input and output definitions ----
  dashboardSidebar(disable=T),
  
  # Main panel for displaying outputs ----
  dashboardBody(
    
    fluidRow(
      
      # Output: Plot
      box(width=6, plotOutput("vowelPlot", height=400)),
 
      
      # Input: Selector for variable 1
      box(fileInput('target_upload', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv'
                )),
      radioButtons("separator","Separator: ",choices = c(",",";",":"), selected=",",inline=TRUE),
      textOutput("instructions")),
      
      fluidRow(

        box(width=8, dataTableOutput("sample_table"))
        

      )
    )
  )
    
  )


# Define server logic required to draw a histogram
server <- function(input, output) {

  df_vowels <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE,sep = input$separator)
    return(df)
  })
  
  output$sample_table <- DT::renderDataTable({
    df <- df_vowels()
    DT::datatable(df)
  })
  
  # Generate a plot of the requested variables ----
  output$vowelPlot <- renderPlot({
    df <- df_vowels()
    f1 <- df$f1
    f2 <- df$f2
    vowel <- df$vowel
    ggplot(df) + geom_point(aes(x = f2, y = f1), alpha= 0.3, size = 2) +
      geom_text(aes(x = f2, y = f1, label = vowel, color = vowel), size = 12) + 
      scale_x_reverse(limits = c(max(f2)+200, min(f2)-50), position = "top") + scale_y_reverse(limits = c(max(f1)+200, min(f1)-50), position = "right") +
      xlab("F2 (Hz)") + ylab("F1 (Hz)") +
      scale_color_viridis_d(end = 0.9) + 
      theme_bw(24) + 
      guides(color = "none", label = "none") +
      theme(legend.position=c(0.9,0.1))
  
  })
  
  output$instructions <- renderText({
    "Please upload a three-column csv file with the following column names: vowel, f1, f2. The vowel column should contain a simple label for the vowel (e.g., i), and the f1 and f2 columns should contain numeric values only. The plot will assume the values are in hertz, but technically any numeric value will be accepted (it'll just be mislabeled on the axis)."
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
