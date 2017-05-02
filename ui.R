## FS live Project

# setwd("~/Documents/Project2")

library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(data.table)
library(stringr)
library(tidyr)
library(rsconnect)
library(plotly)


fluidPage(
  headerPanel(h1("FullScreen Live Venue Projection")),
  sidebarLayout(
    sidebarPanel( h2("Introduction")),
    mainPanel(
      h3("Background and Goals"),
      p(h4("Given the existing dataset of FullScreen Live, 
           We would like to analyze the sales ticket growth based on the venue")
      )
    )
  ),
  titlePanel("Number of event per venue"),
  dataTableOutput("TotArtistperVenue"),
  titlePanel("Grouping Selection"),
  selectInput( inputId = "groupingType",
               label = "Please select the grouping type to be analyzed",
               choices = c("by venue","by city" ,"by state")),
  uiOutput("groupingUI"),
  textOutput("TotalShows"),
  plotOutput("plot_Cumulative"),
  plotlyOutput("plotly_plot"),
    checkboxInput(inputId = "showFilteredTable", 
                  label = "click to show table of the plot above", 
                  value = FALSE),
  dataTableOutput("filteredTable"),
  titlePanel("Input current performance"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file', "Please upload only daily ticket sale",
                accept = c('text/csv','text/comma-separated-values,text/plain','.csv')),
      numericInput('saleDuration', "Please enter expected sales duration \n (default is 42)", value = 42 , min = 7, step = 1),
      conditionalPanel(
        condition = "input.groupingType != 'by venue'",
        numericInput('venueCap', "Please enter expected venue capacity \n (this option only appears when you group by city or state)", value = 50 , min = 50, step = 1)),
      actionButton(inputId = "go", label = "calculate!"),
      checkboxGroupInput(inputId = "showUserTable",
                         label = "Click to show tables",
                         choices = c("your input"  = "showRawUserInput",
                                     "your input with cumulative ticket sold" = "showUserInput_cumsum"),
                         selected = NULL)
    ),
    mainPanel(
      dataTableOutput("UserInput"),
      dataTableOutput("contents_cumsum"))
  ),
  
  plotlyOutput("comparisonPlot"),
  dataTableOutput("CompiledData"),
  dataTableOutput("test_lm_fitted")
  
)
