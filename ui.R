#load all libraries
library(methods)
library(base)
library(stats)
library(datasets)

library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(broom)
library(gganimate)
library(transformr)
library(gifski)
library(shinybusy)

fluidPage(
  h1("Static Line Chart"),
  hr(),
  h3("Nilai Impor dalam juta USD"),
  
  sidebarLayout(position = "left",
    sidebarPanel(
       dateInput("nilaiFrom", "From:", value = "2014-01-01", min = "2014-01-01", max = "2021-08-01",format = "mm/dd/yyyy"),
       dateInput("nilaiTo", "To:", value = "2021-08-01", min = "2014-01-01", max = "2021-08-01", format = "mm/dd/yyyy"),
    ),
    mainPanel(
      fluidRow(
          div(plotOutput("nilai_plot"), style="text-align: center;"),
      )
    )
  ),
  
  br(),
  br(),
  
  h3("Berat Impor dalam ribu Ton"),
  
  sidebarLayout(position = "left",
    sidebarPanel(
       dateInput("beratFrom", "From:", value = "2014-01-01", min = "2014-01-01", max = "2021-08-01",format = "mm/dd/yyyy"),
       dateInput("beratTo", "To:", value = "2021-08-01", min = "2014-01-01", max = "2021-08-01", format = "mm/dd/yyyy"),
    ),
    mainPanel(
      fluidRow(
          div(plotOutput("berat_plot"), style="text-align: center;"),
      )
    )
  ),
  
  hr(),
  
  h1("Line Chart Race"),
  dateInput("dateFrom", "From:", value = "2014-01-01", min = "2014-01-01", max = "2021-08-01", format = "mm/dd/yyyy"),
  dateInput("dateTo", "To:", value = "2021-08-01", min = "2014-01-01", max = "2021-08-01", format = "mm/dd/yyyy"),
  br(),
  actionButton("nilai_line", "Nilai Impor"),
  actionButton("berat_line", "Berat Impor"),
  
  hr(),
  div(div(imageOutput("line_race"), style="text-align: center; margin: auto;"), style="text-align: center; margin: auto; height: 720px; width: 1080px;"),
  
  hr(),
  h1("Bar Chart Race"),
  h3("Negara Asal Impor"),
  dateInput("bar_dateFrom", "From:", value = "2017-01-01", min = "2017-01-01", max = "2021-08-01", format = "mm/dd/yyyy"),
  dateInput("bar_dateTo", "To:", value = "2021-08-01", min = "2017-01-01", max = "2021-08-01", format = "mm/dd/yyyy"),
  br(),
  actionButton("race", "Submit"),
  
  hr(),
  div(imageOutput("bar_race"), style="text-align: center;"),
  
  add_busy_spinner(spin = "fading-circle")
)