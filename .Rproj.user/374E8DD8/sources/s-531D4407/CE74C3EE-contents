library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(lubridate)
library(ggplot2)
library(drc)
library(repr)
library(gridExtra)
library(leaflet)
library(shiny)
library(shinydashboard)
library(plotly)
library(data.table)
library(dplyr)
library(tidyr)
library(scales)
library(forcats)
library(grid)
library(shiny)
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(flexdashboard)
shinyUI(
  dashboardPage(skin="yellow",
                dashboardHeader(title="Corona virus "),
                dashboardSidebar(
                  
                  sidebarMenu(    menuItem("Corona virus in the world" ,icon = icon("dashboard"),
                                           menuSubItem("Overview",tabName = "Corona2" ),
                                           menuSubItem("visualization by country ",tabName = "Covid" ),
                                           menuSubItem("Ratio",tabName = "Covid2" )),
                                  menuItem("Corona virus in the Tunisia", icon = icon("dashboard"),
                                           menuSubItem("Overview",tabName = "Corona4"),
                                           menuSubItem("visualization",tabName = "corona5"),
                                           menuSubItem("Forcasting",tabName = "corona6")) )
                ),
                dashboardBody(tabItems(tabItem(tabName = "Corona2",
                                               fluidRow( box(flexdashboard::gaugeOutput("chart3"),
                                                             title="confirmed",width=4),
                                                         box(flexdashboard::gaugeOutput("chart"),
                                                             title="Recoverd",width=4),
                                                         box(flexdashboard::gaugeOutput("chart2"),
                                                             title="Deaths",width=4)
                                               ),fluidRow(
                                                 tabBox(
                                                   title='Corona Virus in The world ',
                                                   tabPanel("Map", leafletOutput("mymap")
                                                   ),
                                                   
                                                   tabPanel("Table",DT::dataTableOutput("filedf")),width=15
                                                 )
                                                 
                                                 
                                               )) ,
                                       tabItem(tabName = "Corona4",fluidRow (  infoBoxOutput("Total"),
                                                                               infoBoxOutput("Recovred"),
                                                                               infoBoxOutput("Death")
                                       ),fluidRow(
                                         tabBox(
                                           title='Corona Virus in Tunisia ',
                                           tabPanel("Map", leafletOutput("mymap2")
                                           ),
                                           tabPanel("Table",DT::dataTableOutput("filedf2")),width=15
                                         )
                                         
                                         
                                       )
                                       ),
                                       tabItem(tabName = "corona5",fluidRow(box(plotlyOutput("pie")),box(plotlyOutput("bar")),box(title="Distribution of cases by region and gender",plotOutput("bar2")),box(plotlyOutput("pie2")),box(title = "Cases in Tunisia",plotlyOutput("line")),box(plotlyOutput("pie4")),box(plotlyOutput("bed")))),
                                       
                                       tabItem(tabName = "corona6", fluidRow(box(plotOutput("pred"), width = 600 , height = 500))),
                                       tabItem(tabName = "Covid",fluidRow(box(title="choose a country",leafletOutput("leaflet")),box(title = "DATA",DT::dataTableOutput("filedf3")),box(plotlyOutput("tout")),box(shinydashboard::valueBoxOutput("value",width = 15),shinydashboard::valueBoxOutput("value4",width = 15),shinydashboard::valueBoxOutput("value5",width = 15)))),
                                       tabItem(tabName = "Covid2",fluidRow(tabBox(
                                         title='Ratio=Deaths/duration',
                                         tabPanel("visualization", box(plotlyOutput("plotlyy")),box(plotlyOutput("plotlyy1")),box(plotlyOutput("plotlyy2")),box(plotlyOutput("plotlyy3"))
                                         ),
                                         tabPanel("Table",DT::dataTableOutput("filedf5")),width=15
                                       )
                                       ))
                ))
                
                
  )
)