#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggfortify)
library(leaflet)
library(plotly)
library(DT)
library(forecast)
library(tidyr)
library(dplyr)
library(lubridate)
library(scales)

monthChoices <- list("January 2017" = "2017-01", "February 2017" = "2017-02", "March 2017" = "2017-03",
                     "April 2017" = "2017-04", "May 2017" = "2017-05", "June 2017" = "2017-06",
                     "July 2017" = "2017-07", "August 2017" = "2017-08", "September 2017" = "2017-09",
                     "October 2017" = "2017-10", "November 2017" = "2017-11", "December 2017" = "2017-12",
                     "January 2018" = "2018-01", "February 2018" = "2018-02", "March 2018" = "2018-03",
                     "April 2018" = "2018-04", "May 2018" = "2018-05", "June 2018" = "2018-06",
                     "July 2018" = "2018-07", "August 2018" = "2018-08", "September 2018" = "2018-09",
                     "October 2018" = "2018-10", "November 2018" = "2018-11", "December 2018" = "2018-12")

header <- dashboardHeader(title="LSOA Heat Map",
                          tags$li(class = "dropdown", tags$a("Built by: Scott Robertson", href ="https://www.linkedin.com/in/scott-robertson-80b99435/")),
                          tags$li(class = "dropdown", tags$a("Data source: data.police.uk", href ="https://data.police.uk")),
                          tags$li(tags$style("#month{display:inline}"), class = "dropdown", tags$a(HTML(paste("Month selected - ", textOutput("month"))))),
                          tags$li(tags$style("#crime{display:inline}"), class = "dropdown", tags$a(HTML(paste("Crime type selected - ", textOutput("crime"))))))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Heatmap", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Definitions", tabName = 'def', icon = icon("book-open")),
    # menuItem("Get code", href = "https://github.com/strobertson/LondonCrimeMap", icon = icon("code")),
    selectInput("month", label = h4("Select month"), 
                choices = monthChoices, 
                selected = "2017-01"),
    radioButtons("crime_group", label = h4("Select crime group"), 
                 choices = list("All crime" = "All crime",
                                "Anti-social behaviour" = "Anti-social behaviour",
                                "Burglary" = "Burglary",
                                "Bicycle theft" = "Bicycle theft",
                                "Criminal damage and arson" = "Criminal damage and arson",
                                "Drugs" = "Drugs",
                                "Other crime" = "Other crime",
                                "Other theft" = "Other theft",
                                "Public order" = "Public order",
                                "Possession of weapons" = "Possession of weapons",
                                "Robbery" = "Robbery",
                                "Shoplifting" = "Shoplifting",
                                "Theft from the person" = "Theft from the person",
                                "Vehicle crime" = "Vehicle crime",
                                "Violence and sexual offences" = "Violence and sexual offences"),
                 selected = "All crime")
  )
)

body <- dashboardBody(
  tags$head(tags$style(HTML('
                            .skin-blue .main-header .logo {
                            background-color: #003366
                            }
                            
                            .skin-blue .main-header .logo:hover {
                            background-color: #003366
                            }
                            
                            .skin-blue .main-header .navbar {
                            background-color: #003366
                            }
                            
                            .skin-blue .main-header .logo {
                            background-color: #003366
                            }
                            '))),
  tabItems(
    tabItem(tabName = "dashboard", 
            class = "active",
            h2("Month Dashboard"),
            fluidRow(
              infoBoxOutput("infoBox1"),
              infoBoxOutput("infoBox2"),
              infoBoxOutput("infoBox3")
            ),
            box(width = NULL, solidHeader = TRUE,
                leafletOutput("lsoaMap", height=400)
            ),
            box(width=NULL,
                dataTableOutput("lsoaTable")
            )
    ),
    tabItem(tabName = "def",
            h2("Crime type definitions"),
            fluidRow(
              box(width = 4, 
                  title = "Anti Social Behaviour",
                  p("Includes personal, environmental and nuisance anti-social behaviour."),
                  collapsible = TRUE,
                  collapsed = TRUE),
              box(width = 4,
                  title = "Burglary",
                  p("Includes offences where a person enters a house or other building with the intention of stealing."),
                  br(),
                  h4("Specific offences"),
                  DTOutput("burglary"),
                  collapsible = TRUE,
                  collapsed = TRUE),
              box(width = 4,
                  title = "Bicycle theft",
                  p("Includes the taking without consent or theft of a pedal cycle."),
                  br(),
                  h4("Specific offences"),
                  DTOutput("bicycle"),
                  collapsible = TRUE,
                  collapsed = TRUE)
            ),
            fluidRow(
              box(width = 4,
                  title = "Criminal damage and arson",
                  p("Includes damage to buildings and vehicles and deliberate damage by fire."),
                  br(),
                  h4("Specific offences"),
                  DTOutput("criminal"),
                  collapsible = TRUE,
                  collapsed = TRUE),
              box(width = 4,
                  title = "Drugs",
                  p("Includes offences related to possession, supply and production."),
                  br(),
                  h4("Specific offences"),
                  DTOutput("drugs"),
                  collapsible = TRUE,
                  collapsed = TRUE),
              box(width = 4,
                  title = "Other crime",
                  p("Includes forgery, perjury and other miscellaneous crime."),
                  br(),
                  h4("Specific offences"),
                  DTOutput("other_c"),
                  collapsible = TRUE,
                  collapsed = TRUE)
            ),
            fluidRow(
              box(width = 4,
                  title = "Other theft",
                  p("Includes theft by an employee, blackmail and making off without payment."),
                  br(),
                  h4("Specific offences"),
                  DTOutput("other_t"),
                  collapsible = TRUE,
                  collapsed = TRUE),
              box(width = 4,
                  title = "Public order",
                  p("Includes offences which cause fear, alarm or distress."),
                  br(),
                  h4("Specific offences"),
                  DTOutput("public"),
                  collapsible = TRUE,
                  collapsed = TRUE),
              box(width = 4,
                  title = "Possession of weapons",
                  p("Includes possession of a weapon, such as a firearm or knife."),
                  br(),
                  h4("Specific offences"),
                  DTOutput("weapon"),
                  collapsible = TRUE,
                  collapsed = TRUE)
            ),
            fluidRow(
              box(width = 4,
                  title = "Robbery",
                  p("Includes offences where a person uses force or threat of force to steal."),
                  br(),
                  h4("Specific offences"),
                  DTOutput("robbery"),
                  collapsible = TRUE,
                  collapsed = TRUE),
              box(width = 4,
                  title = "Shoplifting",
                  p("Includes theft from shops or stalls."),
                  br(),
                  h4("Specific offences"),
                  DTOutput("shop"),
                  collapsible = TRUE,
                  collapsed = TRUE),
              box(width = 4,
                  title = "Theft from the person",
                  p("Includes crimes that involve theft directly from the victim (including handbag, wallet, cash, mobile phones) but without the use or threat of physical force."),
                  br(),
                  h4("Specific offences"),
                  DTOutput("theft"),
                  collapsible = TRUE,
                  collapsed = TRUE)
            ),
            fluidRow(
              box(width = 4,
                  title = "Vehicle crime",
                  p("Includes theft from or of a vehicle or interference with a vehicle."),
                  br(),
                  h4("Specific offences"),
                  DTOutput("vehicle"),
                  collapsible = TRUE,
                  collapsed = TRUE),
              box(width = 4,
                  title = "Violence and sexual offences",
                  p("Includes offences against the person such as common assaults, Grievous Bodily Harm and sexual offences."),
                  br(),
                  h4("Specific offences"),
                  DTOutput("violence"),
                  collapsible = TRUE,
                  collapsed = TRUE)
            )
          )
        )
      )

dashboardPage(
  header,
  sidebar, 
  body
)
