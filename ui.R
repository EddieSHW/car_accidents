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
library(shinydashboardPlus)
library(plotly)
library(tidyverse)
library(DT)
library(leaflet)
library(leaflet.extras)


ui <- dashboardPage(
    dashboardHeader(title = "Taipei Traffic Accident Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Year Analysis", tabName = "year", icon = icon("calendar")),
            menuItem("City Analysis", tabName = "city", icon = icon("city")),
            menuItem("Map", tabName = "map", icon = icon("map")),
            menuItem("Trend & Prediction", tabName = "trend", icon = icon("chart-line"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "dashboard",
                fluidRow(
                    box(plotlyOutput("totalAccidentsPlot"), width = 6),
                    box(plotlyOutput("topCitiesPlot"), width = 6)
                ),
                fluidRow(
                    box(plotlyOutput("monthlyTrendPlot"), width = 12)
                ),
                fluidRow(
                    box(leafletOutput("miniMap", height = 300), width = 6),
                    box(plotlyOutput("accidentTypePlot"), width = 6)
                )
            ),
            tabItem(
                tabName = "year",
                fluidRow(
                    box(selectInput("year_set", "Select Year", choices = c("2018", "2019", "2020")), width = 3),
                    box(plotlyOutput("yearBar"), width = 9)
                ),
                fluidRow(
                    box(plotlyOutput("yearType"), width = 6),
                    box(plotlyOutput("yearMonth"), width = 6)
                ),
                fluidRow(
                    box(plotlyOutput("yearHour"), width = 12)
                )
            ),
            tabItem(
                tabName = "city",
                fluidRow(
                    box(
                        selectInput("city_set", "Select City",
                            choices = c(
                                "Nantou_County", "Chiayi_City", "Chiayi_County", "Keelung_City", "Yilan_County",
                                "Pingtung_County", "Changhua_County", "New_Taipei_City", "Hsinchu_City",
                                "Hsinchu_County", "Taoyuan_City", "Penghu_County", "Taichung_City", "Taipei_City",
                                "Tainan_City", "Taitung_County", "Hualien_County", "Miaoli_County",
                                "Lienchiang_County", "Kinmen_County", "Yunlin_County", "Kaohsiung_City"
                            )
                        ),
                        width = 3
                    ),
                    box(plotlyOutput("cityBar"), width = 9)
                ),
                fluidRow(
                    box(plotlyOutput("cityType"), width = 6),
                    box(plotlyOutput("cityMonth"), width = 6)
                ),
                fluidRow(
                    box(plotlyOutput("cityHour"), width = 12)
                )
            ),
            tabItem(
                tabName = "map",
                fluidRow(
                    box(
                        leafletOutput("map", height = "calc(100vh - 80px)"),
                        checkboxInput("showHeatmap", "Show Heatmap", value = TRUE),
                        checkboxInput("showMarkers", "Show Markers", value = TRUE),
                        width = 12
                    )
                )
            ),
            tabItem(
                tabName = "trend",
                fluidRow(
                    box(plotlyOutput("trendPlot"), width = 12)
                ),
                fluidRow(
                    box(
                        title = "Prediction Settings",
                        sliderInput("futureYears", "Years to Predict:", min = 1, max = 10, value = 5),
                        actionButton("predictBtn", "Predict")
                    ),
                    box(plotlyOutput("predictionPlot"), width = 8)
                )
            )
        )
    )
)
