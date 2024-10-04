library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(tidyverse)
library(DT)
library(leaflet)

# read data function
load_data <- function(file_name) {
    tryCatch(
        read.table(file_name, header = TRUE, sep = ",", check.names = FALSE),
        error = function(e) {
            stop(paste("Error: Failed to read data file.", file_name, "-", e$message))
        }
    )
}

# read data
df_main <- read_csv("./data/taipei_map.csv")
yearData <- load_data("./data/city_case_count_new.csv")
cityData <- load_data("./data/citycasecountnewT_new.csv")
yearTypeData <- load_data("./data/donuttableT_new.csv")
yearMonthData <- load_data("./data/monthtime.csv")
yearHourData <- load_data("./data/hour.csv")
cityTypeData <- load_data("./data/cartype.csv")
cityMonthData <- load_data("./data/month.csv")
cityHourData <- load_data("./data/cityhour.csv")
