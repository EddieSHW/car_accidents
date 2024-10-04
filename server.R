#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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
library(forecast)
library(lubridate)


# Define plot functions
create_bar_plot <- function(data, x, y, fill_color, x_label, y_label) {
    ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
        geom_bar(stat = "identity", fill = fill_color) +
        labs(x = x_label, y = y_label, title = "") +
        theme_minimal() +
        theme(
            axis.text.x = element_text(angle = 90, hjust = 1),
            plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
        )
}

create_pie_chart <- function(data, labels, values) {
    plot_ly(data, labels = ~ .data[[labels]], values = ~ .data[[values]], type = "pie") %>%
        layout(
            showlegend = TRUE,
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
}

create_line_plot <- function(data, x, y, x_label, y_label) {
    plot_ly(data, x = ~ .data[[x]], y = ~ .data[[y]], type = "scatter", mode = "lines") %>%
        layout(
            xaxis = list(title = x_label),
            yaxis = list(title = y_label)
        )
}

# Server logic
server <- function(input, output) {
    # Year-wise bar chart
    output$yearBar <- renderPlotly({
        plot_ly(cityData, x = ~city, y = ~ .data[[input$year_set]], type = "bar") %>%
            layout(
                title = paste("Accidents in", input$year_set),
                xaxis = list(title = "City"),
                yaxis = list(title = "Number of Accidents")
            )
    })

    # Year-wise type pie chart
    output$yearType <- renderPlotly({
        create_pie_chart(yearTypeData, "type", input$year_set)
    })

    # Year-wise month-wise line chart
    output$yearMonth <- renderPlotly({
        create_line_plot(yearMonthData, "month", input$year_set, "Month", "Number of Accidents")
    })

    # Year-wise hour-wise line chart
    output$yearHour <- renderPlotly({
        create_line_plot(yearHourData, "hour", input$year_set, "Hour", "Number of Accidents")
    })

    # City-wise bar chart
    output$cityBar <- renderPlotly({
        plot_ly(yearData, x = ~year, y = ~ .data[[input$city_set]], type = "bar") %>%
            layout(
                title = paste("Accidents in", input$city_set),
                xaxis = list(title = "Year"),
                yaxis = list(title = "Number of Accidents")
            )
    })

    # City-wise type pie chart
    output$cityType <- renderPlotly({
        create_pie_chart(cityTypeData, "type", input$city_set)
    })

    # City-wise month-wise line chart
    output$cityMonth <- renderPlotly({
        create_line_plot(cityMonthData, "month", input$city_set, "Month", "Number of Accidents")
    })

    # City-wise hour-wise line chart
    output$cityHour <- renderPlotly({
        create_line_plot(cityHourData, "hour", input$city_set, "Hour", "Number of Accidents")
    })

    # Map
    output$map <- renderLeaflet({
        leaflet(df_main) %>%
            addTiles() %>%
            addMarkers(
                data = df_main,
                lat = ~lat,
                lng = ~lon,
                popup = ~ as.character(avg_flow),
                clusterOptions = markerClusterOptions(showCoverageOnHover = TRUE, zoomToBoundsOnClick = TRUE)
            ) %>%
            addHeatmap(
                lng = ~lon, lat = ~lat, intensity = ~avg_flow,
                blur = 20, max = 0.05, radius = 15
            ) %>%
            addEasyButton(easyButton(
                icon = "fa-crosshairs",
                title = "ME",
                onClick = JS("function(btn, map){ map.locate({setView: true}); }")
            )) %>%
            setView(lng = mean(df_main$lon), lat = mean(df_main$lat), zoom = 11) # default starting place
    })

    # Trend analysis
    output$trendPlot <- renderPlotly({
        yearly_accidents <- yearData %>%
            pivot_longer(cols = -year, names_to = "city", values_to = "accidents") %>%
            group_by(year) %>%
            summarise(total_accidents = sum(accidents))

        plot_ly(yearly_accidents, x = ~year, y = ~total_accidents, type = "scatter", mode = "lines+markers") %>%
            layout(
                title = "Yearly Trend of Traffic Accidents",
                xaxis = list(title = "Year"),
                yaxis = list(title = "Total Accidents")
            )
    })

    # Prediction model
    observeEvent(input$predictBtn, {
        yearly_accidents <- yearData %>%
            pivot_longer(cols = -year, names_to = "city", values_to = "accidents") %>%
            group_by(year) %>%
            summarise(total_accidents = sum(accidents))

        ts_data <- ts(yearly_accidents$total_accidents, start = min(as.numeric(yearly_accidents$year)), frequency = 1)

        # use auto.arima
        model <- arima(ts_data, order = c(1, 1, 1))

        future_years <- input$futureYears
        forecast_result <- forecast(model, h = future_years)

        output$predictionPlot <- renderPlotly({
            plot_ly() %>%
                add_lines(x = ~ yearly_accidents$year, y = ~ yearly_accidents$total_accidents, name = "Historical Data") %>%
                add_lines(
                    x = ~ seq(max(yearly_accidents$year) + 1, by = 1, length.out = future_years),
                    y = ~ forecast_result$mean, name = "Forecast"
                ) %>%
                add_ribbons(
                    x = ~ seq(max(yearly_accidents$year) + 1, by = 1, length.out = future_years),
                    ymin = ~ forecast_result$lower[, 2],
                    ymax = ~ forecast_result$upper[, 2],
                    name = "95% Confidence Interval",
                    fillcolor = "rgba(68, 68, 68, 0.3)",
                    line = list(color = "transparent")
                ) %>%
                layout(
                    title = "Traffic Accident Forecast",
                    xaxis = list(title = "Year"),
                    yaxis = list(title = "Total Accidents")
                )
        })
    })

    # Dashboard plots
    output$totalAccidentsPlot <- renderPlotly({
        yearly_accidents <- yearData %>%
            pivot_longer(cols = -year, names_to = "city", values_to = "accidents") %>%
            group_by(year) %>%
            summarise(total_accidents = sum(accidents))

        plot_ly(yearly_accidents, x = ~year, y = ~total_accidents, type = "bar") %>%
            layout(
                title = "Total Accidents by Year",
                xaxis = list(title = "Year"),
                yaxis = list(title = "Total Accidents")
            )
    })

    output$topCitiesPlot <- renderPlotly({
        top_cities <- yearData %>%
            pivot_longer(cols = -year, names_to = "city", values_to = "accidents") %>%
            group_by(city) %>%
            summarise(total_accidents = sum(accidents)) %>%
            arrange(desc(total_accidents)) %>%
            head(5)

        plot_ly(top_cities, x = ~city, y = ~total_accidents, type = "bar") %>%
            layout(
                title = "Top 5 Cities by Total Accidents",
                xaxis = list(title = "City"),
                yaxis = list(title = "Total Accidents")
            )
    })

    output$monthlyTrendPlot <- renderPlotly({
        monthly_trend <- yearMonthData %>%
            pivot_longer(cols = -month, names_to = "year", values_to = "accidents")

        plot_ly(monthly_trend, x = ~month, y = ~accidents, color = ~year, type = "scatter", mode = "lines+markers") %>%
            layout(
                title = "Monthly Trend of Accidents",
                xaxis = list(title = "Month"),
                yaxis = list(title = "Number of Accidents")
            )
    })

    output$miniMap <- renderLeaflet({
        leaflet(df_main) %>%
            addTiles() %>%
            addHeatmap(
                lng = ~lon, lat = ~lat, intensity = ~avg_flow,
                blur = 20, max = 0.05, radius = 15
            ) %>%
            setView(lng = mean(df_main$lon), lat = mean(df_main$lat), zoom = 11)
    })

    output$accidentTypePlot <- renderPlotly({
        accident_types <- yearTypeData %>%
            pivot_longer(cols = -type, names_to = "year", values_to = "count") %>%
            group_by(type) %>%
            summarise(total_count = sum(count))

        plot_ly(accident_types, labels = ~type, values = ~total_count, type = "pie") %>%
            layout(title = "Accident Types Distribution")
    })

    observe({
        leafletProxy("map") %>%
            clearMarkers() %>%
            clearHeatmap()

        if (input$showMarkers) {
            leafletProxy("map") %>%
                addMarkers(
                    data = df_main,
                    lat = ~lat,
                    lng = ~lon,
                    popup = ~ as.character(avg_flow),
                    clusterOptions = markerClusterOptions(showCoverageOnHover = TRUE, zoomToBoundsOnClick = TRUE)
                )
        }

        if (input$showHeatmap) {
            leafletProxy("map") %>%
                addHeatmap(
                    data = df_main,
                    lng = ~lon, lat = ~lat, intensity = ~avg_flow,
                    blur = 20, max = 0.05, radius = 15
                )
        }
    })
}
