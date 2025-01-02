# Load required libraries
library(shiny)
library(leaflet)
library(ggplot2)
library(DT)
library(httr2)
library(tidyverse)
library(lubridate)
library(plotly)
library(shinydashboard)

# Define default locations
default_locations <- list(
  "Bangkok" = list(lat = 13.7563, lon = 100.5018),
  "Chiang Mai" = list(lat = 18.7883, lon = 98.9853),
  "Phuket" = list(lat = 7.8804, lon = 98.3923),
  "Pattaya" = list(lat = 12.9236, lon = 100.8824)
)

TMD_token <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImp0aSI6ImI1Y2Y4ODk4ZDYyZTUxMjc3YjgzYWJkNzViYTMxNzQ3N2EyOGU4N2RkYThkNWQ4MWU0NTc5YzQzYWNiZTFkMTA1NWYyMDJiOWM4ZDcwZTY0In0.eyJhdWQiOiIyIiwianRpIjoiYjVjZjg4OThkNjJlNTEyNzdiODNhYmQ3NWJhMzE3NDc3YTI4ZTg3ZGRhOGQ1ZDgxZTQ1NzljNDNhY2JlMWQxMDU1ZjIwMmI5YzhkNzBlNjQiLCJpYXQiOjE3MjgwMzA4NTksIm5iZiI6MTcyODAzMDg1OSwiZXhwIjoxNzU5NTY2ODU5LCJzdWIiOiIyMjk2Iiwic2NvcGVzIjpbXX0.UZKPGcezqCvXsgk5OvW86P2OouzMGJnLmP35ZT3QXoDUbnQE6iMHp7QZSCw9_fNMisAHEkl-qMbl7T7OE-fTrP2PmyCO2B-cYEIvIcdPo_vzOmt4wcx_r5qrKc2dSm_GNTAb9vgw7AoPz6ff74lQhr9iDLwFmw1lC1MqY7OdibsvOaB6W9IJwXQgbw8DmlEtTDhPJ7rPym-ewfuRMfja_09cpXVoeczLiY-kB7IT0YwsAiBd6TuwblIcJpRPJKr829xtdHGGEvQCAXi87EP0Da5j27pQRF7zzWYBg5bY9inBQ9BHHDV_-egZydzEvjHtGd5HcrNJQBT29Dz4DMXvk6CPkxh9qIwNVGNk4cOmTWaMGoxZZUS4tHl4JD01GosbgV2-ls1d_GKJ3gXFbiHF22jvBJCdCRkWrZ5Oqfepph_zXYxTc9uOU0wgn8VRwDqysPeO5DYU8Q1Dbilzg-4FlT1xjklBvHUAUp5X3ydJXcHHjIvbkV0WCeefCYqqY97ceZMKODcU52NN85IDsAB40QGqF9iBBU_q6Rk1qvATK2B3kCeCGJiTDIfKfdraeonqtVZQTBel9dPXFhCKGozkmJG3CyLbolj-4iphd6UK_0kOWQRMC_R1PP5nmUqpJPJwUfJpT0qsIjA73k8CHtQNO4ZMbmVg4iYQLisrBTI7Vto"

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Weather Monitoring"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Weather Data", tabName = "weather", icon = icon("cloud")),
      div(
        class = "sidebar-menu", style = "padding: 10px;",
        selectInput("default_location", "Select Location:",
                    choices = c("Custom", names(default_locations)),
                    selected = "Bangkok"),
        numericInput("longitude", "Longitude:", value = default_locations$Bangkok$lon),
        numericInput("latitude", "Latitude:", value = default_locations$Bangkok$lat),
        dateInput("selected_date", "Selected Date:", value = Sys.Date(), max = Sys.Date() + 7),
        div(
          style = "margin: 5px 0;",
          actionButton("run", "Get data and run", class = "btn-primary btn-sm"),
          downloadButton("download_data", "Download CSV", class = "btn-sm")
        ),
        tags$small("Click on map to set location or select from preset locations")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: white; }
        .skin-blue .main-header .logo { font-size: 14px; }
        .form-control { font-size: 12px; height: 28px; padding: 4px 8px; }
        .btn { font-size: 12px; padding: 4px 8px; }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "weather",
              fluidRow(
                column(width = 6,
                       box(
                         width = NULL, leafletOutput("map", height = 350),
                         title = "Location Map", status = "primary", solidHeader = TRUE
                       )
                ),
                column(width = 6,
                       box(
                         width = NULL, dataTableOutput("data_table", height = 350),
                         title = "Weather Data", status = "primary", solidHeader = TRUE
                       )
                )
              ),
              fluidRow(
                column(width = 4,
                       box(
                         width = NULL, plotlyOutput("tc_plot", height = 250),
                         title = "Temperature (°C)", status = "info", solidHeader = TRUE
                       )
                ),
                column(width = 4,
                       box(
                         width = NULL, plotlyOutput("hr_plot", height = 250),
                         title = "Humidity (%)", status = "info", solidHeader = TRUE
                       )
                ),
                column(width = 4,
                       box(
                         width = NULL, plotlyOutput("rain_plot", height = 250),
                         title = "Rainfall (mm)", status = "info", solidHeader = TRUE
                       )
                )
              )
      )
    )
  )
)
query_weather_data <- function(latitude, longitude, date) {
  json <- request("https://data.tmd.go.th/nwpapi/v1/forecast/location/hourly/at") |>
    req_auth_bearer_token(TMD_token) |>
    req_url_query(
      lat = latitude,
      lon = longitude,
      fields = 'cond,tc,rh,rain',
      date = as.character(date),
      hour = 0,
      duration = 24
    ) |>
    req_perform() |>
    resp_body_json()
}

# Server Definition
server <- function(input, output, session) {
  
  # Update coordinates when default location changes
  observeEvent(input$default_location, {
    if (input$default_location != "Custom") {
      selected_location <- default_locations[[input$default_location]]
      updateNumericInput(session, "longitude", value = selected_location$lon)
      updateNumericInput(session, "latitude", value = selected_location$lat)
    }
  })
  
  observe({
    leafletProxy("map") %>% 
      clearMarkers() %>%
      addMarkers(lng = input$longitude, lat = input$latitude)
  })
  
  observeEvent(input$map_click, {
    updateSelectInput(session, "default_location", selected = "Custom")
    updateNumericInput(session, "longitude", value = input$map_click$lng)
    updateNumericInput(session, "latitude", value = input$map_click$lat)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      setView(lng = input$longitude, lat = input$latitude, zoom = 6) %>%
      addMarkers(lng = input$longitude, lat = input$latitude)
  })
  
  data_reactive <- eventReactive(input$run, {
    req(input$longitude, input$latitude)
    
    withProgress(message = 'Fetching data...', {
      tryCatch({
        json <- query_weather_data(input$latitude, 
                                   input$longitude, 
                                   input$selected_date)
        
        json |> 
          pluck(1, 1) |> 
          pluck("forecasts") |> 
          map_dfr(\(x) {
            tibble(
              time = ymd_hms(x |> pluck("time")),
              tc = x |> pluck("data", "tc"),
              rh = x |> pluck("data", "rh"),
              rain = x |> pluck("data", "rain"),
              cond = x |> pluck("data", "cond")
            )
          })
      }, error = function(e) {
        showNotification("Error fetching data", type = "error")
        NULL
      })
    })
  })
  
  plot_theme <- theme_minimal() +
    theme(
      text = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      plot.margin = margin(2, 2, 2, 2)
    )
  
  output$tc_plot <- renderPlotly({
    req(data_reactive())
    gg <- ggplot(data_reactive(), aes(x = time, y = tc)) +
      geom_line(color = "red") +
      geom_point(color = "darkred") +
      scale_y_continuous(limits = c(0, 50)) +
      labs(x = "Time", y = "°C") +
      plot_theme
    ggplotly(gg) %>% 
      layout(
        showlegend = FALSE,
        margin = list(l = 40, r = 20, t = 20, b = 40),
        font = list(size = 10)
      )
  })
  
  output$hr_plot <- renderPlotly({
    req(data_reactive())
    gg <- ggplot(data_reactive(), aes(x = time, y = rh)) +
      geom_line(color = "blue") +
      geom_point(color = "darkblue") +
      scale_y_continuous(limits = c(0, 100)) +
      labs(x = "Time", y = "%") +
      plot_theme
    ggplotly(gg) %>% 
      layout(
        showlegend = FALSE,
        margin = list(l = 40, r = 20, t = 20, b = 40),
        font = list(size = 10)
      )
  })
  
  output$rain_plot <- renderPlotly({
    req(data_reactive())
    gg <- ggplot(data_reactive(), aes(x = time, y = rain)) +
      geom_line(color = "green") +
      geom_point(color = "darkgreen") +
      scale_y_continuous(limits = c(0, 10)) +
      labs(x = "Time", y = "mm") +
      plot_theme
    ggplotly(gg) %>% 
      layout(
        showlegend = FALSE,
        margin = list(l = 40, r = 20, t = 20, b = 40),
        font = list(size = 10)
      )
  })
  
  output$data_table <- renderDataTable({
    req(data_reactive())
    data_reactive() %>%
      mutate(
        time = format(time, "%Y-%m-%d %H:%M"),
        tc = round(tc, 1),
        rh = round(rh, 1),
        rain = round(rain, 2)
      ) %>%
      datatable(
        options = list(
          pageLength = 24,
          dom = 'tp',
          scrollY = "300px",
          scrollCollapse = TRUE
        ),
        colnames = c("Time", "Temp", "Humid", "Rain", "Cond"),
        class = 'compact'
      )
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("weather_data_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(data_reactive(), file, row.names = FALSE)
    }
  )
}

# Run the Shiny app
shinyApp(ui, server)
