library(shiny)
library(dplyr)
library(jsonlite)
library(shinydashboard)
library(timevis)
library(shinythemes)
library(DT)

# Function to load data from GitHub JSON
load_data <- function() {
  url <- "https://raw.githubusercontent.com/richardsondev/pse-outages/refs/heads/main/outages.json"
  data <- fromJSON(url)
  return(data)
}

# UI
ui <- fluidPage(
  titlePanel("PSE Outage Dashboard"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Overview", value = "tab_overview"),
        tabPanel("Outage by City", value = "tab_city"),
        tabPanel("Timeline", value = "tab_detail")
      )
    ),
    mainPanel(
      uiOutput("dashboardUI")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Auto-refresh every 20 minutes
  auto_refresh <- reactiveTimer(1200000)
  
  # Load and return the data
  data <- reactive({
    auto_refresh()
    load_data()
  })
  
  # Flatten nested PointOfInterest into a single dataframe
  all_pois <- reactive({
    pois <- lapply(data()$PseMap$DataProvider, function(x){
      x <- x$PointOfInterest
      if (is.null(poi) || !is.data.frame(poi)) return(NULL)
      poi
      })
    pois <- Filter(Negate(is.null), pois)
    if(length(pois) ==0) return (data.frame())
    poi_df <- do.call(rbind, pois)
    return(poi_df)
  })
  
  # Summary of outages by city
  outages_by_city <- reactive({
    df <- all_pois()
    req(nrow(df) > 0)
    df %>%
      count(Title, name = "Outages") %>%
      rename(City = Title) %>%
      arrange(desc(Outages))
  })
  
  output$dashboardUI <- renderUI({
    req(input$tabs)
    
    if (input$tabs == "tab_overview") {
      overview <- data()$Common
      fluidRow(
        valueBox(overview$CustomerAfftectedCount, "Customers Affected", width = 4),
        valueBox(overview$OutageCount, "Outages", width = 4),
        valueBox(overview$LastUpdatedDatetime, "Last Updated", width = 4)
      )
      
    } else if (input$tabs == "tab_city") {
      fluidRow(
        column(12,
               h3("Outages by City"),
               dataTableOutput("cityTable"),
               uiOutput("cityDetails")
        )
      )
      
    } else if (input$tabs == "tab_detail") {
      fluidRow(
        div(
          style = "height: 80vh; display: flex; flex-direction: column; overflow: hidden;",  # Fixed container height, no shrinking
          # Inner container for timeline with scrolling enabled
          div(
            style = "flex-grow: 1; overflow-y: auto;",  # Allows scrolling within the timeline area
            timevisOutput("timeline", height = "100%")
          ),
          # Incident details container below the timeline
          div(
            style = "height: auto;",  # Incident details will expand without affecting the layout
            uiOutput("incidentDetails")
        )
      )
      )
    }
  })
  
  # Render city table
  output$cityTable <- renderDT({
    city_data <- outages_by_city()
    if(nrow(city_data) == 0) return(NULL)
    
    datatable(city_data, selection = "single", options = list(pageLength = 10))
  })
  
  # Handle row click in city table
  observeEvent(input$cityTable_rows_selected, {
    selected_city <- outages_by_city()$City[input$cityTable_rows_selected]
    req(selected_city)
    
    poi_df <- all_pois()
    selected_ids <- poi_df$Id[poi_df$Title == selected_city]
    
    attributes <- data()$PseMap$DataProvider$Attributes
    selected_attributes <- attributes[which(poi_df$Id %in% selected_ids)]
    
    output$cityDetails <- renderUI({
      if (length(selected_attributes) == 0) return(NULL)
      tagList(
        lapply(selected_attributes, function(attr) {
          if (!is.data.frame(attr)) return(NULL)
          div(
            h4("Outage Details"),
            p(strong("Start Time: "), as.character(attr[1, 2])),
            p(strong("Customers Impacted: "), as.character(attr[2, 2])),
            p(strong("Status: "), as.character(attr[3, 2])),
            p(strong("Cause: "), as.character(attr[4, 2])),
            p(strong("Last Update: "), as.character(attr[5, 2])),
            p(strong("Estimated Restoration Time: "), as.character(attr[6, 2]))
          )
        })
      )
    })
  })
  
  # Timeline data for "Detailed View" tab
  timeline_data <- reactive({
    attributes_list <- data()$PseMap$DataProvider$Attributes
    timeline_data_list <- list()
    current_year <- format(Sys.Date(), "%Y")
    
    for (i in seq_along(attributes_list)) {
      attribute <- attributes_list[[i]]
      if (is.null(attribute) || !is.data.frame(attribute)) next
      
      start_time_raw <- as.character(attribute[1, 2])
      start_time_combined <- paste(current_year, start_time_raw)
      start_time_parsed <- as.POSIXct(start_time_combined, format = "%Y %m/%d %I:%M %p", tz = "UTC")
      
      if (!is.na(start_time_parsed)) {
        timeline_data_list[[i]] <- data.frame(
          id = i,
          content = as.character(attribute[3, 2]),  # Status
          start = start_time_parsed
        )
      }
    }
    
    do.call(rbind, timeline_data_list)
  })
  
  output$timeline <- renderTimevis({
    req(timeline_data())
    timevis(timeline_data(),
            options = list(
              zoomable = TRUE,
              moveable = TRUE,
              zoomMin = 1000 * 60 * 60,
              zoomMax = 1000 * 60 * 60 * 24 * 7,
              format = list(
                majorLabels = list(
                  hour = 'HH:00'
                )
              )
            ))
  })
  
  observeEvent(input$timeline_selected, {
    selected_id <- input$timeline_selected
    if (!is.null(selected_id) && selected_id != "") {
      selected_incident <- data()$PseMap$DataProvider$Attributes[[as.numeric(selected_id)]]
      
      output$incidentDetails <- renderUI({
        div(
          h4("Incident Details"),
          p(strong("Start Time: "), as.character(selected_incident[1, 2])),
          p(strong("Customers Impacted: "), as.character(selected_incident[2, 2])),
          p(strong("Status: "), as.character(selected_incident[3, 2])),
          p(strong("Cause: "), as.character(selected_incident[4, 2])),
          p(strong("Last Update: "), as.character(selected_incident[5, 2])),
          p(strong("Estimated Restoration Time: "), as.character(selected_incident[6, 2]))
        )
      })
    }
  })
}

shinyApp(ui, server)
