#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
library(shinydashboard)
library(shinycssloaders)
library(shinymanager)

source("ca_dataprep.R")

ui <- secure_app(
  fluidPage(
    titlePanel(
      fluidRow(
        column(2, shiny::img(src = "raritylogo.jpg", height = 40, width = 172)),
        column(6, style = "font-size: 25px", strong("Social Drivers and Kidney Disease in California"))
      ), 
      windowTitle = "RTI Rarity SDOH Dashboard"
    ),
    
    fluidRow(
      column(
        6,
        wellPanel(
          tags$style(type="text/css", "img{max-width:80%; float:right;}"),
           shiny::selectInput(
          inputId = "Variable1", 
          "Map 1 Variable: ", 
          list(
            "Local Social Inequity Score" = "LSI", 
            "Life Expectancy" = "LifeExpectancy", 
            "Percent Poverty" = "PovertyPct", 
            "Percent Uninsured" = "NoInsurancePct",
            "Percent Mentally Unhealthy" = "MentallyUnhealthy", 
            "Smoking Prevalence" = "SmokingPrevalence", 
            "Binge Drinking Prevalence" = "BingeDrinkingPrevalence", 
            "Median AQI" = "MedianAQI", 
            "Percent Non-Hispanic Black" = "NHBlackPct", 
            "Percent Hispanic" = "HispanicPct", 
            "Percent Unemployed" = "Unemployment_2019",
            "Percent Physically Inactive" = "PhysicallyInactivePrevalence_2018"
          ), selected = "LSI"), 
          
          withSpinner(leafletOutput(outputId = "map1", width = "600px", height = "350px")
          )
        )
    ),
    
    
    column(
      6, 
      wellPanel(
        tags$style(type="text/css", "img{max-width:80%; float:right;}"),
        shiny::selectInput(
        inputId = "Variable2", 
        "Map 2 Variable (Outcome): ", 
        list(
          "Kidney Disease" = "KIDNEY",
          "Diabetes" = "DIABETES", 
          "Cancer" = "CANCER", 
          "Asthma" = "CASTHMA", 
          "Coronary Heart Disease" = "CHD"
        ), 
        selected = "KIDNEY"), 
        
        withSpinner(leafletOutput(outputId = "map2", width = "600px", height = "350px"))
      )
    )
  ), 
  
  fluidRow(
    column(
      8, 
      shiny::img(src = "RTI_Logo_Tagline_Stacked_rgb_1in.jpg", height = 75, width = 354, align = "right"), 
                HTML('<p>', "Code can be found here: https://github.com/cschwab1/ca_shiny/blob/main/code", '<br>', 
                  "Find out more about RTI at rti.org", '<br>', 
                  "Questions? Reach out by emailing cschwab@rti.org", '<p>')
    )
  )
  
  # ,
  # 
  # fluidRow(
  #   column(
  #     8, 
  #     p("Code can be found here: https://github.com/cschwab1/ca_shiny/blob/main/code")
  #   )
  # )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })
  
  observeEvent(input$Variable, {print(input$Variable)})
  
  # outcomeVar <- eventReactive(input$variable, {
  #   ca_data[, input$Variable] %>% st_drop_geometry() %>% pull() 
  # })
  
  output$map1 <- renderLeaflet({
    validate(need(input$Variable1, 'variable not created yet'))
    pal <- colorNumeric(
      palette = "magma",
      domain = ca_data[, input$Variable1] %>% st_drop_geometry() %>% pull(), 
      na.color = NA, 
      reverse = if_else(input$Variable1 == "LifeExpectancy", TRUE, FALSE))
      # draw the histogram with the specified number of bins
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addMapPane(name = "polygons", zIndex = 410) %>% 
      addMapPane(name = "maplabels", zIndex = 420) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>% 
      addProviderTiles("CartoDB.PositronOnlyLabels", 
                       options = c(leafletOptions(pane = "maplabels"), providerTileOptions(opacity = .5)),
                       group = "map labels") %>% 
      leaflet::addPolygons(data = ca_data,
                           group =  "ca_data",
                           color = "#444444",
                           weight = .5,
                           fillOpacity = .9,
                           fillColor = ~pal(ca_data[, input$Variable1] %>% st_drop_geometry() %>% pull() ),
                           label = lapply(labs, HTML), 
                           options = leafletOptions(pane = "polygons")) %>%
      addLegend("bottomright", pal = pal, values = ca_data[, input$Variable1] %>% st_drop_geometry() %>% pull() ,
                title = "Map 1 Variable",
                opacity = 1, 
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE))) %>% 
        addLayersControl(baseGroups = "CartoDB.PositronNoLabels",
                         overlayGroups = c("map labels",
                                           "ca_data"))
  })
  
  output$map2 <- renderLeaflet({
    validate(need(input$Variable2, 'variable not created yet'))
    pal <- colorNumeric(
      palette = "magma",
      domain = ca_data[, input$Variable2] %>% st_drop_geometry() %>% pull(), 
      na.color = NA, 
      reverse = if_else(input$Variable2 == "LifeExpectancy", TRUE, FALSE))
    # draw the histogram with the specified number of bins
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addMapPane(name = "polygons", zIndex = 410) %>% 
      addMapPane(name = "maplabels", zIndex = 420) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>% 
      addProviderTiles("CartoDB.PositronOnlyLabels", 
                       options = c(leafletOptions(pane = "maplabels"), providerTileOptions(opacity = .5)),
                       group = "map labels") %>% 
      leaflet::addPolygons(data = ca_data,
                           group =  "ca_data",
                           color = "#444444",
                           weight = .5,
                           fillOpacity = .9,
                           fillColor = ~pal(ca_data[, input$Variable2] %>% st_drop_geometry() %>% pull() ),
                           label = lapply(labs, HTML), 
                           options = leafletOptions(pane = "polygons")) %>%
      addLegend("bottomright", pal = pal, values = ca_data[, input$Variable2] %>% st_drop_geometry() %>% pull() ,
                title = "Incidence Pct",
                opacity = 1, 
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE))) %>% 
      addLayersControl(baseGroups = "CartoDB.PositronNoLabels",
                       overlayGroups = c("map labels",
                                         "ca_data"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
