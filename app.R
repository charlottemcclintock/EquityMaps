# Equity Indicators

library(shiny)
library(shinydashboard)

# Load libraries
library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(plotly)
library(googlesheets)
library(tools)
library(scales)
library(shinyWidgets)
library(shinythemes)


# Load data
load("data/app_data.Rdata")

prettytab <- gs_title("prettytable")
pretty <- gs_read(prettytab, ws = "acs_tract")
pretty$goodname <- toTitleCase(pretty$description)
pretty$description <- as.character(pretty$description)
pretty <- data.frame(pretty)


# .....................................................................................

# create ui
ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(collapsed = TRUE, 
                   sidebarMenu(
                     menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                     menuItem("Widgets", tabName = "widgets", icon = icon("th"))
                   )),
  dashboardBody(
    tabItems(
     tabItem(tabName = "dashboard",
      fluidRow(
        box(tags$h3("Equity Indicators Maps"),
            tags$p("A short description of the project here, with things like 
                     why this exists, and how you might use it."),
            tags$br(),
            tags$p("Select an equity indicator:"),
            htmlOutput("category"),
            htmlOutput("indicator"),
            htmlOutput("indicator2"),
            selectInput(
              inputId = "geo", 
              label = "Counties",
              choices = levels(factor(tract_data_geo@data$county.nice)), 
              selected = levels(factor(tract_data_geo@data$county.nice)), 
              multiple = T),
            htmlOutput("time"),
            # tags$a(href = "http://commpas-lab.mystrikingly.com", 
            #        tags$img(height = 90, src = "three-line-bw.png")), 
            width=4), 
        tabBox(tabPanel("Map of Indicator 1", 
                        textOutput("maptitle"),
                        leafletOutput("map", height=600),
                        textOutput("source")),
               tabPanel("Map Compare", 
                        leafletOutput("map_compare", height=600)),
            width=8)), # fluid row
      fluidRow(
        box(textOutput("ind1_name"), 
            textOutput("ind1_abt"), tags$br(),
            textOutput("ind2_name"), 
            textOutput("ind2_abt"),
            width=4),
        tabBox(width = 8,
          tabPanel("Distribution",textOutput("histtitle"), plotlyOutput("hist"), width=8), 
          tabPanel("Compare", textOutput("comptitle"), plotlyOutput("compare"), width=8), 
          tabPanel("By Race")
          )
      
      ), # fluid row
      fluidRow(
        box(tags$h3("Community Policy, Analytics, and Strategy Lab"),
            tags$p("The Community Politics, Analytics and Strategy Lab (CommPAS) 
                               sponsors the community-oriented work and collaboration between 
                               the Batten School of Leadership and Public Policy and the UVA 
                               Library's StatLab. Through courses and research projects, the 
                               CommPAS Lab works in partnership with local agencies, nonprofits, 
                               and citizen groups to produce actionable research and resources. 
                               The CommPAS Lab brings students into community-engaged research 
                               where they learn about local challenges and while developing and 
                               applying their policy and data science skills in the service of 
                               our community partners."), width=8), # box close
        box(width=4,  
            tags$a(href = "http://commpas-lab.mystrikingly.com",
                   tags$img(height = 170, src = "three-line-bw.png")))
        ) # row close
      ), # tab item close
      tabItem(tabName = "widgets", 
              h2("Story Content here"))
    ) # tab items
  ) # dashboard body
) # dashboard page


# .....................................................................................

# create server
server <- function(input, output) {
  # categories of indicators
  output$category <- renderUI({
    selectInput(
      inputId = "category", 
      label = "Category:",
      choices = levels(factor(pretty$group)), 
      selected = levels(factor(pretty$group)),
      multiple = T)
  })
  
  # primary indicators
  output$indicator <- renderUI({
    arb <- input$category
    available <- pretty[pretty$group %in% arb, "goodname"]
    names(available) <- "Indicator 1"
    selectInput(
      inputId = "indicator", 
      label = "Primary Indicator:",
      choices = unique(available),
      selected = unique(available)[1],
      multiple = F)
  })
  
  # secondary indicator
  output$indicator2 <- renderUI({
    arb <- input$category
    available <- pretty[pretty$group %in% arb, "goodname"]
    names(available) <- "Indicator 2"
    selectInput(
      inputId = "indicator2",
      label = "Secondary Indicator:",
      choices = c("None", unique(available)),
      selected = "None",
      multiple = F)
  })
  
  years_avail <- reactive({    
    req(input$indicator)
    df <- filter(tract_data_geo@data, county.nice %in% input$geo)
    col <- paste(pretty[pretty$goodname==input$indicator, "varname"])
    df <- na.omit(df[,c(col, "year")])
    df$year <- as.numeric(df$year)
    sort(unique(df$year))
  })
  
  output$time <- renderUI({
    sliderTextInput(inputId = "time", 
                    label = "",
                    choices = years_avail(),
                    selected = years_avail()[1],
                    animate=F,
                    grid=T )})
  
  
  
  # .....................................................................................
  
  # create data frame
  d <- reactive({
    req(input$indicator)
    df <- filter(tract_data_geo@data, county.nice %in% input$geo & 
                   year %in% input$time)
    col <- paste(pretty[pretty$goodname==input$indicator, "varname"])
    df[,col]
  })
  
  # create palette function
  pal <- reactive({
    req(input$indicator)
    df <- filter(tract_data_geo@data, county.nice %in% input$geo & 
                   year %in% input$time)
    col <- paste(pretty[pretty$goodname==input$indicator, "varname"])
    colorNumeric(palette = mycolors,
                 domain = df[,col]) })
  
  # create data set for second indicator
  d2 <- reactive({
    df <- filter(tract_data_geo@data, county.nice %in% input$geo & 
                   year %in% input$time)
    col <- paste(pretty[pretty$goodname==input$indicator2, "varname"])
    df[,col]
  })
  
    
    # create palette function
  pal2 <- reactive({
    req(input$indicator2)
      df <- filter(tract_data_geo@data, county.nice %in% input$geo & 
                     year %in% input$time)
      col2 <- paste(pretty[pretty$goodname==input$indicator2, "varname"])
      colorNumeric(palette = mycolors,
                   domain = df[,col2])
  })
  
  # .....................................................................................
  
  # titles & source
  
  # output source
  output$source <- renderText({
    req(input$indicator)
    paste(pretty[pretty$goodname==input$indicator, "source"])
  })
  
  # output indicator 1 name
  output$ind1_name <- renderText({
    req(input$indicator)
    paste(input$indicator)
  })
  
  # output indicator 1 description
  output$ind1_abt <- renderText({
    paste(pretty[pretty$goodname=="Estimated Population", "about"]) 
  })
  
  
  # output indicator 2 name
  output$ind2_name <- renderText({
    req(input$indicator2)
    if (input$indicator2=="None") {
      paste("")
      }
    else {
    paste(input$indicator2)
    }
  })
  
  # output indicator 2 description
  output$ind2_abt <- renderText({
    if (input$indicator2=="None") {
      paste("")
    }
    else {
    req(input$indicator2)
    paste(pretty[pretty$goodname==input$indicator, "about"]) }
  })

  # map title
  output$maptitle <- renderText({input$indicator})
  
  # comparison title
  comp.title <- reactive({
    req(input$indicator2)
    if (input$indicator2=="None") {
      paste("You have not selected a second indicator. To compare, select a second indicator from the control panel.")
    }
    else {paste(input$indicator, " vs. ", input$indicator2)}
  })
  
  # comparison title
  output$comptitle <- renderText({comp.title()})
  
  hist.title <- reactive({
    req(input$indicator2)
    if (input$indicator2=="None") {
      paste("Histogram of", input$indicator, "by Census Tract")
    }
    else {
      paste("Histograms of", input$indicator, " and ", input$indicator2, "by Census Tract")}
  })
  
  # comparison title
  output$histtitle <- renderText({hist.title()})
  
  
  # .....................................................................................
  
  # output map base
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(data = subset(tract_data_geo, county.nice %in% input$geo & year %in% input$time),
                  fillColor = ~pal()(d()),
                  fillOpacity = 0.5,
                  color = "white",
                  weight = 2,
                  smoothFactor = 0.2,
                  popup = paste0(input$indicator, ": ",d(),  "<br>",
                                tract_data_geo$NAME.y[tract_data_geo$county.nice %in% input$geo & tract_data_geo$year %in% input$time], "<br>"
                  ),
                  highlight = highlightOptions(
                    weight = 5,
                    fillOpacity = 0.7,
                    bringToFront = F)) %>% 
      addPolygons(data = subset(counties_geo, NAMELSAD %in% input$geo),
                  color = "grey",
                  fill = FALSE,
                  weight = 3) %>% 
      addPolygons(data =  parks_sf, group="Parks", 
                  popup = paste(parks_sf$NAME)) %>% 
      addCircles(data =  schools_sf, group="Schools", 
                 popup = paste(schools_sf$NAME)) %>% 
      addLayersControl(
            overlayGroups = c("Parks", "Schools"),
            options = layersControlOptions(collapsed = FALSE), 
            position = "bottomright"
          ) %>% 
      hideGroup("Parks") %>% 
      hideGroup("Schools") %>% 
      addLegend(pal = pal(),
                values = as.numeric(d()),
                position = "topright",
                opacity = 0.25,
                title = input$indicator)  
  })
  
  output$map_compare <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(data = subset(tract_data_geo, county.nice %in% input$geo & year %in% input$time),
                  fillColor = ~pal2()(d2()),
                  fillOpacity = 0.5,
                  color = "white",
                  weight = 2,
                  smoothFactor = 0.2,
                  popup = paste0(input$indicator2, ": ",d2(),  "<br>",
                                 tract_data_geo$NAME.y[tract_data_geo$county.nice %in% input$geo & tract_data_geo$year %in% input$time], "<br>"
                  ),
                  highlight = highlightOptions(
                    weight = 5,
                    fillOpacity = 0.7,
                    bringToFront = F)) %>% 
      addPolygons(data = subset(counties_geo, NAMELSAD %in% input$geo),
                  color = "grey",
                  fill = FALSE,
                  weight = 3) %>% 
      addPolygons(data =  parks_sf, group="Parks", 
                  popup = paste(parks_sf$NAME)) %>% 
      addCircles(data =  schools_sf, group="Schools", 
                 popup = paste(schools_sf$NAME)) %>% 
      addLayersControl(
        overlayGroups = c("Parks", "Schools"),
        options = layersControlOptions(collapsed = FALSE), 
        position = "bottomright"
      ) %>% 
      hideGroup("Parks") %>% 
      hideGroup("Schools") %>% 
      addLegend(pal = pal2(),
                values = as.numeric(d2()),
                position = "topright",
                opacity = 0.25,
                title = input$indicator2)  
  })
  
  
  # build comparison plot
  output$compare <- renderPlotly({ # add regression line to this
    req(input$indicator2)
    if (!input$indicator2=="None") {
      plot_ly(data=tract_data_geo@data,
              x=tract_data_geo@data[tract_data_geo$county.nice %in% input$geo,
                                    paste(pretty[pretty$goodname==input$indicator, "varname"])],
              y=tract_data_geo@data[tract_data_geo$county.nice %in% input$geo,
                                    paste(pretty[pretty$goodname==input$indicator2, "varname"])],
              color=tract_data_geo@data[tract_data_geo$county.nice %in% input$geo,
                                        "county.nice"],
              type = "scatter", mode = "markers", 
              # Set3 has 12 values, which matches the 12 cities/counties
              colors = "Set3", 
              text=paste0(tract_data_geo@data[tract_data_geo$county.nice %in% input$geo,"county.nice"], "<br>",
                          input$indicator, ": ", tract_data_geo@data[tract_data_geo$county.nice %in% input$geo,
                                                                     paste(pretty[pretty$goodname==input$indicator, "varname"])], "<br>",
                         input$indicator2, ": ", tract_data_geo@data[tract_data_geo$county.nice %in% input$geo,
                                                                     paste(pretty[pretty$goodname==input$indicator2, "varname"])]
                         ), 
              hoverinfo='text') %>% 
        layout(xaxis=list(title=input$indicator),
               yaxis=list(title=input$indicator2))
    }
  })
  
  
  output$hist <- renderPlotly({ # add regression line to this
    req(input$indicator2)
    if (!input$indicator2=="None") {
    p1 <- plot_ly(data=tract_data_geo@data,
            x=tract_data_geo@data[tract_data_geo$county.nice %in% input$geo,
                                  paste(pretty[pretty$goodname==input$indicator, "varname"])],
            type="histogram") %>%
      layout(xaxis=list(title=input$indicator))
    p2 <- plot_ly(data=tract_data_geo@data,
            x=tract_data_geo@data[tract_data_geo$county.nice %in% input$geo,
                                  paste(pretty[pretty$goodname==input$indicator2, "varname"])],
            type="histogram") %>%
      layout(xaxis=list(title=input$indicator2))
    subplot(p1, p2, titleX = T, titleY = T) %>%
      layout(showlegend = FALSE)
    }
    else {
      plot_ly(data=tract_data_geo@data,
              x=tract_data_geo@data[tract_data_geo$county.nice %in% input$geo,
                                    paste(pretty[pretty$goodname==input$indicator, "varname"])],
              type="histogram") %>%
        layout(xaxis=list(title=input$indicator))
    }
  })
  
  
}

# dataTableOutput("table")
# renderDataTable()
# run the application 
shinyApp(ui = ui, server = server)
