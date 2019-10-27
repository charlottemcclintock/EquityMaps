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

# Load data
# tract_data <- readRDS("data/tract_data.RDS")
# tract_data_geo <- readRDS("data/tract_data_geo.RDS")
# counties_geo <- readRDS("data/county_geo.RDS")
# 
# # .....................................................................................
# 
# # # define two palettes
# nb.cols <- 10
# mycolors <- colorRampPalette(brewer.pal(8, "YlGnBu"))(nb.cols)
# 
# # read in descriptions crosswalk
# # gs_auth(new_user = TRUE)
# pretty <- gs_title("prettytable")
# pretty <- gs_read(pretty, ws = "acs")
# pretty$goodname <- toTitleCase(pretty$description)
# 
# # join pretty names to existing data
# tab <- select(tract_data_geo@data, COUNTYFP, NAME.1)
# tab <- separate(tab, NAME.1, 
#                 into=c("tract","county.nice", "state"), sep=", ", remove=F)
# 
# tab <- unique(select(tab, COUNTYFP, county.nice))
# tract_data_geo@data <- left_join(tract_data_geo@data, tab, by="COUNTYFP")
# save.image(file = "data/app_data.Rdata")

# Load data
load("app_data.Rdata")

# .....................................................................................

# create ui
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(collapsed = TRUE),
  dashboardBody(
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
          tags$a(href = "http://commpas-lab.mystrikingly.com", 
                 tags$img(height = 90, src = "three-line-bw.png")), width=4), 
      box(textOutput("maptitle"),
          leafletOutput("map", height=600),
          textOutput("source"),
          width=8)), # fluid row
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
                             our community partners."), width=4),
      tabBox(width = 8,
        tabPanel("Distribution",textOutput("histtitle"), plotlyOutput("hist"), width=8), 
        tabPanel("Compare", textOutput("comptitle"), plotlyOutput("compare"), width=8))
    
    ) # fluid row
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
  
  # indicators
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
  
  
  
  # .....................................................................................
  
  # create data frame
  d <- reactive({
    req(input$indicator)
    df <- filter(tract_data_geo@data, county.nice %in% input$geo)
    col <- paste(pretty[pretty$goodname==input$indicator, "varname"])
    df[,col]
  })
  
  # create palette function
  pal <- reactive({
    req(input$indicator)
    df <- filter(tract_data_geo@data, county.nice %in% input$geo)
    col <- paste(pretty[pretty$goodname==input$indicator, "varname"])
    colorNumeric(palette = mycolors,
                 domain = df[,col])
    
  })
  
  
  # .....................................................................................
  
  # titles & source
  
  # output source
  output$source <- renderText({
    req(input$indicator)
    paste(pretty[pretty$goodname==input$indicator, "source"])
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
      # addPolygons(data = counties_geo,
      #             color = "grey",
      #             fillOpacity = 0,
      #             weight = 2) %>% 
      # clearShapes() %>%
      addPolygons(data = subset(counties_geo, NAMELSAD %in% input$geo),
                  color = "grey",
                  fillOpacity = 0,
                  weight = 2) %>% 
      addPolygons(data = subset(tract_data_geo, county.nice %in% input$geo),
                  fillColor = ~pal()(d()),
                  fillOpacity = 0.5,
                  color = "white",
                  weight = 2,
                  smoothFactor = 0.2,
                  popup = paste(input$indicator, ":",d(),  "<br>",
                                tract_data_geo$NAME.1[tract_data_geo$county.nice %in% input$geo], "<br>"
                  ),
                  highlight = highlightOptions(
                    weight = 5,
                    fillOpacity = 0.7,
                    bringToFront = TRUE)) %>% 
      addLegend(pal = pal(),
                values = as.numeric(d()),
                position = "topright",
                opacity = 0.25,
                title = input$indicator)
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
              colors = "Set3") %>% 
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