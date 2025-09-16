################################################################################
#                   Author: Joshua Thompson
#   O__  ----       Email:  joshua.thompson@ofwat.gov.uk
#  c/ /'_ ---
# (*) \(*) --
# ======================== Script  Information =================================
# PURPOSE: company postcode lookup
#
# PROJECT INFORMATION:
#   Name: company postcode lookup
#
# HISTORY:----
#   Date		        Remarks
#	-----------	   ---------------------------------------------------------------
#	 16/09/2025    Created script                                   JThompson (JT)
#===============================  Environment Setup  ===========================
#==========================================================================================

# load libraries
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(wesanderson) 
library(shinycssloaders)
library(shinyjs)
library(shinyBS)
library(httr)
library(jsonlite)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(extrafont)
library(stringr)
library(RColorBrewer)
library(plotly)
library(DT)
library(Cairo)
library(writexl)
library(purrr)

# load data
data_path <- "PostcodeWaterCompanyPointFC.rds"
postcode_combined <- readRDS(data_path)

# check lat/long present nad if geometry exists, extract coords
if ("geometry" %in% names(postcode_combined)) {
  # if it's sf 
  if (inherits(postcode_combined, "sf")) {
    coords <- st_coordinates(st_as_sf(postcode_combined))
    postcode_combined <- postcode_combined %>%
      st_set_geometry(NULL) %>%
      mutate(long = coords[,1], lat = coords[,2])
  }
}

#  validation
if (!all(c("Postcode", "lat", "long") %in% names(postcode_combined))) {
  stop("postcode_combined must contain columns: Postcode , lat, long (or an sf geometry). Update the RDS accordingly.)")
}

# get choices for autocomplete 
postcode_choices <- unique(postcode_combined$Postcode)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
    #initial_loader {
      position: fixed;
      width: 100%;
      height: 100%;
      background: white;
      display: flex;
      flex-direction: column;
      justify-content: center;
      align-items: center;
      z-index: 10000;
      text-align: center;
    }
    #initial_loader img {
      width: 400px;
      height: auto;
    }
    .loading-bar {
      width: 400px;
      margin-top: 20px;
      height: 20px;
      background-color: #f3f3f3;
      border: 1px solid #ccc;
      border-radius: 5px;
      position: relative;
    }
    .loading-fill {
      height: 100%;
      background-color: #003296;
      width: 0%;
      transition: width 0.3s;
    }
    .loading-percent {
      margin-top: 10px;
      font-family: : 'Krub', sans-serif;
      font-size: 16px;
    }
    .leaflet-popup-content {
        font-family: 'Krub', sans-serif;
        font-size: 14px;
    }
    #supplier_info {
        font-family: 'Krub', sans-serif;
        font-size: 14px;
        white-space: pre-wrap;   /* keeps line breaks */
      }
  "))
  ),
  tags$script(HTML("
  let percent = 0;
  const interval = setInterval(() => {
    if (percent >= 100) {
      clearInterval(interval);
      document.getElementById('initial_loader').style.display = 'none';
    } else {
      percent += 1;
      document.querySelector('.loading-fill').style.width = percent + '%';
      document.querySelector('.loading-percent').innerText = percent + '%';
    }
  }, 40); // simulate loading every 40ms
")),
  

  div(id = "initial_loader",
      img(src = "https://www.ofwat.gov.uk/wp-content/themes/ofwat/assets/images/ofwat_logo_blue.svg", alt = "Loading..."),
      div(class = "loading-bar",
          div(class = "loading-fill")
      ),
      div(class = "loading-percent", "0%")
  ),
  # ui aesthetics
  tags$link(rel = "stylesheet", type = "text/css", href = "css/krub.css"),
  tags$head(HTML("<title>Open Data Aggregator</title>"),
            tags$style(HTML(
              "
              .navbar{background-color: #FFFFFF !important; padding-left: 0px; margin-left:0px; padding-right: 0px; margin-right:0px;padding-top: 0px; margin-top:0px;}
              .navbar-default .navbar-brand:hover {color: blue;}
              .navbar { background-color: gray;}
              .navbar-default .navbar-nav > li > a {color:black;}
              .navbar-default .navbar-nav > .active > a,
              .navbar-default .navbar-nav > .active > a:focus,
              .navbar-default .navbar-nav > .active > a:hover {color: white;background-color: #505250;}
              .navbar-default .navbar-nav > li > a:hover {color: white;background-color:#505250;text-decoration:underline;}
              .butt{background-color:#505250; color:#FFFFFF; border-color:#080808;}
              .btn-file{background-color:#505250; color:#FFFFFF; border-color:#080808;}
              .action-button{background-color:#505250; color:#FFFFFF; border-color:#080808;}
              .radio input{background-color:#505250; color:#FFFFFF; border-color:#080808;}
              input[type='radio'] {filter: saturate(0);}
              input[type='checkbox'] {filter: saturate(0);}
              * {font-family: 'Krub', sans-serif;}
              /*html {overflow:   scroll;}
              ::-webkit-scrollbar {width: 0px; background: transparent; */}
              "
            )), 
            tags$script('
      function copyToClipboard() {
        var textArea = document.createElement("textarea");
        textArea.value = "joshua.thompson@ofwat.gov.uk";
        document.body.appendChild(textArea);
        textArea.select();
        document.execCommand("Copy");
        document.body.removeChild(textArea);
        alert("Email address copied to clipboard!");
      }
    ')),
  
  titlePanel(
    fluidRow(style = "background-color:#ffffff; padding-top: 0px; margin-top:0px; padding-bottom: 20px; margin-bottom:0px",
             column(9,h2(("Find your Water & Wastewater Supplier"),style="font-size:26px;font-style:bold; font-weight: 600; color:black;"),
                    p("A tool to find your wastewater and water supplier by postcode",style="font-size:18px;font-style:normal; font-weight: 400; color:black;"),
                    a(actionButton(inputId = "github1", label = "  Developer",icon = icon("github", lib = "font-awesome"),
                                   style = "background-color:#505250; color:#FFFFFF; border-color:#080808"),href="https://github.com/joshuajdthompson",target="_blank")),
             column(3, tags$a(img(src='https://www.ofwat.gov.uk/wp-content/themes/ofwat/assets/images/ofwat_logo_blue.svg', align = "right", width = '300px', height = 'auto', style="padding: 0px")))
    )),
  sidebarLayout(
    sidebarPanel(
      helpText("Type a postcode (7-character outcode like 'AB1 0AA' or compact 'AB10AA'). Autocomplete enabled."),
      selectizeInput(
        inputId = "Postcode",
        label = "Postcode",
        choices = NULL,
        multiple = FALSE,
        options = list(
          placeholder = 'Start typing postcode...',
          maxOptions = 10,
          create = FALSE
        )
      ),
      actionButton("locate", "Locate on map"),
      hr(),
      verbatimTextOutput("supplier_info"),
      width = 3
    ),
    mainPanel(
      leafletOutput("map", height = 600),
      width = 9
    )
  )
)

server <- function(input, output, session) {
  # force no initial selection
  updateSelectizeInput(
    session, "Postcode",
    choices = postcode_choices,
    server = TRUE,
    selected = character(0)  
  )
  
  # selected postcode row
  selected_row <- reactive({
    req(input$Postcode)
    # try exact match first and allow not formatted matches
    p <- input$Postcode
    df <- postcode_combined %>% filter(Postcode == p | gsub("\\s+", "", Postcode) == gsub("\\s+", "", p))
    if (nrow(df) == 0) return(NULL)
    df[1, , drop = FALSE]
  })
  
  # inital map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      setView(lng = -1.5, lat = 54, zoom = 6) %>%
      addMiniMap(
        tiles = providers$CartoDB.Positron,
        position = 'topright', 
        width = 200, height = 200,
        toggleDisplay = FALSE,
        aimingRectOptions = list(color = "red", weight = 1, clickable = FALSE),
        zoomLevelOffset=-3)
  })
  
  # reactive for map & popup
  observeEvent(input$locate, {
    row <- selected_row()
    if (is.null(row)) {
      showNotification("Postcode not found in dataset.", type = "error")
      return()
    }
    lng <- as.numeric(row$long)
    lat <- as.numeric(row$lat)
    # pop up contents
    popup <- paste0(
      "Postcode: <strong>", row$Postcode, "</strong><br/>",
      "Water Supply Company: <strong>", coalesce(row$`Water Supply Company`, "(none)"), "</strong> ",
      if (!is.na(row$`Water Supply Company Acronym`)) paste0("(", row$`Water Supply Company Acronym`, ")") else "", "<br/>",
      "Wastewater Company: <strong>", coalesce(row$`Wastewater Company`, "(none)"), "</strong> ",
      if (!is.na(row$`Wastewater Company Acronym`)) paste0("(", row$`Wastewater Company Acronym`, ")") else ""
    )
    
    leafletProxy("map") %>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addMarkers(lng = lng, lat = lat, popup = popup) %>%
      setView(lng = lng, lat = lat, zoom = 14) %>%
      addMiniMap(
        tiles = providers$CartoDB.Positron,
        position = 'topright', 
        width = 200, height = 200,
        toggleDisplay = FALSE,
        aimingRectOptions = list(color = "red", weight = 1, clickable = FALSE),
        zoomLevelOffset=-3)
  })
  
  # update map popup
  output$supplier_info <- renderText({
    row <- selected_row()
    if (is.null(row)) return("No postcode selected or not found in dataset.")
    paste0(
      "Postcode: ", row$Postcode, "\n",
      "Water Supply Company: ", coalesce(row$`Water Supply Company`, "(none)"),
      if (!is.na(row$`Water Supply Company Acronym`)) paste0(" (", row$`Water Supply Company Acronym`, ")") else "", "\n",
      "Wastewater Company: ", coalesce(row$`Wastewater Company`, "(none)"),
      if (!is.na(row$`Wastewater Company Acronym`)) paste0(" (", row$`Wastewater Company Acronym`, ")") else ""
    )
  })
  
  # allow enter for update
  observe({
    if (!is.null(input$Postcode) && input$Postcode != "") {
      invalidateLater(200, session)
      isolate({
        session$sendCustomMessage(type = 'auto_locate', message = list())
      })
    }
  })
}

shinyApp(ui, server)
