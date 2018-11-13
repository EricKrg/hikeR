#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
require(rgbif)
require(tidyverse)
require(sf)
require(sp)
require(rgeos)
require(data.table)
require(leaflet)
require(leaflet.extras)
require(shinydashboard)
require(plotly)
require(stplanr)
require(weatherr)
require(shinyWidgets)
require(mapview)
require(rgdal)
require(raster)
require(shinycssloaders)


require(shinydashboardPlus)
require(shinyjs)
require(stringr)
require(hikeR)
require(openrouteservice)
# constant like vars.
help <- TRUE
height_stats <- FALSE
useSweetAlert()


# Define UI for application that draws a histogram

title <- tags$div(icon("pagelines"),
                'hikeR', target="_blank", color = "green")
dashboardPage(title="hikeR - ease up planning",
  skin = "black",

  dashboardHeader(
    disable = F,
     title = title,
     titleWidth = 350
  ),
  dashboardSidebar(
    width = 350,
    disable = F,
    # fluidRow(valueBox(".  hikeR",icon = icon("pagelines"),color = "olive",
    #                   subtitle = ". ease up planning",width = NULL)),
   searchInput(
      width = NULL,
      "search",
      "Search Region",
      placeholder = "City or Region",
      value = "Jena" ,
      btnSearch = icon("search"),
      btnReset = icon("remove")
   ), # weather
   fluidRow(infoBoxOutput("weather", width = NULL)),
   fluidRow(infoBoxOutput("temp", width = NULL)),
   fluidRow(infoBoxOutput("percip", width = NULL)),
   fluidRow(infoBoxOutput("wind", width = NULL)),
   fluidRow(infoBoxOutput("hum", width = NULL)),
   useSweetAlert(),
   materialSwitch(inputId = "warnungen",
                  label = "Weather warnings",
                  value = FALSE),
   fluidRow("....    *warnings only for Germany"),
   fluidRow(dashboardLabel(actionLink("Help", inputId = "help"), status = "info"))
  ),

  dashboardBody(
    #tags$head(includeHTML('./leaflet_cdn.html')),
    tags$head(includeScript("google_analystics.js")),
    #tags$head(includeScript("geolocation.js")),
    tags$head(tags$style(
    HTML( # custom sized panels
      '.info-box {min-height: 45px;} .info-box-icon {height: 45px; line-height: 45px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'
    )
  )),
  #
  # main panel -----------------------------------------------------------------
  fluidRow(column(
    width = 12,
    box(
      width = NULL,
      solidHeader = TRUE,
      # right column------------------------------------------------------------
      column(
        width = 7,
        tabBox(
          width = NULL,
          tabPanel(
            icon = icon("location-arrow"),
            title = "Routing",
            leafletOutput("leafmap", height = 700)
          ),
          tabPanel(
            icon = icon("compass"),
            title = "In reach",
            leafletOutput("leafmap_reach", height = 700)
          )
        ),
        materialSwitch(
          inputId = "detail",
          label = "Jump to street level",
          status = "danger"
        ),
        boxPlus(
          label_text = "open",
          closable = F,
          enable_label = TRUE,
          label_status = "danger",
          collapsible = T,
          status = "danger",
          collapsed = T,
          solidHeader = F,
          width = NULL,
          title = "Impressions",
          #background = "black",
          uiOutput("pic_box")
        )
      ),
      # Left Column ------------------------------------------------------------
      column(
        width = 5,
        fluidRow(tabBox(
          width = NULL,
          tabPanel(
            icon = icon("tasks"),
            title = "Trip data",
            hikeR::hike_progressBar(
              title = "performance Kilometer (km)" ,
              id = "pKm",
              value = 0,
              status = "danger",
              display_pct = TRUE
            ),
            hikeR::hike_progressBar(
              title = "horizontal distance (km)",
              id = "flat",
              value = 0,
              status = "info",
              display_pct = TRUE
            ),
            hikeR::hike_progressBar(
              title = "vertical up (m)" ,
              id = "up",
              value = 0,
              status = "info",
              display_pct = TRUE
            ),
            hikeR::hike_progressBar(
              title = "vertical down (m)",
              id = "down",
              value = 0,
              status = "info",
              display_pct = TRUE
            )
          ),
          tabPanel(
            icon = icon("caret-up"),
            "Height Profile",
            withSpinner(
              plotly::plotlyOutput("plot", width = NULL),
              type = 6,
              color = "black"
            ),
            switchInput("twoD", label = "2D", value = FALSE)
          ),
          # tabPanel(
          #   icon = icon("caret-up"),
          #   "Route",
          #   withSpinner(
          #     plotly::plotlyOutput("plot_route"),
          #     type = 6,
          #     color = "black"
          #   ),
          #   switchInput("twoDr", label = "2D", value = FALSE)
          # ),
          tabPanel(
            title = "Trip coords.",
            icon = icon("map-marker"),
            #tableOutput("data"),
            DT::dataTableOutput('old_routes')
          )
        )),
        fluidRow(
          box(
            collapsible = T,
            solidHeader = T,
            width = 12,
            background = "black",
            valueBoxOutput("max"),
            valueBoxOutput("min"),
            valueBoxOutput("heightbox")
          )
        ),
        fluidRow(
          boxPlus(
            label_text = "open",
            closable = F,
            enable_label = TRUE,
            label_status = "danger",
            collapsible = T,
            collapsed = T,
            solidHeader = F,
            width = 12,
            title = "In reach Input",
            background = "black",
            sliderInput(
              inputId = "reach_time",
              label = "Travel time (min)",
              min = 1,
              max = 120,
              value = 5
            ),
            radioGroupButtons(
              inputId = "reach_plan",
              label = "Moving style",
              choices = c(
                "cycling-regular" ,
                "cycling-road" ,
                "cycling-safe" ,
                "cycling-mountain" ,
                "cycling-tour" ,
                "foot-walking" ,
                "foot-hiking"
              ),
              status = "danger",
              selected = "cycling-regular"
            )
          )
        ),
        fluidRow(column(
          width = 12,
          box(
            solidHeader = T,
            background = "black",
            width = NULL,
            title = "Routing",
            column(
              width = 8,
              materialSwitch(
                inputId = "string_route",
                label = "Route by adress",
                status = "danger"
              ),
              conditionalPanel(
                condition = "input.string_route",
                materialSwitch(
                  inputId = "more",
                  label = "More then two Adresses",
                  status = "danger"
                ),
                conditionalPanel(
                  condition = "input.more",
                  sliderInput(
                    inputId = "waypoints",
                    label = "Nr. of Waypoints",
                    min = 1,
                    max = 15,
                    value = 1
                  )
                ),
                textInput(
                  inputId = "from",
                  label = "from A",
                  placeholder = "Adress"
                ),
                textInput(
                  inputId = "to",
                  label = "to B",
                  placeholder = "Adress"
                ),
                conditionalPanel(condition = "input.more",
                                 uiOutput("waypoints_panel"))
              ),
              conditionalPanel(
                condition = "input.leafmap_draw_new_feature || input.string_route",
                radioGroupButtons(
                  inputId = "route_opt",
                  label = "Routing providers",
                  choices = c("cycle", "ORS"),
                  # "GHopper"- api not wokring anymore, OSM left out not working
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square",
                                 style = "color: steelblue"),
                    no = tags$i(class = "fa fa-square-o",
                                style = "color: steelblue")
                  )
                ),
                conditionalPanel(
                  condition = 'input.route_opt == "cycle"',
                  radioGroupButtons(
                    inputId = "plan",
                    label = "Routing style",
                    choices = c("fastest", "balanced", "quietest"),
                    status = "danger"
                  )
                ),
                conditionalPanel(
                  condition = 'input.route_opt == "ORS"',
                  radioGroupButtons(
                    inputId = "ors_plan",
                    label = "Routing style",
                    choices = c(
                      "cycling-regular" ,
                      "cycling-road" ,
                      "cycling-safe" ,
                      "cycling-mountain" ,
                      "cycling-tour" ,
                      "foot-walking" ,
                      "foot-hiking"
                    ),
                    status = "danger"
                  )
                ),
                # conditionalPanel( condition = 'input.route_opt == "GHopper"',
                #                   radioGroupButtons(inputId = "graph",
                #                                     label = "Routing style",
                #                                     choices = c("foot","hike","bike", "mtb","racingbike"),
                #                                     status = "danger")),
                materialSwitch("routing", "Route",value = F)
              )
            ),
            column(width = 4,
                   uiOutput("download"))
          )
        )),
        fluidRow(
          box(
            width = 12,
            background = "black",
            title = "Traveltime",
            solidHeader = T,
            column(
              width = 5,
              knobInput(
                "pace",
                label = "Speed in km/h: ",
                value = 5,
                thickness = 0.3,
                cursor = TRUE,
                width = "70%",
                height = "70%",
                min = 2,
                max = 50,
                fgColor = "red" ,
                bgColor = "white"
              )
            ),
            column(width = 5,
                   tableOutput("traveltime"))
          )
        )
      )
    ),
    # footer--------------------------------------------------------------------
    fluidRow(
      widgetUserBox(
        width = 12,
        title = " ",
        subtitle = " ",
        type = 2,
        src = "",
        background = TRUE,
        backgroundUrl = "https://farm2.staticflickr.com/1492/25965397430_9b4403eecd_k.jpg",
        collapsed = T,
        collapseball = T,
        boxToolSize = "xs",
        socialButton(type = "github", url = "https://github.com/EricKrg/hikeR"),
        socialButton(type = "twitter", url = "https://twitter.com/Eric_krg"),
        span("made by Eric Krueger with ", icon("heart"), " for OpenSource")
      )
    )
  )))
)
