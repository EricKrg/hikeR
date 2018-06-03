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
require(openrouteservice)

# constant like vars.
help <- TRUE
height_stats <- FALSE

#style funs
progressBar2 <- function (id, value, total = NULL, display_pct = FALSE, size = NULL,
                          status = NULL, striped = FALSE, title = NULL)
{
  if (is.null(total)) {
    percent <- value
  }
  else {
    percent <- value
  }
  if (!is.null(title) | !is.null(total)) {
    title <- htmltools::tags$span(class = "progress-text",
                                  title, htmltools::HTML("&nbsp;"))

  }
  if (is.null(total)) {
    total <- htmltools::tags$span(class = "progress-number",
                                  htmltools::tags$b(value, id = paste0(id, "-value")))

  }
  tagPB <- htmltools::tags$div(class = "progress-group", title,
                               total, htmltools::tags$div(class = if (!is.null(size))
                                 paste("progress", size)
                                 else "progress", htmltools::tags$div(id = id, style = if (percent >
                                                                                           0)
                                   paste0("width:",percent, "%;"), style = if (display_pct)
                                     "min-width: 2em;", class = "progress-bar", class = if (!is.null(status))
                                       paste0("progress-bar-", status), class = if (striped)
                                         "progress-bar-striped", role = "progressbar", if (display_pct)
                                           paste0(percent, "%"))))

}


# Define UI for application that draws a histogram

dashboardPage(dashboardHeader(disable = T),
              dashboardSidebar(disable = T),
              dashboardBody(
                fluidRow(
                  #box(title ="hikeR",solidHeader = T,background = "black",width = 12)),
                  infoBoxOutput("main",width = 10), infoBoxOutput("git",width = 2)),
                fluidRow(
                  column(width = 12,
                         box(width = NULL,solidHeader = TRUE,
                             searchInput(width = NULL,"search","Search Region", placeholder = "City or Region",
                                         value = "Jena" ,
                                         btnSearch = icon("search"),
                                         btnReset = icon("remove")),
                             # selectizeInput("test", "label",choices= state.name ,selected = NULL,  multiple = F,
                             #                options = list(maxOptions = 2)),
                             materialSwitch(inputId = "detail", label = "Jump to street level", status = "danger"),
                             fluidRow(
                               column(width = 4,
                                      valueBoxOutput("weather",width = NULL)
                               ),
                               column(width = 4,
                                      valueBoxOutput("percip", width = NULL)
                               ),
                               column(width = 4,
                                      valueBoxOutput("temp", width = NULL)
                               )
                             ),
                             fluidRow(
                               box(collapsible = T,collapsed = T,solidHeader = T,width = 12,title = "In reach Input",
                                   background = "black",
                                   sliderInput(inputId = "reach_time",label = "Travel time (min)",min = 1,
                                               max = 120,value = 5),
                                   radioGroupButtons(inputId = "reach_plan",
                                                     label = "Moving style",
                                                     choices = c("cycling-regular" , "cycling-road" ,
                                                                 "cycling-safe" , "cycling-mountain" ,
                                                                 "cycling-tour" , "foot-walking" ,
                                                                 "foot-hiking"),
                                                     status = "danger", selected = "cycling-regular")
                               )
                             ),
                             column(width = 7,
                                    tabBox(width = NULL,
                                    tabPanel(icon = icon("location-arrow"),title = "Routing",
                                    leafletOutput("leafmap",height = 700)),
                                    tabPanel(icon = icon("compass"),title = "In reach",
                                             leafletOutput("leafmap_reach",height = 700)))),
                             column(width = 5,
                                    fluidRow(
                                      tabBox(width = NULL,
                                             tabPanel(icon = icon("tasks"),title = "Trip data",
                                                      progressBar2(title = "performance Kilometer (km)" ,id = "pKm",value = 0, status = "danger",
                                                                   display_pct = TRUE),
                                                      progressBar2(title = "horizontal distance (km)", id = "flat",value = 0, status = "info",
                                                                   display_pct = TRUE),
                                                      progressBar2(title = "vertical up (m)" ,id = "up",value = 0, status = "info",
                                                                   display_pct = TRUE),
                                                      progressBar2(title = "vertical down (m)", id = "down",value = 0, status = "info",
                                                                   display_pct = TRUE)
                                             ),
                                             tabPanel(icon = icon("caret-up"),"Airline", plotly::plotlyOutput("plot",width = NULL),
                                                      switchInput("twoD",label = "2D",value = FALSE)),
                                             tabPanel(icon = icon("caret-up"),"Route", withSpinner(plotly::plotlyOutput("plot_route"),type = 6),
                                                      switchInput("twoDr",label = "2D",value = FALSE)),
                                             tabPanel(title = "Trip coords.",icon = icon("map-marker"),
                                                      tableOutput("data"))
                                      )
                                    ),
                                    fluidRow(
                                      box(collapsible = T,solidHeader = T,width = 12, background = "black",
                                          valueBoxOutput("max"), valueBoxOutput("min"), valueBoxOutput("heightbox"))),
                                    fluidRow(
                                      column(width = 12,
                                             box(solidHeader = T,background = "black", width = NULL,title = "Routing",
                                                 column(width = 8, materialSwitch(inputId = "string_route", label = "Route by adress", status = "danger"),
                                                        conditionalPanel(condition = "input.string_route",
                                                                         materialSwitch(inputId = "more", label = "More then two Adresses", status = "danger"),
                                                                         conditionalPanel(condition = "input.more",
                                                                                          sliderInput(inputId = "waypoints",label = "Nr. of Waypoints",min = 1,max = 15,value = 1)),
                                                                         textInput(inputId = "from",label = "from A",placeholder = "Adress"),
                                                                         textInput(inputId = "to",label = "to B",placeholder = "Adress"),
                                                                         conditionalPanel(condition = "input.more",
                                                                                          uiOutput("waypoints_panel")
                                                                         )),
                                                        conditionalPanel(condition ="input.leafmap_draw_new_feature || input.string_route",
                                                                         radioGroupButtons(inputId = "route_opt",
                                                                                           label = "Routing providers",
                                                                                           choices = c("cycle", "ORS"), # "GHopper"- api not wokring anymore, OSM left out not working
                                                                                           checkIcon = list(yes = tags$i(class = "fa fa-check-square",
                                                                                                                         style = "color: steelblue"),
                                                                                                            no = tags$i(class = "fa fa-square-o",
                                                                                                                        style = "color: steelblue"))),
                                                                         conditionalPanel(condition = 'input.route_opt == "cycle"',
                                                                                          radioGroupButtons(inputId = "plan",
                                                                                                            label = "Routing style",
                                                                                                            choices = c("fastest","balanced","quietest"),
                                                                                                            status = "danger")),
                                                                         conditionalPanel(condition = 'input.route_opt == "ORS"',
                                                                                          radioGroupButtons(inputId = "ors_plan",
                                                                                                            label = "Routing style",
                                                                                                            choices = c("cycling-regular" , "cycling-road" ,
                                                                                                                        "cycling-safe" , "cycling-mountain" ,
                                                                                                                        "cycling-tour" , "foot-walking" ,
                                                                                                                        "foot-hiking"),
                                                                                                            status = "danger")),
                                                                         # conditionalPanel( condition = 'input.route_opt == "GHopper"',
                                                                         #                   radioGroupButtons(inputId = "graph",
                                                                         #                                     label = "Routing style",
                                                                         #                                     choices = c("foot","hike","bike", "mtb","racingbike"),
                                                                         #                                     status = "danger")),
                                                                         actionButton("routing","Route"))),
                                                 column(width = 4,
                                                        uiOutput("download")
                                                 )))
                                    ),
                                    fluidRow(
                                      box(width = 12,background = "black",title = "Traveltime", solidHeader = T,
                                          column(width = 5,
                                                 knobInput("pace", label = "Speed in km/h: ",
                                                           value = 5,
                                                           thickness = 0.3,
                                                           cursor = TRUE,
                                                           width = "70%",
                                                           height = "70%",
                                                           min = 2,max = 50,
                                                           fgColor = "red" ,bgColor = "white"
                                                 )),
                                          column(width = 5,
                                                 tableOutput("traveltime")))
                                    )
                                      )
                    )
                  )
                )
              )
)

# ui <- dashboardPage(
#   dashboardHeader(disable = T),
#   dashboardSidebar(disable = T),
#   body
# )
