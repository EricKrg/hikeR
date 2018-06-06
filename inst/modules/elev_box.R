library(shiny)
library(shinydashboard)

heigth_mod <- function(height){
  renderValueBox ({
    if(!is.null(height)){
      height_diff = floor(as.numeric(height[1]))
    } else {
      height_diff = 0
    }
    valueBox(subtitle = "height difference", icon =icon("flag"),
             color = "black", value =paste0(height_diff, "m"))

  })
}
#-------------------------------------------------------------------------------
max_box <- function(elevp){
  renderValueBox ({
    if(!is.null(elevp)){
      elev <- elevp
      st_geometry(elev) = NULL
      elev <- floor(max(elev[,2]))
    } else {
      elev = 0
    }
    valueBox(subtitle = "Max. height", icon =icon("flag"),
             color = "black", value =paste0(elev, "m"))

  })
}
min_box <- function(elevp){
  renderValueBox ({
    if(!is.null(elevp)){
      elev <- elevp
      st_geometry(elev) = NULL
      elev <- floor(min(elev[,2]))
    } else {
      elev = 0
    }
    valueBox(subtitle = "Min. height", icon =icon("flag"),
             color = "black", value =paste0(elev, "m"))
  })
}
