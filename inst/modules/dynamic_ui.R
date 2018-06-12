library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
## download button if routed
download <- function(tmp_route){
  renderUI({
    if(class(tmp_route)[1] == "sf"){
      dwn <- column(width = 4,box(width = NULL, title = "Download",
                                  solidHeader = T,background = "black",
                                  switchInput(width = 12,inputId = "gpx",
                                              onLabel = "GPX", offLabel = "KML",
                                              label =icon("save")),
                                  downloadButton("downloadData", "Download")))
      return(div(dwn))
    } else {
      dwn <- box(width = NULL, solidHeader = T, background = "black")
      return(div(dwn))
    }
  })  #render the ui
}
#-------------------------------------------------------------------------------
## add ui if waypoints wanted
waypoints <- function(wayp){
  renderUI({
    out <- list()
    for(i in 1:wayp){
      out[[i]] <- textInput(inputId = paste0("to",i),label = "waypoints",placeholder = "Adress")
    }
    return(div(out))
  })
}
#-------------------------------------------------------------------------------
pic_box <- function(term_src, search){
  renderUI({
    out <- list()
    for (i in 1:length(term_src)){
      out[[i]] <- userListItem(
        src = term_src[[i]],
        user_name = search
      )
    }
    return(div(userList(out)))
  })
}

