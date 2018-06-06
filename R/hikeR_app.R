#'
#'run app
#'
#' @param  elevation_api - google elec. https://developers.google.com/maps/documentation/elevation/#api_key
#' @param  cycle_api - cyclestreets.net api
#' @param  graph_hopper_api - optinal api (cause you have to pay for it)
#' @param  ors_api - open route service api
#'@export
hike_app <- function(elevation_api,cycle_api,graph_hopper_api,ors_api){
  if(missing(graph_hopper_api)){
    graph_hopper_api = ""
  }
  Sys.setenv(apikey = elevation_api)
  Sys.setenv(cycle_api = cycle_api)
  Sys.setenv(graph_hopper = graph_hopper_api)
  Sys.setenv(ors = ors_api)
  shiny::shinyAppDir(appDir = system.file(".", package = "hikeR"))
}
