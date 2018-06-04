#'
#' weather
#' @return return simple weather forecast by coords
#' @param  in coords
#' @export


hike_weather <- function(in_data){
  if(class(in_data) == "numeric"){
    mid <- in_data
  }else{
    mid <- sf::st_coordinates(in_data[floor(nrow(in_data)/2),])
  }
  smp_weather <- weatherr::locationforecast(lat = mid[2],
                                  lon = mid[1], exact = F)
  return(smp_weather)
}

