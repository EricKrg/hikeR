#'
#' weather
#' @return return simple weather forecast by coords
#' @param  in_data - point coords to retrive weather info
#' @examples
#' a <- stplanr::geo_code("Jena")
#' hikeR::hike_weather(a)
#' @export
hike_weather <- function(in_data){
  if(class(in_data) == "numeric"){
    mid <- in_data
  }else{
    mid <- sf::st_coordinates(in_data[floor(nrow(in_data)/2),])
  }
  smp_weather <- weatherr::locationforecast(lat = mid[2],
                                  lon = mid[1], exact = F)
  smp_weather2 <- weatherr::locationforecast(lat = mid[2],
                                            lon = mid[1], exact = T)
  weather <- cbind(smp_weather[4,], wind_spd = smp_weather2$windSpeed_mps[1], humidity = smp_weather2$humidity[1])
  return(weather)
}

