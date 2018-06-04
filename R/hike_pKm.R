#'
#' pKm - performance km
#'
#' @param  points with elev, col specify elev column
#' @return performance km
#' @export

hike_performance_km <- function(elev_points, col, tmp_route, routed){
  tmp <- list()
  down <- 0
  up <- 0
  updist <- 0
  data_p <- elev_points
  st_geometry(data_p) <- NULL
  if(routed){
    if(class(tmp_route)[1] == "sf"){
      dist <- as.numeric(st_length(tmp_route)/1000)
    } else {
      dist <- as.numeric(SpatialLinesLengths(tmp_route))
    }
  } else {
    dist <- data_p[nrow(data_p),1]
  }

  for (i in 1:nrow(elev_points)){
    if(i == nrow(elev_points)){
      route_len <-sum(as.numeric(dist))
      pKm <- route_len + up/100 + down/150
      pKm <- data.frame(pKm = pKm, up = up, down = down, flat = route_len)
    } else {
      if(data_p[i,col] - data_p[i+1,col] < 0){
        up_dist <- abs(data_p[i,col] - data_p[i+1,col])
        up <- up + up_dist
      } else {
        down_dist <- abs(data_p[i,col] - data_p[i+1,col])
        down <- down + down_dist
      }
    }
  }
  return(pKm)
}
#-------------------------------------------------------------------------------

#'
#' travel time in min and hours
#'
#' @param  pkm data or nomral distance data, travelling speed
#' @return travel time
#' @export
hike_traveltime <- function(pkm, speed){
  t <- (pkm/speed)*60
  if(t > 60){
    min <- (t - floor(t))*60
    h <- floor(pkm/speed)
    time <- data.frame(h = h, min = min)
  } else {
    min <- t
    time <- data.frame(h = 0, min = min)
  }
}
