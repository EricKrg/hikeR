#'
#'  create lines
#'  @param df with coords
#'  @return create a line from multiple input coords
#'  @export
hike_create_lines <- function(data){  # line string from multiple input points
  if(!is.null(data)){
    #print(data)
    lines <- sf::st_sfc(st_linestring(as.matrix(data)))#,crs = 4326)
    return(lines)
  }
}

#'
#' distance
#' @param a to b, unit
#' @return calc. the distance between to points
#' @export
hike_distance <- function(point_a, point_b, unit){
  lat = (point_a[,2] + point_b[,2]) / 2 * 0.01745
  dx = 111.3 * cos(lat) * (point_a[,1] - point_b[,1])
  dy = 111.3 * (point_a[,2] - point_b[,2])
  distance <-  sqrt((dx * dx) + (dy * dy))
  if(unit == "km"){
    return(distance/1000)
  }
  else if (unit == "m"){
    return(distance)
  }
}

#'
#' redundant fun
#'
#' @export
hike_search_plc <- function(instring){
  xy <- geo_code(instring)
}

#'
#' height difference
#' @param  points with elev data, col = specify elev. data column
#' @return calc. height difference in m
#' @export
#
hike_height_diff <- function(elev_points, col){
  if(is.null(elev_points) & missing(col)){ return(tmp <- 0)}
  st_geometry(elev_points) <- NULL
  tmp <- 0
  for (i in 1:nrow(elev_points)){
    if(i == nrow(elev_points)){
      return(tmp)
      break
    }
    tmp <- tmp + abs(elev_points[i,col] - elev_points[i+1,col])
  }
}
