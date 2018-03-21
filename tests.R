
require(tidyverse)
require(sf)
require(sp)
require(data.table)
require(leaflet)
require(leaflet.extras)
require(elevatr)
require(shiny)
require(shinydashboard)

path <- SpatialLinesDataFrame()


leaflet(path) %>% addTiles() %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  setView(lng = -100 , lat = 40, zoom = 4) %>%
  leaflet.extras::addDrawToolbar(targetGroup = "path",
                                 editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))


cities <- read.csv(textConnection("
City,Lat,Long,Pop
Boston,42.3601,-71.0589,645966
Hartford,41.7627,-72.6743,125017
New York City,40.7127,-74.0059,8406000
Philadelphia,39.9500,-75.1667,1553000
Pittsburgh,40.4397,-79.9764,305841
Providence,41.8236,-71.4222,177994
"))
map <- leaflet(cities) %>% addTiles() %>%
  addCircles(lng = ~Long, lat = ~Lat, weight = 1,
             radius = ~sqrt(Pop) * 30, label = ~City, group ='cities') %>%
  addDrawToolbar(
    targetGroup='cities',
    editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>%
  addLayersControl(overlayGroups = c('cities'), options =
                     layersControlOptions(collapsed=FALSE)) %>%
  addStyleEditor()
test <- data.frame(y = as.numeric(map$x$calls[[2]]$args[[1]]),
                   x = as.numeric(map$x$calls[[2]]$args[[2]]))
test <- st_as_sf(test, coords = c("x", "y"))
st_crs(test) <- 4326


mapview() %>% addDrawToolbar(
  targetGroup='cities',
  editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>%
  addLayersControl(overlayGroups = c('cities'), options =
                     layersControlOptions(collapsed=FALSE))


fName <- 'https://rawgit.com/benbalter/dc-maps/master/maps/ward-2012.geojson'

geoJson <- readr::read_file(fName)
geoJson2 <- rmapshaper::ms_simplify(geoJson, keep=0.01)
## Warning: This function will be removed in the next version, see
## geojsonlint::geojson_hint()
leaflet() %>% addTiles() %>% setView(-77.0369, 38.9072, 12) %>%
  addGeoJSONv2(geoJson2,
               group = 'wards', layerId = 'dc-wards') %>%
  addDrawToolbar(
    targetLayerId='dc-wards',
    editOptions = editToolbarOptions(
      selectedPathOptions = selectedPathOptions()))  %>%
  addLayersControl(overlayGroups = c('wards'),
                   options = layersControlOptions(collapsed=FALSE)) %>%
  addStyleEditor()
get



########################################
#Working
#######################################




data <-data.frame(x = c(-122.08,-115.93), y = c(38.41,37.51))
coordinates(data) <- c("x","y")
lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
lines <- st_as_sf(lines)
st_crs(lines) <- 4326
require(rgeos)
numOfPoints  <-  as.numeric(st_length(lines)/ 2000)
points <- spsample(as(lines,"Spatial"), n = numOfPoints, type = "regular")

points <- st_as_sf(points)
st_crs(points) <- 4326
xy <- as.data.frame(st_coordinates(points))
#mapview::mapview(lines) + mapview::mapview(points)
require(rgbif)
apikey <- "AIzaSyAB6DJYmiY-82HLSgo0CLCDeZ9h2p6l9xY"
t<- elevation(longitude = xy$X,latitude = xy$Y, key = apikey)

tmp <- 0
j <- 1
for (i in 1:nrow(points)){
  if (i == 1){
    points$distance[j] <- 0
  }
  if (nrow(points)==i){
    break
  } else {
  j <- i +1
  print(2)
  tmp <- tmp + st_length(st_linestring(st_coordinates(points[i:j,])))
  print(tmp)
  points$distance[j] <- tmp*100
  }
}
points$elev <- t$elevation

points %>%
  ggplot(aes(x = distance, y = elev)) + geom_area()


require(rnoaa)
options(noaakey = "YpibPzExmRScOhadXYdmqRdEKttZoJWE") # noaa key



