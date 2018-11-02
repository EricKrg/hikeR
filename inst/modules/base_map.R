require(leaflet)
require(leaflet.extras)
require(shinydashboard)
require(shinyWidgets)
require(sf)


base_map <- function(search, detail){
    map <- leaflet() %>%
      addTiles() %>%
      addDrawToolbar(
        editOptions = editToolbarOptions(remove = F),
        singleFeature = T,
        circleMarkerOptions = F,
        circleOptions = F,
        polygonOptions = F,
        rectangleOptions = F,
        markerOptions = F
      ) %>% addProviderTiles(providers$HikeBike.HikeBike, group = "Hike and Bike Map") %>%
      addProviderTiles(providers$Hydda, group = "Standard") %>%
      mapview::addMouseCoordinates()  %>%
      setView(lng =  search[1] ,
              lat = search[2],
              zoom = if (detail) {
                19
              } else{
                12
              }) %>%
      addLayersControl(baseGroups = c("Standard", "Hike and Bike Map")) %>%
      addCircleMarkers(
        lng = search[1],
        lat = search[2],
        radius = 20,
        stroke = F,
        fillColor = viridisLite::inferno(1, begin = 0.5)
      )
    return(map)
}

reach_base_map <- function(search, detail,x,y,z){
  map <- leaflet() %>%
    addTiles() %>%
    addDrawToolbar(editOptions = editToolbarOptions(remove = F),singleFeature = T,
                   circleMarkerOptions = T, circleOptions = F,polygonOptions = F,
                   rectangleOptions = F,markerOptions = F,polylineOptions = F) %>%
    addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
    mapview::addMouseCoordinates()  %>%
    setView(lng =  x , lat = y,
            zoom =if(detail){19} else{z}) %>%
    addCircleMarkers(lng = search[1],
                     lat = search[2],radius = 20,stroke = F,
                     fillColor = viridisLite::inferno(1, begin = 0.5)) %>%
    addLayersControl(
      baseGroups = c("Light", "Dark"))
  return(map)
}
