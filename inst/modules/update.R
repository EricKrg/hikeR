# update stat. bars. and weather

update_all <- function(is_routed,values,tmp_route ,session){
  #print("updated fun")
  if(is_routed){
    if(!missing(tmp_route)){
      #print("get el")
      elev <- hikeR::hike_spatial_elev(tmp_route ,shiny_progress = T,Sys.getenv("apikey"))
      #print("heigth")
      height <- format(hikeR::hike_height_diff(elev, col = "elev"),digits = 5)
    }
  } else if(!missing(values) & is_routed == F) {
    elev <- hikeR::hike_spatial_elev(data = values,shiny_progress = T,apikey = Sys.getenv("apikey"))
    height <- format(hikeR::hike_height_diff(elev, col = "elev"),digits = 5)
  }
  pKm <- hikeR::hike_performance_km(elev,col="elev",tmp_route = tmp_route,routed = is_routed)
  pKm$total_height <- pKm[1,2] + pKm[1,3]
  #print("bars")
  # progressbar update
  updateProgressBar(session = session, id = "pKm", value = round(pKm[1,1],digits = 2),total = pKm[1,1])
  updateProgressBar(session = session, id = "flat", value = round(pKm[1,4],digits = 2),total = pKm[1,1])
  updateProgressBar(session = session, id = "up", value = round(pKm[1,2],digits = 2),total = pKm[1,5])
  updateProgressBar(session = session, id = "down", value = round(pKm[1,3],digits = 2),total = pKm[1,5])

  weatherdata <- hikeR::hike_weather(elev)
  update_list <- list(w = weatherdata, e = elev)
  return(update_list) # returns a list of obj 1- weather 2- elev
  }
