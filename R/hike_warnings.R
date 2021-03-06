#'
#' this function retrives weather warnings from germany from the dwd
#'
#' weather warnings
#' @author Patrick Schratz
#' @author Eric Krueger
#' @return returns sf poly with weather warnings for germany
#' @param  empty
#' @export

hike_warnungen = function() {
  # ogrListLayers("https://maps.dwd.de/geoserver/ows?service=wfs&version=2.0.0&request=GetCapabilities")
  # ogrinfo(dsn, so=TRUE)

  warnungen = tryCatch({
    warnungen = sf::st_read("WFS:https://maps.dwd.de/geoserver/ows?service=wfs&version=2.0.0&request=GetCapabilities",
                            layer = "dwd:Warnungen_Gemeinden",
                            quiet = T) %>%
      sf::st_cast("GEOMETRYCOLLECTION") %>% sf::st_collection_extract("POLYGON") %>%
      dplyr::select(EVENT, DESCRIPTION, SENT, PARAMATERVALUE, WEB) %>%
      dplyr::mutate(EVENT = as.factor(stringr::str_to_title(as.character(EVENT))))
  },
  error=function(cond) {
    message("No weather warnings")
    no = FALSE
  })
  return(warnungen)
}
