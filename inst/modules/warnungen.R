library(sf)
library(dplyr)
library(stringr)


warnungen = function() {
  #ogrListLayers("https://maps.dwd.de/geoserver/ows?service=wfs&version=2.0.0&request=GetCapabilities")
  # ogrinfo(dsn, so=TRUE)

  warnungen = tryCatch({
    warnungen = st_read("WFS:https://maps.dwd.de/geoserver/ows?service=wfs&version=2.0.0&request=GetCapabilities",
    layer = "dwd:Warnungen_Gemeinden",
    quiet = T) %>%
    st_cast("GEOMETRYCOLLECTION") %>% st_collection_extract("POLYGON") %>%
    dplyr::select(EVENT, DESCRIPTION, SENT, PARAMATERVALUE, WEB) %>%
    dplyr::mutate(EVENT = as.factor(stringr::str_to_title(as.character(EVENT))))
    },
    error=function(cond) {
      message("No weather warnings")
      no = TRUE
    })
  return(warnungen)
}
