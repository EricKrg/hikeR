library(sf)
library(dplyr)


warnungen = function(search) {
  ogrListLayers(dsn)
  # ogrinfo(dsn, so=TRUE)

  warnungen = st_read("WFS:https://maps.dwd.de/geoserver/ows?service=wfs&version=2.0.0&request=GetCapabilities",
                 layer = "dwd:Warnungen_Gemeinden") %>%
    st_cast("GEOMETRYCOLLECTION") %>% st_collection_extract("POLYGON") %>%
    select(EVENT)
  return(warnungen)
}
