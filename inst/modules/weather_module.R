
library(shiny)
library(shinyWidgets)
library(shinydashboard)



weather_mod <- function(weatherd){
    renderInfoBox ({
    if(is.null(weatherd)){
      infoBox(
        title = "Draw/load a path for weather data",value = NA,icon = icon("thermometer-empty "),
        color = "olive",fill = T
      )
    } else {
      w_con <- weatherd[1,"weather_id"]
      if(w_con %in% c("Sun")){
        infoBox(
          title = "Weather condition",w_con,icon = icon("certificate",lib = "glyphicon"),
          color = "yellow",fill = T
        )
      }
      else if(w_con %in% c("LightCloud","PartlyCloud","Cloud")){
        infoBox(
          title = "Weather condition", icon = icon("cloud"),
          color = "light-blue", value = w_con,fill = T
        )
      }
      else if(w_con %in% c("LightRainSun","LightRainThunderSun","LightRain","Rain","RainThunder","RainSun")){
        infoBox(
          title = "Weather condition", icon = icon("tint"),
          color = "light-blue", value = w_con,fill = T
        )
      }
      else if(w_con %in% c("Fog")){
        infoBox(
          title = "Weather condition", icon = icon("align-justify "),
          color = "navy", value = w_con,fill = T
        )
      }
      else if(w_con %in% c("SleetThunder",
                                         "DrizzleThunderSun",
                                         "RainThunderSun",
                                         "LightSleetThunderSun",
                                         "HeavySleetThunderSun",
                                         "LightSnowThunderSun",
                                         "HeavySnowThunderSun",
                                         "DrizzleThunder",
                                         "LightSleetThunder",
                                         "HeavySleetThunder",
                                         "LightSnowThunder",
                                         "HeavySnowThunder")){
        infoBox(
          title = "Weather condition",icon = icon("bolt"),
          color = "red", value = w_con,fill = T
        )
      }
      else if(w_con %in% c("DrizzleSun","Drizzle")){
        infoBox(
          title = "Weather condition", icon = icon("braille"),
          color = "purple", value = w_con,fill = T
        )
      }
      else {
        infoBox(
          title = "Weather condition", icon =icon("asterisk", lib = "glyphicon"),
          color = "teal", value =w_con,fill = T
        )
      }
    }
  })
}
#-------------------------------------------------------------------------------
percip <- function(weatherd){
    renderInfoBox ({
    if(is.null(weatherd)){
      infoBox(
        title = "Draw/load a path for weather data",value = NA,icon = icon("thermometer-empty "),
        color = "olive",fill = T
      )
    } else {
      percip <- sum(subset(weatherd,interval == "6")[4])
      if(percip > 0){
        infoBox(
          title = "Precipitation",paste0(percip,"mm "),icon = icon("tint"),
          color = "aqua",fill = T
        )
      } else {
        infoBox(
          title = "no Precipitation",paste0(0, "mm "), icon = icon("tint"),
          color = "green",fill = T
        )
      }
    }
  })
}
#-------------------------------------------------------------------------------
temp <- function(weatherd){
  renderInfoBox ({
    if(is.null(weatherd)){
      infoBox(
        title = "Draw/load a path for weather data",value = NA,icon = icon("thermometer-empty "),
        color = "olive",fill = T
      )
    } else {
      mtemp <- paste0(weatherd[1,"minTemperature"]," / ",
                      weatherd[1,"maxTemperature"])

      if(weatherd[1,5] < 0){
        infoBox(
          title = "min/max - Cold",value = mtemp,icon = icon("thermometer-empty "),
          color = "teal",fill = T
        )
      }
      else if(weatherd[1,6] > 25){
        infoBox(
          title = "min/max - Hot",value = mtemp,icon = icon("thermometer-three-quarters"),
          color = "red",fill = T
        )
      } else {
        infoBox(
          title = "min/max - Regular",value = mtemp,icon = icon("thermometer-quarter"),
          color = "green",fill = T
        )
      }
    }
  })
}
#-------------------------------------------------------------------------------
windspd <- function(weatherd){
  renderInfoBox ({
    if(is.null(weatherd)){
      infoBox(
        title = "Draw/load a path for weather data",value = NA,icon = icon("thermometer-empty "),
        color = "olive",fill = T
      )
    } else {
      wind <- weatherd[1,"wind_spd"]
      if( wind <= 0){
        infoBox(
          title = "wind spd. m/s",value = wind,icon = icon("flag "),
          color = "yellow", fill = T
        )}
      else {
        infoBox(
          title = "wind spd. m/s",value = wind,icon = icon("flag "),
          color = "orange", fill = T
        )}
    }
    })
  }
#-------------------------------------------------------------------------------
humiditiy <- function(weatherd){
  renderInfoBox ({
    if(is.null(weatherd)){
      infoBox(
        title = "Draw/load a path for weather data",value = NA,icon = icon("thermometer-empty "),
        color = "olive",fill = T
      )
    }
    else {
      hum <- as.numeric(weatherd[1,"humidity"])
      if( hum < 20){
        infoBox(
          title = "humidity %",value = hum,icon = icon("align-justify"),
          color = "yellow",fill = T
        )}
      else if( hum > 20 &  hum < 50){
        infoBox(
          title = "humidity %",value = hum,icon = icon("align-justify"),
          color = "yellow",fill = T
        )}
      else if(hum >= 50 & hum <=70){
        infoBox(
          title = "humidity %",value = hum,icon = icon("align-justify"),
          color = "teal",fill = T
        )}
      else if(hum > 70){
        infoBox(
          title = "humidity %",value = hum,icon = icon("align-justify"),
          color = "teal",fill = T
        )}
    }
  })
}

