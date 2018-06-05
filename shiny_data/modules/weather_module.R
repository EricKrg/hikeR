
library(shiny)
library(shinyWidgets)
library(shinydashboard)



weather_mod <- function(weatherd){
    renderValueBox ({
    if(is.null(weatherd)){
      valueBox(
        subtitle = "Draw/load a path for weather data",value = NA,icon = icon("thermometer-empty "),
        color = "olive"
      )
    } else {
      if(weatherd[7] %in% c("Sun")){
        valueBox(
          subtitle = "Weather condition",weatherd[4,7],icon = icon("certificate",lib = "glyphicon"),
          color = "yellow"
        )
      }
      else if(weatherd[7] %in% c("LightCloud","PartlyCloud","Cloud")){
        valueBox(
          subtitle = "Weather condition", icon = icon("cloud"),
          color = "light-blue", value = weatherd[7]
        )
      }
      else if(weatherd[7] %in% c("LightRainSun","LightRainThunderSun","LightRain","Rain","RainThunder","RainSun")){
        valueBox(
          subtitle = "Weather condition", icon = icon("tint"),
          color = "light-blue", value = weatherd[7]
        )
      }
      else if(weatherd[7] %in% c("Fog")){
        valueBox(
          subtitle = "Weather condition", icon = icon("align-justify "),
          color = "navy", value = weatherd[7]
        )
      }
      else if(weatherd[7] %in% c("SleetThunder",
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
        valueBox(
          subtitle = "Weather condition",icon = icon("bolt"),
          color = "red", value = weatherd[7]
        )
      }
      else if(weatherd[7] %in% c("DrizzleSun","Drizzle")){
        valueBox(
          subtitle = "Weather condition", icon = icon("braille"),
          color = "purple", value = weatherd[7]
        )
      }
      else {
        valueBox(
          subtitle = "Weather condition", icon =icon("asterisk", lib = "glyphicon"),
          color = "teal", value =weatherd[7]
        )
      }
    }
  })
}
#-------------------------------------------------------------------------------
percip <- function(weatherd){
    renderValueBox ({
    if(is.null(weatherd)){
      valueBox(
        subtitle = "Draw/load a path for weather data",value = NA,icon = icon("thermometer-empty "),
        color = "olive"
      )
    } else {
      percip <- sum(subset(weatherd,interval == "6")[4])
      if(percip > 0){
        valueBox(
          subtitle = "Precipitation",paste0(percip,"mm "),icon = icon("tint"),
          color = "aqua"
        )
      } else {
        valueBox(
          subtitle = "no Precipitation",paste0(0, "mm "),
          color = "green"
        )
      }
    }
  })
}
#-------------------------------------------------------------------------------
temp <- function(weatherd){
  renderValueBox ({
    if(is.null(weatherd)){
      valueBox(
        subtitle = "Draw/load a path for weather data",value = NA,icon = icon("thermometer-empty "),
        color = "olive"
      )
    } else {
      mtemp <- paste0(weatherd["minTemperature"]," / ",weatherd["maxTemperature"])
      if(weatherd[5] < 0){
        valueBox(
          subtitle = "min/max - Cold",value = mtemp,icon = icon("thermometer-empty "),
          color = "teal"
        )
      }
      else if(weatherd[6] > 25){
        valueBox(
          subtitle = "min/max - Hot",value = mtemp,icon = icon("thermometer-three-quarters"),
          color = "red"
        )
      } else {
        valueBox(
          subtitle = "min/max - Regular",value = mtemp,icon = icon("thermometer-quarter"),
          color = "lime"
        )
      }
    }
  })
}
