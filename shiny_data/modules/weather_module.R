
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
      w_con <- as.character(weatherd["weather_id"])
      if(w_con %in% c("Sun")){
        valueBox(
          subtitle = "Weather condition",w_con,icon = icon("certificate",lib = "glyphicon"),
          color = "yellow"
        )
      }
      else if(w_con %in% c("LightCloud","PartlyCloud","Cloud")){
        valueBox(
          subtitle = "Weather condition", icon = icon("cloud"),
          color = "light-blue", value = w_con
        )
      }
      else if(w_con %in% c("LightRainSun","LightRainThunderSun","LightRain","Rain","RainThunder","RainSun")){
        valueBox(
          subtitle = "Weather condition", icon = icon("tint"),
          color = "light-blue", value = w_con
        )
      }
      else if(w_con %in% c("Fog")){
        valueBox(
          subtitle = "Weather condition", icon = icon("align-justify "),
          color = "navy", value = w_con
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
        valueBox(
          subtitle = "Weather condition",icon = icon("bolt"),
          color = "red", value = w_con
        )
      }
      else if(w_con %in% c("DrizzleSun","Drizzle")){
        valueBox(
          subtitle = "Weather condition", icon = icon("braille"),
          color = "purple", value = w_con
        )
      }
      else {
        valueBox(
          subtitle = "Weather condition", icon =icon("asterisk", lib = "glyphicon"),
          color = "teal", value =w_con
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
      mtemp <- paste0(weatherd[1,"minTemperature"]," / ",
                      weatherd[1,"maxTemperature"])

      if(weatherd[1,5] < 0){
        valueBox(
          subtitle = "min/max - Cold",value = mtemp,icon = icon("thermometer-empty "),
          color = "teal"
        )
      }
      else if(weatherd[1,6] > 25){
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
