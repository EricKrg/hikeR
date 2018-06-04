library(plotly)


plot_air <- function(elevp, values, twoD){
  renderPlotly({
    withProgress(message = 'Creating plot, be patient', value = 0.1, {
      if(is.null(values)){
        p <- ggplot()
      }
      else if(twoD){
        coords <- st_coordinates(elevp)
        elevp$x <- coords[,1]
        elevp$y <- coords[,2]
        p <- plot_ly(elevp, x = ~distance, y = ~elev,
                     type = 'area', mode = 'lines',color = ~elev,
                     text = ~paste0(round(elev,digits = 2), "m"),hoverinfo = "text") %>%
          add_trace(hoverinfo = 'none')
      }
      else {
        a <- Sys.time()
        coords <- st_coordinates(elevp)
        elevp$x <- coords[,1]
        elevp$y <- coords[,2]
        incProgress(0.5)
        p <- plot_ly(elevp, x = ~x, y =  ~y, z = ~elev,
                     type = 'scatter3d', mode = 'lines',color = ~elev, source = "routed",
                     text = ~paste0(round(elev,digits = 2), "m"),hoverinfo = "text") %>%
          add_trace(hoverinfo = 'none')
        incProgress(0.3)
      }
      return(p)
    })
  })
}
#-------------------------------------------------------------------------------
plot_route <- function(elevp, route, twoD){
  renderPlotly({
    withProgress(message = 'Creating plot, be patient', value = 0.1, {
      if(is.null(route)){
        print("empty route")
        p <- ggplot()
      }
      else if(twoD){
        coords <- st_coordinates(elevp)
        elevp$x <- coords[,1]
        elevp$y <- coords[,2]
        p <- plot_ly(elevp, x = ~distance, y = ~elev,
                     type = 'area', mode = 'lines',color = ~elev,
                     text = ~paste0(round(elev,digits = 2), "m"),hoverinfo = "text") %>%
          add_trace(hoverinfo = 'none')
      }
      else {
        a <- Sys.time()
        coords <- st_coordinates(elevp)
        elevp$x <- coords[,1]
        elevp$y <- coords[,2]
        incProgress(0.5)
        p <- plot_ly(elevp, x = ~x, y = ~y, z = ~elev,
                     type = 'scatter3d', mode = 'lines',color = ~elev, source = "routed",
                     text = ~paste0(round(elev,digits = 2), "m"),hoverinfo = "text") %>%
          add_trace(hoverinfo = 'none')
        print(Sys.time()-a)
        print("3d route")
        incProgress(0.3)
      }
      return(p)
    })
  }) #routed
}
