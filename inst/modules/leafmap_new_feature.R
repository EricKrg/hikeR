# fetch drawing on leafmap and update stats and weather
leafmap_new_feature <- function(new_draw, draw_all, ...){
  #  print("All Features")
    if (!is.null(new_draw$type)){
      x <- c()
      y <- c()
      print("create new feature")
      for (i in 1:length(draw_all$features[[1]]$geometry$coordinates)){
        x[i] <- draw_all$features[[1]]$geometry$coordinates[[i]][[2]]
        y[i] <- draw_all$features[[1]]$geometry$coordinates[[i]][[1]]
      }
      values <- data.frame(x = y , y = x) #mixed it up
    }
}


