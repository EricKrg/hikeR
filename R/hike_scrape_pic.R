#'
#'scrape wikipedia pictures by search term
#'
#' @param  searchterm - string searchterm
#' @param  maxItems- nr of max. pic. you want to look for, but it will stop if there is no more result
#' @examples
#' hikeR::hike_scrape_pic("Grace Hopper")
#' @export
hike_scrape_pic <- function(searchTerm,maxItems){
  run = T
  while(run){
    url <- paste0("https://de.wikipedia.org/wiki/",searchTerm)
    doc <- paste0(readr::read_lines(url), collapse="\n")
    result <- stringr::str_extract_all(string = as.character(doc),
                                       pattern = "\\/\\/upload.wikimedia.org\\/wikipedia\\/commons\\/thumb\\/.*?jpg.*?[jpg]{3}")
    if(nchar(doc) < 1){
      url <- paste0("https://en.wikipedia.org/wiki/",searchTerm)
      doc <- paste0(readr::read_lines(url), collapse="\n")
      result <- stringr::str_extract_all(string = as.character(doc),
                                         pattern = "\\/\\/upload.wikimedia.org\\/wikipedia\\/commons\\/thumb\\/.*?jpg.*?[jpg]{3}")
    }
    else{
      run = F
    }
  }
  j = 1
  result_list <- c()
  n = 1
  for(i in 1:maxItems){
    if(is.na(result[[1]][j])){
      break
    } else {
    result_list[n] <- paste0("https:", result[[1]][j])
    j = j + 4
    n = n + 1
    }
  }
  return(result_list)
}
