#'
#' renders a progressbar ui element
#'
#' @export
hike_progressBar <- function (id, value, total = NULL, display_pct = FALSE, size = NULL,
                          status = NULL, striped = FALSE, title = NULL)
{
  if (is.null(total)) {
    percent <- value
  }
  else {
    percent <- value
  }
  if (!is.null(title) | !is.null(total)) {
    title <- htmltools::tags$span(class = "progress-text",
                                  title, htmltools::HTML("&nbsp;"))

  }
  if (is.null(total)) {
    total <- htmltools::tags$span(class = "progress-number",
                                  htmltools::tags$b(value, id = paste0(id, "-value")))

  }
  tagPB <- htmltools::tags$div(class = "progress-group", title,
                               total, htmltools::tags$div(class = if (!is.null(size))
                                 paste("progress", size)
                                 else "progress", htmltools::tags$div(id = id, style = if (percent >
                                                                                           0)
                                   paste0("width:",percent, "%;"), style = if (display_pct)
                                     "min-width: 2em;", class = "progress-bar", class = if (!is.null(status))
                                       paste0("progress-bar-", status), class = if (striped)
                                         "progress-bar-striped", role = "progressbar", if (display_pct)
                                           paste0(percent, "%"))))

}

