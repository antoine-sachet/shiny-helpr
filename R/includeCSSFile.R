
#' Include CSS as a file
#'
#' Similar to `shiny::includeCSS` but inserts the css as a linked file instead
#' of inserting the content directly. This is more convenient to track css
#' properties using a browser's developer tools because the CSS source is more
#' obvious.
#'
#' @param path Local path or URL to the css file to link
#' @importFrom htmltools tags
#' @export
includeCSSFile <- function(path) {
  tags$link(rel = "stylesheet", type = "text/css", href = path)
}
