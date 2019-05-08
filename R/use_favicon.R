
#' Add a favicon to a shiny app
#'
#' Call `use_favicon` anywhere in your shiny app to add the specified favicon.
#'
#' @param favicon Path (local or URL) to the favicon file.
#'
#' @examples
#'
#' \dontrun{
#' eye_logo <- paste0("https://thegraphicsfairy.com/wp-content/",
#'   "uploads/2013/10/Free-Public-Domain-Watching-Eye-Image-GraphicsFairy.jpg")
#' # Blank page with a favicon
#' shinyApp(ui = fluidPage(use_favicon(eye_logo)),
#'          server = function(input, output) {})
#' }
#' @importFrom htmltools tags
#' @export
use_favicon <- function(favicon) {
  tags$head(
    tags$link(rel = "shortcut icon", type = "image/x-icon", href = favicon),
    tags$link(rel = "shortcut icon", type = "image/png", href = favicon))
}
