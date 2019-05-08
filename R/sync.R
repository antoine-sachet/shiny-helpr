
#' Keeps shiny inputs in sync (e.g. across tabs)
#'
#' @param reactives_to_sync Reactives that should always have the same value
#'  Typically outputs from the UI to sync.
#' @param input_ids Unique ids of the inputs to sync. Used in the call to
#'  `update_function`
#' @param update_function Function to call to update the value
#' @param update_arg Name of the argument to update in the update_function
#' @param ... Passed to the update function. Typically, `session` is often
#'  required.
#'
#' @details Be aware that a `uiOutput` is only rendered when visible. This means
#'  that an input inside a `renderUI` does not exist until the corresponding
#'  `uiOutput` has been shown! This can be delayed if the `uiOutput` is in a
#'  separate tab, or in a collapsed box, or for some reason not visible when the app starts.
#'
#'   If such an input is used to sync, then when the synced input suddenly "appears",
#'   it will reset all the synced inputs to the initial value.
#'
#'   To avoid that, you can force the UI to render as soon as the app starts by calling
#'   ` outputOptions(output, ui_id, suspendWhenHidden = FALSE)` in the server function.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' make_selectInput <- function(id) {
#'   selectInput(id, label = id, choices = 1:10)
#' }
#'
#' server <- function(input, output, session) {
#'   sel1 <- reactive(input$First)
#'   sel2 <- reactive(input$Second)
#'   sel3 <- reactive(input$Third)
#'
#'   sync(
#'     reactives_to_sync = list(sel1, sel2, sel3),
#'     input_ids = c("First", "Second", "Third"),
#'     update_function = updateSelectInput,
#'     update_arg = "selected",
#'     session = session
#'   )
#' }
#'
#' shinyApp(
#'   ui = fluidPage(
#'     make_selectInput("First"),
#'     make_selectInput("Second"),
#'     make_selectInput("Third")
#'   ),
#'   server = server
#' )
#'
#' # Also works across tabs.
#' shinyApp(
#'   ui = navbarPage(
#'     "Sync demo",
#'     tabPanel("tab1", make_selectInput("First")),
#'     tabPanel("tab2", make_selectInput("Second")),
#'     tabPanel("tab3", make_selectInput("Third"))
#'   ),
#'   server = server
#' )
#' }
#' @importFrom shiny reactiveValues observeEvent
#' @importFrom rlang list2
#' @importFrom purrr walk2
sync <- function(reactives_to_sync, input_ids, update_function, update_arg = "selected", ...) {
  global <- reactiveValues(
    latest_updated = NULL,
    current_value = NULL
  )

  dots <- rlang::list2(...)

  walk2(reactives_to_sync, input_ids, function(reac, id) {
    observeEvent(reac(), {
      if (!is.null(reac())) {
        if (is.null(global$current_value) || reac() != global$current_value) {
          global$latest_updated <- id
          global$current_value <- reac()
        }
      }
    }, ignoreInit = T, ignoreNULL = T)
  })

  shiny::observeEvent(global$current_value, {
    walk2(reactives_to_sync, input_ids, function(reac, id) {
      if (!is.null(reac())) {
        if (is.null(global$current_value) || reac() != global$current_value) {
          # print(glue("Updating {id} to {global$current_value} (from {global$latest_updated})"))
          args <- rlang::list2(inputId = id, !!!dots)
          args[[update_arg]] <- global$current_value
          do.call(update_function, args)
        }
      }
    })
  }, ignoreInit = T, ignoreNULL = T)
}
