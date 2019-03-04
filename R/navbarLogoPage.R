#' Create a page with a top level navigation bar with logo and favicon.
#'
#' Create a page *with a logo and favicon* that contains a top level navigation bar that can be used to
#' toggle a set of \code{\link{tabPanel}} elements. Optionally includes a dropdown with shinyproxy actions.
#'
#' The key differences with a standard shiny::navbarPage are:
#'
#' * Logo: mandatory image displayed next to the navbar title, path provided via the `logo` argument.
#' * Favicon: optional, path provided via the `favicon` argument.
#' * Shinyproxy dropdown: optional, displayed only if `shinyproxy` is `TRUE` (or `"auto"` and a shinyproxy environment is detected)
#' * `collapsible` is `TRUE` by default
#'
#' @param title The title to display in the navbar
#' @param logo Path (or URL) to logo file
#' @param favicon Path (relative or absolute) to favicon file. URLs not supported.
#' @param shinyproxy Whether the app is running embedded in shinyproxy.
#'   If TRUE, the logo acts as a link to the app list, a "sign out" button is
#'   added to the top right with the user's email displayed next to it.
#'   Defaults to "auto" which detects whether SHINYPROXY_USERNAME is set.
#' @param ... \code{\link{tabPanel}} elements to include in the page. The
#'   \code{navbarMenu} function also accepts strings, which will be used as menu
#'   section headers. If the string is a set of dashes like \code{"----"} a
#'   horizontal separator will be displayed in the menu.
#' @param id If provided, you can use \code{input$}\emph{\code{id}} in your
#'   server logic to determine which of the current tabs is active. The value
#'   will correspond to the \code{value} argument that is passed to
#'   \code{\link{tabPanel}}.
#' @param selected The \code{value} (or, if none was supplied, the \code{title})
#'   of the tab that should be selected by default. If \code{NULL}, the first
#'   tab will be selected.
#' @param position Determines whether the navbar should be displayed at the top
#'   of the page with normal scrolling behavior (\code{"static-top"}), pinned at
#'   the top (\code{"fixed-top"}), or pinned at the bottom
#'   (\code{"fixed-bottom"}). Note that using \code{"fixed-top"} or
#'   \code{"fixed-bottom"} will cause the navbar to overlay your body content,
#'   unless you add padding, e.g.: \code{tags$style(type="text/css", "body
#'   {padding-top: 70px;}")}
#' @param header Tag or list of tags to display as a common header above all
#'   tabPanels.
#' @param footer Tag or list of tags to display as a common footer below all
#'   tabPanels
#' @param inverse \code{TRUE} to use a dark background and light text for the
#'   navigation bar
#' @param collapsible \code{TRUE} to automatically collapse the navigation
#'   elements into a menu when the width of the browser is less than 940 pixels
#'   (useful for viewing on smaller touchscreen device)
#' @param fluid \code{TRUE} to use a fluid layout. \code{FALSE} to use a fixed
#'   layout.
#' @param theme Alternative Bootstrap stylesheet (normally a css file within the
#'   www directory). For example, to use the theme located at
#'   \code{www/bootstrap.css} you would use \code{theme = "bootstrap.css"}.
#' @param windowTitle The title that should be displayed by the browser window.
#'   Useful if \code{title} is not a string.
#' @return A UI defintion that can be passed to the \link{shinyUI} function.
#'
#' @details The \code{navbarMenu} function can be used to create an embedded
#'   menu within the navbar that in turns includes additional tabPanels (see
#'   example below).
#'
#' @seealso \code{\link{navbarPage}}, \code{\link{tabPanel}}, \code{\link{tabsetPanel}},
#'   \code{\link{updateNavbarPage}}, \code{\link{insertTab}},
#'   \code{\link{showTab}}
#'
#' @examples
#' eye_logo <- "https://thegraphicsfairy.com/wp-content/uploads/2013/10/Free-Public-Domain-Watching-Eye-Image-GraphicsFairy.jpg"
#' navbarLogoPage(
#'   title = "The all-seeing eye",
#'   logo = eye_logo,
#'   favicon = eye_logo,
#'   windowTitle = "Totally not a sect",
#'   tabPanel("What it sees", tags$h3("Everything.")),
#'   tabPanel("What it doesn't see", tags$h1("The all-seing eye sees ALL."))
#' )
#' @export
navbarLogoPage <- function(title,
                           logo,
                           ...,
                           id = NULL,
                           selected = NULL,
                           favicon = NULL,
                           shinyproxy = "auto",
                           position = c("fixed-top", "static-top", "fixed-bottom"),
                           header = NULL,
                           footer = NULL,
                           inverse = FALSE,
                           collapsible = TRUE,
                           fluid = TRUE,
                           theme = NULL,
                           windowTitle = title) {

  # alias title so we can avoid conflicts w/ title in withTags
  pageTitle <- title

  # Adding favicon in the head tag
  if (!is.null(favicon)) {
    favicon_header <- tags$head(
      # Adding multiple favicons for wider browser support
      tags$link(rel = "shortcut icon", type = "image/x-icon", href = favicon),
      tags$link(rel = "shortcut icon", type = "image/png", href = favicon)
    )
    header <- tagList(favicon_header, header)
  }

  # navbar class based on options
  navbarClass <- "navbar navbar-default"
  position <- match.arg(position)
  if (!is.null(position))
    navbarClass <- paste(navbarClass, " navbar-", position, sep = "")
  if (grepl(pattern = "fixed", position, fixed = T))
    header <- tagList(header, tags$style(type = "text/css", "body {padding-top: 70px;}"))
  if (inverse)
    navbarClass <- paste(navbarClass, "navbar-inverse")

  if (!is.null(id))
    selected <- restoreInput(id = id, default = selected)

  # build the tabset
  tabs <- list(...)
  tabset <- buildTabset(tabs, "nav navbar-nav", NULL, id, selected)

  # function to return plain or fluid class name
  className <- function(name) {
    if (fluid)
      paste(name, "-fluid", sep = "")
    else
      name
  }

  # build the container div dynamically to support optional collapsibility
  navId <- paste("navbar-collapse-", p_randomInt(1000, 10000), sep = "")
  # Used only if collapsible is TRUE
  collapse_button <- tags$button(class = "navbar-toggle collapsed btn btn-primary navbar-btn",
                                 `data-toggle` = "collapse",
                                 `data-target` = paste0("#", navId),
                                 icon("bars"))

  if (collapsible) {
    navtabs <- span(class = "navbar-header navbar-collapse collapse",
                    id = navId, tabset$navList)
  } else {
    navtabs <- tabset$navList
  }


  if (shinyproxy == "auto") {
    # Detect if container is running embedded in shinyproxy
    shinyproxy <- Sys.getenv("SHINYPROXY_USERNAME") != ""
  }

  # right part of the navbar, where the shinyproxy stuff is
  navbar_header_right <- function() {

    if (!shinyproxy) {
      if (!collapsible)
        return(NULL)
      else
        return(div(class = "navbar-header navbar-right", collapse_button))
    }

    optional <- list()
    usergroups <- strsplit(Sys.getenv("SHINYPROXY_USERGROUPS"),
                           split = ",", fixed = TRUE)[[1]]
    if ("shiny-admin" %in% usergroups) {
      optional$admin_li <-
        tags$li(a(href = "/admin", target = "_top",
                  title = "Admin dashboard", icon("tachometer"), "Admin"))
    }

    dropdown_class <- "dropdown"
    if (collapsible) dropdown_class <- paste(dropdown_class, "pull-right")

    # putting it together
    out <- tags$div(class = "navbar-header navbar-right",
                    tags$span(class = dropdown_class,
                              tags$button(href = "#", `data-toggle` = "dropdown",
                                          class = "btn btn-primary navbar-btn dropdown-toggle",
                                          icon("user"),
                                          span(Sys.getenv("SHINYPROXY_USERNAME"),
                                               class = "visible-lg-inline"),
                                          span(class = "caret")),
                              tags$ul(class = "dropdown-menu",
                                      tags$li(a(href = "/", target = "_top",
                                                title = "Home",
                                                icon("home"), "Home")),
                                      optional$admin_li,
                                      tags$li(a(href = "/logout", target = "_top",
                                                title = "Sign out",
                                                icon("sign-out"), "Sign out"))
                              )
                    )
    )
    # Adding collapse button if collapsible
    if (collapsible) out <- tagAppendChild(out, tags$span(class = "pull-right", collapse_button))
    out
  }

  navbar_css <- system.file("extdata", "www/navbarLogoPage.css", package = "shinyhelpr")

  # Left part of the header
  navbar_header_left <- function() {
    navbar_header_left_class <- "navbar-header navbar-left"
    if (collapsible)
      navbar_header_left_class <- paste(navbar_header_left_class, "pull-left")
    div(class = navbar_header_left_class,
        img(src = logo, class = "navbar-brand"),
        span(class = "navbar-brand hidden-sm hidden-xs", pageTitle),
        includeCSS(navbar_css)
    )
  }

  # Putting it all together. Unused elements are set to NULL.
  containerDiv <- div(class = className("container"),
                      navbar_header_left(),
                      navtabs,
                      navbar_header_right()
  )


  # build the main tab content div
  contentDiv <- div(class = className("container"))
  if (!is.null(header))
    contentDiv <- tagAppendChild(contentDiv, div(class = "row", header))
  contentDiv <- tagAppendChild(contentDiv, tabset$content)
  if (!is.null(footer))
    contentDiv <- tagAppendChild(contentDiv, div(class = "row", footer))

  # build the page
  bootstrapPage(
    title = windowTitle,
    theme = theme,
    tags$nav(class = navbarClass, role = "navigation", containerDiv),
    contentDiv
  )
}


# Helper functions straight from shiny ------------------------------------

# Used to get the global random seed
.globals <- shiny:::.globals

# This function is called internally by navbarPage, tabsetPanel
# and navlistPanel
buildTabset <- function(tabs, ulClass, textFilter = NULL, id = NULL,
                        selected = NULL, foundSelected = FALSE) {

  res <- findAndMarkSelectedTab(tabs, selected, foundSelected)
  tabs <- res$tabs
  foundSelected <- res$foundSelected

  # add input class if we have an id
  if (!is.null(id)) ulClass <- paste(ulClass, "shiny-tab-input")

  if (anyNamed(tabs)) {
    nms <- names(tabs)
    nms <- nms[nzchar(nms)]
    stop("Tabs should all be unnamed arguments, but some are named: ",
         paste(nms, collapse = ", "))
  }

  tabsetId <- p_randomInt(1000, 10000)
  tabs <- lapply(seq_len(length(tabs)), buildTabItem,
                 tabsetId = tabsetId, foundSelected = foundSelected,
                 tabs = tabs, textFilter = textFilter)

  tabNavList <- tags$ul(class = ulClass, id = id,
                        `data-tabsetid` = tabsetId, lapply(tabs, "[[", 1))

  tabContent <- tags$div(class = "tab-content",
                         `data-tabsetid` = tabsetId, lapply(tabs, "[[", 2))

  list(navList = tabNavList, content = tabContent)
}

# Builds tabPanel/navbarMenu items (this function used to be
# declared inside the buildTabset() function and it's been
# refactored for clarity and reusability). Called internally
# by buildTabset.
buildTabItem <- function(index, tabsetId, foundSelected, tabs = NULL,
                         divTag = NULL, textFilter = NULL) {

  divTag <- if (!is.null(divTag)) divTag else tabs[[index]]

  if (is.character(divTag) && !is.null(textFilter)) {
    # text item: pass it to the textFilter if it exists
    liTag <- textFilter(divTag)
    divTag <- NULL

  } else if (inherits(divTag, "shiny.navbarmenu")) {
    # navbarMenu item: build the child tabset
    tabset <- buildTabset(divTag$tabs, "dropdown-menu",
                          navbarMenuTextFilter, foundSelected = foundSelected)

    # if this navbarMenu contains a selected item, mark it active
    containsSelected <- containsSelectedTab(divTag$tabs)
    liTag <- tags$li(
      class = paste0("dropdown", if (containsSelected) " active"),
      tags$a(href = "#",
             class = "dropdown-toggle", `data-toggle` = "dropdown",
             `data-value` = divTag$menuName,
             getIcon(iconClass = divTag$iconClass),
             divTag$title, tags$b(class = "caret")
      ),
      tabset$navList   # inner tabPanels items
    )
    # list of tab content divs from the child tabset
    divTag <- tabset$content$children

  } else {
    # tabPanel item: create the tab's liTag and divTag
    tabId <- paste("tab", tabsetId, index, sep = "-")
    liTag <- tags$li(
      tags$a(
        href = paste("#", tabId, sep = ""),
        `data-toggle` = "tab",
        `data-value` = divTag$attribs$`data-value`,
        getIcon(iconClass = divTag$attribs$`data-icon-class`),
        divTag$attribs$title
      )
    )
    # if this tabPanel is selected item, mark it active
    if (isTabSelected(divTag)) {
      liTag$attribs$class <- "active"
      divTag$attribs$class <- "tab-pane active"
    }
    divTag$attribs$id <- tabId
    divTag$attribs$title <- NULL
  }
  return(list(liTag = liTag, divTag = divTag))
}

# Helpers to build tabsetPanels (& Co.) and their elements
markTabAsSelected <- function(x) {
  attr(x, "selected") <- TRUE
  x
}

isTabSelected <- function(x) {
  isTRUE(attr(x, "selected", exact = TRUE))
}

containsSelectedTab <- function(tabs) {
  any(vapply(tabs, isTabSelected, logical(1)))
}

findAndMarkSelectedTab <- function(tabs, selected, foundSelected) {
  tabs <- lapply(tabs, function(div) {
    if (foundSelected || is.character(div)) {
      # Strings are not selectable items

    } else if (inherits(div, "shiny.navbarmenu")) {
      # Recur for navbarMenus
      res <- findAndMarkSelectedTab(div$tabs, selected, foundSelected)
      div$tabs <- res$tabs
      foundSelected <<- res$foundSelected

    } else {
      # Base case: regular tab item. If the `selected` argument is
      # provided, check for a match in the existing tabs; else,
      # mark first available item as selected
      if (is.null(selected)) {
        foundSelected <<- TRUE
        div <- markTabAsSelected(div)
      } else {
        tabValue <- div$attribs$`data-value` %OR% div$attribs$title
        if (identical(selected, tabValue)) {
          foundSelected <<- TRUE
          div <- markTabAsSelected(div)
        }
      }
    }
    return(div)
  })
  return(list(tabs = tabs, foundSelected = foundSelected))
}

# Returns the icon object (or NULL if none), provided either a
# tabPanel, OR the icon class
getIcon <- function(tab = NULL, iconClass = NULL) {
  if (!is.null(tab)) iconClass <- tab$attribs$`data-icon-class`
  if (!is.null(iconClass)) {
    if (grepl("fa-", iconClass, fixed = TRUE)) {
      # for font-awesome we specify fixed-width
      iconClass <- paste(iconClass, "fa-fw")
    }
    icon(name = NULL, class = iconClass)
  } else NULL
}

anyNamed <- function(x) {
  if (length(x) == 0)
    return(FALSE)
  nms <- names(x)
  if (is.null(nms))
    return(FALSE)
  any(nzchar(nms))
}

randomInt <- function(min, max) {
  if (missing(max)) {
    max <- min
    min <- 0
  }
  if (min < 0 || max <= min)
    stop("Invalid min/max values")
  min + sample(max - min, 1) - 1
}

p_randomInt <- function(...) {

  withPrivateSeed(randomInt(...))
}

withPrivateSeed <- function(expr) {
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    hasOrigSeed <- TRUE
    origSeed <- .GlobalEnv$.Random.seed
  }
  else {
    hasOrigSeed <- FALSE
  }
  if (is.null(.globals$ownSeed)) {
    if (hasOrigSeed) {
      rm(.Random.seed, envir = .GlobalEnv, inherits = FALSE)
    }
  }
  else {
    .GlobalEnv$.Random.seed <- .globals$ownSeed
  }
  on.exit({
    .globals$ownSeed <- .GlobalEnv$.Random.seed
    if (hasOrigSeed) {
      .GlobalEnv$.Random.seed <- origSeed
    } else {
      rm(.Random.seed, envir = .GlobalEnv, inherits = FALSE)
    }
    httpuv::getRNGState()
  })
  expr
}
