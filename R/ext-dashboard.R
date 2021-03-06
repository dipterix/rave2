# Rewrite of shinydashboard page such that control panel is also included
# Also default RAVE icon
appendDependencies = function (x, value) {
  if (inherits(value, "html_dependency"))
    value <- list(value)
  old <- attr(x, "html_dependencies", TRUE)
  htmltools::htmlDependencies(x) <- c(old, value)
  x
}

# Modified addDeps:
addDeps = function (x) {

  if (getOption("shiny.minified", TRUE)) {
    adminLTE_js <- "app.min.js"
    shinydashboard_js <- "shinydashboard.min.js"
    adminLTE_css <- c("AdminLTE.min.css", "_all-skins.min.css")
  }
  else {
    adminLTE_js <- "app.js"
    shinydashboard_js <- "shinydashboard.js"
    adminLTE_css <- c("AdminLTE.css", "_all-skins.css")
  }

  dashboardDeps <- list(
    htmltools::htmlDependency(
      name = "Dipterix", version = as.character(utils::packageVersion(pkg_name)),
      src = c(file = system.file('assets/', package = pkg_name)),
      script = c(
        'dipterix.js',
        'dipterix_inputs.js'
      ),
      stylesheet = 'dipterix.css'
    )
    # # load AdminLTE
    # htmltools::htmlDependency(
    #   "AdminLTE", "2.0.6",
    #   c(file = system.file("AdminLTE", package = "shinydashboard")),
    #   script = adminLTE_js, stylesheet = adminLTE_css)
    # htmltools::htmlDependency(
    #   "shinydashboardPlus", as.character(utils::packageVersion("shinydashboardPlus")),
    #   c(file = system.file("shinydashboardPlus-0.6.0", package = "shinydashboardPlus")),
    #   script = c("js/app.min.js", "js/custom.js"),
    #   stylesheet = c("css/AdminLTE.min.css", "css/_all-skins.min.css")
    # ),
    # htmltools::htmlDependency(
    #   "shinydashboard",
    #   as.character(utils::packageVersion("shinydashboard")),
    #   c(file = system.file(package = "shinydashboard")),
    #   script = shinydashboard_js,
    #   stylesheet = "shinydashboard.css"),
  )
  appendDependencies(x, dashboardDeps)
}

findAttribute = function (x, attr, val) {
  if (is.atomic(x))
    return(FALSE)
  if (!is.null(x$attribs[[attr]])) {
    if (identical(x$attribs[[attr]], val))
      return(TRUE)
    else return(FALSE)
  }
  if (length(x$children) > 0) {
    return(any(unlist(lapply(x$children, findAttribute, attr,val))))
  }
  return(FALSE)
}

rave_dash_page2 <- function(
  header, sidebar, body, rightsidebar = NULL, footer = NULL,
  title = NULL, collapse_sidebar = FALSE,
  sidebar_background = NULL,
  enable_preloader = TRUE, loading_duration = 1, ...
){

  ui <- shinydashboardPlus::dashboardPagePlus(
    header = header,
    sidebar = sidebar,
    body = body,
    rightsidebar = rightsidebar,
    footer = footer,
    title = title,
    collapse_sidebar = collapse_sidebar,
    sidebar_background = sidebar_background,
    sidebar_fullCollapse = TRUE,
    enable_preloader = enable_preloader,
    loading_duration = loading_duration,
    ...
  )

  ui$children <- htmltools::tagAppendChild(shiny::tagList(
    shiny::tags$head(shiny::tags$link(
      rel = "icon", type = "image/x-icon",
      href = dipsaus::to_datauri(system.file('assets/images/favicon.ico', package = pkg_name)))),
    shinyjs::useShinyjs(),
    shinyWidgets::useSweetAlert(),
    dipsaus::use_shiny_dipsaus(),
    shiny::div(
    id = '__rave__mask__',
    class = '',
    shiny::div(
      class = 'loading_info',
      shiny::a(
        href = 'https://openwetware.org/wiki/RAVE',
        target = '_blank',
        shiny::span(
          'R Analysis & Visualization for iEEG Data',
          style = 'color:#444444'
        )
      )
    )
  )), ui$children)

  ui <- addDeps(ui)


}




# to support control panel
rave_dash_header <- function (
  ..., title = NULL, titleWidth = NULL, disable = FALSE,
  btn_text_right = 'Controls', .list = NULL)
{
  items <- .list
  titleWidth <- htmltools::validateCssUnit(titleWidth)
  custom_css <- NULL
  # if (!is.null(titleWidth)) {
  #   custom_css <- shiny::tags$head(shiny::tags$style(shiny::HTML(
  #     gsub("_WIDTH_", titleWidth, fixed = TRUE, "\n      @media (min-width: 768px) {\n        .main-header > .navbar {\n          margin-left: _WIDTH_;\n        }\n        .main-header .logo {\n          width: _WIDTH_;\n        }\n      }\n    "))))
  # }
  shiny::tags$header(
    class = "main-header", custom_css, style = if (disable) "display: none;",
    shiny::tags$nav(
      class = "navbar navbar-fixed-top",
      role = "navigation", shiny::span(shiny::icon("bars"), style = "display:none;"),
      shiny::span(class = "logo", title),
      shiny::div(
        class = 'navbar-collapse pull-left collapse',
        id="navbar-collapse", `aria-expanded`="false",
        shiny::tags$ul(
          class = 'nav navbar-nav',
          shiny::tags$li(
            shiny::a(
              href = "#", class = "nav-item nav-link force-recalculate sidebar-toggle",
              id = "rave_nav_sidebar",
              `data-toggle` = "offcanvas",
              role = "button", shiny::span(class = "sr-only", "Toggle navigation"),
              shiny::icon('th')
              # shiny::span('Switch Dataset')
              # shiny::textOutput('..rave_data_nav..', inline = TRUE),
              # `hover-text` = 'Change Loaded Data'
            )
          ),
          shiny::tags$li(
            # shiny::a(href = "#", class = "nav-item nav-link force-recalculate",
            #          `data-toggle` = "rave-toggle-inputs",
            #          role = "button", shiny::span(class = "sr-only", "Toggle input panel"),
            #          shiny::icon('keyboard-o'), shiny::span('Input Panels')
            # )
            shiny::actionLink("data_select", "Select Data",
                              icon = shiny::icon("tasks"), role = "button", class = "nav-item nav-link")
          ),
          ...
        )
      ),

      shiny::div(
        class = "navbar-custom-menu",
        shiny::tags$ul(
          class = "nav navbar-nav", shiny::tagList( items ),
          shiny::tags$li(
            shiny::a(href = "#", class = "nav-item nav-link force-recalculate",
                     `data-toggle` = "control-sidebar",
                     role = "button",
                     shiny::span(class = "sr-only", "Toggle control"),
                     shiny::span(btn_text_right)
            )
          )
        )
      )
    )
  )
}




rave_dash_control = function (...,
                              disable = FALSE,
                              collapsed = FALSE)
{
  dataValue <- shiny::restoreInput(id = "sidebarCollapsed",
                                   default = collapsed)
  if (disable)
    dataValue <- TRUE
  dataValueString <- if (dataValue)
    "true"
  else
    "false"
  shiny::tagList(
    shiny::tags$aside(
      class = "control-sidebar control-sidebar-dark",
      `data-collapsed` = dataValueString,
      shiny::div(
        class = 'tab-content',
        ...
      )
    ),
    shiny::div(class = "control-sidebar-bg")
  )
}


rave_dash_sidebar <- shinydashboard::dashboardSidebar

rave_dash_body <- shinydashboard::dashboardBody


# allow customized header color and default to full width
box <- function (..., title = NULL, footer = NULL, status = NULL, solidHeader = FALSE,
                 background = NULL, width = 12L, height = NULL, collapsible = FALSE,
                 collapsed = FALSE, headerColor = '#f4f4f4')
{
  boxClass <- "box"
  if (solidHeader || !is.null(background)) {
    boxClass <- paste(boxClass, "box-solid")
  }
  if (!is.null(status)) {
    boxClass <- paste0(boxClass, " box-", status)
  }
  if (collapsible && collapsed) {
    boxClass <- paste(boxClass, "collapsed-box")
  }
  if (!is.null(background)) {
    boxClass <- paste0(boxClass, " bg-", background)
  }
  style <- sprintf('border-top-color: %s; ', headerColor)
  if (!is.null(height)) {
    style <- paste0("height: ", htmltools::validateCssUnit(height))
  }
  titleTag <- NULL
  if (!is.null(title)) {
    titleTag <- shiny::h3(class = "box-title", title)
  }
  collapseTag <- NULL
  if (collapsible) {
    buttonStatus <- status
    buttonStatus %?<-% "default"
    collapseIcon <- if (collapsed)
      "plus"
    else "minus"
    collapseTag <- shiny::div(class = "box-tools pull-right", shiny::tags$button(class = paste0("btn btn-box-tool"),
                                                                                 `data-widget` = "collapse", shiny::icon(collapseIcon)))
  }
  headerTag <- NULL
  if (!is.null(titleTag) || !is.null(collapseTag)) {
    headerTag <- shiny::div(class = "box-header", titleTag, collapseTag, style = sprintf('background-color: %s; ', headerColor))
  }
  shiny::div(class = if (!is.null(width))
    paste0("col-sm-", width), shiny::div(class = boxClass, style = if (!is.null(style))
      style, headerTag, shiny::div(class = "box-body", ...), if (!is.null(footer))
        shiny::div(class = "box-footer", footer)))
}
