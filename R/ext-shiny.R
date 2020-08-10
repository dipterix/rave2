
register_js <- function(rmd = FALSE){
  if(requireNamespace('shinyalert', quietly = TRUE)){
    shiny::tagList(
      # shinyalert::useShinyalert(rmd = rmd),
      shinyjs::useShinyjs(rmd = rmd),
      shinyWidgets::useSweetAlert()
    )
  } else {
    shinyjs::useShinyjs()
  }
}

#' Show Alert in Shiny apps
#' @param title,message title and message of the alert
#' @param type options are \code{"warning"}, \code{"error"},
#' \code{"success"}, and \code{"info"}
#' @param onConfirm R function to call upon confirmation
#' @param closeOnEsc,closeOnClickOutside whether to easy close the modal
#' @param showCancelButton whether to show dismiss button
#' @param confirmButtonText text of confirm button
#' @param html whether not to escape the message
#' @param inputId input ID
#' @param session shiny session
#' @param fancy whether try to use fancy version
#' @export
safe_alert <- function(
  title, message, type = c("warning", "error", "success", "info"),
  onConfirm = NULL, inputId = 'shinyalert',
  closeOnEsc = FALSE, closeOnClickOutside = FALSE, html = FALSE,
  showCancelButton = FALSE, confirmButtonText = 'OK',
  session = shiny::getDefaultReactiveDomain(),
  fancy = TRUE
){
  type <- match.arg(type)
  message <- as.character(message)
  if(shiny_is_running()){
    shiny::withReactiveDomain(session, {
      if(fancy && requireNamespace('shinyalert', quietly = TRUE)){

        if(is.function(onConfirm)){
          callbackR <- function(value){
            on.exit(shinyalert::dismissalert(), add = TRUE, after = TRUE)
            onConfirm(value)
          }
        } else {
          callbackR <- NULL
        }

        shinyalert::shinyalert(
          title = title, text = message, closeOnEsc = closeOnEsc,
          closeOnClickOutside = closeOnClickOutside, html = html, type = type,
          showCancelButton = FALSE, callbackR = callbackR, inputId = inputId,
          confirmButtonText = confirmButtonText, confirmButtonCol = "#AEDEF4"
        )
        if(is.function(callbackR)){
          shinyalert::shinyalert(
            title = 'Running',
            text = 'Executing code, please wait. (This message will be dismissed once finished)',
            closeOnClickOutside = FALSE, showConfirmButton = FALSE, closeOnEsc = TRUE
          )
        }
      } else {
        cancelbtn <- NULL
        if(showCancelButton){
          cancelbtn <- shiny::modalButton('Cancel')
        }
        confirmButtonText
        shiny::showModal(shiny::modalDialog(
          title = title,
          ifelse(isTRUE(html), shiny::HTML(message), message),
          size = 's', easyClose = closeOnClickOutside,
          footer = shiny::tagList(
            dipsaus::actionButtonStyled(session$ns(inputId), confirmButtonText),
            cancelbtn
          )
        ))
        shiny::observeEvent(session$input[[inputId]], {
          dipsaus::updateActionButtonStyled(session, inputId, disabled = TRUE)
          if(is.function(onConfirm)){
            try(onConfirm())
          }
          shiny::removeModal()
        }, once = TRUE, ignoreInit = TRUE, ignoreNULL = TRUE)
      }
    })
  } else {
    fname = list(
      "warning" = 'rave_warn',
      "error" = 'rave_error',
      "success" = 'rave_info',
      "info" = 'rave_debug'
    )[[type]]
    do.call(fname, list(sprintf('%s: %s', title, message)))
  }
  return(invisible(NULL))
}




#' Wrappers to be compatible with Input ID
#' @param inputId character, passed to \code{define_input}
#' @param label label character
#' @param expr expression returning 'html' code
#' @param ... passed to other methods
#' @return \code{plainUI} returns evaluated \code{expr}; \code{plainLabel}
#' returns a label-only input
#' @name customized-ui
NULL

#' @rdname customized-ui
#' @export
plainUI <- function(inputId, expr){
  expr
}

#' @rdname customized-ui
#' @export
plainLabel <- function(inputId, label, ...){
  shiny::div(
    class = "form-group shiny-input-container",
    shiny::tags$label(label, class = "control-label", `for` = inputId),
    shiny::tags$input(id = inputId, type = "text", class = "form-control",
                      style = 'display:none!important;',
                      value = '', placeholder = ''),
    ...
  )
}

SHINY_UPDATE_FUNCTIONS = dipsaus::fastmap2()
SHINY_UPDATE_FUNCTIONS$shiny = list(
  'selectInput' = 'shiny::updateSelectInput'
)
SHINY_UPDATE_FUNCTIONS$dipsaus = list(
  'actionButtonStyled' = 'dipsaus::updateActionButtonStyled',
  'compoundInput2' = 'dipsaus::updateCompoundInput2'
)
SHINY_UPDATE_FUNCTIONS[[pkg_name]] = list(
  'plainLabel' = 'shiny::updateTextInput',
  'plainUI' = 'dipsaus::do_nothing'
)

SHINY_UPDATE_FUNCTIONS$shinyWidgets <- list(
  'actionBttn' = 'shiny::updateActionButton',
  'checkboxGroupButtons' = 'shinyWidgets::updateCheckboxGroupButtons',
  'awesomeCheckboxGroup' = 'shinyWidgets::updateAwesomeCheckboxGroup',
  'prettyCheckboxGroup' = 'shinyWidgets::updatePrettyCheckboxGroup',
  'radioGroupButtons' = 'shinyWidgets::updateRadioGroupButtons',
  'awesomeRadio' = 'shinyWidgets::updateAwesomeRadio',
  'prettyRadioButtons' = 'shinyWidgets::updatePrettyRadioButtons',
  'pickerInput' = 'shinyWidgets::updatePickerInput',
  'sliderTextInput' = 'shinyWidgets::updateSliderTextInput',
  'colorSelectorDrop' = 'dipsaus::do_nothing',
  'multiInput' = 'shinyWidgets::updateMultiInput',
  'spectrumInput' = 'shinyWidgets::updateSpectrumInput',
  'verticalTabsetPanel' = 'shinyWidgets::updateVerticalTabsetPanel'
)

SHINY_UPDATE_FUNCTIONS$default = c(
  SHINY_UPDATE_FUNCTIONS$shiny,
  SHINY_UPDATE_FUNCTIONS$dipsaus,
  SHINY_UPDATE_FUNCTIONS$rave,
  SHINY_UPDATE_FUNCTIONS[[pkg_name]],
  SHINY_UPDATE_FUNCTIONS$threeBrain,
  SHINY_UPDATE_FUNCTIONS$shinyWidgets
)

guess_shiny_update <- function(call, parse = TRUE){
  # call <- quote(shiny::textInput('asda'))

  funname = call[[1]]
  pkgname = NA

  if(!is.name(funname)){
    # check if first is :: or :::
    if(identical(funname[[1]], quote(`::`)) || identical(funname[[1]], quote(`:::`))){
      pkgname = as.character(funname[[2]])
      # print(pkgname)
      funname = funname[[3]]
    }
  }
  if(!is.name(funname)){
    rave_error('Cannot find shiny update function for function {sQuote(funname)}')
    return(NULL)
  }

  funname <- as.character(funname)
  if(is.na(pkgname)){
    pkgname = 'default'
  }
  update_str = SHINY_UPDATE_FUNCTIONS[[pkgname]][[funname]]

  if(is.null(update_str)){
    update_str = paste0('update', stringr::str_to_upper(stringr::str_sub(funname, end = 1)), stringr::str_sub(funname, start = 2))
    if(pkgname != 'default'){
      fun <- asNamespace(pkgname)[[update_str]]
      if(!is.function(fun)){
        rave_error("Cannot find shiny update function for function {sQuote(funname)}")
      }
      update_str = sprintf('%s:::%s', pkgname, update_str)
    }
  }

  if(parse){
    eval(parse(text = update_str))
  } else{
    update_str
  }
}




SHINY_OUTPUT_FUNCTIONS = dipsaus::fastmap2()
SHINY_OUTPUT_FUNCTIONS$shiny = list(
  'htmlOutput' = 'shiny::renderUI',
  'verbatimTextOutput' = 'shiny::renderPrint',
  'plotOutput' = 'shiny::renderPlot'
)
SHINY_OUTPUT_FUNCTIONS$threeBrain = list(
  'threejsBrainOutput' = 'threeBrain::renderBrain'
)
SHINY_OUTPUT_FUNCTIONS[[pkg_name]] = list(
  'customizedUI' = 'shiny::renderUI'
)

SHINY_OUTPUT_FUNCTIONS$rave = list(
  'customizedUI' = 'shiny::renderUI'
)
SHINY_OUTPUT_FUNCTIONS[[pkg_name]] = SHINY_OUTPUT_FUNCTIONS$rave
SHINY_OUTPUT_FUNCTIONS$DT = list(
  'DTOutput' = 'DT::renderDT',
  'dataTableOutput' = 'DT::renderDataTable'
)
SHINY_OUTPUT_FUNCTIONS$default = c(
  SHINY_OUTPUT_FUNCTIONS$shiny,
  SHINY_OUTPUT_FUNCTIONS$dipsaus,
  SHINY_OUTPUT_FUNCTIONS$rave,
  SHINY_OUTPUT_FUNCTIONS[[pkg_name]],
  SHINY_OUTPUT_FUNCTIONS$threeBrain
)


guess_shiny_output <- function(call, parse = TRUE){
  # call <- quote(shiny::verbatimTextOutput('asd'))

  funname = call[[1]]
  pkgname = NA

  if(!is.name(funname)){
    # check if first is :: or :::
    if(identical(funname[[1]], quote(`::`)) || identical(funname[[1]], quote(`:::`))){
      pkgname = as.character(funname[[2]])
      # print(pkgname)
      funname = funname[[3]]
    }
  }
  if(!is.name(funname)){
    rave_error('Cannot find shiny output function for call {sQuote(funname)}')
    return(NULL)
  }

  funname <- as.character(funname)
  if(is.na(pkgname)){
    pkgname = 'default'
  }
  update_str = SHINY_OUTPUT_FUNCTIONS[[pkgname]][[funname]]

  if(is.null(update_str)){
    update_str = paste0('render', stringr::str_to_upper(stringr::str_sub(funname, end = 1)), stringr::str_sub(funname, start = 2))
    update_str = stringr::str_remove(update_str, '[oO]utput$')
    if(pkgname != 'default'){
      fun <- asNamespace(pkgname)[[update_str]]
      if(!is.function(fun)){
        rave_error("Cannot find shiny output function for name {sQuote(funname)}")
      }
      update_str = sprintf('%s:::%s', pkgname, update_str)
    }
  }

  if(parse){
    eval(parse(text = update_str))
  } else{
    update_str
  }
}










# creating observers, but keep track of the handlers
make_observe <- function(map, error_handler = NULL, on_invalidate = NULL){
  stopifnot(inherits(map, 'fastmap2'))

  function(x, env = parent.frame(), quoted = FALSE, ..., label = rand_string(10)){
    if(!quoted){ x = substitute(x) }
    x = rlang::quo_squash(rlang::quo({
      tryCatch({ !!x }, error = function(e){
        # TODO: Signal STOP command to session
        local({
          error_handler <- !!error_handler
          if(is.function(error_handler)){
            error_handler(e)
          } else {
            print(e$call)
            rave_debug("Event expression with error raised")
            cat(!!deparse(x), sep = '\n')
            rave_error("[Module ERROR] {e$message}")
          }
        })
      })
    }))
    if(!length(label) || is.na(label)){
      label = rand_string(11)
    }
    call <- as.call(list(
      quote(shiny::observe),
      x = x, env = env, quoted = FALSE, ...,
      label = label
    ))
    map[[label]] <- local({eval(call)})
    if(is.function(on_invalidate)){
      map[[label]]$onInvalidate(on_invalidate)
    }
    invisible(map[[label]])
  }

}


make_observeEvent <- function(map, error_handler = NULL, on_invalidate = NULL){
  stopifnot(inherits(map, 'fastmap2'))


  function(eventExpr, handlerExpr,
           event.env = parent.frame(), handler.env = parent.frame(),
           event.quoted = FALSE, handler.quoted = FALSE, ..., ignoreInit = TRUE, label = rand_string(12)){
    if( !event.quoted ){ eventExpr = substitute(eventExpr) }
    if( !handler.quoted ){ handlerExpr = substitute(handlerExpr) }
    eventExpr = rlang::quo_squash(rlang::quo({
      tryCatch({ !!eventExpr }, error = function(e){
        local({
          error_handler <- !!error_handler
          if(is.function(error_handler)){
            error_handler(e)
          } else {
            rave_debug("Event expression with error raised")
            print(e$call)
            cat(!!deparse(eventExpr), sep = '\n')
            rave_error("[Module ERROR] {e$message}")
          }
        })
      })
    }))
    handlerExpr = rlang::quo_squash(rlang::quo({
      tryCatch({ !!handlerExpr }, error = function(e){

        local({
          error_handler <- !!error_handler
          if(is.function(error_handler)){
            error_handler(e)
          } else {
            rave_debug("Event expression with error raised")
            print(e$call)
            cat(!!deparse(handlerExpr), sep = '\n')
            rave_error("[Module ERROR] {e$message}")
          }
        })
      })
    }))
    if(!length(label) || is.na(label)){
      label = rand_string(13)
    }
    call <- as.call(list(
      quote(shiny::observeEvent),
      eventExpr = eventExpr, handlerExpr = handlerExpr,
      event.quoted = FALSE, handler.quoted = FALSE,
      event.env = event.env, handler.env = handler.env,
      ignoreInit = ignoreInit, ..., label = label
    ))
    map[[label]] <- local({ eval(call) })
    if(is.function(on_invalidate)){
      map[[label]]$onInvalidate(on_invalidate)
    }

    invisible(map[[label]])
  }

}


remove_observers <- function(map){
  stopifnot(inherits(map, 'fastmap2'))

  for(nm in names(map)){
    try({
      map[[nm]]$suspend()
      map[[nm]]$destroy()
      .subset2(map, 'remove')(nm)
    }, silent = TRUE)
  }

}


#' Safe way to show shiny notifications
#' @description Show notification when shiny is running, and show console
#' messages when shiny is offline
#' @param ... messages to display
#' @param type message type, choices are "message", "warning", "error",
#' "default"
#' @param duration seconds for notification to stay
#' @param closeBotton,action,id,session passed to
#' \code{\link[shiny]{showNotification}}
#' @export
module_notification <- function(
  ..., type = c("message", "warning", "error", "default"),
  duration = 10, closeBotton = TRUE, action = NULL, id,
  session = shiny::getDefaultReactiveDomain()){

  type = match.arg(type)
  context = from_rave_context('context')
  msg <- paste(..., sep = '')

  if(context == 'rave_running'){
    if(missing(id)){
      id = paste0('..rave-notification-', from_rave_context('module_id'))
    }
    shiny::showNotification(ui = shiny::p(...), action = action, duration = duration,
                            closeButton = closeBotton, id = id, type = type, session = session)
    msg <- stringr::str_remove_all(msg, '<[^<>]+>')
  } else {
    level = list(
      "message" = "Info",
      "warning" = 'Warning',
      "error" = 'Error',
      "default" = 'Debug'
    )[[type]]

    msg <- stringr::str_remove_all(msg, '<[^<>]+>')

    with_rave_handlers({
      rave_condition(
        msg,
        .envir = environment(),
        class = sprintf('rave%s', level),
        call = NULL,
        immediate. = TRUE,
        style = stringr::str_to_lower(level)
      )
    })
    # catgl(..., level = level)
  }
  invisible(msg)
}

#' Safe way to remove shiny notification within module
#' @param id,... passed to \code{\link[shiny]{removeNotification}}
#' @export
module_remove_notification <- function(id, ...){
  context = from_rave_context('context')
  if(context == 'rave_running'){
    if(missing(id)){
      id = paste0('..rave-notification-', from_rave_context('module_id'))
    }
    shiny::removeNotification(id, ...)
  }
}
