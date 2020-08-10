#' @importFrom dipsaus %?<-%
#' @importFrom dipsaus do_nothing
#' @importFrom dipsaus shiny_is_running
#' @importFrom dipsaus add_to_session
#' @importFrom dipsaus clear_env
#' @import raveio
#' @import ravebase
NULL

# might change name in the future
pkg_name <- 'rave2'

#' @export
ravebase::rave_context

# --------------------------- Utility functions ------------------------


stopifnot2 <- function(..., msg = 'Condition not satisfied'){
  if(!all(c(...))){
    rave_fatal(msg)
  }
}

`%within%` <- function(a, b){
  (a >= min(b)) & (a <= max(b))
}

rand_string <- function(length = 50){
  paste(sample(c(letters, LETTERS, 0:9), length, replace = TRUE), collapse = '')
}

compare_rave_version <- function(ver, strict = FALSE){
  if(length(ver) != 1){
    return(FALSE)
  }
  rave_ver <- utils::packageVersion(pkg_name)
  compare <- utils::compareVersion(as.character(ver), as.character(rave_ver))
  if(compare > 0){ return(TRUE) }
  if(compare == 0 && !strict){ return(TRUE) }
  return(FALSE)
}



# --------------------------- Dev-use ----------------------------------
soft_deprecated <- function(){
  env = parent.frame()
  call = do.call(match.call, envir = env, args = list())
  rave_warn('Function {sQuote(call[[1]])} is soft-Deprecated in call \n{deparse(call)}')
}

hard_deprecated <- function(){
  env = parent.frame()
  call = do.call(match.call, envir = env, args = list())
  rave_fatal('Function {sQuote(call[[1]])} is hard-Deprecated in call {deparse(call)}')
}


# --------------------------------- Misc -------------------------------


rave_options <- ravebase:::rave_options
