% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module-compile_ui.R
\docType{data}
\name{define_input}
\alias{define_input}
\alias{UPDATE_LEVEL}
\title{Define 'RAVE' module inputs}
\format{
An object of class \code{list} of length 3.
}
\usage{
define_input(
  definition,
  init_args,
  init_expr,
  keyword = "inputId",
  update_level = 2,
  ...
)

UPDATE_LEVEL
}
\arguments{
\item{definition}{R expression; see example or details}

\item{init_args}{arguments to change when data is loaded}

\item{init_expr}{expression to calculate argument values}

\item{keyword}{argument refering to shiny input ID, default is
\code{"inputId"}}

\item{update_level}{update level: 1, 2, or 0; see details.}

\item{...}{passed to other methods}
}
\description{
Used in 'RAVE' module file \code{'comp.R'} to define
input components
}
\details{
\code{definition} is un-evaluated R expression defining shiny inputs
for example \code{textInput('text_id', 'Input:')}. All inputs in the
following packages are supported: 'shiny' , 'dipsaus', 'shinyWidgets'

Because when defining inputs, there is no assumption on data content, all
inputs values/choices should not be hard-coded. For example, before
any subject is loaded, condition types are unknown, hence an input selecting
condition types will need 'choices' to be reloaded once subject is loaded.
\code{init_args} specify which arguments need to be changed once data
is loaded. \code{init_expr} will be evaluated after initialization code
and variables created during this procedure will be used to update input
arguments.

\code{update_level} has three levels: 1 is render only, updating input
only results in outputs to be re-rendered; 2 is run all, not only outputs
will be re-rendered, main function will be executed; 0 is manual input,
meaning the input will remain as ordinary shiny input and one has to
use shiny observer to capture value changes
}
\examples{

\dontrun{

# This code is ran in rave_validate
repo <- loaded_rave_repository(
  'demo/DemoSubject' ,
  reference = 'default',
  epoch = 'auditory_onset',
  before_onset = 1,
  after_onset = 2
)

define_input(

  # No condition is known before data/subject is chosen
  # repo is not available at this time
  definition = shiny::selectInput(inputId = 'cond', label = 'Condition',
                                  choices = character(0), selected = NULL),

  # two arguments will be changed once subject is loaded
  init_args = c('choices', 'selected'),

  init_expr = {
    # at this moment, you know some information about subject
    # for example, repo is created and is available
    choices <- unique(repo$epoch$table$Condition)
    # So does selected
    selected <- raveio::get_val2(repo, 'cond', choices[[1]])
  }

)
}

}
\keyword{datasets}
