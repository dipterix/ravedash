% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core-app.R
\name{rave-session}
\alias{rave-session}
\alias{new_session}
\alias{use_session}
\alias{launch_session}
\alias{session_getopt}
\alias{session_setopt}
\alias{remove_session}
\alias{remove_all_sessions}
\alias{list_session}
\alias{start_session}
\alias{shutdown_session}
\alias{session_log}
\title{Create, register, list, and remove 'RAVE' sessions}
\usage{
new_session(update = FALSE, app_root = NULL)

use_session(x, ...)

launch_session(
  x,
  host = "127.0.0.1",
  port = NULL,
  modules = NULL,
  dry_run = FALSE,
  options = list(jupyter = TRUE, jupyter_port = NULL, as_job = TRUE, launch_browser =
    TRUE, single_session = FALSE, page_title = NULL, sidebar_open = TRUE)
)

session_getopt(keys, default = NA, namespace = "default")

session_setopt(..., .list = NULL, namespace = "default")

remove_session(x)

remove_all_sessions()

list_session(path = session_root(), order = c("none", "ascend", "descend"))

start_session(
  session,
  new = NA,
  modules = NULL,
  page_title = NULL,
  sidebar_open = TRUE,
  host = "127.0.0.1",
  port = NULL,
  jupyter = NA,
  jupyter_port = NULL,
  as_job = TRUE,
  launch_browser = TRUE,
  single_session = FALSE,
  app_root = NULL,
  dry_run = FALSE
)

shutdown_session(
  returnValue = invisible(NULL),
  jupyter = TRUE,
  session = shiny::getDefaultReactiveDomain()
)

session_log(x, max_lines = 200, modules = NULL)
}
\arguments{
\item{update}{logical, whether to update to latest 'RAVE' template}

\item{..., .list}{named list of key-value pairs of session options. The
keys must be characters, and values must be simple data types (such as
numeric vectors, characters)}

\item{host}{host 'IP' address, default is 'localhost'}

\item{port}{port to listen}

\item{modules}{selected module ID to launch; used to only show a subset of
modules; default is \code{NULL} (select all modules); hidden modules are
always selected}

\item{dry_run}{whether to dry-run (do not launch) the 'RAVE' session}

\item{options}{additional options, including \code{jupyter},
\code{jupyter_port}, \code{as_job}, and \code{launch_browser}}

\item{keys}{vector of characters, one or more keys of which the values
should be obtained}

\item{default}{default value if key is missing}

\item{namespace}{namespace of the option; default is \code{'default'}}

\item{path, app_root}{root path to store the sessions; default is the
\code{"tensor_temp_path"} in \code{\link[raveio]{raveio_getopt}}}

\item{order}{whether to order the session by date created; choices are
\code{'none'} (default), \code{'ascend'}, \code{'descend'}}

\item{session, x}{session identification string, or session object; use
\code{list_session} to list all existing sessions}

\item{new}{whether to create a new session instead of using the most recent
one, default is false}

\item{page_title}{session web page title and logo text; can have length
of either one (page title and logo text are the same); or length of two,
with page title be the first element and logo text be the second.}

\item{sidebar_open}{whether to open the side-bar by default; default \code{TRUE}
when more than one module is to be displayed}

\item{jupyter}{logical, whether to launch 'jupyter' instances when starting
'RAVE' sessions, or to stop the 'jupyter' instances when shutting down. It
requires additional setups to enable 'jupyter' lab; see 'Installation Guide
Step 3' in the 'RAVE' wiki page.}

\item{jupyter_port}{port used by 'jupyter' lab, can be set by
\code{'jupyter_port'} option in \code{\link[raveio]{raveio_setopt}}}

\item{as_job}{whether to launch the application as 'RStudio' job, default is
true if 'RStudio' is detected; when running without 'RStudio', this option
is always false}

\item{launch_browser}{whether to launch browser, default is true}

\item{single_session}{whether to enable single-session mode. Under this
mode, closing the main frame will terminate 'RAVE' run-time session,
otherwise the 'RAVE' instance will still open in the background}

\item{returnValue}{passed to \code{\link[shiny]{stopApp}}}

\item{max_lines}{maximum number of log entries to return; default is 200}
}
\value{
\describe{
\item{\code{new_session}}{returns a session object with character
\code{'session_id'} and a function \code{'launch_session'} to launch the
application from this session}
\item{\code{use_session}}{returns a session object, the same as
\code{new_session} under the condition that corresponding session exists,
or raise an error if the session is missing}
\item{\code{list_session}}{returns a list of all existing session objects
under the session root}
\item{\code{remove_session}}{returns a logical whether the corresponding
session has been found and removed}
}
}
\description{
Create, register, list, and remove 'RAVE' sessions
}
\examples{

if(interactive()){

  sess <- new_session()
  sess$launch_session()

  all_sessions <- list_session()
  print(all_sessions)

  # Use existing session
  session_id <- all_sessions[[1]]$session_id
  sess <- use_session(session_id)
  sess$launch_session()

  # Remove session
  remove_session(session_id)
  list_session()
}

}
