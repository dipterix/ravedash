#' @importFrom shidashi render
#' @importFrom dipsaus %?<-%
#' @importFrom dipsaus %OF%
#' @importFrom shinyvalidate InputValidator
NULL

gray_label_color <- "#c8c9ca"

ansi_regex <- "(?:(?:\\x{001b}\\[)|\\x{009b})(?:(?:[0-9]{1,3})?(?:(?:;[0-9]{0,3})*)?[A-M|f-m])|\\x{001b}[A-M]"

strip_style <- function(x, ...) {
  gsub(ansi_regex, "", x, perl = TRUE, useBytes = TRUE)
}

stopifnot2 <- function (..., msg = "Condition not satisfied") {
  if (!all(c(...))) {
    stop(msg)
  }
}

rand_string <- function (length = 20) {
  paste(sample(c(letters, LETTERS, 0:9), length, replace = TRUE),
        collapse = "")
}

deparse1 <- function (expr, collapse = " ") {
  paste(deparse(expr), collapse = collapse)
}

R_user_dir <- function (package, which = c("data", "config", "cache"))
{
  stopifnot(is.character(package), length(package) == 1L)
  which <- match.arg(which)
  home <- normalizePath("~")
  path <- switch(which, data = {
    p <- Sys.getenv("R_USER_DATA_DIR")
    if (!nzchar(p)) {
      p <- Sys.getenv("XDG_DATA_HOME")
      if (!nzchar(p)) {
        if (.Platform$OS.type == "windows") {
          p <- file.path(Sys.getenv("APPDATA"), "R",
                         "data")
        } else if (Sys.info()["sysname"] == "Darwin") {
          p <- file.path(home, "Library", "Application Support",
                         "org.R-project.R")
        } else {
          p <- file.path(home, ".local", "share")
        }
      }
    }
    p
  }, config = {
    p <- Sys.getenv("R_USER_CONFIG_DIR")
    if (!nzchar(p)) {
      p <- Sys.getenv("R_USER_CONFIG_DIR")
      if (!nzchar(p)) {
        p <- Sys.getenv("XDG_CONFIG_HOME")
        if (!nzchar(p)) {
          if (.Platform$OS.type == "windows") {
            p <- file.path(Sys.getenv("APPDATA"), "R",
                           "config")
          } else if (Sys.info()["sysname"] == "Darwin") {
            p <- file.path(home, "Library", "Preferences",
                           "org.R-project.R")
          } else {
            p <- file.path(home, ".config")
          }
        }
      }
    }
    p
  }, cache = {
    p <- Sys.getenv("R_USER_CACHE_DIR")
    if (!nzchar(p)) {
      p <- Sys.getenv("XDG_CACHE_HOME")
      if (!nzchar(p)) {
        if (.Platform$OS.type == "windows") {
          p <- file.path(Sys.getenv("LOCALAPPDATA"),
                         "R", "cache")
        } else if (Sys.info()["sysname"] == "Darwin") {
          p <- file.path(home, "Library", "Caches", "org.R-project.R")
        } else {
          p <- file.path(home, ".cache")
        }
      }
    }
    p
  })
  file.path(path, "R", package)
}


#' @export
dipsaus::`%OF%`

#' @importFrom dipsaus shiny_alert2
#' @export
dipsaus::shiny_alert2

#' @importFrom dipsaus close_alert2
#' @export
dipsaus::close_alert2

#' @importFrom shidashi show_notification
#' @export
shidashi::show_notification

#' @importFrom shidashi clear_notifications
#' @export
shidashi::clear_notifications

#' @name random-text
#' @title Randomly choose a text from a list of strings
#' @param candidates character vectors, a list of candidates
#' @return \code{be_patient_text} returns a text asking users to be patient;
#' \code{finished_text} returns the text indicating the task has finished.
#' @examples
#'
#' be_patient_text()
#'
#' finished_text()
#'
#' @export
be_patient_text <- function(candidates){
  if(missing(candidates)) {
    sp <- c(
      "Please be patient, running in progress...",
      "Grab a cup of coffee, this might take a while...",
      "Grab a cup of coffee, this might take a while... (Can I get the decaf one? Thanks)",
      "Time to stand and stretch yourself...",
      "Wait a second (maybe minutes)",
      "I hate it when I have to let you wait, but I have no choice...",
      "Do you see the progress bar? If so, stare at it.",
      "Do you see the progress bar? Yeah, I know, it's pretty addictive to stare at it ;)",
      "It's time to watch some videos, or listen to some music. I'll be ready when it's ready.",
      "The last time when you ran this module is at last time. (Alright, I'll go back to work..."
    )
  } else {
    sp <- candidates
  }

  prob <- rev(seq_along(sp))
  sample(
    sp,
    size = 1,
    prob = prob / sum(prob)
  )
}

#' @rdname random-text
#' @export
finished_text <- function(candidates){
  if(missing(candidates)) {
    sp <- c(
      "Please proceed to the next step",
      "It's done!",
      "Yay, finished!",
      "Finally!",
      "See, I said I can finish it!",
      "Oh, finally...",
      "Have you finished your coffee? I have finished my work haha.",
      "I guess you are happy with the progress bar disappearing? ;)",
      "Time to get back to work!",
      "The last time when you finished this module is just now!"
    )
  } else {
    sp <- candidates
  }

  prob <- rev(seq_along(sp))
  sample(
    sp,
    size = 1,
    prob = prob / sum(prob)
  )
}
