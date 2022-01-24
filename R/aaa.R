#' @importFrom shidashi render
NULL

rand_string <- function (length = 20) {
  paste(sample(c(letters, LETTERS, 0:9), length, replace = TRUE),
        collapse = "")
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
`%OF%` <- function(lhs, rhs){
  if(length(rhs)){ de <- rhs[[1]] } else { de <- rhs }
  lhs <- lhs[!is.na(lhs)]
  if(!length(lhs)){ return(de) }
  sel <- lhs %in% rhs
  if(any(sel)){ return(lhs[sel][[1]]) }
  return(de)
}

