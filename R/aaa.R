#' @importFrom shidashi render
#' @importFrom dipsaus %?<-%
#' @importFrom dipsaus %OF%
#' @importFrom shinyvalidate InputValidator
#' @importFrom R6 R6Class
#' @importFrom checkmate assert
NULL

gray_label_color <- "#c8c9ca"

ansi_regex <- "(?:(?:\\x{001b}\\[)|\\x{009b})(?:(?:[0-9]{1,3})?(?:(?:;[0-9]{0,3})*)?[A-M|f-m])|\\x{001b}[A-M]"


DEFAULT_PALETTES <- local({

  LINE <- list(
    'Beautiful Field' = c("orange", "dodgerblue3", "darkgreen", "orangered", "brown",  "purple3"),
    'J5' = c("#407899","#deba6f", "#65743a", "#de6449", "#4B296B"),
    'Okabe-Ito' = c("black", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                    "#D55E00", "#CC79A7", "gray60"),
    'Okabe-Ito 2' = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                      "#D55E00", "#CC79A7", "gray60"),
    'Black+tan' = c('#1A1A19', '#A25D34', '#353634', '#D0A380'),
    'Accent' = c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0','#f0027f','#bf5b17','#656565'),
    'Dark2' = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#a6761d','#656565'),
    'R4' = c("black", "#DF536B", "#61D04F", "#2297E6", "#28E2E5", "#CD0BBC", "#F5C710", "gray62"),
    'R4Permed' = c("#2297E6", "#F5C710", "#61D04F", "#DF536B", "#CD0BBC"),
    'Paired' = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00'),
    'Pastel1' = c('#fbb4ae','#b3cde3','#ccebc5','#decbe4','#fed9a6','#ffffcc','#e5d8bd','#fddaec'),
    'Pastel2' = c('#b3e2cd','#fdcdac','#cbd5e8','#f4cae4','#e6f5c9','#fff2ae','#f1e2cc','#cccccc'),
    'Set1' = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf'),
    'Set2' = c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854','#ffd92f','#e5c494','#b3b3b3'),
    'Set3' = c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5')
  )

  HEATMAP <- list(
    BlueWhiteRed = rev(c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#ffffff",
                         "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")),
    BlueGrayRed = rev(c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#b4b4b4",
                        "#92c5de", "#4393c3", "#2166ac", "#053061")),
    Spectral = rev(c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf',
                     '#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2')),
    BrownWhiteGreen = c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5',
                        '#c7eae5','#80cdc1','#35978f','#01665e','#003c30'),
    PinkWhiteGreen =c('#8e0152','#c51b7d','#de77ae','#f1b6da','#fde0ef','#f7f7f7',
                      '#e6f5d0','#b8e186','#7fbc41','#4d9221','#276419'),
    PurpleWhiteGreen = c('#40004b','#762a83','#9970ab','#c2a5cf','#e7d4e8','#f7f7f7',
                         '#d9f0d3','#a6dba0','#5aae61','#1b7837','#00441b'),
    OrangeWhitePurple = c('#7f3b08','#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7',
                          '#d8daeb','#b2abd2','#8073ac','#542788','#2d004b'),
    BlackWhiteRed = rev(c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','#ffffff',
                          '#e0e0e0','#bababa','#878787','#4d4d4d','#1a1a1a')),
    BlueYellowRed = rev(c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf',
                          '#e0f3f8','#abd9e9','#74add1','#4575b4','#313695')),
    GreenYellowRed = rev(c('#a50026','#d73027','#f46d43','#fdae61','#fee08b','#ffffbf',
                           '#d9ef8b','#a6d96a','#66bd63','#1a9850','#006837'))
  )


  set <- function(type = c("line", "heatmap"), name, col) {
    stopifnot2(length(col) >= 3, msg = "length(col) must >= 3 to set a palette")
    type <- match.arg(type)
    if(type == "line") {
      LINE[[ name ]] <<- col
    } else {
      HEATMAP[[ name ]] <<- col
    }
    invisible(name)
  }

  get <- function(name, type = c("line", "heatmap"), get_palettes = FALSE, get_palette_names = FALSE) {
    type <- match.arg(type)

    if(type == "line") {
      pals <- LINE
    } else {
      pals <- HEATMAP
    }

    if(missing(name)) {
      if( get_palette_names ) {
        return (names(pals))
      }
      return (pals)
    }

    if(!length(name)) {
      logger("Invalid palette requested: [{deparse1(name)}]. Returning the first palette", level = "warning", use_glue = TRUE)
      pal <- pals[[1]]
    } else {
      pal <- pals[[ name ]]
    }

    attr(pal, "name") <- name
    return (pal)
  }

  return(list(
    set = set,
    get = get
  ))

})

DEFAULT_GRAPHICS <- list(
  rave_cex.main = 1.5,
  rave_cex.axis = 1.3,
  # putting this to 1.4 because 1.5 causes some clipping of the axis(2) label, we could also try to increase
  # the (outer) left margin to compensate
  rave_cex.lab = 1.4,
  rave_axis_tcl = -0.3,
  plot_time_range = c(-Inf,Inf),
  draw_decorator_labels = FALSE,
  plot_title_options = c('Subject ID', 'Electrode #', 'Condition', 'Frequency Range',
                         'Sample Size', 'Baseline Window', 'Analysis Window'),

  # no shiny, shiny running, all(mfrow == 2), any(mfrow > 2)
  cex_multiplier = c(1, 1/0.88, 1/0.66),

  ## this is now managed through ravedash theme
  background_plot_color_hint = 'white',
  champions_tunic = '#009edd',

  invert_colors_in_palette = FALSE,
  reverse_colors_in_palette = FALSE,

  analysis_window.shade.color = 'gray70',
  analysis_window.stroke.color = 'match',

  heatmap_number_color_values = 101,
  invert_colors_in_heatmap_palette = FALSE,
  reverse_colors_in_heatmap_palette = FALSE,

  show_outliers_on_plots = TRUE,

  log_scale = FALSE,
  max_zlim = 0,
  percentile_range = TRUE,
  sort_trials_by_type = 'Trial Number',

  max_columns_in_figure = 3
)



strip_style <- function(x, ...) {
  gsub(ansi_regex, "", x, perl = TRUE, useBytes = TRUE)
}

stopifnot2 <- function (..., msg = "Condition not satisfied") {
  if (!all(c(...))) {
    stop(msg)
  }
}

shiny_validation_error <- function(message, ..., error_class = NULL) {
  e <- simpleError(message, ...)
  cls <- unique(c("shiny.silent.error", error_class, "validation", class(e)))
  class(e) <- cls
  logger("Validation failed (not an error): ", message, level = "warning")
  stop(e)
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

get_function_from <- function(name, package) {
  asNamespace(package)[[name]]
}

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
