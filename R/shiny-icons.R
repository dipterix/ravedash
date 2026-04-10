# icons
#' @title Shiny icons
#' @details The goal of create this list is to keep 'shiny' icons (which are
#' essentially 'font-awesome' icons) up-to-date.
#' @export
shiny_icons <- structure(list(), class = "ravedash_shiny_icons")

.shiny_icons_methods <- local({
  li <- NULL
  dep <- NULL

  ensure_li <- function() {
    if (is.null(li)) {
      li <<- list(
        brain = "brain",
        bars = "bars",
        grid = "th",
        keyboard = "keyboard",
        help = "question-circle",
        sync = "sync",
        expand = "expand",
        tasks = "tasks",
        angle_right = "angle-right",
        angle_left = "angle-left",
        angle_up = "angle-up",
        angle_down = "angle-down",
        angle_double_right = "angle-double-right",
        angle_double_left = "angle-double-left",
        angle_double_up = "angle-double-up",
        angle_double_down = "angle-double-down",
        arrow_right = "arrow-right",
        arrow_left = "arrow-left",
        arrow_up = "arrow-up",
        arrow_down = "arrow-down",
        external_link = "external-link-alt",
        plus = "plus",
        minus = "minus",
        download = "download",
        save = "save",
        trash = "trash",
        export = "file-export",
        puzzle = "puzzle-piece",
        user_md = "user-md",
        image = "file-image",
        magic = "magic",
        check = "check",
        simplybuilt = "simplybuilt",
        table = "table",
        times = "times",
        code = "code",
        terminal = "terminal",
        filter = "filter",
        tools = "tools",
        wrench = "wrench"
      )
    }
    if (is.null(dep)) {
      dep <<- htmltools::htmlDependency(
        name = "fontawesome-free-ravedash",
        version = "5.15.4",
        package = "ravedash",
        src = c(file = "assets"),
        stylesheet = "css/all.min.css",
        all_files = TRUE
      )
    }
    li
  }

  get_icon <- function(name, class = NULL) {
    ensure_li()
    re <- li[[name]]
    if (is.null(re)) {
      re <- name
    }
    # class <- shidashi::combine_html_class(sprintf("fa-%s", re), class)
    # if(is.null(class) || !grepl("fa[sb]{0,1}", class)) {
    #   class <- shidashi::combine_html_class("fa", class)
    # }
    # re <- htmltools::tags$li(
    #   class = class,
    #   role = "presentation",
    #   `aria-label` = sprintf("%s icon", re),
    #   dep
    # )
    re <- shiny::icon(
      re,
      class = class,
      verify_fa = FALSE,
      html_dependency = dep
    )
    attr(re, "browsable_html") <- TRUE
    re
  }

  get_name <- function() {
    ensure_li()
    names(li)
  }

  set_name <- function(name, icon) {
    if (name %in% get_name()) {
      stop(
        "Icon with name `",
        name,
        "` has been registered. Please consider other names"
      )
    }
    if (!is.character(icon)) {
      stop("`set_name` icon must be characters")
    }
    li[[name]] <<- icon
  }

  list(
    get_icon = get_icon,
    get_name = get_name,
    set_name = set_name,
    ensure_li = ensure_li
  )

})

#' @export
names.ravedash_shiny_icons <- function(x) {
  .shiny_icons_methods$get_name()
}

#' @export
`$.ravedash_shiny_icons` <- function(x, name) {
  .shiny_icons_methods$get_icon(name)
}

#' @export
`[[.ravedash_shiny_icons` <- `$.ravedash_shiny_icons`

#' @export
`[.ravedash_shiny_icons` <- function(x, i, ...) {
  .shiny_icons_methods$get_icon(i, shidashi::combine_html_class(c(...)))
}


#' @export
`$<-.ravedash_shiny_icons` <- function(x, name, value) {
  stop("Cannot set shiny_icons")
}

#' @export
`[[<-.ravedash_shiny_icons` <- `$<-.ravedash_shiny_icons`

#' @export
`[<-.ravedash_shiny_icons` <- `$<-.ravedash_shiny_icons`
