#' @name register_output
#' @title Register output and output options
#' @description Enable advanced output gadgets such as expanding the output
#' in another browser window, or downloading the rendered data.
#' @param outputId output ID in the scope of current shiny session
#' @param session shiny session instance
#' @param output_opts,.opt output options
#' @param extras extra information to store
#' @param render_function shiny render function
#' @param output_type type of export file formats supported, options are
#' \code{'image'} (for figures, default), \code{'data'},
#' \code{'threeBrain'} (for 'RAVE' 3D viewers), and
#' \code{'no-download'} (do not export).
#' @param download_fileformat download file format, supports 'glue'
#' @param extensions a list of file extensions and their descriptions;
#' the names will be used to display the modal selectors, and values are the
#' underlying extension read by \code{download_fileformat}
#' @param title,cancel_btn,confirm_btn,... title, button labels, and additional
#' 'HTML' elements that are to be shown in the modal
#' @param download_function core function that writes the data into the files;
#' default is set for \code{'image'} and \code{'threeBrain'} automatically;
#' see 'Default' and 'Examples'.
#' @param quoted whether \code{render_function} is quoted; default is false
#' @returns Registered output or output options.
#' @details
#'
#' The following steps are done when \code{register_output} is called:
#'
#' * Register the render function to shiny output \code{output[[outputId]]}
#' * Register the render information to session which can be retrieved via \code{get_output}
#' * Register (if \code{download_function} is a function) a download handler that listen to the shiny event. The event ID is \code{paste0(outputId, '__download2')}.
#'
#' When downloading event is triggered, a modal will pop up asking for exporting
#' format (always exists) and image dimensions (if output type is \code{'image'})
#' or title (when output type is \code{'threeBrain'}). Users will choose the
#' proper inputs, which will be passed into \code{download_function}.
#'
#' The file \code{extensions} is a named list. Its names are printable
#' descriptions of the formats, and values are the file extensions (without the
#' leading \code{'.'}). for example, \code{list("compressed CSV" = "csv")}.
#' Users will see \code{"compressed CSV"} in the format selector, and
#' \code{download_function} sees \code{"csv"}.
#'
#' When output type is image, users will be asked to enter the image size in
#' inches; default width is \code{7}, and height is calculated based on
#' current image aspect ratio.
#'
#' If you would like to show more on the modal, pass 'HTML' elements to \code{...}
#'
#' Function \code{download_function} is a function containing four inputs:
#'
#' * \code{con}: file where the data should be written into
#' * \code{params}: a named list of \code{params$extension} (file extension), \code{width}, \code{height} (type is image), or \code{title} (3D viewer)
#' * \code{render_expr} a quoted rendering expression of the rendering function
#' * \code{render_env} the rendering environment of the rendering function.
#'
#' Default \code{download_function} is provided when not specified.
#'
#' @examples
#'
#'
#' if(interactive()) {
#'
#' library(shiny)
#' library(ravedash)
#'
#' # ---- Use this in RAVE --------
#'
#' # UI
#' output_gadget_container(
#'   plotOutput("plot", brush = shiny::brushOpts("plot__brush")),
#' )
#'
#' # server
#' server <- function(input, output, session) {
#'   register_output(
#'     renderPlot({
#'       # ... plot it
#'     }),
#'     outputId = "plot",
#'     output_opts = list(brush = shiny::brushOpts("plot__brush"))
#'   )
#' }
#'
#'
#' # ---- Low-level method ------------
#'
#' rave_id <- paste(sample(c(letters, LETTERS, 0:9), 20, replace = TRUE),
#'                  collapse = "")
#'
#' ui <- function(req) {
#'   query_string <- req$QUERY_STRING
#'   if(length(query_string) != 1) {
#'     query_string <- "/"
#'   }
#'   query_result <- httr::parse_url(query_string)
#'
#'   if(!identical(toupper(query_result$query$standalone), "TRUE")) {
#'     # normal page
#'     basicPage(
#'       output_gadget_container(
#'         plotOutput("plot", brush = shiny::brushOpts("plot__brush")),
#'       )
#'     )
#'   } else {
#'     # standalone viewer
#'     uiOutput("viewer")
#'   }
#' }
#'
#' server <- function(input, output, session) {
#'
#'   bindEvent(
#'     safe_observe({
#'       query_string <- session$clientData$url_search
#'       query_result <- httr::parse_url(query_string)
#'
#'       if(!identical(toupper(query_result$query$module), "standalone_viewer")) {
#'         # normal page
#'         register_rave_session(session = session, .rave_id = rave_id)
#'         register_output(
#'           renderPlot({
#'             input$btn
#'             plot(rnorm(100), pch = 16)
#'           }),
#'           outputId = "plot",
#'           output_opts = list(brush = shiny::brushOpts("plot__brush"))
#'         )
#'       } else {
#'         # standalone viewer
#'         standalone_viewer(outputId = "plot", rave_id = rave_id)
#'       }
#'     }),
#'     session$clientData$url_search
#'   )
#'
#'
#' }
#'
#' shinyApp(ui, server, options = list(port = 8989))
#' }
#'
NULL

#' @rdname register_output
#' @export
register_output_options <- function(
    outputId, ..., .opt = list(), extras = list(),
    session = shiny::getDefaultReactiveDomain()) {
  reactive_handlers <- get_default_handlers(session)
  if(!"output_options" %in% names(reactive_handlers)) {
    reactive_handlers$output_options <- dipsaus::fastmap2()
  }
  output_options <- reactive_handlers$output_options
  re <- as.list(output_options[[session$ns(outputId)]])
  re$args <- c(list(...), .opt)
  if(!inherits(re$extras, "fastmap2")) {
    re$extras <- dipsaus::fastmap2()
  }
  dipsaus::list_to_fastmap2(as.list(extras), re$extras)
  output_options[[session$ns(outputId)]] <- re
  invisible(re)
}

#' @rdname register_output
#' @export
get_output_options <- function(outputId, session = shiny::getDefaultReactiveDomain()) {
  reactive_handlers <- get_default_handlers(session = session)
  output_options <- reactive_handlers$output_options
  if(!is.list(output_options)) {
    return(list())
  }
  as.list(output_options[[session$ns(outputId)]])
}


# register_output <- function(
#     render_function, outputId,
#     export_type = c("none", "custom", "pdf", "csv", "3dviewer", "htmlwidget"),
#     export_settings = list(), quoted = FALSE,
#     output_opts = list(),
#     session = shiny::getDefaultReactiveDomain(), ...) {
#
#   export_type <- match.arg(export_type)
#   fenv <- new.env(parent = parent.frame())
#
#   # this is root session
#   opt <- get_output_options(outputId, session = session)
#   extras <- opt$extras
#   if(!is.list(extras) || !inherits(extras, "fastmap2")) {
#     extras <- list()
#   }
#
#   for(nm in names(output_opts)) {
#     if(nchar(nm)) {
#       opt$args[[nm]] <- output_opts[[nm]]
#     } else {
#       stop("register_output: `output_opts` must be a named list")
#     }
#   }
#
#   # no file downloader is considered
#   # obtain the expression
#   find_expr <- function(call) {
#     if(!is.call(call)) { return() }
#     call <- match.call(
#       definition = eval(call[[1]], envir = fenv),
#       call = call, expand.dots = TRUE)
#     call_list <- as.list(call)
#     argnames <- names(call_list)
#     if("expr" %in% argnames) {
#       expr <- call[['expr']]
#       env <- eval(call[['env']], envir = fenv)
#       if(!is.environment(env)) {
#         env <- eval(call[['envir']], envir = fenv)
#       }
#       if(!is.environment(env)) {
#         env <- fenv
#       }
#       return(list(
#         expr = expr,
#         env = env
#       ))
#     }
#     if("x" %in% argnames) {
#       Recall(call[['x']])
#     }
#   }
#
#   if(!quoted) {
#     render_expr0 <- substitute(render_function)
#   } else {
#     render_expr0 <- render_function
#     render_function <- eval(render_function, envir = fenv)
#   }
#
#   render_details <- find_expr(render_expr0)
#   if(is.list(render_details)) {
#     extras$render_expr <- render_details$expr
#     extras$render_env <- render_details$env
#   }
#
#   extras$export_type <- export_type
#   if( export_type != "none" ) {
#     logger("Registering output {outputId} with {export_type} download type.", level = "trace", use_glue = TRUE)
#
#     export_settings <- as.list(export_settings)
#     export_settings$outputId <- outputId
#     export_settings <- do.call(sprintf("format_export_settings.%s", export_type), list(export_settings))
#     extras$export_settings <- export_settings
#
#     session$output[[sprintf("%s__download", outputId)]] <- shiny::downloadHandler(
#       filename = function() {
#         fname <- export_settings$filename
#         if(is.null(fname)) {
#           fname <- sprintf("[rave-export]%s.%s", session$ns(outputId),
#                            export_settings$extension)
#         } else {
#           fname <- paste0(fname, ".", export_settings$extension)
#         }
#         fname
#       },
#       content = function(con) {
#
#         data <- NULL
#         opt <- get_output_options(outputId, session = session)
#         render_expr <- opt$extras$render_expr
#         render_env <- opt$extras$render_env
#
#         if(is.environment(render_env) && is.language(render_expr)) {
#           export_settings$pre(con)
#           on.exit({ export_settings$post(con, data) }, add = TRUE, after = FALSE)
#           shiny::isolate({
#             data <- eval(render_expr, envir = new.env(parent = render_env))
#           })
#         } else {
#           stop("Cannot find renderer's details from the following renderer's expression: \n", deparse1(render_expr0))
#         }
#
#       }
#     )
#   }
#
#   session$output[[outputId]] <- render_function
#
#   register_output_options(
#     outputId = outputId, .opt = opt$args,
#     session = session, extras = extras
#   )
#
# }

#' @rdname register_output
#' @export
register_output <- function(
    render_function, outputId,
    ...,
    output_opts = list(), quoted = FALSE,
    download_function = NULL,
    download_fileformat = "{ outputId }-{ format(Sys.time(), '%b_%d_%Y_%H_%M_%S') }.{ extension }",
    output_type = c("image", "data", "threeBrain", "no-download"),
    extensions = NULL,
    title = "Download widget",
    cancel_btn = "Cancel", confirm_btn = "Download",
    session = shiny::getDefaultReactiveDomain()) {

  output_type <- match.arg(output_type)

  fenv <- new.env(parent = parent.frame())

  trigger_id <- sprintf("%s__download2", outputId)
  download_id <- sprintf("%s__.download_do.", outputId)
  type_id <- sprintf("%s__.extension.", outputId)
  width_id <- sprintf("%s__.width.", outputId)
  height_id <- sprintf("%s__.height.", outputId)
  title_id <- sprintf("%s__.title.", outputId)

  opt <- get_output_options(outputId, session = session)
  extras <- opt$extras
  if(!is.list(extras) || !inherits(extras, "fastmap2")) {
    extras <- dipsaus::fastmap2()
  }

  for(nm in names(output_opts)) {
    if(nchar(nm)) {
      opt$args[[nm]] <- output_opts[[nm]]
    } else {
      stop("register_output: `output_opts` must be a named list")
    }
  }

  # no file downloader is considered
  # obtain the expression
  find_expr <- function(call) {
    if(!is.call(call)) { return() }
    call <- match.call(
      definition = eval(call[[1]], envir = fenv),
      call = call, expand.dots = TRUE)
    call_list <- as.list(call)
    argnames <- names(call_list)
    if("expr" %in% argnames) {
      expr <- call[['expr']]
      env <- eval(call[['env']], envir = fenv)
      if(!is.environment(env)) {
        env <- eval(call[['envir']], envir = fenv)
      }
      if(!is.environment(env)) {
        env <- fenv
      }
      return(list(
        expr = expr,
        env = env
      ))
    }
    if("x" %in% argnames) {
      Recall(call[['x']])
    }
  }

  if(!quoted) {
    render_expr0 <- substitute(render_function)
  } else {
    render_expr0 <- render_function
    render_function <- eval(render_function, envir = fenv)
  }

  render_details <- find_expr(render_expr0)
  if(is.list(render_details)) {
    extras$render_expr <- render_details$expr
    extras$render_env <- render_details$env
  }

  if( output_type != "no-download" ) {

    # construct download widget

    # get file extension options if not provided by module devs
    if(is.null(extensions)) {
      switch(
        output_type,
        "image" = {
          extensions <- list(
            'pdf' = 'pdf',
            'png' = 'png',
            'jpeg' = 'jpg',
            'bmp' = 'bmp',
            'tiff (lzw)' = "tiff"
          )
        },
        "threeBrain" = {
          extensions <- list(
            "Packaged ZIP" = "zip"
          )
        },
        {
          extensions <- list(
            'HDF5' = 'h5',
            'Compressed CSV' = 'csv.zip',
            'FST' = 'fst',
            'RDS' = 'rdata'
          )
        }
      )
    }

    # use default download functions if not provided
    if(!is.function(download_function)) {

      download_function <- switch(
        output_type,
        "image" = function(con, params, render_expr, render_env) {
          # params -> extension, width, height

          switch(
            params$extension,
            'pdf' = {
              grDevices::pdf(
                con, useDingbats = FALSE, title = 'RAVE Plot Export',
                width = params$width, height = params$height)
            },
            'png' = {
              scl = 1
              grDevices::png(
                con, units = 'in', res=72 * 4,
                width = scl * params$width, height = scl * params$height)
            },
            'jpg' = {
              grDevices::jpeg(
                con, units = 'in', quality = 90, res=72*4,
                width = params$width, height = params$height)
            },
            'bmp' = {
              grDevices::bmp(
                con, units = 'in', res=72*4,
                width = params$width, height = params$height)
            },
            'tiff' = {
              grDevices::tiff(
                con, compression = 'lzw', units = 'in', res=72*4,
                width = params$width, height = params$height)
            },
            {
              # Default is unknown format, stop
              stop(sprintf("Unknown format requested [%s] (not yet implemented)", params$extension))
            }
          )

          on.exit({ grDevices::dev.off() })

          eval(render_expr, render_env)
        },
        "threeBrain" = function(con, params, render_expr, render_env) {
          # params -> extensions, title
          title <- trimws(paste(params$title, collapse = ""))
          if( !nzchar(title) ) {
            title <- "RAVE 3D Viewer"
          }
          widget <- eval(render_expr, render_env)

          if( is.null(widget) ) {
            stop("Cannot properly obtain the 3D brain. Please check if you have rendered the brain yet.")
          }
          tf <- tempfile()
          on.exit({ unlink(tf, recursive = TRUE) }, add = TRUE)

          threeBrain::save_brain(widget, directory = tf, as_zip = TRUE, title = title)
          file.copy(file.path(tf, "compressed.zip"), con)

        },
        {
          NULL
        }
      )


    }
  }

  if(is.function(download_function)) {
    shiny::bindEvent(
      safe_observe({

        opt1 <- NULL
        opt2 <- NULL

        if( output_type == "image" ) {
          output_data <- get_output(outputId, session = session)
          width <- output_data$client_data$width
          height <- output_data$client_data$height
          current_width <- session$input[[width_id]]
          current_height <- session$input[[height_id]]
          if( length(current_width) != 1 || current_width < 1 ) { current_width <- 7 }
          if( length(current_height) != 1 || current_height < 1 ) { current_height <- 4 }
          if(length(width) ==1 && length(height) == 1 &&width > 0 && height > 0) {
            current_height <- ceiling( current_width / width * height * 4 ) / 4
          }
          opt1 <- shiny::column(
            width = 4L,
            shiny::numericInput(
              inputId = session$ns(width_id),
              label = "Width:",
              min = 1,
              max = 40,
              step = 0.25,
              value = current_width,
              width = "100%"
            )
          )
          opt2 <- shiny::column(
            width = 4L,
            shiny::numericInput(
              inputId = session$ns(height_id),
              label = "Height:",
              min = 1,
              max = 40,
              step = 0.25,
              value = current_height,
              width = "100%"
            ),
            shiny::span(
              "Unit: in (=2.54cm)"
            )
          )
        } else if( output_type == "threeBrain" ) {
          opt1 <- shiny::column(
            width = 8L,
              shiny::textInput(
              inputId = session$ns(title_id),
              label = "Title:",
              value = shiny::isolate(session$input[[ title_id ]]),
              width = "100%"
            )
          )
        }

        shiny::showModal(shiny::modalDialog(
          title = title,
          footer = shiny::tagList(
            shiny::modalButton( label = cancel_btn ),
            shiny::downloadButton(
              outputId = session$ns(download_id),
              label = confirm_btn,
              class = "btn-primary"
            )
          ),
          shiny::fluidRow(

            shiny::column(
              width = 4L,
              shiny::selectInput(
                inputId = session$ns(type_id),
                label = "Type:",
                choices = names(extensions),
                selected = shiny::isolate(session$input[[type_id]]) %OF% names(extensions)
              )
            ),
            opt1,
            opt2
          ),
          ...
        ))

      }),
      session$input[[trigger_id]], ignoreNULL = TRUE, ignoreInit = TRUE
    )

    session$output[[download_id]] <- shiny::downloadHandler(
      filename = function(...) {
        extension <- extensions[[ session$input[[type_id]] %OF% names(extensions) ]]
        force(outputId)
        return(raveio::glue(download_fileformat))
      },
      content = function(con) {
        on.exit({
          shiny::removeModal(session = session)
          Sys.sleep(0.5)
          dipsaus::close_alert2()
        })
        dipsaus::shiny_alert2(
          title = "Exporting in progress",
          text = "Please wait...",
          icon = "info",
          auto_close = FALSE,
          buttons = FALSE
        )
        params <- list(
          extension = extensions[[ session$input[[type_id]] %OF% names(extensions) ]],
          output_type = output_type
        )
        if(output_type == "image") {
          params$width <- session$input[[width_id]]
          params$height <- session$input[[height_id]]
        } else if ( output_type == "threeBrain" ) {
          params$title <- session$input[[title_id]]
        }

        opt <- get_output_options(outputId, session = session)
        render_expr <- opt$extras$render_expr
        render_env <- opt$extras$render_env
        download_function(con, params, render_expr, render_env)
      }
    )
  }

  session$output[[outputId]] <- render_function

  register_output_options(
    outputId = outputId, .opt = opt$args,
    session = session, extras = extras
  )

}


#' @rdname register_output
#' @export
get_output <- function(outputId, session = shiny::getDefaultReactiveDomain()) {

  module_id <- session$ns(NULL)
  # make sure we are working on the root scope
  session <- session$rootScope()

  # get module information
  if(!is.null(session)) {
    module_info <- get_active_module_info(session = session)
    module_id <- module_info$id
  }

  # get renderer's information
  reactive_handlers <- get_default_handlers(session = session)
  output_options <- reactive_handlers$output_options
  ns <- shiny::NS(module_id)
  outputId_full <- ns(outputId)

  output_opt <- get_output_options(outputId = outputId_full, session = session)

  render_function <- session$getOutput(outputId_full)

  if(!is.function(render_function)) {
    stop("Cannot find render function for output: ", outputId)
  }

  # get output function
  ui_function <- attr(render_function, "outputFunc")

  # get output options
  output_opt <- as.list(output_options[[outputId_full]])

  client_data <- shiny::isolate({
    list(
      width = session$clientData[[sprintf("output_%s_width", outputId_full)]],
      height = session$clientData[[sprintf("output_%s_height", outputId_full)]],
      hidden = session$clientData[[sprintf("output_%s_hidden", outputId_full)]]
    )
  })

  list(
    namespace = ns(NULL),
    outputId = outputId,
    outputId_full = outputId_full,
    client_data = client_data,
    render_function = render_function,
    output_function = ui_function,
    output_args = output_opt$args,
    extras = output_opt$extras
  )

}



#' Shiny plot output with minimum height and additional classes
#' @param outputId,width,height,... passed to \code{\link[shiny]{plotOutput}}
#' @param class additional 'HTML' class of the output wrapper
#' @param min_height minimum height of the image; default is 400 pixels
#' @returns A plot output element that can be included in a panel.
#' @examples
#'
#' plotOutput2("plot", class = "rounded overflow-hidden",
#'             min_height = 300)
#'
#'
#' @export
plotOutput2 <- function(outputId, class = NULL,
                        width = "100%", height = "100%", min_height = "400px", ...) {

  res <- shiny::plotOutput(
    outputId = outputId,
    height = width, width = height, ...
  )

  res$attribs$class <- dipsaus::combine_html_class(res$attribs$class, class)

  res$attribs$style <- sprintf(
    "%s;min-height:%s",
    paste(res$attribs$style, collapse = ""),
    shiny::validateCssUnit(min_height)
  )

  res
}
