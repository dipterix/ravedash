#' @title 'RAVE' plot container class
#' @description
#' Container to generate 'RAVE' built-in figures
#'
#' @examples
#'
#' library(ravedash)
#'
#' # Low-level usage
#' container <- RAVEPlotContainer$new()
#'
#' # set data
#' container$set_data("x", 1:10)
#' container$set_data("y", 1:20)
#' container$set_data("data", matrix(1:200, nrow = 10))
#'
#' # derivative data can be dynamically calculated
#' container$set_derivative("zlim", function(data) {
#'   max(abs(range(data, na.rm = TRUE))) * c(-1,1)
#' })
#' container$set_derivative("title", "x: {{ length(x) }}, y: {{ length(y) }}",
#'                        use_glue = TRUE)
#'
#' # optional, reset renderer state
#' container$reset("renderers")
#'
#' container$add_renderer({
#'   image(data, x = x, y = y, zlim = zlim, main = title)
#' })
#'
#' container$add_renderer({
#'   image(t(data), x = y, y = x, zlim = zlim, main = title)
#'
#'   # renderer will go through with a warning
#'   stop(123)
#' })
#'
#' container$add_renderer({
#'   plot(x)
#' }, order = 3)
#'
#' # Pre-pend renderers
#' container$add_renderer({
#'   par(mfrow = c(1, 3))
#' }, order = -1)
#'
#' # reset status
#' container$add_renderer({
#'   logger("Reset graphical status")
#'   par(mfrow = c(1,1))
#' }, order = 100)
#'
#'
#' # Render all
#' plot(container)
#'
#' # Partial render
#' plot(container, order_range = 3)
#'
#'
#'
#' @export
RAVEPlotContainer <- R6::R6Class(
  classname = "RAVEPlotContainer",
  portable = TRUE,
  cloneable = FALSE,
  lock_class = TRUE,
  lock_objects = TRUE,
  private = list(
    .verbose = function(..., level = "info", .envir = parent.frame()) {
      if(!self$verbose) { return(invisible()) }
      ravedash::logger(level = level, ..., .envir = .envir)
    },
    .warn = function(..., .envir = parent.frame()) {
      private$.verbose(..., level = "warning", .envir = .envir)
    },
    .validator = NULL,
    .data = NULL,
    .derivative = NULL,
    .renderers = NULL,

    class_type = "basic",

    finalize = function(...) {
      self$reset("all")
    },
    .basic_validator = function() {
      dup <- intersect(names(private$.data), names(private$.derivative))
      if(length(dup)) {
        stop("Cannot set data or derivative with the following name(s): ", paste(dup, collapse = ", "),
             ". One variable name can only exist in data or derivative, not both.")
      }
    }
  ),
  public = list(

    #' @field verbose whether to verbose information, warnings
    verbose = TRUE,

    #' @description
    #' Constructor
    initialize = function() {
      private$.validator <- list()
      private$.data <- dipsaus::fastmap2()
      private$.derivative <- dipsaus::fastmap2()
      private$.renderers <- dipsaus::fastqueue2()
    },


    #' @description
    #' Reset \code{'renderers'}, \code{'data'}, \code{'derivative'}, settings,
    #' or \code{'validator'}
    #' @param type type of data to reset
    reset = function(type = c("renderers", "all", "data", "derivative", "validator")) {
      type <- match.arg(type)
      if(type == "all") {
        type <- c("data", "derivative", "validator", "renderers")
      }

      if("data" %in% type) {
        private$.data$`@reset`()
      }
      if("derivative" %in% type) {
        private$.derivative$`@reset`()
      }
      if("renderers" %in% type) {
        private$.renderers$reset()
      }
      if("validator" %in% type) {
        private$.validator <- list()
      }

      return(invisible())
    },

    #' @description
    #' Low-level set data or derivative
    #' @param ...,.list named key-value pairs to be stored
    #' @param type type of data, \code{'data'} or \code{'derivative'}
    #' @param validate whether to validate the data; when error occurs, the
    #' container state will be restored to previous state
    mset = function(..., .list = list(), type = c("data", "derivative"), validate = TRUE) {
      type <- match.arg(type)
      map <- private[[sprintf(".%s", type)]]

      keys <- c(...names(), names(.list))

      if(is.null(keys) || anyNA(keys) || '' %in% keys) { stop("Set data: values must be named") }

      keys <- unique(keys)
      keys0 <- keys
      # check if has keys
      keys <- keys[map$`@has`(keys)]

      old_vals <- structure(map[keys], names = keys)
      is_valid <- FALSE

      on.exit({
        if(!is_valid) {
          private$.verbose("Rewind values for the following keys: [", paste(keys0, collapse = ", "), "]")
          map$`@remove`(keys0)
          if(length(old_vals)) {
            map$`@mset`(.list = old_vals)
          }
        }
      })

      # set new values
      if(length(keys0)) {
        map$`@mset`(..., .list = .list)
      }

      if( validate ) {
        self$validate()
      }

      is_valid <- TRUE
      return(invisible())
    },

    #' @description
    #' Store data
    #' @param key name of data
    #' @param value value of the key to be stored
    #' @param validate whether to validate data; default is true; when error
    #' occurs, the container state will be restored to previous state
    set_data = function(key, value, validate = TRUE) {
      stopifnot(length(key) == 1)
      self$mset(.list = structure(list(value), names = key), type = "data", validate = validate)
    },

    #' @description
    #' Get data
    #' @param key key of data
    #' @param if_missing return this value when key is missing
    #' @returns The data value if key is found, or a missing value when key is missing
    get_data = function(key, if_missing) {
      stopifnot(length(key) == 1)
      if(missing(if_missing)) {
        return(private$.data$`@get`(key))
      } else {
        return(private$.data$`@get`(key, missing = if_missing))
      }
    },

    #' @description
    #' Remove data
    #' @param keys one or more keys to remove
    remove_data = function(keys) {
      return(invisible(private$.data$`@remove`(keys)))
    },

    #' @description
    #' Store a derivative
    #' @param key name of the derivative
    #' @param value value of the derivative; can be an arbitrary value; some
    #' special treatments will be done to characters and functions. See
    #' \code{get_derivative} for details.
    #' @param use_glue whether to glue the derivative; default is false
    #' @param validate whether to validate data; default is true; when error
    #' occurs, the container state will be restored to previous state
    set_derivative = function(key, value, use_glue = FALSE, validate = TRUE) {
      stopifnot(length(key) == 1)

      if(use_glue) {
        attr(value, ".use_glue") <- TRUE
      }

      self$mset(.list = structure(list(value), names = key), type = "derivative", validate = validate)
    },

    #' @description
    #' Get derivative
    #' @param key key of the derivative
    #' @param if_missing return this value when key is missing
    #' @param env environment to evaluate derivatives
    #' @param .internal internally used
    #' @returns The derivative value if key is found, or a missing value when
    #' the key is missing. The derivative will be "processed". When the
    #' derivative is a function, then the function will be called with all the
    #' data (see \code{set_data}) passed as named arguments (see 'Examples').
    #' When \code{use_glue} is true, then the derivative will be passed into
    #' \code{\link[raveio]{glue}} for lazy assessment (see 'Examples').
    #' @examples
    #'
    #' container <- RAVEPlotContainer$new()
    #' container$set_derivative("xlen", function(x) {
    #'   length(x)
    #' })
    #' container$set_derivative(
    #'   key = "xlab",
    #'   "x ({{ typeof(x) }})",
    #'   use_glue = TRUE
    #' )
    #' container$set_data("x", letters)
    #' container$get_derivative("xlen")
    #' container$get_derivative("xlab")
    #'
    get_derivative = function(key, if_missing, env = parent.frame(), .internal = FALSE) {
      stopifnot(length(key) == 1)
      if(missing(if_missing)) {
        re <- private$.derivative$`@get`(key)
      } else {
        re <- private$.derivative$`@get`(key, missing = if_missing)
      }
      use_glue <- isTRUE(attr(re, ".use_glue"))
      if(is.function(re)) {
        nms <- names(formals(re))
        if('...' %in% nms) {
          args <- private$.data$`@as_list`()
        } else {
          args <- private$.data[nms]
        }
        re <- do.call(re, args)
      }

      if( use_glue ) {
        if( !.internal ) {
          env <- list2env(private$.data$`@as_list`(), parent = env)
        }
        re <- raveio::glue(
          re,
          .trim = FALSE,
          .envir = env,
          .open = "{{",
          .close = "}}",
          .null = "NULL",
          .comment = "##"
        )
      }
      return(re)
    },

    #' @description
    #' Get all the derivatives in a list
    #' @param env environment to evaluate derivatives
    #' @param .internal internally used; please do not change
    #' @returns A named list of derivatives
    get_all_derivative = function(env = parent.frame(), .internal = FALSE) {
      force(env)
      nms <- self$derivative_names
      env2 <- new.env(parent = env)
      env3 <- list2env(private$.data$`@as_list`(), parent = env2)

      re <- structure(
        lapply(nms, function(nm) {
          self$get_derivative(nm, env = env3, .internal = TRUE)
        }),
        names = nms
      )

      if( .internal ) {
        list2env(re, envir = env2)
        return(env3)
      }
      return(re)
    },

    #' @description
    #' Remove derivatives
    #' @param keys one or more keys to remove
    remove_derivative = function(keys) {
      return(invisible(private$.derivative$`@remove`(keys)))
    },

    #' @description
    #' Add functions to validate the container
    #' @param name name (ID) of the \code{'validator'}; if name exists, then old
    #' validation function will be replaced.
    #' @param fun validation function, must take one argument (this container)
    #' as the argument
    add_validator = function(name, fun) {

      stopifnot(is.function(fun))
      if(length(formals(fun)) == 0) {
        stop("Validator must take one argument (instance of RAVEPlotContainer) as argument")
      }
      private$.validator[[length(private$.validator) + 1]] <- list(
        name = name,
        validator = fun
      )

    },

    #' @description
    #' Remove a validation function by name
    #' @param name name of the validation function to remove
    remove_validator = function(name) {
      sel <- self$validator_names %in% name
      if(any(sel)) {
        self$validator_names <- self$validator_names[!sel]
      }
      return(invisible())
    },

    #' @description
    #' Validate data integrity of container
    #' @param fail action when validation fails; default is the throw the first
    #' \code{'error'}; other choices are \code{'inspect'} (run all the
    #' validation and report all the errors in matrix)
    #' @returns \code{NULL} if no errors found, or a matrix of two columns
    #' when \code{fail='inspect'}: the first column is the validation name, and
    #' the second column is the error message
    validate = function(fail = c("error", "inspect")) {
      fail <- match.arg(fail)

      vs <- c(
        list(list(
          name = "(Internal)",
          fun = function(...) {
            private$.basic_validator()
          }
        )),
        private$.validator
      )

      if(fail == "error") {
        lapply(vs, function(item) {
          item$fun(self)
        })
        return(invisible())
      } else {
        re <- lapply(vs, function(item) {
          tryCatch({
            item$fun(self)
            return(NULL)
          }, error = function(e) {
            c(item$name, paste(e$message, collapse = ""))
          })

        })
        re <- re[!vapply(re, is.null, FALSE)]
        if(!length(re)) { return() }

        return(do.call("rbind", re))
      }

    },

    #' @description
    #' Format the container
    #' @param ... ignored
    format = function(...) {

      errs <- self$validate(fail = "inspect")

      if(!length(errs)) {
        errs <- NULL
      } else {
        errs <- unlist(apply(errs, 1, function(item) {
          sprintf("    - %s failed: %s\n", item[[1]], item[[2]])
        }))
      }

      paste(
        collapse = "",
        sep = "",
        c(
          sprintf("<RAVEPlotContainer, type %s>\n", private$class_type),
          sprintf("  Data: %s\n", length(private$.data)),
          unlist(lapply(
            self$data_names,
            function(nm) {
              sprintf(
                "   - %s (type: [%s], class: [%s])\n", nm,
                paste(typeof(private$.data[[nm]]), collapse = ","),
                paste(class(private$.data[[nm]]), collapse = ",")
              )
            }
          )),
          sprintf("  Derivative: %s\n", length(private$.derivative)),
          unlist(lapply(names(private$.derivative), function(nm) {
            fmt <- format(private$.derivative[[ nm ]])
            if(length(fmt) > 1) {
              fmt <- sprintf("%s ...", fmt[[1]])
            }
            sprintf("    - %s: %s\n", nm, fmt)
          })),
          sprintf("  Renderers: %s\n", length(private$.renderers)),
          sprintf("  Validators: %s\n", length(private$.validator) + 1),
          errs
        )
      )
    },

    #' @description
    #' Add render expressions
    #' @param expr R expression; all the data and derivatives will be available
    #' as variables; please make sure all the global variables in \code{expr}
    #' come from data or derivatives, or the rendering results might error
    #' out. See \code{\link{bquote}} on how to partial evaluate variables (
    #' and set \code{quoted} to true if \code{expr} is (partially) quoted).
    #' @param quoted whether \code{expr} is quoted; default is false
    #' @param order render order; smaller values will be rendered first;
    #' default value is 0. See \code{order_range} in \code{$render} method
    #' for selective render
    #' @param label label of the render expression, can be missing, mainly
    #' for verbose and debug use.
    add_renderer = function(expr, quoted = FALSE, order = 0, label = NULL) {
      if(!quoted) {
        expr <- substitute(expr)
      }

      order <- as.numeric(order)[[1]]
      if(is.na(order)) {
        order <- 0
      }

      private$.renderers$add(list(
        expr = expr,
        order = order,
        label = label
      ))
      invisible()
    },

    #' @description
    #' Render the container
    #' @param ...,.list additional parameters to use for evaluating rendering
    #' expressions; can be used to mask out derivatives or add flags (
    #' data will not be masked out)
    #' @param order_range range of render expressions to evaluate; rendering
    #' order outside of this range will not be evaluated
    #' @param env environment to rendering the container; default is the caller
    #' environment
    #' @param validate whether to validate the container first before rendering
    #' @param strict whether to be strict: if true, then stops
    #' when any render function error out; or keep rendering regardless of
    #' errors
    render = function(..., .list = list(), order_range = c(-1, 100),
                      env = parent.frame(), validate = TRUE, strict = FALSE) {

      force(strict)
      if( validate ) {
        self$validate()
      }
      data_env <- self$get_all_derivative(env = env, .internal = TRUE)
      relay_env <- list2env(c(list(...), .list), parent = data_env)
      runtime_env <- new.env(parent = relay_env)

      suppressWarnings({
        order_range <- range(as.numeric(order_range), na.rm = TRUE)
      })

      renderers <- private$.renderers$as_list()

      odr <- vapply(renderers, function(item) {
        as.numeric(item$order)
      }, 0.0)

      renderers <- renderers[order(odr, decreasing = FALSE)]

      return(invisible(unlist(
        lapply(renderers, function(item) {
          if(item$order < order_range[[1]] || item$order > order_range[[2]]) {
            return(NULL)
          }
          tryCatch({
            re <- list(eval(item$expr, envir = runtime_env))
            if(length(item$label)) {
              names(re) <- item$label
            }
            re
          }, error = function(e) {
            label <- item$label
            if(!length(label)) {
              label <- "(Unnamed)"
            }
            private$.warn("Cannot render {label[[1]]}: {e$message} (See expressions below)", use_glue = TRUE)
            if(self$verbose) {
              print(item$expr)
            }
            if( strict ) {
              stop(e)
            }
          })
        }),
        recursive = FALSE, use.names = TRUE
      )))
    },

    #' @description
    #' Render the container, see \code{$render} method
    #' @param ...,env passed to \code{$render} method
    plot = function(..., env = parent.frame()) {
      self$render(..., env = env)
    }

  ),
  active = list(
    #' @field validator_names names of validation expressions
    validator_names = function() {
      vapply(private$.validator, "[[", "name")
    },

    #' @field data_names names of data variables
    data_names = function() {
      names(private$.data)
    },

    #' @field derivative_names names of derivative variables
    derivative_names = function() {
      names(private$.derivative)
    }
  )

)
