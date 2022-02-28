RAVEShinyComponentContainer <- R6::R6Class(
  classname = "RAVEShinyComponentContainer",
  portable = TRUE,
  public = list(
    module_id = character(0L),
    pipeline_name = character(0L),
    pipeline_path = character(0L),
    settings_path = character(0L),

    components = NULL,
    cache = NULL,

    initialize = function(
      module_id = NULL,
      pipeline_name = module_id,
      pipeline_path = raveio::pipeline_find(pipeline_name),
      settings_file = "settings.yaml"
    ){
      settings_path <- file.path(pipeline_path, settings_file)
      sel <- file.exists(settings_path)
      settings_path <- settings_path[sel]
      pipeline_path <- pipeline_path[sel]
      if(!length(settings_path)){
        stop("Invalid pipeline_path and settings_file combinations: cannot find pipeline settings file")
      }
      self$module_id <- module_id
      self$pipeline_name <- pipeline_name
      self$pipeline_path <- pipeline_path[[1]]
      self$settings_path <- settings_path[[1]]
      self$components <- dipsaus::fastmap2()
      self$cache <- dipsaus::fastmap2()
    },

    add_components = function(..., .list = list()){
      comps <- c(list(...), .list)
      for(comp in comps){
        stopifnot(inherits(comp, 'RAVEShinyComponent'))
        self$components[[comp$id]] <- comp
        comp$container <- self
      }
    },

    get_cache = function(key, default = NULL){
      if(self$cache$`@has`(key)){
        item <- self$cache[[key]]
        if(Sys.time() - item$timestamp < item$expire_after) {
          return(item$value)
        } else {
          self$cache$`@remove`(key)
        }
      }
      default
    },

    set_cache = function(key, value, expire_after = Inf){
      self$cache[[key]] <- list(
        timestamp = Sys.time(),
        value = value,
        expire_after = expire_after
      )
    }

  )
)

RAVEShinyComponent <- R6::R6Class(
  classname = "RAVEShinyComponent",
  portable = TRUE,
  private = list(
    .ui_func = NULL,  # function(id, value)
    .server_func = NULL,
    .sv = NULL,
    .initialized = FALSE,
    server_func0 = function(input, output, session){
      if(private$.initialized){ return() }

      depends <- self$container$components[self$depends]

      deps_has_validator <- FALSE
      for(dep in depends){
        if(!dep$initialized){
          dep$server_func(input = input, output = output, session = session)
        }
        if(!is.null(dep$sv)){
          deps_has_validator <- TRUE
        }
      }
      sv <- NULL
      if(deps_has_validator || self$validators$size()){
        sv <- shinyvalidate::InputValidator$new(session = session)
        for(dep in depends){
          if(!is.null(dep$sv)){
            sv$add_validator(validator = dep$sv)
          }
        }
        lapply(self$validators$as_list(), function(v){
          sv$add_rule(inputId = self$id, rule = v, session. = session)
        })
      }
      private$.sv <- sv
      sv$enable()
      if(is.function(private$.server_func)){
        private$.server_func(input = input, output = output, session = session)
      }
      private$.initialized <- TRUE
      return()
    }
  ),
  public = list(
    container = NULL,
    id = character(0L),
    varname = character(0L),
    validators = NULL,
    depends = NULL,

    initialize = function(id, varname = id){
      self$id = id
      self$varname = varname
      self$validators = dipsaus::fastqueue2()
    },

    add_rule = function(rule){
      if(!is.function(rule) || length(formals(rule)) != 1){
        stop("`rule` must be a function that takes one parameters: `value`, i.e., the input value")
      }
      self$validators$add(rule)
    },

    get_settings_value = function(default = NULL, constraint = NULL,
                                  use_cache = FALSE){
      if(!inherits(self$container, "RAVEShinyComponentContainer")){
        stop("Please add this components to a RAVEShinyComponentContainer")
      }
      settings <- NULL
      if(use_cache){
        settings <- self$container$get_cache("pipeline_settings", default = NULL)
      }
      if(is.null(settings)){
        settings <- raveio::pipeline_settings_get(
          pipeline_settings_path = self$container$settings_path)
        self$container$set_cache("pipeline_settings", settings, expire_after = 1)
      }

      key <- self$varname
      if(!settings$`@has`(key)){
        re <- default
      } else {
        re <- settings[[key]]
      }

      if(length(constraint)){
        re <- re %OF% constraint
      }
      re

    },

    get_dependent_component = function(depend_id){
      if(!inherits(self$container, "RAVEShinyComponentContainer")){
        stop("Please add this components to a RAVEShinyComponentContainer")
      }
      self$container$components[[depend_id]]
    }

  ),
  active = list(
    sv = function(){
      private$.sv
    },

    initialized = function(){
      private$.initialized
    },

    ui_func = function(ui){
      if(!missing(ui)){
        if(!is.function(ui) || !all(c('id', 'value', 'depends') %in% names(formals(ui)))){
          stop("`ui` must be a function that takes three parameters: `id`, `value`, and `depends`")
        }
        private$.ui_func <- function(){
          # self = comp
          ns <- shiny::NS(self$container$module_id)
          value <- self$get_settings_value(use_cache = TRUE)
          depends <- self$container$components[self$depends]
          ui(id = ns(self$id), value = value, depends = depends)
        }
      }
      private$.ui_func
    },
    server_func = function(server){
      if(!missing(server)){
        if(!is.function(server) || !all(c('input', 'output', 'session') %in% names(formals(server)))){
          stop("`server` must be a function that takes three parameters: `input`, `output`, and `session`")
        }
        private$.server_func <- server
      }
      private$server_func0
    },

    current_value = function(){
      session <- shiny::getDefaultReactiveDomain()
      if(!is.environment(session)){ return(NULL) }
      session$input[[self$id]]
    }
  )
)


#' @name rave-ui-preset
#' @param id input or output ID of the element; this ID will be prepended with
#' module namespace
#' @param varname,varnames variable name(s) in the module's settings file
#' @param label,labels readable label(s) of the element
#' @param height height of the element
#' @param loader_project_id the ID of \code{presets_loader_project} if
#' different to the default
#' @param loader_subject_id the ID of \code{presets_loader_subject} if
#' different to the default
#' @param loader_reference_id the ID of \code{presets_loader_reference} if
#' different to the default
#' @param loader_electrodes_id the ID of \code{presets_loader_electrodes} if
#' different to the default
#' @title Preset reusable 'RAVE' front-end elements
#' @export
presets_loader_project <- function(
  id = "loader_project_name", varname = "project_name",
  label = "Project"
){

  comp <- RAVEShinyComponent$new(id = id, varname = varname)

  comp$ui_func <- function(id, value, depends){
    choices <- raveio::get_projects(refresh = FALSE)
    shiny::selectInput(
      inputId = id,
      label = label,
      choices = choices,
      selected = value %OF% choices,
      multiple = FALSE
    )
  }
  comp$add_rule(function(value){
    if(length(value) != 1 || is.na(value)){
      return("Missing project name. Please choose one.")
    }
    project <- raveio::as_rave_project(value, strict = FALSE)
    if(!dir.exists(project$path)){
      return(raveio::glue("Cannot find path to project `{value}`"))
    }
    return(NULL)
  })

  comp

}


#' @rdname rave-ui-preset
#' @export
presets_loader_subject <- function(
  id = "loader_subject_code", varname = "subject_code",
  label = "Subject",
  loader_project_id = "loader_project_name"
){
  comp <- RAVEShinyComponent$new(id = id, varname = varname)
  comp$depends <- loader_project_id

  comp$ui_func <- function(id, value, depends){
    shiny::selectInput(
      inputId = id,
      label = label,
      choices = value,
      selected = value,
      multiple = FALSE
    )
  }
  comp$server_func <- function(input, output, session){
    loader_project <- comp$get_dependent_component(loader_project_id)

    shiny::observe({
      if(!loader_project$sv$is_valid()){ return() }
      project_name <- input[[loader_project$id]]
      project <- raveio::as_rave_project(project_name)
      all_subjects <- project$subjects()
      selected <- comp$get_settings_value(
        default = input[[comp$id]],
        constraint = all_subjects,
        use_cache = TRUE
      )

      shiny::updateSelectInput(session = session, inputId = comp$id,
                               choices = all_subjects, selected = selected)
    }) |>
      shiny::bindEvent(
        input[[loader_project$id]],
        ignoreNULL = TRUE
      )

  }
  comp$add_rule(function(subject_code){
    if(length(subject_code) != 1 || is.na(subject_code) ||
       startsWith(subject_code, "_")){
      return("Invalid subject code.")
    }

    loader_project <- comp$get_dependent_component(loader_project_id)

    project_name <- loader_project$current_value
    if(is.null(project_name)){
      return("No project name found.")
    }
    subject_id <- sprintf("%s/%s", project_name, subject_code)
    subject <- comp$container$get_cache("loader_subject_instance", default = NULL)
    if(!inherits(subject, "RAVESubject") ||
       !identical(subject$subject_id, subject_id)) {

      subject <- raveio::as_rave_subject(subject_id, strict = FALSE)
      comp$container$set_cache(key = "loader_subject_instance",
                               value = subject, expire_after = 10)
    }


    if(!dir.exists(subject$path)){
      return("Subject directory is broken or missing.")
    }
    if(!any(subject$preprocess_settings$has_wavelet)) {
      return("Please run wavelet on this subject first.")
    }
    return(NULL)
  })

  comp

}


#' @rdname rave-ui-preset
#' @export
presets_loader_epoch <- function(
  id = "loader_epoch_name",
  varnames = c("epoch_name", "trial_starts", "trial_ends"),
  labels = c("Epoch name", "Pre", "Post"),
  loader_project_id = "loader_project_name",
  loader_subject_id = "loader_subject_code"
){
  comp <- RAVEShinyComponent$new(id = id, varname = varnames[[1]])
  comp$depends <- c(loader_project_id, loader_subject_id)

  pre_id <- paste0(id, "_pre")
  post_id <- paste0(id, "_post")
  check_id <- paste0(id, "_default")

  comp$ui_func <- function(id, value, depends){

    pre <- -raveio::pipeline_settings_get(
      key = varnames[2], default = 1,
      pipeline_settings_path = comp$container$settings_path
    )
    post <- raveio::pipeline_settings_get(
      key = varnames[3], default = 1,
      pipeline_settings_path = comp$container$settings_path
    )

    ravedash::flex_group_box(
      title = "Epoch and Trial Duration",
      shidashi::flex_item(
        size = 2,
        shiny::selectInput(
          inputId = id,
          label = labels[[1]],
          choices = value,
          selected = value,
          multiple = FALSE
        )
      ),
      shidashi::flex_item(
        shiny::numericInput(
          inputId = paste0(id, "_pre"),
          label = labels[[2]],
          min = 0,
          value = pre
        )
      ),
      shidashi::flex_item(
        shiny::numericInput(
          inputId = paste0(id, "_post"),
          label = labels[[3]],
          min = 0,
          value = post
        )
      ),
      shidashi::flex_break(),
      shidashi::flex_item(
        shinyWidgets::prettyCheckbox(
          inputId = paste0(id, "_default"),
          label = "Set as the default",
          status = "success",
          shape = "square",
          animation = "smooth"
        )
      )
    )
  }
  comp$server_func <- function(input, output, session){
    loader_project <- comp$get_dependent_component(loader_project_id)
    loader_subject <- comp$get_dependent_component(loader_subject_id)

    shiny::observe({
      open_loader <- watch_loader_opened(session = session)
      if(!open_loader){ return() }
      if(!loader_subject$sv$is_valid()){ return() }
      project_name <- loader_project$current_value
      subject_code <- loader_subject$current_value
      subject_id <- sprintf("%s/%s", project_name, subject_code)
      subject <- comp$container$get_cache("loader_subject_instance", default = NULL)
      if(!inherits(subject, "RAVESubject") ||
         !identical(subject$subject_id, subject_id)) {

        subject <- raveio::as_rave_subject(subject_id, strict = FALSE)
        comp$container$set_cache(key = "loader_subject_instance",
                                 value = subject, expire_after = 10)
      }

      epoch_choices <- subject$epoch_names

      default_epochname <- subject$get_default("epoch_name")
      if(length(default_epochname)){
        default_epochname <- default_epochname[[1]]
        shinyWidgets::updatePrettyCheckbox(
          session, inputId = paste0(id, "_default"),
          label = sprintf("Set as the default (current: %s)", default_epochname)
        )
      } else {
        shinyWidgets::updatePrettyCheckbox(
          session, inputId = paste0(id, "_default"),
          label = "Set as the default"
        )
      }
      epoch_name <- comp$get_settings_value(
        default = subject$get_default("epoch_name"),
        constraint = epoch_choices,
        use_cache = TRUE)

      shiny::updateSelectInput(
        session = session,
        inputId = id,
        choices = epoch_choices,
        selected = epoch_name
      )
    }) |>
      shiny::bindEvent(
        input[[loader_project$id]],
        input[[loader_subject$id]],
        watch_loader_opened(session = session),
        ignoreNULL = TRUE
      )

  }

  comp

}

#' @rdname rave-ui-preset
#' @export
presets_loader_reference <- function(
  id = "loader_reference_name",
  varname = "reference_name",
  label = "Reference name",
  loader_project_id = "loader_project_name",
  loader_subject_id = "loader_subject_code"
){
  comp <- RAVEShinyComponent$new(id = id, varname = varname)
  comp$depends <- c(loader_project_id, loader_subject_id)

  check_id <- paste0(id, "_default")

  comp$ui_func <- function(id, value, depends){

    shiny::tagList(shidashi::flex_item(
      shiny::selectInput(
        inputId = id,
        label = label,
        choices = '',
        selected = NULL,
        multiple = FALSE
      ),
      shinyWidgets::prettyCheckbox(
        inputId = paste0(id, "_default"),
        label = "Set as the default",
        status = "success",
        shape = "square",
        animation = "smooth"
      )
    ))
  }
  comp$server_func <- function(input, output, session){
    loader_project <- comp$get_dependent_component(loader_project_id)
    loader_subject <- comp$get_dependent_component(loader_subject_id)

    shiny::observe({
      open_loader <- watch_loader_opened(session = session)
      if(!open_loader){ return() }
      if(!loader_subject$sv$is_valid()){ return() }
      project_name <- loader_project$current_value
      subject_code <- loader_subject$current_value
      subject_id <- sprintf("%s/%s", project_name, subject_code)
      subject <- comp$container$get_cache("loader_subject_instance", default = NULL)
      if(!inherits(subject, "RAVESubject") ||
         !identical(subject$subject_id, subject_id)) {

        subject <- raveio::as_rave_subject(subject_id, strict = FALSE)
        comp$container$set_cache(key = "loader_subject_instance",
                                 value = subject, expire_after = 10)
      }

      default_refname <- subject$get_default("reference_name")
      if(length(default_refname)){
        default_refname <- default_refname[[1]]
        shinyWidgets::updatePrettyCheckbox(
          session, inputId = paste0(id, "_default"),
          label = sprintf("Set as the default (current: %s)", default_refname)
        )
      } else {
        shinyWidgets::updatePrettyCheckbox(
          session, inputId = paste0(id, "_default"),
          label = "Set as the default"
        )
      }
      ref_choices <- subject$reference_names
      reference_name <-
        comp$get_settings_value(
          default = default_refname,
          constraint = ref_choices,
          use_cache = TRUE
        )
      shiny::updateSelectInput(
        session = session,
        inputId = id,
        choices = ref_choices,
        selected = reference_name
      )

    }) |>
      shiny::bindEvent(
        input[[loader_project$id]],
        input[[loader_subject$id]],
        watch_loader_opened(session = session),
        ignoreNULL = TRUE
      )

  }

  comp

}

#' @rdname rave-ui-preset
#' @export
presets_loader_electrodes <- function(
  id = "loader_electrode_text",
  varname = "loaded_electrodes",
  label = "Electrodes",
  loader_project_id = "loader_project_name",
  loader_subject_id = "loader_subject_code"
){
  comp <- RAVEShinyComponent$new(id = id, varname = varname)
  comp$depends <- c(loader_project_id, loader_subject_id)

  comp$ui_func <- function(id, value, depends){
    shiny::textInput(
      inputId = id,
      label = label,
      placeholder = "E.g. 1-84,90-100",
      value = ""
    )
  }
  comp$server_func <- function(input, output, session){
    loader_project <- comp$get_dependent_component(loader_project_id)
    loader_subject <- comp$get_dependent_component(loader_subject_id)

    shiny::observe({
      if(!loader_subject$sv$is_valid()){ return() }
      project_name <- loader_project$current_value
      subject_code <- loader_subject$current_value
      subject_id <- sprintf("%s/%s", project_name, subject_code)
      subject <- comp$container$get_cache("loader_subject_instance", default = NULL)
      if(!inherits(subject, "RAVESubject") ||
         !identical(subject$subject_id, subject_id)) {

        subject <- raveio::as_rave_subject(subject_id, strict = FALSE)
        comp$container$set_cache(key = "loader_subject_instance",
                                 value = subject, expire_after = 10)
      }

      # electrodes
      # check if subject is last input
      electrode_text <- dipsaus::deparse_svec(subject$electrodes)
      if(isTRUE(loader_subject$get_settings_value(use_cache = TRUE) == subject$subject_code)) {
        electrode_text <- comp$get_settings_value(default = electrode_text, use_cache = TRUE)
      }
      shiny::updateTextInput(
        session = session,
        inputId = id,
        value = electrode_text
      )

    }) |>
      shiny::bindEvent(
        input[[loader_project$id]],
        input[[loader_subject$id]],
        ignoreNULL = TRUE
      )

  }

  comp

}

#' @rdname rave-ui-preset
#' @export
presets_loader_3dviewer <- function(
  id = "loader_3d_viewer",
  height = "600px",
  loader_project_id = "loader_project_name",
  loader_subject_id = "loader_subject_code",
  loader_reference_id = "loader_reference_name",
  loader_electrodes_id = "loader_electrode_text"
) {
  comp <- RAVEShinyComponent$new(id = id)
  comp$depends <- c(loader_project_id, loader_subject_id, loader_electrodes_id, loader_reference_id)

  comp$ui_func <- function(id, value, depends){
    threeBrain::threejsBrainOutput(
      outputId = id,
      height = height,
      reportSize = FALSE
    )
  }
  comp$server_func <- function(input, output, session){
    tools <- register_rave_session(session)
    loader_project <- comp$get_dependent_component(loader_project_id)
    loader_subject <- comp$get_dependent_component(loader_subject_id)
    loader_electrodes <- comp$get_dependent_component(loader_electrodes_id)
    loader_reference <- comp$get_dependent_component(loader_reference_id)

    electrode_table <- shiny::reactive({
      if(!loader_subject$sv$is_valid()){ return() }

      project_name <- loader_project$current_value
      subject_code <- loader_subject$current_value
      electrodes_text <- loader_electrodes$current_value
      reference_name <- loader_reference$current_value
      subject_id <- sprintf("%s/%s", project_name, subject_code)
      subject <- comp$container$get_cache("loader_subject_instance", default = NULL)
      if(!inherits(subject, "RAVESubject") ||
         !identical(subject$subject_id, subject_id)) {

        subject <- raveio::as_rave_subject(subject_id, strict = FALSE)
        comp$container$set_cache(key = "loader_subject_instance",
                                 value = subject, expire_after = 10)
      }

      brain <- comp$container$get_cache("loader_subject_brain", default = NULL)
      if(!inherits(brain, "rave-brain") ||
         !identical(brain$subject_code, subject_code)){
        logger("Re-generate loader's brain", level = 'trace')
        brain <- raveio::rave_brain(subject, surfaces = 'pial')
      } else {
        logger("Using cached loader's brain", level = 'trace')
      }
      comp$container$set_cache(key = "loader_subject_brain",
                               value = brain, expire_after = 100)
      # TODO: brain is null
      if(is.null(brain)){ return() }

      electrodes <- dipsaus::parse_svec(electrodes_text)
      all_electrodes <- subject$electrodes
      valid_electrodes <- subject$valid_electrodes(reference_name = reference_name)
      val <- rep("Not Loading", length(all_electrodes))
      val[all_electrodes %in% electrodes] <- "Excluded"
      val[all_electrodes %in% electrodes & all_electrodes %in% valid_electrodes] <- "Loading"
      val <- factor(val, levels = c("Loading", "Excluded", "Not Loading"))
      tbl <- data.frame(
        Subject = subject$subject_code,
        Electrode = subject$electrodes,
        Value = val
      )

      tbl

    }) |>
      shiny::bindEvent(
        loader_project$current_value,
        loader_subject$current_value,
        loader_electrodes$current_value,
        loader_reference$current_value,
        ignoreNULL = TRUE, ignoreInit = TRUE
      )

    viewer <- shiny::reactive({

      shiny::invalidateLater(500)

      tbl <- electrode_table()
      if(!is.data.frame(tbl)){ return() }
      brain <- comp$container$get_cache("loader_subject_brain", default = NULL)
      if(!inherits(brain, "rave-brain")){ return() }

      brain$set_electrode_values(tbl)

      theme <- shidashi::get_theme(tools$theme_event)
      logger("Re-generate loader's viewer", level = 'trace')
      wg <- brain$plot(
        volumes = FALSE,
        start_zoom = 1,
        atlases = FALSE,
        side_canvas = FALSE,
        control_display = FALSE,
        # show_modal = TRUE,
        background = theme$background,
        palettes = list(Value = c("navy", "red", "gray80")),
        controllers = list(
          "Background Color" = theme$background,
          "Show Time" = FALSE
        )
      )
      # comp$container$set_cache("loader_subject_brain_instance", wg, expire_after = Inf)
      wg

    }) |>
      shiny::bindCache(
        shidashi::get_theme(tools$theme_event),
        electrode_table(),
        cache = "session"
      ) |>
      shiny::bindEvent(
        shidashi::get_theme(tools$theme_event),
        electrode_table(),
        ignoreNULL = TRUE
      )


    output$loader_3d_viewer <- threeBrain::renderBrain({
      wg <- viewer()
      shiny::validate(shiny::need(!is.null(wg), message = ""))

      return(wg)

    }) |>
      shiny::bindEvent(
        viewer(), ignoreNULL = FALSE, ignoreInit = FALSE
      )

  }

  comp

}



# component_container <- RAVEShinyComponentContainer$new(
#   module_id = 'power_explorer', pipeline_name = 'power_explorer',
#   pipeline_path = "/Users/dipterix/Dropbox (Personal)/projects/rave-pipelines/_pipelines/power_explorer",
#   settings_file = "settings.yaml"
# )
# component_container$add_component(loader_project <- presets_loader_project())
# component_container$add_component(loader_subject <- presets_loader_subject())
# component_container$add_component(comp)
# self <- comp
