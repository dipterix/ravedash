#' @rdname rave-ui-preset
#' @export
presets_analysis_electrode_selector2 <- function(
  id = "electrode_text", varname = "analysis_electrodes",
  label = "Select Electrodes",
  loader_project_id = "loader_project_name",
  loader_subject_id = "loader_subject_code",
  pipeline_repository = "repository",
  start_simple = FALSE,
  multiple = TRUE
) {
  comp <- RAVEShinyComponent$new(id = id, varname = varname)
  comp$depends <- c(loader_project_id, loader_subject_id)
  comp$repository_name <- pipeline_repository

  category_str <- "category"                 # electrode_category_selector
  category_choices_str <- "category_choices" # "electrode_category_selector_choices"
  reset_str <- "reset"                       # "electrode_selectors_reset",
  selected_electrode_text_str <- "selected_electrode_text" # card_electrode_selector
  merge_hemisphere_str <- "merge_hemisphere_labels"
  download_str <- "download"
  btn_previous_str <- "previous"
  btn_next_str <- "next"

  css_class_optional <- ifelse(start_simple, "rave-optional soft-hidden", "rave-optional")

  comp$no_save <- c(reset_str, category_choices_str, selected_electrode_text_str, btn_previous_str, btn_next_str)

  # repository_name <- "repository"
  get_repo <- function(){

    data_loaded <- isTRUE(shiny::isolate(watch_data_loaded()))
    has_repository <- comp$container$data[['@has']](pipeline_repository)

    if(!data_loaded) {
      if( has_repository ) {
        comp$container$data[["@remove"]](pipeline_repository)
      }
      return(NULL)
    }

    if(!has_repository) {
      logger("Trying to get repository object...", level = "trace")
      repository <- raveio::pipeline_read(var_names = pipeline_repository,
                                          pipe_dir = comp$container$pipeline_path)
      comp$container$data[[pipeline_repository]] <- repository
    } else {
      repository <- comp$container$data[[pipeline_repository]]
    }
    if(!inherits(repository, "rave_repository")) {
      return(NULL)
    }
    repository
  }

  get_subject <- function(){
    repo <- get_repo()
    if(inherits(repo, "rave_repository")) {
      subject <- repo$subject
      return(subject)
    }
    return(NULL)
  }

  get_default <- function(sub_id, missing = NULL, use_cache = TRUE, constraint = NULL) {
    vname <- comp$get_sub_element_varnames(sub_id)
    subject <- get_subject()
    if(inherits(subject, "RAVESubject")) {
      missing <- subject$get_default(vname,
                                     default_if_missing = missing, simplify = TRUE)
    }
    comp$get_settings_value(use_cache = use_cache, default = missing,
                            key = vname, constraint = constraint)
  }

  # component_container$add_components(comp)

  comp$ui_func <- function(id, value, depends){

    input_card(
      toggle_advanced = TRUE,
      class_header = "shidashi-anchor",
      href = card_href(label, type = "input",
                       module_id = comp$container$module_id),
      title = shiny::tagList(
        label, " ",
        shiny::textOutput(comp$get_sub_element_id(selected_electrode_text_str,with_namespace = TRUE),
                          inline = TRUE, shiny::tags$small)
      ),
      shiny::div(
        class = css_class_optional,
        shiny::selectInput(
          inputId = comp$get_sub_element_id(category_str,
                                            with_namespace = TRUE),
          label = "Electrode categories",
          choices = ""
        ),
        shiny::selectInput(
          inputId = comp$get_sub_element_id(category_choices_str,
                                            with_namespace = TRUE),
          label = sprintf("Select electrode by category (%s-select)", ifelse(multiple, "multi", "single")),
          choices = "",
          multiple = multiple
        ),
        shiny::checkboxInput(
          inputId = comp$get_sub_element_id(merge_hemisphere_str,
                                            with_namespace = TRUE),
          label = "Merge LH/RH categories"
        )
      ),
      local({
        if(multiple) {
          shiny::textInput(
            inputId = id,
            label = "Select electrode by number",
            value = "",
            placeholder = "E.g. 1-30,55-60,88"
          )
        } else {
          shiny::tagList(
            shiny::selectInput(
              inputId = id,
              label = "Select electrode by number",
              choices = character()
            ),
            shiny::fluidRow(
              shiny::column(
                width = 6L,
                shiny::actionButton(
                  inputId = comp$get_sub_element_id(btn_previous_str,
                                                    with_namespace = TRUE),
                  label = shiny::tagList(shiny_icons$angle_double_left, " Previous")
                )
              ),
              shiny::column(
                width = 6L,
                shiny::actionButton(
                  inputId = comp$get_sub_element_id(btn_next_str,
                                                    with_namespace = TRUE),
                  label = shiny::tagList("Next ", shiny_icons$angle_double_right)
                )
              )
            )
          )
        }
      }),
      footer = shiny::div(
        class = css_class_optional,
        shiny::div(
          class = "form-group",
          shiny::actionLink(
            inputId = comp$get_sub_element_id(reset_str,
                                              with_namespace = TRUE),
            label = "Reset electrode selectors"
          )
        ),
        shiny::div(
          class = "form-group",
          shiny::downloadLink(
            outputId = comp$get_sub_element_id(download_str,
                                               with_namespace = TRUE),
            label = "Download copy of meta data for all electrodes"
          )
        )
      )
    )
  }

  comp$server_func <- function(input, output, session){

    comp$ready_to_collect <- function(){
      shiny::isolate(isFALSE(watch_loader_opened()))
    }

    # get pipeline's default, or subject's default, or program default
    reset <- function(...){
      logger("Updating {id}", level = "trace", use_glue = TRUE)
      repo <- get_repo()
      if(is.null(repo)) { return() }

      electrodes <- repo$electrode_list
      electrode_table <- repo$electrode_table
      electrode_table <- electrode_table[electrode_table$Electrode %in% electrodes, ]
      electrode_table_names <- names(electrode_table)

      electrode_text <- dipsaus::parse_svec(get_default(sub_id = NULL, missing = NULL))
      electrode_text <- electrode_text[electrode_text %in% repo$electrode_list]
      if(!length(electrode_text)){
        electrode_text <- electrodes[[1]]
      }
      if(!multiple) {
        electrode_text <- electrode_text[[1]]
      }
      electrode_text <- dipsaus::deparse_svec(electrode_text)

      # get category selector in the following sequence
      # 1. settings.yaml (pipeline settings)
      # 2. subject's default
      # 3. guess default
      electrode_category_selector <- get_default(
        sub_id = category_str, missing = c('freesurferlabel', "FSLabel",
                                           comp$get_sub_element_input(category_str)),
        constraint = electrode_table_names
      )

      logger("Updating `{id}__{category_str}`, value: {electrode_category_selector} {length(electrode_table_names)}", level = "trace", use_glue = TRUE)
      shiny::updateSelectInput(
        session = session,
        inputId = comp$get_sub_element_id(category_str,
                                          with_namespace = FALSE),
        choices = electrode_table_names,
        selected = electrode_category_selector
      )
      electrode_list_text <- dipsaus::deparse_svec(repo$electrode_list, collapse = ", ")
      logger("Updating `{id}`, value: {electrode_text}, label: Select electrode by number (currently loaded: {electrode_list_text})", level = "trace", use_glue = TRUE)

      if(multiple) {
        shiny::updateTextInput(
          session = session, inputId = comp$get_sub_element_id(with_namespace = FALSE),
          label = sprintf("Select electrode by number (currently loaded: %s)", electrode_list_text),
          value = electrode_text
        )
      } else {
        shiny::updateSelectInput(
          session = session, inputId = comp$get_sub_element_id(with_namespace = FALSE),
          label = sprintf("Select electrode by number (currently loaded: %s)", electrode_list_text),
          choices = as.character(repo$electrode_list),
          selected = electrode_text
        )
      }

    }

    dipsaus::sync_shiny_inputs(
      input, session,
      c(
        comp$get_sub_element_id(with_namespace = FALSE),
        comp$get_sub_element_id(with_namespace = FALSE,
                                sub_id = category_choices_str)
      ),
      uniform = list(
        function(electrode_text) {
          repository <- get_repo()
          if(is.null(repository)){ return(NULL) }
          electrodes <- dipsaus::parse_svec(electrode_text)
          electrodes <- electrodes[electrodes %in% repository$electrode_list]
          if(!length(electrodes)){ electrodes <- NULL }
          electrodes
        },
        function(category_selected) {
          if(!length(category_selected)){ return(NULL) }
          repository <- get_repo()
          if(is.null(repository)){ return(NULL) }
          category_name <- comp$get_sub_element_input(category_str)
          current_electrodes <- dipsaus::parse_svec(comp$current_value)
          current_electrodes <- current_electrodes[current_electrodes %in% repository$electrode_list]
          if(!length(current_electrodes)){ current_electrodes <- NULL }

          if(
            length(category_name) &&
            category_name %in% names(repository$electrode_table) &&
            length(repository$electrode_table[[category_name]])
          ) {
            choices <- repository$electrode_table[[category_name]]
            all_electrodes <- repository$electrode_table$Electrode
            expected_category <- choices[all_electrodes %in% current_electrodes]
            if(!setequal(expected_category, category_selected)){

              electrodes <- all_electrodes[
                choices %in% category_selected &
                  repository$electrode_table$isLoaded
              ]
              return(electrodes)
            }
          }

          return(current_electrodes)

        }
      ),
      updates = list(
        function(electrodes) {
          if(length(electrodes)){
            if(multiple) {
              new_value <- dipsaus::deparse_svec(electrodes)
              if(!identical(new_value, comp$current_value)){
                val <- dipsaus::deparse_svec(electrodes)
                logger("Updating `{id}`, value: {val}", level = "trace", use_glue = TRUE)
                shiny::updateTextInput(
                  session = session, inputId = id,
                  value = val
                )
              }
            } else {
              val <- as.character(electrodes[[1]])
              logger("Updating `{id}`, value: {val}", level = "trace", use_glue = TRUE)
              shiny::updateSelectInput(
                session = session, inputId = id,
                selected = val
              )
            }

          }
        },
        function(electrodes) {
          if(length(electrodes)){
            repository <- get_repo()
            if(is.null(repository)){ return(NULL) }
            input_str <- comp$get_sub_element_id(category_choices_str,
                                                 with_namespace = FALSE)
            category_name <- comp$get_sub_element_input(category_str)
            if(
              length(category_name) &&
              category_name %in% names(repository$electrode_table) &&
              length(repository$electrode_table[[category_name]])
            ) {
              choices <- repository$electrode_table[[category_name]]
              all_electrodes <- repository$electrode_table$Electrode
              expected_category <- choices[all_electrodes %in% electrodes]
              category_selected <- comp$get_sub_element_input(category_choices_str)
              if(!setequal(expected_category, category_selected)){

                if(!length(expected_category)){
                  expected_category <- character(0L)
                }

                logger("Updating `{id}__{category_choices_str}` ({length(expected_category)})",
                                 level = "trace", use_glue = TRUE)

                shiny::updateSelectInput(
                  session = session,
                  inputId = input_str,
                  selected = expected_category
                )
              }
            }

          }
        }
      )
    )


    initialize_with_new_data_reactive <- function(){
      shidashi::clear_notifications(
        class = "_presets_analysis_electrode_selector2_error_",
        session = session)
      repository <- get_repo()
      if(is.null(repository)){
        shidashi::show_notification(
          title = "Initialization Error",
          message = c(
            "Unable to initialize preset input `",
            id, "`. The container repository has not been set up yet. ",
            "This is a module error. Please contact the module author to ",
            "fix this issue."
          ),
          type = "warning", close = TRUE, autohide = FALSE,
          collapse = "", session = session,
          class = "_presets_analysis_electrode_selector2_error_"
        )
        return()
      }
      # ssss
      electrodes <- dipsaus::parse_svec(get_default(NULL, missing = ""))
      electrodes <- electrodes[electrodes %in% repository$electrode_list]
      if(!length(electrodes)){
        electrodes <- repository$electrode_list[[1]]
      }
      category <- comp$get_sub_element_input(category_str)
      electrode_table_names <- names(repository$electrode_table)
      if(!length(category) || !isTRUE(category %in% electrode_table_names)) {
        category <- get_default(
          sub_id = category_str, missing = c('freesurferlabel', "FSLabel"),
          constraint = electrode_table_names
        )
        logger("Updating `{id}__{category_str}`, value: {category} ({length(electrode_table_names)})",
                         level = "trace", use_glue = TRUE)
        shiny::updateSelectInput(
          session = session,
          inputId = comp$get_sub_element_id(category_str,
                                            with_namespace = FALSE),
          choices = electrode_table_names,
          selected = category
        )
      }

      choices <- character(0L)
      if(length(category) && isTRUE(category %in% electrode_table_names)) {
        choices <- repository$electrode_table[[category]]
        if(!length(choices)){ choices <- character(0L) }
      }

      # selected <- choices[repository$electrode_table$Electrode %in% electrodes]
      logger("Updating choices of `{id}__{category_choices_str}` ({length(choices)})", level = "trace", use_glue = TRUE)
      shiny::updateSelectInput(
        session = session,
        inputId = comp$get_sub_element_id(
          sub_id = category_choices_str,
          with_namespace = FALSE),
        choices = unique(choices),
        selected = character(0L)
      )

      if(length(electrodes) && !multiple) {
        electrodes <- electrodes[[1]]
      }

      v <- dipsaus::deparse_svec(electrodes)
      if(multiple && identical(v, comp$current_value)){
        v <- sprintf("%s ", v)
      }
      # logger("Updating `{id}`, value: {v}", level = "trace", use_glue = TRUE)
      # shiny::updateTextInput(
      #   session = session,
      #   inputId = id,
      #   value = v
      # )

      electrode_list_text <- dipsaus::deparse_svec(repository$electrode_list, collapse = ", ")
      logger("Updating `{id}`, value: {v}, label: Select electrode by number (currently loaded: {electrode_list_text})", level = "trace", use_glue = TRUE)
      if(multiple) {
        shiny::updateTextInput(
          session = session,
          inputId = id,
          label = sprintf("Select electrode by number (currently loaded: %s)",
                          electrode_list_text),
          value = v
        )
      } else {
        shiny::updateSelectInput(
          session = session,
          inputId = id,
          label = sprintf("Select electrode by number (currently loaded: %s)",
                          electrode_list_text),
          choices = as.character(repository$electrode_list),
          selected = as.character(v)
        )
      }

    }

    shiny::bindEvent(
      observe({
        initialize_with_new_data_reactive()
      }),
      comp$get_sub_element_input(category_str),
      # local_reactives$refresh,
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    shiny::bindEvent(
      observe({
        reset()
      }),
      comp$get_sub_element_input(reset_str),
      ignoreNULL = TRUE, ignoreInit = TRUE
    )
    output[[comp$get_sub_element_id(
      selected_electrode_text_str,
      with_namespace = FALSE
    )]] <- shiny::renderText({
      dipsaus::deparse_svec(dipsaus::parse_svec(comp$current_value))
    })

    shiny::bindEvent(
      observe({
        repository <- get_repo()
        if(is.null(repository)){ return(NULL) }
        if(!length(repository$electrode_list)) { return(NULL) }
        current_electrodes <- dipsaus::parse_svec(comp$current_value)
        if(length(current_electrodes) || any(current_electrodes %in% repository$electrode_list)) {
          elec <- current_electrodes[current_electrodes %in% repository$electrode_list]
          elec <- elec[[1]]
          idx <- which(repository$electrode_list == elec)
          if(idx == 1) {
            idx <- length(repository$electrode_list)
          } else {
            idx <- idx - 1
          }
          elec <- repository$electrode_list[[idx]]
        } else {
          elec <- repository$electrode_list[[1]]
        }
        shiny::updateSelectInput(
          session = session, inputId = id,
          selected = as.character(elec)
        )
      }),
      comp$get_sub_element_input(btn_previous_str),
      ignoreNULL = TRUE, ignoreInit = TRUE
    )

    shiny::bindEvent(
      observe({
        repository <- get_repo()
        if(is.null(repository)){ return(NULL) }
        if(!length(repository$electrode_list)) { return(NULL) }
        current_electrodes <- dipsaus::parse_svec(comp$current_value)
        if(length(current_electrodes) || any(current_electrodes %in% repository$electrode_list)) {
          elec <- current_electrodes[current_electrodes %in% repository$electrode_list]
          elec <- elec[[1]]
          idx <- which(repository$electrode_list == elec)
          if(idx == length(repository$electrode_list)) {
            idx <- 1
          } else {
            idx <- idx + 1
          }
          elec <- repository$electrode_list[[idx]]
        } else {
          elec <- repository$electrode_list[[1]]
        }
        shiny::updateSelectInput(
          session = session, inputId = id,
          selected = as.character(elec)
        )
      }),
      comp$get_sub_element_input(btn_next_str),
      ignoreNULL = TRUE, ignoreInit = TRUE
    )

    comp$set_tool("reset", reset, server_needed = TRUE)
    comp$set_tool("initialize_with_new_data", function(){
      shiny::isolate(initialize_with_new_data_reactive())
    }, server_needed = TRUE)

    comp

  }

  comp


}

