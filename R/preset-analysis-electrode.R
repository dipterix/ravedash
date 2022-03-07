#' @export
presets_analysis_electrode_selector2 <- function(
  id = "electrode_text", varname = "analysis_electrodes",
  label = "Select Electrodes",
  loader_project_id = "loader_project_name",
  loader_subject_id = "loader_subject_code",
  pipeline_repository = "repository"
) {
  comp = RAVEShinyComponent$new(id = id, varname = varname)
  comp$depends <- c(loader_project_id, loader_subject_id)
  comp$no_save <- c("electrode_selectors_reset",
                    "electrode_category_selector_choices")

  # repository_name <- "repository"
  get_repo <- function(){
    if(!comp$container$data[['@has']](pipeline_repository)) {
      repository <- raveio::pipeline_read(var_names = pipeline_repository,
                                          pipe_dir = comp$container$pipeline_path)
      comp$container$data[[pipeline_repository]] <- repository
    } else {
      repository <- comp$container$data[[pipeline_repository]]
    }
    if(!inherits(repository, "rave_prepare_subject")) {
      return(NULL)
    }
    repository
  }

  # component_container$add_components(comp)

  comp$ui_func <- function(id, value, depends){

    comp$ready_to_collect <- function(){
      shiny::isolate(isFALSE(watch_loader_opened()))
    }

    ravedash::input_card(
      toggle_advanced = TRUE,
      class_header = "shidashi-anchor",
      title = shiny::tagList(
        label, " ",
        shiny::textOutput(comp$get_sub_element_id("card_electrode_selector",
                                                  with_namespace = TRUE),
                          inline = TRUE, shiny::tags$small)
      ),
      shiny::div(
        class = "rave-optional",
        shiny::selectInput(
          inputId = comp$get_sub_element_id("electrode_category_selector",
                                            with_namespace = TRUE),
          label = "Electrode categories",
          choices = ""
        ),
        shiny::selectInput(
          inputId = comp$get_sub_element_id("electrode_category_selector_choices",
                                            with_namespace = TRUE),
          label = "Select electrode by category (multi-select)",
          choices = "",
          multiple = TRUE
        ),
        shiny::checkboxInput(
          inputId = comp$get_sub_element_id("merge_hemisphere_labels",
                                            with_namespace = TRUE),
          label = "Merge LH/RH categories"
        )
      ),
      shiny::textInput(
        inputId = id,
        label = "Select electrode by number",
        value = "",
        placeholder = "E.g. 1-30,55-60,88"
      ),
      footer = shiny::div(
        class = "rave-optional",
        shiny::div(
          class = "form-group",
          shiny::actionLink(
            inputId = comp$get_sub_element_id("electrode_selectors_reset",
                                              with_namespace = TRUE),
            label = "Reset electrode selectors"
          )
        ),
        shiny::div(
          class = "form-group",
          shiny::downloadLink(
            outputId = comp$get_sub_element_id("electrode_csv_download",
                                               with_namespace = TRUE),
            label = "Download copy of meta data for all electrodes"
          )
        )
      )
    )
  }

  comp$server_func <- function(input, output, session){

    # get pipeline's default, or subject's default, or program default
    reset_electrode_selectors <- function(from_pipeline = TRUE){
      logger("Reset electrode selectors", level = "trace")
      repo <- get_repo()
      if(is.null(repo)) {
        return()
      }

      electrodes <- repo$electrode_list
      electrode_table <- repo$electrode_table
      electrode_table <- electrode_table[electrode_table$Electrode %in% electrodes, ]
      electrode_table_names <- names(electrode_table)

      if(from_pipeline){
        electrode_text <- comp$get_settings_value(default = dipsaus::deparse_svec(electrodes))
        electrode_category_selector <- comp$get_settings_value(
          key = "electrode_category_selector",
          default = c('freesurferlabel', "FSLabel",
                      input$electrode_category_selector),
          constraint = electrode_table_names
        )
      } else {
        electrode_text <- dipsaus::deparse_svec(electrodes)
        electrode_category_selector <- repo$subject$get_default("electrode_category_selector", default_if_missing = c('freesurferlabel', "FSLabel")) %OF% electrode_table_names
      }

      ravedash::logger("Initializing `electrode_category_selector`: {electrode_category_selector}", level = "trace", use_glue = TRUE)
      shiny::updateSelectInput(
        session = session,
        inputId = comp$get_sub_element_id("electrode_category_selector",
                                          with_namespace = FALSE),
        choices = electrode_table_names,
        selected = electrode_category_selector
      )
      ravedash::logger("Initializing `electrode_text`: {electrode_text}", level = "trace", use_glue = TRUE)
      shiny::updateTextInput(
        session = session, inputId = comp$get_sub_element_id(with_namespace = FALSE),
        label = sprintf("Select electrode by number (currently loaded: %s)", dipsaus::deparse_svec(repo$electrode_list)),
        value = electrode_text
      )

    }

    dipsaus::sync_shiny_inputs(
      input, session,
      c(
        comp$get_sub_element_id(with_namespace = FALSE),
        comp$get_sub_element_id(with_namespace = FALSE,
                                sub_id = "electrode_category_selector_choices")
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
          category_name <- comp$get_sub_element_input("electrode_category_selector")
          current_electrodes <- dipsaus::parse_svec(
            comp$current_value
          )
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
            new_value <- dipsaus::deparse_svec(electrodes)
            if(!identical(new_value, comp$current_value)){
              val <- dipsaus::deparse_svec(electrodes)
              ravedash::logger("Updating `{id}` - {val}", level = "trace", use_glue = TRUE)
              shiny::updateTextInput(
                session = session, inputId = id,
                value = val
              )
            }
          }
        },
        function(electrodes) {
          if(length(electrodes)){
            repository <- get_repo()
            if(is.null(repository)){ return(NULL) }
            input_str <- comp$get_sub_element_id("electrode_category_selector_choices",
                                                 with_namespace = FALSE)
            category_name <- comp$get_sub_element_input("electrode_category_selector")
            if(
              length(category_name) &&
              category_name %in% names(repository$electrode_table) &&
              length(repository$electrode_table[[category_name]])
            ) {
              choices <- repository$electrode_table[[category_name]]
              all_electrodes <- repository$electrode_table$Electrode
              expected_category <- choices[all_electrodes %in% electrodes]
              category_selected <- comp$get_sub_element_input("electrode_category_selector_choices")
              if(!setequal(expected_category, category_selected)){

                if(!length(expected_category)){
                  expected_category <- character(0L)
                }

                ravedash::logger("Updating `{input_str}` ({length(expected_category)})",
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
      electrodes <- dipsaus::parse_svec(comp$get_settings_value(default = comp$current_value))
      electrodes <- electrodes[electrodes %in% repository$electrode_list]
      if(!length(electrodes)){
        electrodes <- repository$electrode_list
      }
      category <- comp$get_sub_element_input("electrode_category_selector")

      choices <- character(0L)
      if(length(category) && isTRUE(category %in% names(repository$electrode_table))) {
        choices <- repository$electrode_table[[category]]
        if(!length(choices)){ choices <- character(0L) }
      }

      input_str <- comp$get_sub_element_id(
        sub_id = "electrode_category_selector_choices",
        with_namespace = FALSE)
      # selected <- choices[repository$electrode_table$Electrode %in% electrodes]
      ravedash::logger("Initializing choices of `{input_str}`", level = "trace", use_glue = TRUE)
      shiny::updateSelectInput(
        session = session,
        inputId = comp$get_sub_element_id(
          sub_id = "electrode_category_selector_choices",
          with_namespace = FALSE),
        choices = unique(choices),
        selected = character(0L)
      )

      v <- dipsaus::deparse_svec(electrodes)
      if(identical(v, comp$current_value)){
        v <- sprintf("%s ", v)
      }
      shiny::updateTextInput(
        session = session,
        inputId = id,
        value = v
      )
    }

    observe({
      initialize_with_new_data_reactive()
    }) |>
      shiny::bindEvent(
        comp$get_sub_element_input("electrode_category_selector"),
        # local_reactives$refresh,
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

    observe({
      reset_electrode_selectors()
    }) |>
      shiny::bindEvent(
        comp$get_sub_element_input("electrode_selectors_reset"),
        ignoreNULL = TRUE, ignoreInit = TRUE
      )
    output[[comp$get_sub_element_id(
      "card_electrode_selector",
      with_namespace = FALSE
    )]] <- shiny::renderText({
      dipsaus::deparse_svec(dipsaus::parse_svec(comp$current_value))
    })

    comp$set_tool("reset_electrode_selectors", reset_electrode_selectors, server_needed = TRUE)
    comp$set_tool("initialize_with_new_data", function(){
      shiny::isolate(initialize_with_new_data_reactive())
    }, server_needed = TRUE)

    comp

  }

  comp


}

