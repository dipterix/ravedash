
#' @rdname rave-ui-preset
#' @export
presets_loader_3dviewer <- function(
  id = "loader_3d_viewer",
  height = "600px",
  loader_project_id = "loader_project_name",
  loader_subject_id = "loader_subject_code",
  loader_reference_id = "loader_reference_name",
  loader_electrodes_id = "loader_electrode_text",
  gadgets = c("standalone", "download")
) {
  comp <- RAVEShinyComponent$new(id = id)
  comp$depends <- c(loader_project_id, loader_subject_id, loader_electrodes_id, loader_reference_id)
  comp$no_save <- TRUE

  gadgets <- gadgets[gadgets %in% c("standalone", "download2")]

  comp$ui_func <- function(id, value, depends){
    output_gadget_container(
      threeBrain::threejsBrainOutput(
        outputId = id,
        height = height,
        reportSize = FALSE
      ),
      gadgets = gadgets
    )
  }
  comp$server_func <- function(input, output, session){
    tools <- register_rave_session(session)
    loader_project <- comp$get_dependent_component(loader_project_id)
    loader_subject <- comp$get_dependent_component(loader_subject_id)
    loader_electrodes <- comp$get_dependent_component(loader_electrodes_id)
    loader_reference <- comp$get_dependent_component(loader_reference_id)

    get_subject <- loader_subject$get_tool("get_subject")

    local_reactives <- shiny::reactiveValues(
      brain = NULL
    )

    brain_proxy <- threeBrain::brain_proxy(outputId = "loader_3d_viewer", session = session)

    get_loading_table <- function() {
      if(!loader_subject$sv$is_valid()){ return(NULL) }
      subject <- get_subject()

      all_electrodes <- subject$electrodes

      if(!length(all_electrodes)) { return(NULL) }

      subject_code <- subject$subject_code
      project_name <- subject$project_name
      electrodes_text <- loader_electrodes$current_value
      reference_name <- loader_reference$current_value

      electrodes <- dipsaus::parse_svec(electrodes_text)

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
    }
    electrode_table <- shiny::debounce(shiny::bindEvent(
      shiny::reactive({
        get_loading_table()
      }),
      loader_project$current_value,
      loader_subject$current_value,
      loader_electrodes$current_value,
      loader_reference$current_value,
      ignoreNULL = FALSE, ignoreInit = FALSE
    ), millis = 500, domain = session)

    shiny::bindEvent(
      observe({
        tbl <- electrode_table()
        if(!is.data.frame(tbl)) { return() }
        brain_proxy$set_electrode_data(data = tbl, palettes = list(Value = c("pink", "orange", "gray80")))
      }),
      local_reactives$brain,
      electrode_table(),
      ignoreNULL = FALSE, ignoreInit = TRUE
    )

    shiny::bindEvent(
      observe({
        theme <- shidashi::get_theme(tools$theme_event)
        brain_proxy$set_background(theme$background)
      }),
      shidashi::get_theme(tools$theme_event),
      ignoreNULL = TRUE, ignoreInit = TRUE
    )



    viewer_output_id <- "loader_3d_viewer"
    register_output(
      shiny::bindEvent(
        threeBrain::renderBrain({

          shiny::validate(
            shiny::need(loader_subject$sv$is_valid(), message = "")
          )

          subject <- get_subject()

          brain <- raveio::rave_brain(subject, surfaces = 'pial', overlays = NULL, annotations = NULL)

          if(is.null(brain)) {
            stop("No brain instance")
          }

          tbl <- get_loading_table()
          if(is.data.frame(tbl) && nrow(tbl)) {
            brain$set_electrode_values(tbl)
          }

          theme <- shidashi::get_theme(tools$theme_event)

          logger("Re-generate loader's viewer", level = 'trace')

          new_brain <- shiny::isolate(is.null(local_reactives$brain))

          local_reactives$brain <- brain

          if( new_brain ) {
            camera_position <- c(0, 0, 500)
            camera_up <- c(0, 1, 0)

            brain$plot(
              volumes = FALSE,
              atlases = FALSE,
              side_canvas = FALSE,
              control_display = FALSE,
              start_zoom = 1,
              background = theme$background,
              palettes = list(Value = c("pink", "orange", "gray80")),
              controllers = list(
                "Background Color" = theme$background,
                "Outlines" = "on",
                "Show Time" = FALSE
              ),
              custom_javascript = sprintf(
                '
                // Remove the focus box
                if( canvas.focus_box ) {
                  canvas.focus_box.visible = false;
                }

                // set camera
                canvas.mainCamera.position.set( %f , %f , %f );
                canvas.mainCamera.up.set( %f , %f , %f );
                canvas.mainCamera.updateProjectionMatrix();

                // Let shiny know the viewer is ready
                if( window.Shiny ) {
                  window.Shiny.setInputValue("%s", "%f");
                }

                // Force render one frame (update the canvas)
                canvas.needsUpdate = true;
                ',
                camera_position[[1]], camera_position[[2]], camera_position[[3]],
                camera_up[[1]], camera_up[[2]], camera_up[[3]],
                session$ns( viewer_output_id ),
                Sys.time()
              )
            )
          } else {

            brain$render(
              outputId = viewer_output_id,
              volumes = FALSE,
              atlases = FALSE,
              side_canvas = FALSE,
              control_display = FALSE,
              # show_modal = TRUE,
              # start_zoom = 1,
              # background = theme$background,
              palettes = list(Value = c("pink", "orange", "gray80")),
              controllers = list(
                "Background Color" = theme$background,
                "Outlines" = "on",
                "Show Time" = FALSE
              )
            )

          }

        }),
        loader_project$current_value,
        loader_subject$current_value,
        loader_subject$sv$is_valid(),
        ignoreNULL = FALSE, ignoreInit = FALSE
      ),
      outputId = viewer_output_id,
      export_type = "3dviewer",
      session = session
    )

  }

  comp

}

#' @rdname rave-ui-preset
#' @export
presets_loader_3dviewer2 <- function(
    id = "loader_3d_viewer",
    height = "600px",
    loader_project_id = "loader_project_name",
    loader_subject_id = "loader_subject_code",
    loader_electrodes_id = "loader_electrode_text",
    gadgets = c("standalone", "download")
) {
  comp <- RAVEShinyComponent$new(id = id)
  comp$depends <- c(loader_project_id, loader_subject_id, loader_electrodes_id)
  comp$no_save <- TRUE

  gadgets <- gadgets[gadgets %in% c("standalone", "download2")]

  comp$ui_func <- function(id, value, depends){
    output_gadget_container(
      threeBrain::threejsBrainOutput(
        outputId = id,
        height = height,
        reportSize = FALSE
      ),
      gadgets = gadgets
    )
  }
  comp$server_func <- function(input, output, session){
    tools <- register_rave_session(session)
    loader_project <- comp$get_dependent_component(loader_project_id)
    loader_subject <- comp$get_dependent_component(loader_subject_id)
    loader_electrodes <- comp$get_dependent_component(loader_electrodes_id)

    get_subject <- loader_subject$get_tool("get_subject")

    local_reactives <- shiny::reactiveValues(
      brain_flag = NULL
    )

    brain_proxy <- threeBrain::brain_proxy(outputId = "loader_3d_viewer", session = session)

    get_loading_table <- function() {
      if(!loader_subject$sv$is_valid()){ return(NULL) }
      subject <- get_subject()

      all_electrodes <- subject$electrodes

      if(!length(all_electrodes)) { return(NULL) }

      subject_code <- subject$subject_code
      project_name <- subject$project_name
      electrodes_text <- loader_electrodes$current_value

      electrodes <- dipsaus::parse_svec(electrodes_text)

      valid_electrodes <- subject$electrodes
      val <- rep("Not Loading", length(all_electrodes))
      # val[all_electrodes %in% electrodes] <- "Excluded"
      val[all_electrodes %in% electrodes & all_electrodes %in% valid_electrodes] <- "Loading"
      val <- factor(val, levels = c("Loading", "Not Loading"))
      tbl <- data.frame(
        Subject = subject$subject_code,
        Electrode = subject$electrodes,
        Value = val
      )

      tbl
    }

    electrode_table <- shiny::debounce(shiny::bindEvent(
      shiny::reactive({
        get_loading_table()
      }),
      loader_project$current_value,
      loader_subject$current_value,
      loader_electrodes$current_value,
      ignoreNULL = FALSE, ignoreInit = FALSE
    ), millis = 500, domain = session)

    shiny::bindEvent(
      observe({
        tbl <- electrode_table()
        if(!is.data.frame(tbl)) { return() }
        if(!length(tbl$Value)) { return() }
        # values <- unique(tbl$Value)
        # if("Loading" %in% c(values)) {
        #   palettes <- list(Value = dipsaus::col2hexStr(c("orange", "gray80")))
        # } else {
        #   palettes <- list(Value = dipsaus::col2hexStr(c("gray80", "gray80")))
        # }
        brain_proxy$set_electrode_data(data = tbl)
                                       # , palettes = palettes, clear_first = TRUE)
      }),
      local_reactives$brain_flag,
      electrode_table(),
      ignoreNULL = FALSE, ignoreInit = FALSE
    )

    shiny::bindEvent(
      observe({
        theme <- shidashi::get_theme(tools$theme_event)
        brain_proxy$set_background(theme$background)
      }),
      shidashi::get_theme(tools$theme_event),
      ignoreNULL = TRUE, ignoreInit = TRUE
    )

    viewer_output_id <- "loader_3d_viewer"
    register_output(
      shiny::bindEvent(
        threeBrain::renderBrain({

          shiny::validate(
            shiny::need(loader_subject$sv$is_valid(), message = "")
          )

          subject <- get_subject()

          brain <- raveio::rave_brain(subject, surfaces = 'pial', overlays = NULL, annotations = NULL)
          new_brain <- shiny::isolate(is.null(local_reactives$brain))

          if(is.null(brain)) {
            local_reactives$brain_flag <- NULL
            stop("No brain instance")
          }

          tbl <- get_loading_table()
          if(is.data.frame(tbl) && nrow(tbl)) {
            brain$set_electrode_values(tbl)
          }

          theme <- shidashi::get_theme(tools$theme_event)

          logger("Re-generate loader's viewer", level = 'trace')

          local_reactives$brain_flag <- Sys.time()

          if( new_brain ) {
            camera_position <- c(0, 0, 500)
            camera_up <- c(0, 1, 0)

            brain$plot(
              volumes = FALSE,
              atlases = FALSE,
              side_canvas = FALSE,
              control_display = FALSE,
              start_zoom = 1,
              background = theme$background,
              palettes = list(Value = c("orange", "gray80")),
              controllers = list(
                "Background Color" = theme$background,
                "Outlines" = "on",
                "Show Time" = FALSE
              ),
              custom_javascript = sprintf(
                '
                // Remove the focus box
                if( canvas.focus_box ) {
                  canvas.focus_box.visible = false;
                }

                // set camera
                canvas.mainCamera.position.set( %f , %f , %f );
                canvas.mainCamera.up.set( %f , %f , %f );
                canvas.mainCamera.updateProjectionMatrix();

                // Let shiny know the viewer is ready
                if( window.Shiny ) {
                  window.Shiny.setInputValue("%s", "%f");
                }

                // Force render one frame (update the canvas)
                canvas.needsUpdate = true;
                ',
                camera_position[[1]], camera_position[[2]], camera_position[[3]],
                camera_up[[1]], camera_up[[2]], camera_up[[3]],
                session$ns( viewer_output_id ),
                Sys.time()
              )
            )
          } else {

            brain$render(
              outputId = viewer_output_id,
              volumes = FALSE,
              atlases = FALSE,
              side_canvas = FALSE,
              control_display = FALSE,
              # show_modal = TRUE,
              # start_zoom = 1,
              # background = theme$background,
              palettes = list(Value = c("pink", "orange", "gray80")),
              controllers = list(
                "Background Color" = theme$background,
                "Outlines" = "on",
                "Show Time" = FALSE
              )
            )

          }

        }),
        loader_project$current_value,
        loader_subject$current_value,
        loader_subject$sv$is_valid(),
        ignoreNULL = FALSE, ignoreInit = FALSE
      ),
      outputId = viewer_output_id,
      export_type = "3dviewer",
      session = session
    )
  }

  comp

}
