#' Create report wizard to be used within the interactive modules
#' @param pipeline \code{ravepipeline} pipeline
#' @param session shiny session
#' @returns A list of functions: \code{launch} with argument \code{subject} to
#' be called when users want to pop up a wizard allowing users to choose
#' reports; \code{generate} with arguments \code{subject} and
#' \code{report_names} to generate reports
#' @export
create_report_wizard <- function(pipeline, session = shiny::getDefaultReactiveDomain()) {
  stopifnot(!is.null(session))

  available_reports <- pipeline$available_reports
  if(!length(available_reports)) { return() }

  labels <- unname(unlist(lapply(available_reports, "[[", "label")))
  if(!length(labels)) { return() }

  ns <- session$ns

  local_data <- new.env(parent = emptyenv())

  generate <- function(subject, report_names) {
    nms <- unname(unlist(lapply(available_reports, "[[", "name")))
    reports <- available_reports[nms %in% report_names]

    if(!length(reports)) {
      show_notification(
        title = "No report to generate!",
        type = "warning",
        message = "There is no report selected. Please choose one.",
        close = TRUE,
        autohide = TRUE,
        session = session,
        class = ns("_report_wizard_notif")
      )
      return()
    }

    subject <- ravecore::as_rave_subject(subject)

    clear_notifications(session = session, class = ns("_report_wizard_notif"))

    report_promises <- lapply(reports, function(report_item) {
      job_id <- pipeline$generate_report(
        name = report_item$name,
        subject = subject
      )
      promise <- ravepipeline::as.promise(job_id)
      promise$then(onFulfilled = function(path) {
        url <- NULL
        try({
          params <- as.list(attr(path, "params"))
          params$module <- "standalone_report"
          params$type <- "widget"
          params$project_name <- subject$project_name
          params$subject_code <- subject$subject_code
          if(!length(params$report_filename)) {
            params$report_filename <- basename(dirname(path))
          }
          params <- unlist(lapply(names(params), function(nm) {
            sprintf("%s=%s", nm, htmltools::urlEncodePath(params[[nm]]))
          }))
          params <- paste(params, collapse = "&")
          url <- sprintf("/?%s", params)
        })
        try({
          clear_notifications(class = ns("_report_wizard_notif"), session = session)
          show_notification(
            title = "Report generated!",
            type = "default",
            message = shiny::div(
              shiny::p(
                sprintf("A report `%s` has been generated at:", report_item$label)
              ),
              shiny::p(
                shiny::tags$code(
                  class = "bg-secondary",
                  path
                )
              ),
              if(!is.null(url)) {
                shiny::a(target = "_blank", href = url, class = "btn btn-sm btn-success",
                         shiny::span("View report ", shiny_icons$external_link))
              }
            ),
            close = TRUE,
            autohide = FALSE
          )
        })
      }, onRejected = function(e) {
        try({
          clear_notifications(class = ns("_report_wizard_notif"), session = session)
          ravepipeline::logger_error_condition(e)
          error_notification(
            e,
            title = sprintf("Error in report - %s", report_item$label),
            class = ns("_report_wizard_notif")
          )
        })
      })
    })

    # Avoid GC
    local_data$report_promises <- report_promises

    shiny::removeModal(session = session)

    clear_notifications(class = ns("_report_wizard_notif"), session = session)
    show_notification(
      title = "Report(s) scheduled",
      message = shiny::div(
        shiny::p(
          "Report(s) scheduled. Please check the subject report directory later: \n",
          subject$report_path,
          " \nFeel free to dismiss this message."
        )
      ),
      autohide = FALSE,
      type = "white",
      class = ns("_report_wizard_notif"),
      session = session,
      close = TRUE
    )
  }

  handler_generate <- shiny::bindEvent(
    safe_observe({

      report_labels <- session$input$`_report_label`
      labels <- unname(unlist(lapply(available_reports, "[[", "label")))
      reports <- available_reports[labels %in% report_labels]
      report_names <- lapply(reports, "[[", "name")
      generate(local_data$subject_id, report_names)

    }, domain = session),
    session$input$`_report_generate`,
    ignoreNULL = TRUE, ignoreInit = TRUE)

  launch <- function(subject, ..., multiple = TRUE) {
    subject <- ravecore::as_rave_subject(subject)
    local_data$subject_id <- subject$subject_id

    settings <- pipeline$get_settings()
    settings_yaml <- ""
    conn <- textConnection("settings_yaml", open = "w", local = TRUE)
    ravepipeline::save_yaml(settings, conn)
    close(conn)

    current_selection <- shiny::isolate(as.character(settings$input$`_report_label`))
    if(!length(current_selection)) {
      current_selection <- labels
    }

    shiny::showModal(shiny::modalDialog(
      title = "Generate reports",
      size = "l",
      easyClose = FALSE,

      shiny::fluidRow(
        shiny::column(
          width = 12L,
          ...
        ),
        shiny::column(
          width = 12L,
          shiny::selectInput(
            inputId = ns("_report_label"),
            label = "Choose one or more reports to generate",
            choices = labels,
            selected = current_selection,
            multiple = multiple, width = "100%"
          )
        ),
        shiny::column(
          width = 12L,
          shiny::tags$details(
            shiny::tags$summary("See current inputs:"),
            shiny::div(
              class = "fill-width",
              shiny::HTML(sprintf("<pre><code>%s</code></pre>",
                                  paste(settings_yaml, collapse = "\n")))
            )
          )
        )
      ),

      footer = shiny::tagList(
        shiny::modalButton("Dismiss"),
        dipsaus::actionButtonStyled(inputId = ns("_report_generate"), label = "Generate reports")
      )
    ))

  }

  attr(launch, "local_data") <- local_data
  attr(launch, "handler_generate") <- handler_generate

  list(
    launch = launch,
    generate = generate
  )

}
