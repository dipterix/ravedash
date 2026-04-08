testthat::test_that("registerShinySession", {capture.output({
  session <- shiny::MockShinySession$new()
  rave_id <- ravedash:::rand_string()

  # register_rave_session is deprecated; it should warn and return invisible()
  testthat::expect_warning(
    result <- ravedash::register_rave_session(session, .rave_id = rave_id),
    "deprecated"
  )
  testthat::expect_null(result)

  # Side effect: handler map should be initialized
  map <- session$userData$ravedash
  testthat::expect_s3_class(map, "fastmap2")
  testthat::expect_s3_class(map$handlers, "fastmap2")
  testthat::expect_s3_class(map$handlers$output_options, "fastmap2")

}, type = "message")})
