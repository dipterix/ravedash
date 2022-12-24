testthat::test_that("registerShinySession", {capture.output({
  session <- shiny::MockShinySession$new()
  rave_id <- ravedash:::rand_string()
  tools <- ravedash::register_rave_session(session, .rave_id = rave_id)

  testthat::expect_equal(tools$rave_id, rave_id)
  testthat::expect_true(shiny::is.reactivevalues(tools$theme_event))
  testthat::expect_true(shiny::is.reactivevalues(tools$rave_event))

  map <- session$userData$ravedash
  testthat::expect_s3_class(map, "fastmap2")

  testthat::expect_identical(tools$rave_event, map$rave_event)
  testthat::expect_identical(tools$theme_event, map$theme_event)

  a <- ravedash:::rand_string()
  ravedash::fire_rave_event(key = "a", value = a, global = FALSE, session = session)
  testthat::expect_identical(shiny::isolate(map$rave_event$a), a)


}, type = "message")})
