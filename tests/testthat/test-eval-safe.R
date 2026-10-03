# `safe_reactive()`: a reactive expression whose errors never reach its
# consumers. Shiny caches the condition a reactive raised and re-raises it to
# every consumer until the next invalidation, so the last good value has to be
# kept by the wrapper itself.

with_reactive_console <- function(expr) {
  shiny::reactiveConsole(TRUE)
  on.exit(shiny::reactiveConsole(FALSE), add = TRUE)
  force(expr)
}

test_that("safe_reactive returns the last good value when the expression errors", {
  with_reactive_console({
    source_val <- shiny::reactiveVal(1)
    r <- safe_reactive({
      v <- source_val()
      if (v < 0) { stop("boom") }
      v * 10
    })

    expect_equal(r(), 10)

    # the error is swallowed and the previous value stands
    source_val(-1)
    expect_equal(r(), 10)

    # a later success replaces it
    source_val(2)
    expect_equal(r(), 20)

    source_val(-2)
    expect_equal(r(), 20)
  })
})

test_that("safe_reactive yields `initial` before its first success", {
  with_reactive_console({
    r <- safe_reactive({ stop("never") }, initial = "init")
    expect_identical(r(), "init")
  })
})

test_that("safe_reactive lets shiny validation errors through", {
  with_reactive_console({
    r <- safe_reactive({
      shiny::req(FALSE)
      1
    })
    expect_error(r(), class = "shiny.silent.error")
  })
})

test_that("safe_reactive evaluates the expression in the caller's environment", {
  with_reactive_console({
    local_value <- 41
    r <- safe_reactive({ local_value + 1 })
    expect_equal(r(), 42)
  })
})

test_that("safe_reactive accepts a quoted expression and an environment", {
  with_reactive_console({
    env <- new.env()
    env$x <- 5
    r <- safe_reactive(quote(x * 2), env = env, quoted = TRUE)
    expect_equal(r(), 10)
  })
})
