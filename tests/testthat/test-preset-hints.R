# Input hints of preset inputs, and the loader state agents see (through
# shidashi's `shiny_input_info`)

test_that("a preset's hints come from its defaults, then the caller", {
  comp <- RAVEShinyComponent$new(id = "loader_epoch_name",
                                 varname = "epoch_choice")
  defaults <- c(main = "loader_mandatory",
                trial_starts_rel_to_event = "loader_optional")

  hint_of <- preset_hint_getter(comp, NULL, defaults)
  expect_identical(hint_of(), "loader_mandatory")
  expect_identical(hint_of("trial_starts_rel_to_event"), "loader_optional")
  expect_identical(hint_of("trial_ends"), "no_hint")

  # by input ID
  hint_of <- preset_hint_getter(
    comp, c(loader_epoch_name__trial_ends = "loader_forbidden"), defaults
  )
  expect_identical(hint_of("trial_ends"), "loader_forbidden")
  expect_identical(hint_of(), "loader_mandatory")

  # one value for every input
  hint_of <- preset_hint_getter(comp, "analysis_optional", defaults)
  expect_identical(hint_of(), "analysis_optional")
  expect_identical(hint_of("trial_ends"), "analysis_optional")
})

test_that("the project preset registers its input as loader_mandatory", {
  skip_on_cran()
  registered_hint <- function(...) {
    helpers <- shidashi:::mcp_wrapper_input_output()
    env <- new.env()
    env$.register_input <- helpers$input_helpers$register_input_specification
    comp <- presets_loader_project(env = env, ...)
    # `comp$ui_func` wraps the preset's UI function for a module container;
    # call the preset's own function instead
    preset_ui <- environment(comp$ui_func)$ui
    preset_ui(id = "loader_project_name", value = NULL, depends = NULL)
    spec <- helpers$input_helpers$get_input_specification()
    spec$hint[spec$inputId == "loader_project_name"]
  }
  expect_identical(registered_hint(), "loader_mandatory")
  expect_identical(registered_hint(hint = "loader_optional"), "loader_optional")
})

# test_that("module servers report whether the data loader is open", {
#   withr::local_options(list(shidashi.shared_id = NULL))
#   app_env <- new.env()
#   shidashi::init_app(app_env)
#   session <- shiny::MockShinySession$new()

#   register_loader_state(session)

#   state <- shidashi:::input_state_values(session)
#   expect_identical(state$loader_opened$value, FALSE)
#   expect_match(state$loader_opened$description, "data loader", fixed = TRUE)
# })
