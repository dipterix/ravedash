create_module <- function(
  path, module_id, module_label, ..., pipeline_name = module_id, overwrite = FALSE){

  force(module_id)
  force(module_label)

  # path <- '~/Dropbox (Personal)/projects/rave-pipelines/'
  module_root <- file.path(path, "modules")
  module_path <- file.path(module_root, pipeline_name)

  template_root <- system.file("rstudio", "templates", "projects", "template_module",
                               package = "ravedash")

  stopifnot(template_root != '')

  raveio::pipeline_create_template(
    root_path = module_root,
    pipeline_name = pipeline_name,
    overwrite = overwrite,
    activate = FALSE,
    template_type = "rmd"
  )

  raveio::dir_create2(file.path(module_path, "R"))

  fs <- list.files(template_root, all.files = FALSE, full.names = FALSE, recursive = TRUE, include.dirs = FALSE, no.. = TRUE)

  for(f in fs) {
    s <- readLines(file.path(template_root, f))
    s <- gsub("\\{\\{[ ]*PIPELINE_NAME[ ]*\\}\\}", pipeline_name, s)
    s <- gsub("\\{\\{[ ]*MODULE_ID[ ]*\\}\\}", module_id, s)
    writeLines(s, file.path(module_path, f))
  }

  raveio::pipeline_build(pipe_dir = module_path)

  # add to module.yaml
  yaml <- file.path(path, "modules.yaml")
  if(file.exists(yaml)) {
    yaml_settings <- as.list(raveio::load_yaml(yaml))
    file.copy(yaml, sprintf("%s.bkup", yaml))
  } else {
    yaml_settings <- list(
      modules = list()
    )
  }

  item <- list(
    label = module_label,
    ...
  )
  item <- item[!names(item) %in% ""]
  yaml_settings$modules[[module_id]] <- item
  raveio::save_yaml(yaml_settings, yaml)

  invisible()
}
