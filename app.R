# app.R -- Deployment entry point for shinyapps.io
# This file sources all R/ files directly to avoid rsconnect package detection
# issues with golem-structured apps.
#
# For local development, use: shinyExametrika::run_app() or devtools::load_all()
# For deployment, run: rsconnect::deployApp()

# Load required libraries
library(shiny)
library(golem)
library(bslib)
library(config)
library(DT)
library(exametrika)
library(shiny.i18n)
library(shinyjs)
library(shinyWidgets)
library(waiter)
if (requireNamespace("ggExametrika", quietly = TRUE)) {
  library(ggExametrika)
}

# Override app_sys() to use local inst/ directory instead of system.file()
app_sys <- function(...) {
  file.path("inst", ...)
}

# Source all R files (except app_config.R which defines app_sys, and run_app.R)
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
skip_files <- c("R/app_config.R", "R/run_app.R")
for (f in r_files) {
  if (!f %in% skip_files) {
    source(f, local = TRUE)
  }
}

# Define get_golem_config (from app_config.R, using local app_sys)
get_golem_config <- function(
  value,
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE",
    Sys.getenv("R_CONFIG_ACTIVE", "default")
  ),
  use_parent = TRUE,
  file = app_sys("golem-config.yml")
) {
  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}

# Launch app
options("golem.app.prod" = TRUE)
shinyApp(
  ui = app_ui,
  server = app_server
)
