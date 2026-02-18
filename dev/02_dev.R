# Development helpers
#
# Add modules, functions, and external resources.

# Module templates ----
# golem::add_module(name = "ctt", with_test = TRUE)
# golem::add_module(name = "irt", with_test = TRUE)

# Function templates ----
# golem::add_fct("helpers", with_test = TRUE)
# golem::add_utils("helpers", with_test = TRUE)

# External resources ----
# golem::add_css_file("custom")
# golem::add_js_file("custom")

# Dependencies ----
usethis::use_package("exametrika", min_version = "1.9.0")
usethis::use_package("bslib")
usethis::use_package("DT")
usethis::use_package("shiny.i18n")
usethis::use_package("shinyjs")
usethis::use_package("shinyWidgets")
usethis::use_package("waiter")

# Document and reload ----
# golem::document_and_reload()
