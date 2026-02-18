# Building a Prod-Ready, Robust Shiny Application.
#
# Each step is optional.
#

# 1. Set options here
golem::set_golem_options()

# 2. Fill in the DESCRIPTION ----
golem::fill_desc(
  pkg_name = "shinyExametrika",
  pkg_title = "A Shiny GUI for the exametrika Psychometric Package",
  pkg_description = "Provides a web-based graphical user interface for the exametrika psychometric analysis package.",
  authors = person(
    given = "Koji",
    family = "Kosugi",
    email = "kosugitti@gmail.com",
    role = c("aut", "cre"),
    comment = c(ORCID = "0000-0002-9014-738X")
  ),
  repo_url = "https://github.com/kosugitti/shinyExametrika",
  pkg_version = "0.0.0.9000"
)

# 3. Set common Files ----
usethis::use_mit_license("Koji Kosugi")
golem::use_recommended_tests()

# 4. If you want to change the favicon
# golem::use_favicon(path = "path/to/ico")
