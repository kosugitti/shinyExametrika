# shinyExametrika (development version)

## Changes

### shinyapps.io deployment support (2026-02-23)

- Created `app.R` deployment entry point for shinyapps.io
  - Sources R/ files directly instead of using `pkgload::load_all()` to avoid rsconnect 1.7.0 package source detection issues with golem-structured apps
  - Overrides `app_sys()` to use local `inst/` directory path
  - Loads required libraries explicitly (shiny, golem, bslib, exametrika, ggExametrika, etc.)
- Successfully deployed to shinyapps.io: https://kosugitti.shinyapps.io/shinyExametrika/
  - Account: kosugitti (free plan)
  - All 9 tabs operational: Data, Descriptives, CTT, IRT, GRM, LCA, LRA, Biclustering, IRM
  - EN/JA language switch functional
- Installed `rsconnect` package (v1.7.0) for deployment management
- Note: DESCRIPTION/NAMESPACE files are excluded from the shinyapps.io deployment bundle to prevent rsconnect from treating the golem app as a package dependency

### Remove non-ASCII characters for CRAN compliance (2026-02-23)

- Replaced all Japanese comments in R/ files with English equivalents (14 files, ~300 lines)
  - Roxygen documentation (`#'`): all translated to English
  - Section separator comments (`#`): all translated to English
  - Inline code comments: all translated to English
- Fixed `grepl()` pattern in `R/mod_grm.R` to remove non-ASCII characters from the regex
- Rewrote NEWS.md entirely in English (was fully in Japanese)
- Replaced em dashes (U+2014) with `--` in README.md
- Translated Japanese comments in tests/ to English
- Verified: DESCRIPTION, NAMESPACE, man/ were already clean
- All changes confirmed: zero non-ASCII characters remain in package source

### CI support for GitHub-hosted package installation (2026-02-23)

- Added `Remotes` field to `DESCRIPTION`: specifies installation of exametrika and ggExametrika from GitHub repositories
  - exametrika >= 1.9.0 is not yet on CRAN, so the GitHub version is needed for CI
  - ggExametrika is also not on CRAN; specified similarly
  - `r-lib/actions/setup-r-dependencies@v2` reads `Remotes` automatically

### plotRMP_gg workaround removal (2026-02-23)

- `R/mod_lra.R`: replaced manual ggplot2 rendering code for RMP plots with `ggExametrika::plotRMP_gg()` call
  - ggExametrika v0.0.29 completed `$n_rank` / `$n_class` support, enabling correct operation with LRA objects
  - Added individual student selection for RMP in base plot fallback
- `R/mod_biclustering.R` (feature/mod-biclustering branch): similarly replaced manual RMP rendering with `plotRMP_gg()`
  - Unified `req()` conditions for CMP/RMP student selector
  - Unified CMP/RMP branching in base plot fallback

### CI / test environment setup (2026-02-23)

- `.github/workflows/R-CMD-check.yaml` newly added: automated R CMD check via GitHub Actions
  - Runs automatically on push (main / develop) and on pull requests
  - Checks on 3 environments: macOS-latest + ubuntu-latest (release / devel)
  - Uses r-lib/actions v2
- `tests/testthat.R` newly added: testthat test runner
- `tests/testthat/test-golem-recommended.R` newly added: golem-recommended basic tests
  - Checks existence and type of app_ui / app_server / app_sys / golem-config
- `tests/testthat/test-fct_analysis.R` newly added: unit tests for common helper functions
  - safe_field: new name priority / old name fallback / NULL when undefined
  - extract_fit_indices: handling of ModelFit object / data.frame input

### LCA / LRA module addition (2026-02-20)

- `R/mod_lca.R` newly added: LCA (Latent Class Analysis) module
  - Sidebar: class count slider (2-10) + run button
  - Results tab: fit indices / class profile (IRP) / class summary / student class membership
  - Item Fit tab: item fit indices table
  - Plots tab: IRP (with item selection) / TRP / LCD / CMP (with student selection)
  - CSV download (IRP, Students) + plot PNG download
  - ggExametrika preferred, base plot fallback supported
- `R/mod_lra.R` newly added: LRA (Latent Rank Analysis) module
  - Sidebar: rank slider / estimation method (GTM/SOM) / monotone increasing constraint checkbox
  - Results tab: fit indices / IRP table / IRP Index / rank summary / student rank membership
  - Item Fit tab: item fit indices table
  - Plots tab: IRP (with item selection) / TRP / LRD / RMP (with student selection, manual ggplot2 rendering)
  - RMP is manually rendered in ggplot2 to work around bugs in both exametrika and ggExametrika
- `R/app_ui.R`: switched LCA / LRA tabs from placeholder to actual modules
- `R/app_server.R`: added mod_lca_server / mod_lra_server
- `inst/i18n/translation.json`: added translation keys for LCA / LRA

- Added NEWS.md recording rule to CLAUDE.md (permanent rule to record all changes in NEWS.md)
- `R/fct_analysis.R` newly created: added common helper functions for analysis results
  - `extract_ability()`: ability estimate extraction function supporting both IRT (`$ability` data.frame) and GRM (`$EAP`/`$MAP`/`$PSD` individual vectors) formats
  - `extract_fit_indices()`: fit indices extraction function supporting both TestFitIndices named list and data.frame formats
- `R/mod_irt.R`: added `is.data.frame()` branching + `tryCatch()` defensive code for TestFitIndices display (strengthened to same robustness as GRM module). Unified ability estimate display/download to common helper function `extract_ability()`
- `R/mod_grm.R`: unified fit indices and ability estimate display/download to common helper functions `extract_fit_indices()` / `extract_ability()` (eliminated logic duplication)

### Following unified return value structure of exametrika v1.9.0 (2026-02-19)

- `R/fct_analysis.R`:
  - Explicitly adapted `extract_fit_indices()` to ModelFit class (16 fields). Added `inherits(fit, "ModelFit")` check. Includes fallback to BINET legacy name `MG_FitIndices`
  - Added LCA/LRA/Biclustering format (`$Students` data.frame) pattern to `extract_ability()`. Supports future Phase 2/3 module implementation
  - `safe_field()` helper function newly added: generic field accessor that prioritizes snake_case new names and falls back to old names (n_class/Nclass, n_field/Nfield, n_rank/Nrank, n_cycle/N_Cycle, etc.)
  - Added "rules for new module implementation" comment at the top of the file (use snake_case names, assume ModelFit, access BINET via TestFitIndices, add log_lik, Estimate column in Students)
