# shinyExametrika (development version)

## Changes

### Fix "unused argument (envir = env)" when loading a sample dataset (2026-06-03)

- Selecting a built-in sample dataset could fail with
  `Error loading data : unused argument (envir = env)`. The cause was the
  unqualified `get(name, envir = env)` used to pull the dataset out of a
  temporary environment: if a package on the user's search path masks `get()`
  with a version that lacks an `envir` argument (and RStudio's "Run App" runs
  in the console session, inheriting whatever is attached there), that masked
  `get()` is called instead. Replaced with `env[[name]]`, which cannot be
  masked. Reproduced and confirmed with a stand-in masked `get()`; regression
  test added.

### Gate analysis tabs by data readiness + dataset indicator + Data-tab layout (2026-06-03)

- **Tab gating.** Analysis tabs now start disabled and only enable once data has
  been formatted, and only when the loaded data matches the tab's required type
  (e.g. GRM stays disabled for binary data; only Descriptives + GRM enable for
  ordinal data). Implemented with `analysis_tab_requirements()` (in
  `fct_precheck.R`), an `observe()` in app_server that toggles a `.nav-disabled`
  class on each tab's nav link **via shinyjs** (`addClass`/`removeClass` with a
  `selector`), and the matching style. If the active tab becomes disabled (data
  changed underneath the user) they are returned to the Data tab. (shinyjs is
  used rather than a hand-rolled custom message handler, whose registration
  raced with Shiny's startup and left the enable step silently dead -- the bug
  where Format Data succeeded but the analysis tabs never lit up.)
- **Loaded-dataset indicator.** The navbar header now shows, in red on the left
  of the EN/JA toggle, the currently loaded dataset and its shape, e.g.
  `● k2022.csv  [binary, 20 × 6]`, or "No dataset loaded" before any data. The
  data-upload module now returns `list(data, name)` so app_server can label it.
- **Data-tab layout.** Upload-vs-sample is an either/or, so the two stacked
  sections were replaced by a single "Data source" radio that shows just the
  relevant input (file upload or sample dropdown).
- New i18n strings (Data source, Use sample data, No dataset loaded, dataset).

### Fix clipped/squished cards on the Guide page (2026-06-03)

- The Guide laid its sections out with
  `layout_column_wrap(heights_equal = "row")`. Inside page_navbar's fill layout
  that grid gives every card an equal slice of the viewport height and clips the
  overflow, so each section showed only a couple of lines (the welcome blurb and
  card bodies were cut off). Replaced the wrapper with a plain vertical flex
  stack and set `fill = FALSE` on the cards, so each section sizes to its content
  and the page scrolls normally.

### Live language switching now covers all static UI labels (2026-06-03)

- Fixed the EN/JA toggle only translating part of the interface (e.g. the Data
  tab's "Settings", upload labels and other sidebar text stayed English).
  shiny.i18n only live-swaps text wrapped in a `<span class="i18n" data-key>`,
  which `i18n$t()` emits **only after** `use_js()` has been called on the
  translator. `usei18n()` was placed at the end of the UI, so every `i18n$t()`
  above it had already rendered as plain text. `app_ui()` now calls
  `i18n$use_js()` immediately after creating the translator, before any label is
  built, so all static labels become swappable. (~340 labels now switch live.)
- New `R/utils_i18n.R` with `t_plain()`: returns a bare translated string for
  HTML *attribute* contexts (input `placeholder`, `buttonLabel`) that cannot
  hold a span. Applied to the missing-value-code placeholder and the DAG
  file-input in `fct_dag.R`.
- Note: server-rendered dynamic text (the Data-tab value boxes, result tables)
  still updates on the next data interaction rather than on a bare toggle; that
  is tracked separately.
- **Follow-up (verified in a real browser via chromote):** the language switch
  no longer routes through `shiny.i18n::update_lang()`. That call round-trips
  through shiny.i18n 0.3.0's `#i18n-state` input binding, which is incompatible
  with shiny >= 1.x and threw "Unexpected input value mode: '[object Object]'"
  on every toggle (and once at startup before the dictionary was ready). The
  observer now rewrites the `.i18n` spans directly with `shinyjs::runjs()` from
  the injected `i18n_translations` dictionary, and uses `ignoreInit = TRUE`.
  Result: EN/JA switching works with zero console errors.

### Column selection on the Data tab: ID picker + analysis-variable picker (2026-06-03)

- The Data tab previously offered only "First column" / "No ID column" for the
  identifier, so a dataset with two ID-like columns (e.g. `ID` + `GID`) fed the
  extra column into the analysis and `dataFormat()` auto-detected it as
  `nominal`. The sidebar now has a column-name **ID picker** plus a multi-select
  **Analysis Variables** picker (populated from the uploaded columns; default =
  first column is the ID, the rest are analysis variables). Deselecting `GID`
  yields the expected binary data.
- The chosen ID is automatically removed from the analysis-variable selection,
  and at least one analysis variable is required (otherwise a warning is shown).
- testServer coverage in `tests/testthat/test-mod_data_upload.R`.

### Inline model help and parameter guidance on analysis tabs (2026-06-03)

- New `R/fct_modelhelp.R`: each analysis tab now has a collapsible
  "About this model" panel (native `<details>`, collapsed by default) showing
  the model's full name, one-line description and data-type badge. The text
  reuses the strings already on the Guide tab, so the explanation lives where
  the user needs it without leaving the tab. Wired into all 10 modules.
- New `R/fct_param_help.R`: `param_label()` adds a hover "?" tooltip to the
  parameters that need statistical judgement — IRT 2PL/3PL/4PL, LRA GTM/SOM,
  the monotone-increasing constraint, Biclustering classes/fields/method, IRM
  concentration parameters, and the BNM/LDLRA structure-learning knobs
  (analysis mode, max parents, population size, mutation rate, learning rate).
  Each tooltip explains what the parameter does and gives a sensible default.
- 21 new bilingual (en/ja) strings in `inst/i18n/translation.json` (3 for the
  help panels, 18 for the parameter tooltips).
- 30 new tests in `tests/testthat/test-fct_modelhelp.R` (116 tests pass total).
- Priority A-2 and A-3 of the 2026-06 UX refinement backlog.

### Data-readiness pre-check banner on all analysis tabs (2026-06-03)

- New `R/fct_precheck.R` with `check_data_requirement()` and `precheck_banner()`.
  Every analysis tab now shows a clear warning banner at the top when no data
  has been loaded, or when the loaded data is the wrong response type for that
  analysis (e.g. opening IRT with ordinal data). Previously each tab relied on a
  silent `req(formatted_data())`, leaving the user with no explanation for why
  nothing happened.
- Wired into all 10 modules (Descriptives, CTT, IRT, GRM, LCA, LRA,
  Biclustering, IRM, BNM, LDLRA). Required type per tab: binary for
  CTT/IRT/LCA/LRA/Biclustering/IRM/BNM/LDLRA, ordinal/rated for GRM, any for
  Descriptives.
- Two new i18n strings (en/ja) in `inst/i18n/translation.json`.
- 19 new tests in `tests/testthat/test-fct_precheck.R` (86 tests pass total).
- First item of the 2026-06 UX refinement backlog (priority A-1). See
  `CLAUDE.md` "UX 洗練" and the exametrika-dev cross-cutting note.

### Japanese translation spacing fix for GRM/BNM/LDLRA progress messages (2026-06-02)

- `inst/i18n/translation.json`: Inserted a half-width space between the
  Latin abbreviation and the following Japanese in 6 progress strings so
  the typography matches the rest of the file. Found during a family-wide
  user-facing string audit (exametrika/ggExametrika/shinyExametrika)
  triggered by the `Clusterd` -> `Clustered` rename. The other 8 parallel
  messages (CTT, IRT, LCA, LRA, Biclustering, IRM, ...) already had the
  space.
  - `"GRM分析を実行中..."` -> `"GRM 分析を実行中..."`
  - `"BNM分析を実行中..."` -> `"BNM 分析を実行中..."`
  - `"BNM_GA構造学習を実行中..."` -> `"BNM_GA 構造学習を実行中..."`
  - `"BNM_PBIL構造学習を実行中..."` -> `"BNM_PBIL 構造学習を実行中..."`
  - `"LDLRA分析を実行中..."` -> `"LDLRA 分析を実行中..."`
  - `"LDLRA_PBIL構造学習を実行中..."` -> `"LDLRA_PBIL 構造学習を実行中..."`

### DAG plot height slider for BNM and LDLRA (2026-03-26)

- `R/mod_bnm.R`: Added height slider (400-1200px) to DAG plot options; `renderPlot` uses dynamic `height` function so the plot container auto-resizes
- `R/mod_ldlra.R`: Same DAG plot height slider added to DAG plot options
- `inst/i18n/translation.json`: Added "Plot Height (px)" translation key (EN/JA)

### R CMD check NOTE fixes for CRAN compliance (2026-03-25)

- `.Rbuildignore`: added `^\.github$`, `^LICENSE\.md$`, `^app\.R$`, `^rsconnect$` to exclude non-package files from the build tarball
- `LICENSE`: converted to CRAN-required DCF format (`YEAR: 2026` / `COPYRIGHT HOLDER: Koji Kosugi`)
- `LICENSE.md`: newly added with full MIT License text (excluded from build via `.Rbuildignore`)
- `DESCRIPTION`: removed `waiter` from Imports (was listed but never actually used in any R/ code)
- `DESCRIPTION`: added `Depends: R (>= 4.1.0)` to specify minimum R version requirement
- Result: R CMD check now passes with 0 errors, 0 warnings, 0 notes

### LDLRA module implementation (2026-02-28)

- `R/mod_ldlra.R` newly added: Locally Dependent Latent Rank Analysis (LDLRA) module
  - Two analysis modes:
    - LDLRA (Fixed DAG): user uploads rank-specific DAGs via CSV with From/To/Rank columns, analyzed with `exametrika::LDLRA()`
    - LDLRA_PBIL (Structure Learning): structure learning via `exametrika::LDLRA_PBIL()` with full parameter UI (population, survival rate, mutation rate, max parents, max generations, learning rate, estimation method)
  - Common parameters: number of ranks (2-10), method (Rank/Class)
  - Results tab: fit indices, Ordinal Alignment Conditions (SOAC/WOAC), IRP table, IRP Index, rank summary (TRP/LRD), CCRR table, Estimation table (PIRP per rank), student membership
  - Plots tab: IRP (per item), TRP, LRD, RMP (per student), DAG (per rank) via `ggExametrika::plotGraph_gg()` with layout/direction selectors and igraph base plot fallback
  - CSV download for IRP, CCRR table, and student membership; PNG download for plots
  - Binary data validation, progress indicators, error handling with notifications
- `R/fct_dag.R`: added two new functions for rank-specific DAG handling
  - `parse_ranked_dag_csv()`: CSV parser for rank-specific DAGs (From/To/Rank columns), with per-rank acyclicity validation, rank value range checking, and item label matching
  - `dag_status_display_ranked()`: Reactive status indicator showing per-rank edge counts or error messages
- `R/app_ui.R`: replaced LDLRA placeholder with `mod_ldlra_ui()`
- `R/app_server.R`: added `mod_ldlra_server()` call
- `R/mod_guide.R`: updated LDLRA from "Coming Soon" placeholder to active card with full description and Binary data badge
- `inst/i18n/translation.json`: added 18 translation keys for LDLRA module (EN/JA)
- `tests/testthat/test-fct_dag.R`: added 11 unit tests for `parse_ranked_dag_csv()` (valid parsing, missing Rank column, out-of-range ranks, per-rank cycles, cross-rank anti-parallel edges, empty ranks, item label validation, matrix dimensions, self-loops, duplicate edges)
- Total tests: 67 (all PASS), R CMD check: 0 errors, 0 warnings

### BNM module and DAG input component (2026-02-28)

- `R/fct_dag.R` newly added: Shared DAG input components for Phase 3 modules (BNM, LDLRA, LDB, BINET)
  - `dag_input_ui()`: Reusable UI components for DAG CSV upload with sample download button
  - `parse_dag_csv()`: CSV parser supporting both simple (From/To) and extended header formats
  - `check_dag_acyclic()`: Acyclicity validation using Kahn's topological sort algorithm
  - `generate_sample_dag_csv()`: Dynamic sample CSV generation (uses actual item labels when available)
  - `dag_status_display()`: Reactive status indicator showing parsed edge/node counts or errors
  - Validates: self-loops, duplicate edges, cycles, node-label mismatches
  - Supports optional Rank column for LDLRA/LDB/BINET rank-specific adjacency
- `R/mod_bnm.R` newly added: Bayesian Network Model (BNM) analysis module
  - Three analysis modes:
    - BNM (Fixed DAG): user uploads a DAG via CSV, analyzed with `exametrika::BNM()`
    - BNM_GA (Genetic Algorithm): structure learning via `exametrika::BNM_GA()` with full parameter UI (population, survival rate, mutation rate, max parents, max generations, crossover type, elitism)
    - BNM_PBIL (PBIL): structure learning via `exametrika::BNM_PBIL()` with learning rate and estimation method parameters
  - Results tab: fit indices, adjacency matrix, PIRP parameter estimates, CCRR table
  - Plots tab: DAG visualization via `ggExametrika::plotGraph_gg()` with layout algorithm and direction selectors, igraph base plot fallback
  - CSV download for adjacency matrix and CCRR table; PNG download for DAG plot
  - Binary data validation, progress indicators, error handling with notifications
- `R/app_ui.R`: added BNM tab with `mod_bnm_ui()`, plus LDLRA/LDB/BINET placeholder tabs
- `R/app_server.R`: added `mod_bnm_server()` call
- `R/mod_guide.R`: updated IRM from "Coming Soon" to active (with Binary data badge), added BNM card with description, added Phase 3 placeholder cards (LDLRA, LDB, BINET)
- `inst/i18n/translation.json`: added 42 translation keys for DAG input and BNM module (EN/JA)
- `tests/testthat/test-fct_dag.R` newly added: 13 unit tests for DAG helper functions (acyclicity, parsing, validation, CSV generation)
- Total tabs: 14 (Guide, Data, Descriptives, CTT, IRT, GRM, LCA, LRA, Biclustering, IRM, BNM, LDLRA*, LDB*, BINET*) *placeholder

### Documentation and version updates (2026-02-28)

- `CLAUDE.md`: updated last-modified date to 2026-02-28
- `CLAUDE.md`: changed Phase 2 status from "in progress" to "nearly complete (only GridSearch integration remaining)"
- `CLAUDE.md`: marked IRM seed UI as completed in TODO section
- `CLAUDE.md`: updated all exametrika version references from v1.9.0 to v1.10.0 (v1.9.0 skipped per upstream decision)
- `CLAUDE.md`: updated repository state section date to 2026-02-28
- `CLAUDE.md`: updated README.md status note (now reflects completed update)
- `README.md`: updated Phase 2 status to reflect LCA, LRA, Biclustering, IRM completion (GridSearch remaining)
- `DESCRIPTION`: updated exametrika dependency from `>= 1.9.0` to `>= 1.10.0`

### IRM seed UI exposure (2026-02-26)

- `R/mod_irm.R`: added random seed input field to IRM module UI
  - `numericInput` for seed value (default: 123, range: 1-99999) placed between concentration parameters and the help text
  - Server passes `seed` argument directly to `exametrika::Biclustering_IRM()` (which calls `set.seed()` internally)
  - Falls back to 123 if seed is NULL or NA
- `inst/i18n/translation.json`: added 2 translation keys for seed UI (EN/JA)
  - "Random Seed" / "乱数シード"
  - "Set a random seed for reproducibility of IRM results." / "IRM の結果を再現するための乱数シードを設定します。"

### IRM module implementation (2026-02-26)

- `R/mod_irm.R` newly added: Infinite Relational Model (IRM) analysis module
  - Parameters: `gamma_c` (concentration parameter for classes) and `gamma_f` (concentration parameter for fields), both defaulting to 1.0
  - Calls `exametrika::Biclustering_IRM()` which automatically determines the optimal number of classes and fields via the Chinese Restaurant Process
  - Results tab: discovered structure summary (n_class / n_field), fit indices, FRP, FRP Index, class summary, field summary, student membership, field analysis
  - Plots tab: FRP, TRP, Array with ggExametrika support and base plot fallback (CMP/RMP not supported by IRM)
  - CSV download for FRP and student membership; PNG download for plots
- `R/app_ui.R`: replaced `mod_placeholder_ui` with `mod_irm_ui` for the IRM tab
- `R/app_server.R`: added `mod_irm_server` call
- `inst/i18n/translation.json`: added 9 IRM-specific translation keys (EN/JA)

### Fix: add ggplot2 to Suggests (2026-02-26)

- Added `ggplot2` to `Suggests` in DESCRIPTION to resolve R CMD check WARNING
  - `ggplot2::ggsave()` is used in 5 modules (IRT, GRM, LCA, LRA, Biclustering) for plot download
  - These calls are conditional (only when ggExametrika is available), so `Suggests` is appropriate

### Guide page addition (2026-02-24)

- `R/mod_guide.R` newly added: Guide page module (UI-only, no server logic)
  - Hero section with welcome message and app description
  - Getting Started section with 4-step walkthrough (Load Data, Format Data, Run Analysis, View Results)
  - Explicitly explains where the "Run Analysis" button is located (blue button with play icon in the left sidebar) and that analysis does NOT start automatically
  - Screen Layout section with ASCII diagram showing the sidebar/main-area two-panel structure
  - Available Analysis Methods section listing all supported methods (CTT, IRT, GRM, LCA, LRA, Biclustering) with data type badges, plus IRM as coming soon
  - Tips section with practical advice for new users
  - Reference to Shojima (2022) textbook
- `R/app_ui.R`: added Guide tab as the first tab in the navigation bar (landing page)
- `inst/i18n/translation.json`: added 60+ translation keys for the guide page (English and Japanese)
- Full i18n support: all guide text is translatable via the EN/JA language toggle

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
