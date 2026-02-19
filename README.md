# shinyExametrika

A Shiny GUI application for the [exametrika](https://github.com/kosugitti/exametrika) R package.

Perform psychometric analyses — CTT, IRT, LCA, LRA, Biclustering, and more — without writing code.

## Features

- **Interactive data upload**: CSV files and built-in sample datasets
- **Multiple analysis methods**: CTT, IRT (2PL/3PL/4PL), GRM, LCA, LRA, Biclustering, IRM, BNM, and more
- **Rich visualization**: Powered by [ggExametrika](https://github.com/kosugitti/ggExametrika)
- **Bilingual UI**: English / Japanese
- **Downloadable results**: Tables (CSV) and plots (PDF/PNG)

## Installation

```r
# Install dependencies
install.packages(c("shiny", "golem", "bslib", "DT", "shiny.i18n"))

# Install exametrika and ggExametrika
install.packages("exametrika")
remotes::install_github("kosugitti/ggExametrika")

# Install shinyExametrika
remotes::install_github("kosugitti/shinyExametrika")
```

## Usage

```r
library(shinyExametrika)
run_app()
```

## Development Status

This project is under active development.

| Phase | Scope | Status |
|-------|-------|--------|
| Phase 0 | Project setup, data upload, i18n | Done |
| Phase 1 | Descriptives, CTT, IRT, GRM | Done |
| Phase 2 | LCA, LRA, Biclustering, IRM, GridSearch | In progress (LCA, LRA done) |
| Phase 3 | BNM, LDLRA, LDB, BINET | Planned |
| Phase 4 | Polish, deploy, documentation | Planned |

## Related Packages

- [exametrika](https://github.com/kosugitti/exametrika) — Psychometric analysis engine
- [ggExametrika](https://github.com/kosugitti/ggExametrika) — ggplot2 visualization for exametrika

## License

MIT License. See [LICENSE](LICENSE) for details.

## References

Shojima, K. (2022). *Test Data Engineering: Latent Rank Analysis, Biclustering, and Bayesian Network*. Springer.
