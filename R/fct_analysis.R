# =============================================================================
# fct_analysis.R -- Common helper functions for analysis result processing
# =============================================================================
#
# [Rules for implementing new modules (compliant with exametrika v1.9.0)]
#
# 1. Always use snake_case new names for field names:
#    - n_class, n_field, n_rank
#    - n_cycle
#    - Use safe_field() to supply a default when a field may be absent
#
# 2. TestFitIndices assumes 16 fields + ModelFit class:
#    - model_log_like, bench_log_like, null_log_like, model_Chi_sq,
#      null_Chi_sq, model_df, null_df, NFI, RFI, IFI, TLI, CFI,
#      RMSEA, AIC, CAIC, BIC
#    - Access via extract_fit_indices()
#
# 3. BINET uses TestFitIndices (legacy MG_FitIndices is deprecated)
#
# 4. log_lik is added at the top level of all functions (result$log_lik)
#
# 5. Estimate column added to Students in Biclustering nominal/ordinal
#
# =============================================================================

#' Helper to extract ability estimates / membership estimates from analysis results
#'
#' Extracts ability estimates (or membership estimates) from an exametrika
#' analysis result object as a consistent data.frame.
#'
#' Supported output formats:
#' - IRT: `$ability` data.frame (columns: ID, EAP, PSD)
#' - GRM: `$EAP`, `$MAP`, `$PSD` as individual vectors
#' - LCA/LRA/Biclustering: `$Students` data.frame (includes class membership and probabilities)
#'
#' @param result An exametrika analysis result object
#'
#' @return data.frame with columns depending on the analysis method.
#'   Returns a 1-row data.frame with EAP = NA, PSD = NA on extraction failure.
#'
#' @details
#' Field naming convention since exametrika v1.9.0:
#' - Uses n_class, n_field, n_rank
#' - Estimate column added to Students (Biclustering nominal/ordinal)
#'
#' Always use snake_case new names when implementing new modules.
#'
#' @examples
#' \dontrun{
#' # IRT case
#' result_irt <- exametrika::IRT(fd, model = 2)
#' ability_df <- extract_ability(result_irt)
#' # -> data.frame with columns: ID, EAP, PSD
#'
#' # GRM case
#' result_grm <- exametrika::GRM(fd)
#' ability_df <- extract_ability(result_grm)
#' # -> data.frame with columns: EAP, MAP, PSD
#'
#' # LCA/LRA/Biclustering case
#' result_lca <- exametrika::LCA(fd, ncls = 3)
#' students_df <- extract_ability(result_lca)
#' # -> result$Students data.frame
#' }
#'
#' @noRd
extract_ability <- function(result) {
  if (is.null(result)) {
    return(data.frame(EAP = NA_real_, PSD = NA_real_, stringsAsFactors = FALSE))
  }

  tryCatch({
    # ========== Pattern 1: IRT format ($ability is a data.frame) ==========
    if (!is.null(result$ability) && is.data.frame(result$ability)) {
      return(result$ability)
    }

    # ========== Pattern 2: GRM format ($EAP/$MAP/$PSD as individual vectors) ==========
    if (!is.null(result$EAP) && is.numeric(result$EAP)) {
      df <- data.frame(EAP = result$EAP, stringsAsFactors = FALSE)

      # MAP exists in GRM but may not exist in IRT
      if (!is.null(result$MAP) && is.numeric(result$MAP)) {
        df$MAP <- result$MAP
      }

      if (!is.null(result$PSD) && is.numeric(result$PSD)) {
        df$PSD <- result$PSD
      }

      # Reorder columns (EAP, MAP, PSD order)
      col_order <- intersect(c("EAP", "MAP", "PSD"), colnames(df))
      df <- df[, col_order, drop = FALSE]

      return(df)
    }

    # ========== Pattern 3: LCA/LRA/Biclustering format ($Students) ==========
    if (!is.null(result$Students) && is.data.frame(result$Students)) {
      return(result$Students)
    }

    # ========== Pattern 4: Unknown format ==========
    # For future extensibility, emit a warning and return an empty data.frame
    warning("extract_ability: unsupported result format. ",
            "Class: ", paste(class(result), collapse = ", "))
    data.frame(EAP = NA_real_, PSD = NA_real_, stringsAsFactors = FALSE)

  }, error = function(e) {
    warning("extract_ability: extraction failed: ", e$message)
    data.frame(EAP = NA_real_, PSD = NA_real_, stringsAsFactors = FALSE)
  })
}


#' Helper to safely retrieve snake_case fields from analysis results
#'
#' Reads a field that a model may or may not report, falling back to a
#' default. The PascalCase aliases (Nclass, Nfield, Nrank, N_Cycle) that
#' this helper used to fall back on were removed in exametrika 2.0.0 and
#' had been deprecated since 1.8.0; since this package requires >= 1.10.0,
#' no supported version reaches that path any more.
#'
#' @param result An exametrika analysis result object
#' @param field The field name (e.g., "n_class")
#' @param default Default value if the field is not found
#'
#' @return The field value, or default if not found.
#'
#' @noRd
safe_field <- function(result, field, default = NULL) {
  return(result[[field]] %||% default)
}


#' Helper to extract fit indices consistently from analysis results
#'
#' Extracts TestFitIndices from an exametrika analysis result object
#' as a consistent data.frame with two columns (Index, Value).
#'
#' Since exametrika v1.9.0, TestFitIndices is unified as the ModelFit class
#' (16 fields). Also supports fallback to legacy named list / data.frame formats.
#'
#' @param result An exametrika analysis result object
#'
#' @return data.frame (columns: Index, Value)
#'
#' @details
#' The ModelFit class in exametrika v1.9.0 has the following 16 fields:
#' model_log_like, bench_log_like, null_log_like, model_Chi_sq, null_Chi_sq,
#' model_df, null_df, NFI, RFI, IFI, TLI, CFI, RMSEA, AIC, CAIC, BIC
#'
#' The legacy BINET name MG_FitIndices has been unified to TestFitIndices.
#' Always access via TestFitIndices when implementing new modules.
#'
#' @noRd
extract_fit_indices <- function(result) {
  if (is.null(result)) {
    return(data.frame(Index = "N/A", Value = NA_real_, stringsAsFactors = FALSE))
  }

  # Retrieve TestFitIndices (with fallback to BINET legacy name MG_FitIndices)
  fit <- result$TestFitIndices %||% result$MG_FitIndices

  if (is.null(fit)) {
    return(data.frame(Index = "N/A", Value = NA_real_, stringsAsFactors = FALSE))
  }

  tryCatch({
    # ModelFit class (v1.9.0 unified format): is.list == TRUE, is.data.frame == FALSE
    if (inherits(fit, "ModelFit") || (is.list(fit) && !is.data.frame(fit))) {
      data.frame(
        Index = names(fit),
        Value = as.numeric(unlist(fit)),
        stringsAsFactors = FALSE
      )
    } else if (is.data.frame(fit)) {
      # Legacy format fallback: data.frame format
      data.frame(
        Index = colnames(fit),
        Value = as.numeric(fit[1, ]),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(Index = "Unknown format", Value = NA_real_, stringsAsFactors = FALSE)
    }
  }, error = function(e) {
    data.frame(Index = "Error", Value = NA_real_, stringsAsFactors = FALSE)
  })
}

#' Base-plot fallback for a single Field Reference Profile
#'
#' `plot(result, type = "FRP")` in exametrika draws one plot per field and
#' ignores field selection (`fields` is not a formal argument; as of
#' exametrika 1.14.0 it is forwarded to base graphics and triggers
#' "not a graphical parameter" warnings). Inside `renderPlot()` only the
#' last field would be shown regardless of the user's selection, so this
#' helper draws just the selected field, mirroring exametrika's base style.
#'
#' @param result An exametrika result object with an `$FRP` matrix
#'   (fields x classes/ranks) and an `$msg` label ("Class"/"Rank").
#' @param idx Field index (1-based). Coerced to 1 when missing/invalid.
#'
#' @noRd
plot_frp_field <- function(result, idx) {
  frp <- result$FRP
  idx <- suppressWarnings(as.integer(idx))
  if (length(idx) == 0 || is.na(idx) || idx < 1 || idx > nrow(frp)) idx <- 1L
  msg <- if (!is.null(result$msg)) result$msg else "Class"
  plot(
    as.numeric(frp[idx, ]),
    type = "b",
    ylab = "Correct Response Rate",
    xlab = paste("Latent", msg),
    ylim = c(0, 1),
    main = paste("Field", idx)
  )
}
