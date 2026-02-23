# =============================================================================
# fct_analysis.R -- Common helper functions for analysis result processing
# =============================================================================
#
# [Rules for implementing new modules (compliant with exametrika v1.9.0)]
#
# 1. Always use snake_case new names for field names:
#    - n_class (legacy: Nclass), n_field (legacy: Nfield), n_rank (legacy: Nrank)
#    - n_cycle (legacy: N_Cycle / em_cycle / EM_Cycle)
#    - Use safe_field() when fallback to legacy names is needed
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
#' - Uses n_class (legacy: Nclass), n_field (legacy: Nfield), n_rank (legacy: Nrank)
#' - Estimate column added to Students (Biclustering nominal/ordinal)
#'
#' Always use snake_case new names when implementing new modules.
#' Use `result$n_class %||% result$Nclass` for fallback to legacy names when needed.
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
#' Prioritizes snake_case names from exametrika v1.9.0 and later,
#' with fallback to legacy names.
#'
#' @param result An exametrika analysis result object
#' @param new_name The snake_case new field name (e.g., "n_class")
#' @param old_name The PascalCase legacy field name (e.g., "Nclass")
#' @param default Default value if the field is not found
#'
#' @return The field value, or default if not found.
#'
#' @details
#' Mapping table (new name / legacy name):
#' - n_class / Nclass (LCA, LDLRA, BINET, Biclustering)
#' - n_field / Nfield (LDB, BINET, Biclustering)
#' - n_rank / Nrank (LRA, LDB)
#' - n_cycle / N_Cycle (IRM, Biclustering nominal/ordinal)
#'
#' @noRd
safe_field <- function(result, new_name, old_name, default = NULL) {
  result[[new_name]] %||% result[[old_name]] %||% default
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
