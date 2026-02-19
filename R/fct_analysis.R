# =============================================================================
# fct_analysis.R — 分析結果処理の共通ヘルパー関数群
# =============================================================================
#
# 【新モジュール実装時のルール（exametrika v1.9.0 準拠）】
#
# 1. フィールド名は必ず snake_case 新名称を使用する:
#    - n_class（旧 Nclass）, n_field（旧 Nfield）, n_rank（旧 Nrank）
#    - n_cycle（旧 N_Cycle / em_cycle / EM_Cycle）
#    - 旧名称へのフォールバックが必要な場合は safe_field() を使う
#
# 2. TestFitIndices は 16 フィールド + ModelFit クラスを前提とする:
#    - model_log_like, bench_log_like, null_log_like, model_Chi_sq,
#      null_Chi_sq, model_df, null_df, NFI, RFI, IFI, TLI, CFI,
#      RMSEA, AIC, CAIC, BIC
#    - extract_fit_indices() でアクセスすること
#
# 3. BINET は TestFitIndices でアクセスする（旧 MG_FitIndices は非推奨）
#
# 4. 全関数のトップレベルに log_lik が追加されている（result$log_lik）
#
# 5. Biclustering nominal/ordinal の Students に Estimate 列が追加
#
# =============================================================================

#' 分析結果からの能力推定値・所属推定値抽出ヘルパー
#'
#' exametrika の分析結果オブジェクトから能力推定値（または所属推定値）を
#' 統一的な data.frame として抽出する。
#'
#' 対応する出力形式:
#' - IRT: `$ability` data.frame（列: ID, EAP, PSD）
#' - GRM: `$EAP`, `$MAP`, `$PSD` 個別ベクトル
#' - LCA/LRA/Biclustering: `$Students` data.frame（所属クラス・確率を含む）
#'
#' @param result exametrika の分析結果オブジェクト
#'
#' @return data.frame。分析手法に応じた列を持つ。
#'   抽出失敗時は EAP = NA, PSD = NA の 1 行 data.frame を返す。
#'
#' @details
#' exametrika v1.9.0 以降のフィールド名規則:
#' - n_class（旧 Nclass）, n_field（旧 Nfield）, n_rank（旧 Nrank）を使用
#' - Students に Estimate 列が追加されている（Biclustering nominal/ordinal）
#'
#' 新モジュール実装時は必ず snake_case 新名称を使用すること。
#' 旧名称が必要な場合は `result$n_class %||% result$Nclass` でフォールバックする。
#'
#' @examples
#' \dontrun{
#' # IRT の場合
#' result_irt <- exametrika::IRT(fd, model = 2)
#' ability_df <- extract_ability(result_irt)
#' # -> data.frame with columns: ID, EAP, PSD
#'
#' # GRM の場合
#' result_grm <- exametrika::GRM(fd)
#' ability_df <- extract_ability(result_grm)
#' # -> data.frame with columns: EAP, MAP, PSD
#'
#' # LCA/LRA/Biclustering の場合
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
    # ========== パターン 1: IRT 形式（$ability が data.frame） ==========
    if (!is.null(result$ability) && is.data.frame(result$ability)) {
      return(result$ability)
    }

    # ========== パターン 2: GRM 形式（$EAP/$MAP/$PSD が個別ベクトル） ==========
    if (!is.null(result$EAP) && is.numeric(result$EAP)) {
      df <- data.frame(EAP = result$EAP, stringsAsFactors = FALSE)

      # MAP は GRM にはあるが IRT にはない場合がある
      if (!is.null(result$MAP) && is.numeric(result$MAP)) {
        df$MAP <- result$MAP
      }

      if (!is.null(result$PSD) && is.numeric(result$PSD)) {
        df$PSD <- result$PSD
      }

      # 列の並び順を整える（EAP, MAP, PSD の順）
      col_order <- intersect(c("EAP", "MAP", "PSD"), colnames(df))
      df <- df[, col_order, drop = FALSE]

      return(df)
    }

    # ========== パターン 3: LCA/LRA/Biclustering 形式（$Students） ==========
    if (!is.null(result$Students) && is.data.frame(result$Students)) {
      return(result$Students)
    }

    # ========== パターン 4: 未知の形式 ==========
    # 将来の拡張に備え、警告を出して空の data.frame を返す
    warning("extract_ability: unsupported result format. ",
            "Class: ", paste(class(result), collapse = ", "))
    data.frame(EAP = NA_real_, PSD = NA_real_, stringsAsFactors = FALSE)

  }, error = function(e) {
    warning("extract_ability: extraction failed: ", e$message)
    data.frame(EAP = NA_real_, PSD = NA_real_, stringsAsFactors = FALSE)
  })
}


#' 分析結果から snake_case フィールドを安全に取得するヘルパー
#'
#' exametrika v1.9.0 以降の snake_case 名を優先し、
#' 旧名称にフォールバックする。
#'
#' @param result exametrika の分析結果オブジェクト
#' @param new_name snake_case の新フィールド名（例: "n_class"）
#' @param old_name PascalCase の旧フィールド名（例: "Nclass"）
#' @param default フィールドが見つからない場合のデフォルト値
#'
#' @return フィールドの値。見つからない場合は default。
#'
#' @details
#' 対応表（新名 / 旧名）:
#' - n_class / Nclass（LCA, LDLRA, BINET, Biclustering）
#' - n_field / Nfield（LDB, BINET, Biclustering）
#' - n_rank / Nrank（LRA, LDB）
#' - n_cycle / N_Cycle（IRM, Biclustering nominal/ordinal）
#'
#' @noRd
safe_field <- function(result, new_name, old_name, default = NULL) {
  result[[new_name]] %||% result[[old_name]] %||% default
}


#' 分析結果から適合度指標を統一的に抽出するヘルパー
#'
#' exametrika の分析結果オブジェクトから TestFitIndices を
#' 統一的な data.frame（Index, Value の 2 列）として抽出する。
#'
#' exametrika v1.9.0 以降、TestFitIndices は ModelFit クラス（16フィールド）に
#' 統一されている。旧バージョンの named list / data.frame 形式にもフォールバック対応する。
#'
#' @param result exametrika の分析結果オブジェクト
#'
#' @return data.frame（列: Index, Value）
#'
#' @details
#' exametrika v1.9.0 の ModelFit クラスは以下の 16 フィールドを持つ:
#' model_log_like, bench_log_like, null_log_like, model_Chi_sq, null_Chi_sq,
#' model_df, null_df, NFI, RFI, IFI, TLI, CFI, RMSEA, AIC, CAIC, BIC
#'
#' BINET の旧名 MG_FitIndices は TestFitIndices に統一された。
#' 新モジュール実装時は必ず TestFitIndices でアクセスすること。
#'
#' @noRd
extract_fit_indices <- function(result) {
  if (is.null(result)) {
    return(data.frame(Index = "N/A", Value = NA_real_, stringsAsFactors = FALSE))
  }

  # TestFitIndices を取得（BINET 旧名 MG_FitIndices へのフォールバック付き）
  fit <- result$TestFitIndices %||% result$MG_FitIndices

  if (is.null(fit)) {
    return(data.frame(Index = "N/A", Value = NA_real_, stringsAsFactors = FALSE))
  }

  tryCatch({
    # ModelFit クラス（v1.9.0 統一形式）: is.list == TRUE, is.data.frame == FALSE
    if (inherits(fit, "ModelFit") || (is.list(fit) && !is.data.frame(fit))) {
      data.frame(
        Index = names(fit),
        Value = as.numeric(unlist(fit)),
        stringsAsFactors = FALSE
      )
    } else if (is.data.frame(fit)) {
      # 旧形式フォールバック: data.frame 形式
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
