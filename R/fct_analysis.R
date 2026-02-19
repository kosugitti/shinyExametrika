#' 分析結果からの能力推定値抽出ヘルパー
#'
#' exametrika の分析結果オブジェクトから能力推定値を統一的な data.frame として抽出する。
#' IRT（$ability data.frame）と GRM（$EAP/$MAP/$PSD 個別ベクトル）の
#' 両方の出力形式に対応する。
#'
#' @param result exametrika の分析結果オブジェクト（IRT または GRM）
#'
#' @return data.frame。列は以下のいずれか:
#'   - IRT 形式の場合: ID, EAP, PSD（$ability がそのまま返る）
#'   - GRM 形式の場合: EAP, PSD（$MAP があれば MAP 列も含む）
#'   - 抽出失敗時: EAP = NA, PSD = NA の 1 行 data.frame
#'
#' @details
#' IRT() は `$ability` に data.frame（列: ID, EAP, PSD）を返す。
#' GRM() は `$EAP`, `$MAP`, `$PSD` にそれぞれ numeric vector を返す。
#' この関数は両方のパターンを自動判別し、統一的な data.frame を返す。
#'
#' 将来的に LCA/LRA 等の潜在クラス・ランクモデルの所属確率にも
#' 拡張可能な設計としている。
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

    # ========== パターン 3: 未知の形式 ==========
    # 将来の拡張に備え、警告を出して空の data.frame を返す
    warning("extract_ability: unsupported result format. ",
            "Class: ", paste(class(result), collapse = ", "))
    data.frame(EAP = NA_real_, PSD = NA_real_, stringsAsFactors = FALSE)

  }, error = function(e) {
    warning("extract_ability: extraction failed: ", e$message)
    data.frame(EAP = NA_real_, PSD = NA_real_, stringsAsFactors = FALSE)
  })
}


#' 分析結果から適合度指標を統一的に抽出するヘルパー
#'
#' exametrika の分析結果オブジェクトから TestFitIndices を
#' 統一的な data.frame（Index, Value の 2 列）として抽出する。
#' named list / data.frame どちらの形式にも対応する。
#'
#' @param result exametrika の分析結果オブジェクト
#'
#' @return data.frame（列: Index, Value）
#'
#' @noRd
extract_fit_indices <- function(result) {
  if (is.null(result) || is.null(result$TestFitIndices)) {
    return(data.frame(Index = "N/A", Value = NA_real_, stringsAsFactors = FALSE))
  }

  fit <- result$TestFitIndices

  tryCatch({
    if (is.data.frame(fit)) {
      data.frame(
        Index = colnames(fit),
        Value = as.numeric(fit[1, ]),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        Index = names(fit),
        Value = as.numeric(unlist(fit)),
        stringsAsFactors = FALSE
      )
    }
  }, error = function(e) {
    data.frame(Index = "Error", Value = NA_real_, stringsAsFactors = FALSE)
  })
}
