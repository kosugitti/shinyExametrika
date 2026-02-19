# shinyExametrika (development version)

## Changes

- CLAUDE.md に NEWS.md 記録ルールを追加（全ての変更を NEWS.md に記録する恒久ルール）
- `R/fct_analysis.R` を新規作成: 分析結果からの共通ヘルパー関数を追加
  - `extract_ability()`: IRT（$ability data.frame）と GRM（$EAP/$MAP/$PSD 個別ベクトル）の両形式に対応した能力推定値抽出関数
  - `extract_fit_indices()`: TestFitIndices の named list / data.frame 両形式に対応した適合度指標抽出関数
- `R/mod_irt.R`: TestFitIndices 表示に `is.data.frame()` 分岐 + `tryCatch()` による防御的コードを追加（GRM モジュールと同等の堅牢性に強化）。能力推定値の表示・ダウンロードを共通ヘルパー関数 `extract_ability()` に統一
- `R/mod_grm.R`: 適合度指標・能力推定値の表示・ダウンロードを共通ヘルパー関数 `extract_fit_indices()` / `extract_ability()` に統一（ロジックの重複を解消）
