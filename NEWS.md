# shinyExametrika (development version)

## Changes

- CLAUDE.md に NEWS.md 記録ルールを追加（全ての変更を NEWS.md に記録する恒久ルール）
- `R/fct_analysis.R` を新規作成: 分析結果からの共通ヘルパー関数を追加
  - `extract_ability()`: IRT（$ability data.frame）と GRM（$EAP/$MAP/$PSD 個別ベクトル）の両形式に対応した能力推定値抽出関数
  - `extract_fit_indices()`: TestFitIndices の named list / data.frame 両形式に対応した適合度指標抽出関数
- `R/mod_irt.R`: TestFitIndices 表示に `is.data.frame()` 分岐 + `tryCatch()` による防御的コードを追加（GRM モジュールと同等の堅牢性に強化）。能力推定値の表示・ダウンロードを共通ヘルパー関数 `extract_ability()` に統一
- `R/mod_grm.R`: 適合度指標・能力推定値の表示・ダウンロードを共通ヘルパー関数 `extract_fit_indices()` / `extract_ability()` に統一（ロジックの重複を解消）

### exametrika v1.9.0 返り値構造統一への追従（2026-02-19）

- `R/fct_analysis.R`:
  - `extract_fit_indices()` を ModelFit クラス（16フィールド）に明示的に対応。`inherits(fit, "ModelFit")` による判定を追加。BINET 旧名 `MG_FitIndices` へのフォールバック付き
  - `extract_ability()` に LCA/LRA/Biclustering 形式（`$Students` data.frame）のパターンを追加。将来の Phase 2/3 モジュール実装に対応
  - `safe_field()` ヘルパー関数を新規追加: snake_case 新名称を優先し旧名称にフォールバックする汎用フィールド取得関数（n_class/Nclass, n_field/Nfield, n_rank/Nrank, n_cycle/N_Cycle 等）
  - ファイル冒頭に「新モジュール実装時のルール」コメントを追加（snake_case 名称使用、ModelFit 前提、BINET は TestFitIndices でアクセス、log_lik 追加、Students の Estimate 列）
