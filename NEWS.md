# shinyExametrika (development version)

## Changes

### plotRMP_gg 回避コード解消（2026-02-23）

- `R/mod_lra.R`: RMP プロットの手動 ggplot2 描画コードを `ggExametrika::plotRMP_gg()` 呼び出しに置換
  - ggExametrika v0.0.29 で `$n_rank` / `$n_class` 対応が完了し、LRA オブジェクトで正常動作するようになった
  - base plot フォールバックに RMP 個別受検者指定を追加
- `R/mod_biclustering.R`（feature/mod-biclustering ブランチ）: 同様に RMP の手動描画を `plotRMP_gg()` に置換
  - CMP/RMP の受検者セレクタ `req()` 条件を統一
  - base plot フォールバックの CMP/RMP 分岐を統一

### CI / テスト環境整備（2026-02-23）

- `.github/workflows/R-CMD-check.yaml` を新規追加: GitHub Actions による R CMD check 自動実行
  - push（main / develop）および PR 時に自動実行
  - macOS-latest + ubuntu-latest（release / devel）の3環境でチェック
  - r-lib/actions v2 を使用
- `tests/testthat.R` を新規追加: testthat テストランナー
- `tests/testthat/test-golem-recommended.R` を新規追加: golem 推奨の基本テスト
  - app_ui / app_server / app_sys / golem-config の存在・型チェック
- `tests/testthat/test-fct_analysis.R` を新規追加: 共通ヘルパー関数のユニットテスト
  - safe_field: 新名称優先 / 旧名称フォールバック / 未定義時 NULL
  - extract_fit_indices: ModelFit オブジェクト / data.frame 入力の処理

### LCA / LRA モジュール追加（2026-02-20）

- `R/mod_lca.R` を新規追加: LCA（潜在クラス分析）モジュール
  - サイドバー: クラス数スライダー（2-10）+ 実行ボタン
  - Results タブ: 適合度指標 / クラスプロファイル(IRP) / クラスサマリー / 受検者クラス帰属
  - Item Fit タブ: 項目適合度指標テーブル
  - Plots タブ: IRP（項目選択付き）/ TRP / LCD / CMP（受検者選択付き）
  - CSV ダウンロード（IRP, Students）+ プロット PNG ダウンロード
  - ggExametrika 優先、base plot フォールバック対応
- `R/mod_lra.R` を新規追加: LRA（潜在ランク分析）モジュール
  - サイドバー: ランク数スライダー / 推定方法（GTM/SOM）/ 単調増加制約チェック
  - Results タブ: 適合度指標 / IRP テーブル / IRP Index / ランクサマリー / 受検者ランク帰属
  - Item Fit タブ: 項目適合度指標テーブル
  - Plots タブ: IRP（項目選択付き）/ TRP / LRD / RMP（受検者選択付き、ggplot2 手動描画）
  - RMP は exametrika / ggExametrika 双方のバグを回避するため ggplot2 で手動描画
- `R/app_ui.R`: LCA / LRA タブを placeholder から実モジュールに切り替え
- `R/app_server.R`: mod_lca_server / mod_lra_server を追加
- `inst/i18n/translation.json`: LCA / LRA 関連の翻訳キーを追加

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
