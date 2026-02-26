# shinyExametrika — プロジェクト CLAUDE.md

**最終更新: 2026-02-26**

## プロジェクト概要

exametrika パッケージの Shiny GUI アプリケーション。
コードを書かずにテストデータ分析（CTT, IRT, LCA, LRA, Biclustering 等）を実行・可視化できる Web アプリを提供する。

- **バージョン**: 0.0.0.9000（開発版）
- **開発者**: 小杉（kosugitti）+ 学生チーム（分担開発）
- **関連パッケージ**:
  - [exametrika](https://github.com/kosugitti/exametrika) — 小杉が開発する心理測定パッケージ（v1.9.0、main にpush済み、CRAN投稿準備中）
  - [ggExametrika](https://github.com/kosugitti/ggExametrika) — 学生が開発する ggplot2 可視化パッケージ（v0.0.34、CRAN投稿準備中）
- **参考書籍**: Shojima (2022) "Test Data Engineering"

---

## アーキテクチャ

### フレームワーク: golem

R パッケージ構造で Shiny アプリを管理する。モジュール化・テスト・デプロイが容易。

```
shinyExametrika/
├── DESCRIPTION
├── NAMESPACE
├── R/
│   ├── app_config.R          # golem 設定
│   ├── app_server.R          # メインサーバ
│   ├── app_ui.R              # メイン UI（bslib ベース）
│   ├── run_app.R             # アプリ起動関数
│   ├── mod_guide.R           # ガイドページモジュール（ランディングタブ）
│   ├── mod_data_upload.R     # データ読み込みモジュール
│   ├── mod_descriptives.R    # 記述統計モジュール
│   ├── mod_ctt.R             # CTT 分析モジュール
│   ├── mod_irt.R             # IRT 分析モジュール
│   ├── mod_grm.R             # GRM 分析モジュール
│   ├── mod_lca.R             # LCA 分析モジュール
│   ├── mod_lra.R             # LRA 分析モジュール
│   ├── mod_biclustering.R    # Biclustering 分析モジュール
│   ├── mod_placeholder.R     # プレースホルダーモジュール（未実装タブ用）
│   └── fct_analysis.R        # 分析ヘルパー関数（safe_field, extract_fit_indices 等）
├── inst/
│   ├── app/www/              # CSS, JS, 画像等の静的アセット
│   ├── golem-config.yml      # golem 設定ファイル
│   └── i18n/
│       └── translation.json  # 翻訳ファイル（shiny.i18n 形式、EN/JA 統合）
├── tests/
│   ├── testthat.R            # testthat テストランナー
│   └── testthat/
│       ├── test-golem-recommended.R  # golem 基本テスト
│       └── test-fct_analysis.R       # ヘルパー関数ユニットテスト
├── dev/                      # golem 開発用スクリプト + サンプルデータ
│   ├── 01_start.R
│   ├── 02_dev.R
│   ├── 03_deploy.R
│   ├── run_dev.R
│   ├── sample01.csv          # 二値テストデータ（100人 × 20項目）
│   └── sample02.csv          # 多値テストデータ（150人 × 15項目）
├── .github/workflows/
│   └── R-CMD-check.yaml      # GitHub Actions CI（macOS + Ubuntu）
├── app.R                     # shinyapps.io デプロイ用エントリポイント
├── man/
├── .gitignore
├── CLAUDE.md                 # このファイル
├── NEWS.md                   # 変更履歴（全変更を必ず記録）
└── README.md
```

### UI 構成（全10タブ）

- **ナビゲーション**: bslib の `page_navbar` によるタブベースナビゲーション
  1. Guide（ランディングページ — 使い方ガイド）
  2. Data（データ読み込み・フォーマット）
  3. Descriptives（記述統計）
  4. CTT（古典的テスト理論）
  5. IRT（項目応答理論 2PL/3PL/4PL）
  6. GRM（段階反応モデル — 多値 IRT）
  7. LCA（潜在クラス分析）
  8. LRA（潜在ランク分析）
  9. Biclustering（バイクラスタリング / ランクラスタリング）
  10. IRM（プレースホルダー — 未実装）
- **言語切替**: shiny.i18n による日英切替（ヘッダーにトグル配置）
- **翻訳キー数**: 188キー（inst/i18n/translation.json、EN/JA 統合ファイル）
- **デプロイ先**: shinyapps.io（https://kosugitti.shinyapps.io/shinyExametrika/）

### 各分析モジュールの共通構造

1. **パラメータ入力パネル**（サイドバー）: 分析ごとの設定項目
2. **実行ボタン**: 分析を明示的に開始（自動実行しない）
3. **結果表示パネル**（メイン）:
   - 適合度指標テーブル
   - パラメータ推定値テーブル
   - プロット（ggExametrika の関数を使用）
   - 結果のダウンロード（CSV / PDF）

---

## 開発フェーズ

### Phase 0: プロジェクト基盤 — 完了

- [x] golem プロジェクト初期化
- [x] 依存パッケージのセットアップ
- [x] CI/CD 設定（GitHub Actions: R-CMD-check.yaml）
- [x] i18n 基盤の構築（shiny.i18n, translation.json）
- [x] データ読み込みモジュール（CSV / サンプルデータ）
- [x] ガイドページ（mod_guide.R — ランディングタブ）

### Phase 1: 基本分析 — 完了

- [x] Descriptives モジュール（記述統計）
- [x] CTT モジュール
- [x] IRT モジュール（2PL / 3PL / 4PL）
- [x] GRM モジュール（多値 IRT）

### Phase 2: 潜在構造分析 — 進行中

- [x] LCA モジュール
- [x] LRA モジュール
- [x] Biclustering モジュール
- [ ] IRM モジュール（現在プレースホルダー）
- [ ] GridSearch 統合

### Phase 3: ネットワーク・局所依存モデル — 未着手

- [ ] BNM モジュール（DAG 入力 UI 含む）
- [ ] LDLRA モジュール
- [ ] LDB モジュール
- [ ] BINET モジュール

### Phase 4: 仕上げ — 一部着手

- [ ] UI/UX 改善
- [ ] ドキュメント・ヘルプ統合
- [x] デプロイ対応（shinyapps.io デプロイ済み）
- [ ] パフォーマンス最適化

---

## 今後の TODO

### 短期（Phase 2 完了に向けて）

- [ ] IRM モジュールの実装（`mod_placeholder.R` → `mod_irm.R` に置換）
  - `Biclustering_IRM()` のラッパー。CRP パラメータ（gamma_c, gamma_f）の入力 UI
  - 計算コストが高いため `withProgress()` + タイムアウト対策が必須
- [ ] GridSearch 統合（Biclustering モジュール内 or 独立タブ）

### 中期（Phase 3）

- [ ] BNM モジュール（DAG 入力 UI が最大の課題。隣接行列 CSV アップロード or インタラクティブ DAG エディタ）
- [ ] LDLRA モジュール
- [ ] LDB モジュール
- [ ] BINET モジュール
- ggExametrika の DAG 可視化（plotGraph_gg）が BNM は対応済み、LDLRA は基本実装済み、LDB/BINET は未実装

### CRAN 準備

- [ ] R CMD check WARNING/NOTE の修正
- [ ] README に shinyapps.io URL とスクリーンショットを追記
- [ ] CRAN 登録は exametrika と ggExametrika の CRAN 公開後（依存パッケージが CRAN にないと登録不可）
  - exametrika v1.9.0 CRAN 投稿 → ggExametrika CRAN 投稿 → shinyExametrika CRAN 投稿の順
  - Remotes フィールドは CRAN 投稿時に削除する必要がある

---

## コーディング規約

**グローバル CLAUDE.md（`~/.claude/CLAUDE.md`）の規約に準拠する。**以下はプロジェクト固有の追加規約。

### golem モジュール命名規則

- モジュールファイル: `mod_<機能名>.R`（例: `mod_ctt.R`）
- モジュール UI 関数: `mod_<機能名>_ui(id)`
- モジュールサーバ関数: `mod_<機能名>_server(id, ...)`
- ヘルパー関数ファイル: `fct_<カテゴリ>.R`
- ユーティリティ: `utils_<カテゴリ>.R`

### Shiny 固有

- `NS(id)` による名前空間を必ず使用する
- リアクティブ値は `reactiveVal()` または `reactiveValues()` で管理する
- 重い計算は `shiny::withProgress()` で進捗表示する
- エラーハンドリングは `tryCatch()` + `shiny::showNotification()` で UI に表示する
- グローバル変数を使わない。モジュール間のデータ受け渡しはリアクティブ値で行う

### 可視化

- プロットは ggExametrika の関数を優先的に使用する
- ggExametrika に未実装のプロットは exametrika の `plot()` で代替する
- プロットのダウンロード機能を各プロットに付ける（`downloadHandler` + `ggsave`）

### i18n（多言語対応）

- UI テキストはすべて翻訳キーで管理する（ハードコードしない）
- 翻訳ファイルは `inst/i18n/` に JSON 形式で配置する
- 新しい UI テキストを追加したら、translation.json の EN/JA 両方を更新する

---

## 依存パッケージ

### Imports（DESCRIPTION の Imports フィールド）

| パッケージ | 用途 |
|-----------|------|
| shiny | Shiny フレームワーク |
| golem | アプリ構造管理 |
| config | golem 設定管理 |
| exametrika (>= 1.9.0) | 分析エンジン |
| shiny.i18n | 多言語対応 |
| bslib | モダン UI コンポーネント |
| DT | インタラクティブテーブル |
| shinyjs | JavaScript 操作 |
| shinyWidgets | 拡張 UI ウィジェット |
| waiter | ローディング画面 |

### Suggests

| パッケージ | 用途 |
|-----------|------|
| ggExametrika | ggplot2 ベース可視化（なくても base plot フォールバックで動作） |
| testthat (>= 3.0.0) | テスト |

**注意**: exametrika v1.9.0 は CRAN 未公開（v1.8.1 が CRAN 最新）、ggExametrika も CRAN 未公開のため、`Remotes` フィールドで GitHub インストールを指定している。両パッケージの CRAN 公開後に Remotes を削除し、バージョン制約を追加する予定。

---

## チーム開発ルール

### ブランチ戦略

- `main`: 安定版。直接プッシュ禁止
- `develop`: 開発統合ブランチ
- `feature/<機能名>`: 機能開発ブランチ（例: `feature/mod-ctt`）
- PR（Pull Request）経由で `develop` にマージする

### 分担の単位

- 1モジュール = 1担当者が基本
- モジュール単位で独立して開発・テストできる設計にする
- 共通部分（データ読み込み、i18n、ユーティリティ）の変更は事前に相談する

### コミットメッセージ

- 日本語 OK
- プレフィックスを付ける: `feat:`, `fix:`, `docs:`, `refactor:`, `test:`
- 例: `feat: CTTモジュールの基本UIを実装`

---

## exametrika 分析関数リファレンス（主要）

Shiny モジュール開発時の参照用。各モジュールがラップすべき関数と主要パラメータ。

### データ入力

```r
dataFormat(data, na = NULL, id = 1, Z = NULL, w = NULL,
           response.type = NULL, CA = NULL)
# response.type: "binary", "ordinal", "rated", "nominal"
```

### CTT

```r
CTT(U, Z = NULL, w = NULL, na = NULL)
# 返り値: Reliability, ItemAnalysis, TestStatistics, TestFitIndices
```

### IRT

```r
IRT(U, model = 2, Z = NULL, w = NULL, na = NULL)
# model: 2 (2PL), 3 (3PL), 4 (4PL)
# 返り値: ability, lambda, SE, TestFitIndices
# プロット: ICC, TRF, IIC, TIC
```

### GRM

```r
GRM(U, Z = NULL, w = NULL, na = NULL)
# 多値 IRT（Graded Response Model）
# プロット: ICC, TRF, IIC, TIC
```

### LCA

```r
LCA(U, ncls = 2, Z = NULL, w = NULL, na = NULL)
# ncls: 潜在クラス数（2-20）
# プロット: IRP, TRP, LCD, CMP
```

### LRA

```r
LRA(U, nrank = 2, method = "GTM", mic = FALSE, Z = NULL, w = NULL, na = NULL)
# nrank: 潜在ランク数, method: "GTM"/"SOM", mic: 単調増加制約
# プロット: IRP, TRP, LRD, RMP
```

### Biclustering

```r
Biclustering(U, ncls = 2, nfld = 2, method = "B", mic = FALSE,
             Z = NULL, w = NULL, na = NULL)
# method: "B"(Biclustering) / "R"(Ranklustering)
# プロット: FRP, TRP, Array, CMP/RMP, CRV/RRV
```

### IRM

```r
Biclustering_IRM(U, gamma_c = 1, gamma_f = 1, Z = NULL, w = NULL, na = NULL)
# クラス数・フィールド数を自動決定（Chinese Restaurant Process）
```

### GridSearch

```r
GridSearch(U, method = "B", max_ncls = 10, max_nfld = 10, index = "BIC",
           Z = NULL, w = NULL, na = NULL)
# index: "AIC", "BIC", "loglik"
```

### BNM

```r
BNM(U, adj_matrix = NULL, adj_file = NULL, g = NULL, Z = NULL, w = NULL, na = NULL)
BNM_GA(U, population = 20, Rs = 0.5, Rm = 0.002, maxParents = 2, ...)
BNM_PBIL(U, population = 20, Rs = 0.5, Rm = 0.005, maxParents = 2, ...)
# DAG の入力が必要（隣接行列 / CSV / igraph）
```

### 局所依存モデル

```r
LDLRA(U, ncls = 5, adj_matrix = NULL, adj_file = NULL, g_list = NULL, ...)
LDB(U, ncls = 5, conf = NULL, adj_file = NULL, ...)
BINET(U, ncls = 13, nfld = 12, conf = NULL, adj_file = NULL, ...)
```

---

## CI / テスト構成

### GitHub Actions

- ワークフロー: `.github/workflows/R-CMD-check.yaml`
- トリガー: push（main / develop）および pull request
- マトリクス: macOS-latest (release) + Ubuntu-latest (release / devel) の3環境
- 現状: **失敗中**（2026-02-26 時点）。exametrika v1.9.0 が CRAN 未公開のため、CI 環境でのインストールに問題がある可能性あり

### テストファイル

| ファイル | 内容 |
|---------|------|
| `tests/testthat/test-golem-recommended.R` | golem 基本テスト（app_ui / app_server / app_sys / golem-config の存在確認） |
| `tests/testthat/test-fct_analysis.R` | ヘルパー関数のユニットテスト（safe_field, extract_fit_indices） |

### ローカルテスト・ビルド確認

```bash
cd ~/Dropbox/Git/shinyExametrika
Rscript -e "devtools::document()"
Rscript -e "devtools::test()"
Rscript -e "devtools::check()"
Rscript -e "shinyExametrika::run_app()"  # アプリの動作確認
```

---

## デプロイ

- **shinyapps.io**: デプロイ済み（https://kosugitti.shinyapps.io/shinyExametrika/）
  - アカウント: kosugitti（free plan）
  - `app.R` がデプロイ用エントリポイント（golem 構造を直接 source する方式）
  - rsconnect v1.7.0 使用
- **その他候補**: Shiny Server（研究室サーバ） / shinylive（GitHub Pages）— 未対応

---

## 変更履歴の記録（必須ルール）

**軽微なバグ修正でも何でも、変更を加えたら必ず NEWS.md に記録すること。**

- これは全ての作業に適用される恒久的なルールである
- 新機能追加、バグ修正、リファクタリング、ドキュメント修正など、種類を問わず記録する
- NEWS.md のフォーマット: バージョン番号ごとにセクションを分け、変更内容を箇条書きで記載する

---

## 現在のリポジトリ状態（2026-02-26 時点）

### ブランチ

- `main` と `develop` は完全同期済み（差分なし）
- feature ブランチは全てマージ済み・削除済み（ローカル・リモートとも）

### GitHub Issues / PR

- Open Issues: 0
- Open PR: 0
- 全 9 PR が MERGED 済み（#1 data-format 〜 #9 mod-biclustering）

### CI

- GitHub Actions: R-CMD-check（macOS-latest + ubuntu-latest release/devel の 3 環境）
- `.github/workflows/R-CMD-check.yaml`

### テスト

- testthat: 2 テストファイル
  - `test-golem-recommended.R`: golem 基本テスト（app_ui, app_server, app_sys, golem-config の存在と型）
  - `test-fct_analysis.R`: ヘルパー関数ユニットテスト（safe_field, extract_fit_indices）

---

## メモ・注意点

- exametrika の分析は計算コストが高いものがある（特に GridSearch, BNM_GA, IRM）。UI にプログレスバーを表示し、タイムアウト対策を入れること
- exametrika v1.8.0 以降、出力オブジェクトのフィールド名が snake_case に移行中（`n_class`, `n_field` 等）。新しい名前を使うこと
- ggExametrika は開発中（v0.0.34）。未実装プロットがある場合は exametrika の `plot()` にフォールバックする
- shinyapps.io にデプロイ済み。`app.R` を変更する場合は `R/` の変更と整合性を保つこと
- R/ のコードには非 ASCII 文字（日本語コメント含む）を入れないこと（CRAN コンプライアンス対応済み）
- README.md の Phase 2 の Status 記載が「LCA, LRA done」のまま。Biclustering 完了分を含め更新が必要（次回作業時）

---

## テスト用サンプルデータ（dev/）

| ファイル | 行数 | 項目数 | 回答形式 | ID列 | 欠損値 | 説明 |
|---------|------|--------|---------|------|--------|------|
| `sample01.csv` | 100 | 20 | 二値（0/1） | あり（先頭列, S001〜S100） | 99（約1%） | CSV アップロード + ID列あり + 欠損値処理のテスト用 |
| `sample02.csv` | 150 | 15 | 4段階（1〜4） | なし | 99（約1%） | 多値データ + ID列なし + 欠損値処理のテスト用 |

- 乱数シード: `set.seed(42)` で生成（再現可能）
- 欠損値コード `99` を dataFormat() の `na` 引数に指定して読み込むこと
