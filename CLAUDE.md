# shinyExametrika — プロジェクト CLAUDE.md

## プロジェクト概要

exametrika パッケージの Shiny GUI アプリケーション。
コードを書かずにテストデータ分析（CTT, IRT, LCA, LRA, Biclustering 等）を実行・可視化できる Web アプリを提供する。

- **開発者**: 小杉（kosugitti）+ 学生チーム（分担開発）
- **関連パッケージ**:
  - [exametrika](https://github.com/kosugitti/exametrika) — 小杉が開発する心理測定パッケージ（v1.9.0）
  - [ggExametrika](https://github.com/kosugitti/ggExametrika) — 学生が開発する ggplot2 可視化パッケージ（v0.0.20）
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
│   ├── app_ui.R              # メイン UI
│   ├── run_app.R             # アプリ起動関数
│   ├── mod_data_upload.R     # データ読み込みモジュール
│   ├── mod_ctt.R             # CTT 分析モジュール
│   ├── mod_irt.R             # IRT 分析モジュール
│   ├── mod_lca.R             # LCA 分析モジュール
│   ├── mod_lra.R             # LRA 分析モジュール
│   ├── mod_biclustering.R    # Biclustering 分析モジュール
│   ├── mod_irm.R             # IRM 分析モジュール
│   ├── mod_bnm.R             # BNM 分析モジュール（Phase 2）
│   ├── mod_ldlra.R           # LDLRA 分析モジュール（Phase 3）
│   ├── mod_ldb.R             # LDB 分析モジュール（Phase 3）
│   ├── mod_binet.R           # BINET 分析モジュール（Phase 3）
│   ├── fct_analysis.R        # 分析ヘルパー関数
│   ├── fct_i18n.R            # 多言語対応ヘルパー
│   └── utils_*.R             # ユーティリティ関数群
├── inst/
│   ├── app/www/              # CSS, JS, 画像等の静的アセット
│   ├── golem-config.yml      # golem 設定ファイル
│   └── i18n/                 # 翻訳ファイル（JSON）
│       ├── en.json
│       └── ja.json
├── tests/
│   └── testthat/
├── data-raw/                 # サンプルデータ生成スクリプト
├── dev/                      # golem 開発用スクリプト
│   ├── 01_start.R
│   ├── 02_dev.R
│   └── 03_deploy.R
├── man/
├── .gitignore
├── CLAUDE.md                 # このファイル
├── NEWS.md                   # 変更履歴（全変更を必ず記録）
└── README.md
```

### UI 構成

- **ナビゲーション**: タブベース（shinydashboard または bslib）
  - データ読み込みタブ
  - 分析タブ（分析手法ごとにサブタブ）
  - 結果表示タブ（テーブル + プロット）
- **言語切替**: shiny.i18n による日英切替（ヘッダーにトグル配置）

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

### Phase 0: プロジェクト基盤（現在）

- [ ] golem プロジェクト初期化
- [ ] 依存パッケージのセットアップ
- [ ] CI/CD 設定（GitHub Actions）
- [ ] i18n 基盤の構築
- [ ] データ読み込みモジュール（CSV / サンプルデータ）

### Phase 1: 基本分析

- [ ] CTT モジュール
- [ ] IRT モジュール（2PL / 3PL / 4PL）
- [ ] GRM モジュール（多値 IRT）

### Phase 2: 潜在構造分析

- [ ] LCA モジュール
- [ ] LRA モジュール
- [ ] Biclustering モジュール
- [ ] IRM モジュール
- [ ] GridSearch 統合

### Phase 3: ネットワーク・局所依存モデル

- [ ] BNM モジュール（DAG 入力 UI 含む）
- [ ] LDLRA モジュール
- [ ] LDB モジュール
- [ ] BINET モジュール

### Phase 4: 仕上げ

- [ ] UI/UX 改善
- [ ] ドキュメント・ヘルプ統合
- [ ] デプロイ対応
- [ ] パフォーマンス最適化

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
- 新しい UI テキストを追加したら、en.json と ja.json の両方を更新する

---

## 依存パッケージ

### 必須

| パッケージ | 用途 |
|-----------|------|
| shiny | Shiny フレームワーク |
| golem | アプリ構造管理 |
| exametrika (>= 1.9.0) | 分析エンジン |
| ggExametrika | ggplot2 ベース可視化 |
| shiny.i18n | 多言語対応 |
| bslib | モダン UI コンポーネント |
| DT | インタラクティブテーブル |

### 推奨

| パッケージ | 用途 |
|-----------|------|
| shinyjs | JavaScript 操作 |
| waiter | ローディング画面 |
| shinyWidgets | 拡張 UI ウィジェット |

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

## デプロイ

デプロイ先は未定。まずローカルで動作するものを作り、後で決定する。
候補: shinyapps.io / Shiny Server（研究室サーバ） / shinylive（GitHub Pages）

---

## 変更履歴の記録（必須ルール）

**軽微なバグ修正でも何でも、変更を加えたら必ず NEWS.md に記録すること。**

- これは全ての作業に適用される恒久的なルールである
- 新機能追加、バグ修正、リファクタリング、ドキュメント修正など、種類を問わず記録する
- NEWS.md のフォーマット: バージョン番号ごとにセクションを分け、変更内容を箇条書きで記載する

---

## メモ・注意点

- exametrika の分析は計算コストが高いものがある（特に GridSearch, BNM_GA, IRM）。UI にプログレスバーを表示し、タイムアウト対策を入れること
- exametrika v1.8.0 以降、出力オブジェクトのフィールド名が snake_case に移行中（`n_class`, `n_field` 等）。新しい名前を使うこと
- ggExametrika は開発中（v0.0.20）。未実装プロットがある場合は exametrika の `plot()` にフォールバックする

---

## テスト用サンプルデータ（dev/）

| ファイル | 行数 | 項目数 | 回答形式 | ID列 | 欠損値 | 説明 |
|---------|------|--------|---------|------|--------|------|
| `sample01.csv` | 100 | 20 | 二値（0/1） | あり（先頭列, S001〜S100） | 99（約1%） | CSV アップロード + ID列あり + 欠損値処理のテスト用 |
| `sample02.csv` | 150 | 15 | 4段階（1〜4） | なし | 99（約1%） | 多値データ + ID列なし + 欠損値処理のテスト用 |

- 乱数シード: `set.seed(42)` で生成（再現可能）
- 欠損値コード `99` を dataFormat() の `na` 引数に指定して読み込むこと
