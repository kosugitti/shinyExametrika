# shinyExametrika Work Log

Per-session internal narrative for shinyExametrika. User-facing changes
go in `NEWS.md`; this file captures *why* and *what was investigated*.
Entries are newest-first.

## CLAUDE.mdからの退避 (2026-07-07)

CLAUDE.md のダイエットに伴い、日付つきの経緯・完了済みタスクのログを一字一句そのまま以下へ退避した（要約・書き換えなし。「元:」ラベルのみ退避時に付与）。

### 元: ヘッダの最終更新行

**最終更新: 2026-06-07**（使い方動画 日英公開 + shinylive 4アプリ分割実装・公開は見送り + CI 緑化。当セッションで一区切り）

### 元: 開発フェーズ（完了済みチェック項目）

Phase 0: プロジェクト基盤

- [x] golem プロジェクト初期化
- [x] 依存パッケージのセットアップ
- [x] CI/CD 設定（GitHub Actions: R-CMD-check.yaml）
- [x] i18n 基盤の構築（shiny.i18n, translation.json）
- [x] データ読み込みモジュール（CSV / サンプルデータ）
- [x] ガイドページ（mod_guide.R — ランディングタブ）

Phase 1: 基本分析

- [x] Descriptives モジュール（記述統計）
- [x] CTT モジュール
- [x] IRT モジュール（2PL / 3PL / 4PL）
- [x] GRM モジュール（多値 IRT）

Phase 2: 潜在構造分析

- [x] LCA モジュール
- [x] LRA モジュール
- [x] Biclustering モジュール
- [x] IRM モジュール（CRP によるクラス数・フィールド数自動決定）

Phase 3: ネットワーク・局所依存モデル

- [x] DAG 入力共通コンポーネント（fct_dag.R — Phase 3 全モジュール共用、ranked DAG対応済み）
- [x] BNM モジュール（固定DAG / BNM_GA / BNM_PBIL、DAG可視化対応）
- [x] LDLRA モジュール（固定DAG / LDLRA_PBIL、ランク別DAG入力、OAC表示、2026-03-25 実装済み）

Phase 4: 仕上げ

- [x] デプロイ対応（shinyapps.io デプロイ済み）

### 元: 今後のTODO 短期（完了済み項目）

- [x] IRM モジュールの実装（PR #10 でマージ済み、2026-02-26）
- [x] IRM seed UI の公開（再現性のための乱数シード指定、2026-02-26 実装済み）

### 元: 今後のTODO 中期（完了済み項目）

- [x] DAG 入力共通コンポーネント（fct_dag.R、From/To CSV アップロード + バリデーション + ranked DAG対応、2026-02-28〜03-25 実装済み）
- [x] BNM モジュール（固定DAG / BNM_GA / BNM_PBIL の3モード、plotGraph_gg DAG可視化対応、2026-02-28 実装済み）
- [x] LDLRA モジュール（固定DAG / LDLRA_PBIL、ランク別DAG入力、OAC表示・CCRR・PIRP対応、2026-03-25 実装済み）

### 元: UX 洗練（完了済み項目）

- [x] **優先A-1: データ未読込/型不一致の pre-check 統一**（2026-06-03 実装）
  - `fct_precheck.R` に `check_data_requirement()` / `precheck_banner()` を新設
  - 全10分析タブの冒頭に `uiOutput(ns("precheck"))` を配置し、データ未設定/型不一致を
    黄色アラートで明示（従来は silent な `req()` のみ）。i18n（en/ja）対応
  - 型要件: binary=CTT/IRT/LCA/LRA/Biclustering/IRM/BNM/LDLRA、ordinal+rated=GRM、any=Descriptives
- [x] **優先A-2: パラメータ guidance**（2026-06-03 実装）
  - `fct_param_help.R` の `param_label()` で対象パラメータのラベルに (?) ホバーツールチップ
    （説明＋推奨/既定値）を付与。IRT 2PL/3PL/4PL、LRA GTM/SOM・MIC、Biclustering ncls/nfld/
    method/MIC、IRM gamma_c/gamma_f/seed、BNM/LDLRA の analysis_mode/max_parents/population/
    mutation_rate/pbil_alpha 等。i18n（en/ja）18文字列追加
- [x] **優先A-3: 各分析タブ上部に折りたたみ式「このモデルは何か」help**（2026-06-03 実装）
  - `fct_modelhelp.R` の `model_help_block()`（native `<details>`、既定で折りたたみ）を全10タブへ。
    モデル名・1行説明・データ型バッジを表示。文言は mod_guide の既存翻訳を再利用（新規i18nは3文字列）
- [x] **タブのデータ準備ゲーティング＋データセット表示＋データタブ整理**（2026-06-03 実装）
  - 分析タブは dataFormat 前は無効（`.nav-disabled`）、整形後かつ型一致時のみ有効化
    （binaryではGRM無効、ordinalではDescriptives+GRMのみ）。`analysis_tab_requirements()`
    （`fct_precheck.R`）＋app_serverの`observe`→**shinyjs::addClass/removeClass(selector=)**。
    現タブが無効化されたらデータタブへ誘導。chromoteで実ブラウザ検証済み
  - **i18n ライブ切替の不具合修正**: `shiny.i18n::update_lang()` は shiny.i18n 0.3.0 の
    `#i18n-state` 入力バインディング経由で、shiny 1.13 と非互換（`Unexpected input value mode`
    エラーを起動時＋トグル毎に投げる）。`update_lang` をやめ，app_serverの言語observerで
    **shinyjs::runjs により `.i18n` span を `i18n_translations` 辞書から直接書き換え**＋
    `ignoreInit=TRUE`。EN/JA切替がエラー0で動作（実ブラウザ検証済み）。
    当初のタブ制御カスタムJSハンドラはShiny初期化とのレースで有効化が無反応になっていた（→shinyjs化で解消）
  - ナビヘッダ左に現読み込みデータセットを赤字常時表示（`● k2022.csv [binary, 20 × 6]`）。
    `mod_data_upload_server` の戻り値を `list(data, name)` に拡張
  - データ取得方法を「アップロード／サンプル」のラジオ1段に整理（conditionalPanelで該当入力のみ表示）
- [x] **データタブ: 列選択UI**（2026-06-03 実装）
  - ID列をプルダウン（列名）＋分析変数を複数選択（`selectizeInput`）に。複数ID列（ID+GID 等）で
    余分な列が分析に混入し nominal 誤判定になる問題を解消。既定=先頭列ID・残りを分析変数。
    選択ID は分析変数から自動除外、変数0個は警告。`mod_data_upload.R`、testServer テスト追加
- [x] **i18n ライブ切替の修正**（2026-06-03）
  - EN/JA 切替で一部（Settings・Upload 等の静的ラベル）が翻訳されない問題。shiny.i18n は
    `use_js()` 後の `i18n$t()` が出す `.i18n` span だけをJS差し替えする仕様。`usei18n()` がUI末尾に
    あり、それ以前の `i18n$t()` がプレーンテキスト化していた。`app_ui()` 冒頭で `i18n$use_js()` を
    呼ぶ順序に修正（約340ラベルがライブ切替対象に）。属性文脈用に `utils_i18n.R` の `t_plain()` 新設
  - 残: サーバ描画の動的テキスト（データタブの value box・結果表）はトグルでは即時更新されず，
    次のデータ操作時に反映（別途対応）

### 元: 依存パッケージの注意書き

**注意**: exametrika v1.10.1 CRAN 公開済み（2026-03-20）。ggExametrika v1.0.0 CRAN 審査中（2026-03-25 時点）。ggExametrika CRAN 公開後に Remotes を削除し、shinyExametrika の CRAN 投稿準備に入る。

### 元: CI / テスト構成 GitHub Actions の現状行

- 現状: **失敗中**（2026-02-28 時点）。exametrika v1.10.0 が CRAN 未公開のため、CI 環境でのインストールに問題がある可能性あり

### 元: 現在のリポジトリ状態（2026-03-25 時点）セクション全体

### ブランチ

- `develop` は `main` より先行（LDLRA モジュール追加分）
- feature ブランチは全てマージ済み・削除済み

### GitHub Issues / PR

- Open Issues: 0
- Open PR: 0
- 全 10 PR が MERGED/CLOSED 済み（#1 data-format 〜 #10 mod-irm）
- LDLRA はローカルで直接マージ（feature/mod-ldlra → develop）

### CI

- GitHub Actions: R-CMD-check（macOS-latest + ubuntu-latest release/devel の 3 環境）
- `.github/workflows/R-CMD-check.yaml`

### テスト

- testthat: 3 テストファイル、67 テスト
  - `test-golem-recommended.R`: golem 基本テスト（app_ui, app_server, app_sys, golem-config の存在と型）
  - `test-fct_analysis.R`: ヘルパー関数ユニットテスト（safe_field, extract_fit_indices）
  - `test-fct_dag.R`: DAGヘルパー関数ユニットテスト（acyclicity, parsing, validation, ranked DAG）

### R CMD check 状態

- 0 errors / 0 warnings / 4 NOTEs（2026-03-25 時点）
- NOTEs は全て既知（.github, License, app.R/rsconnect, 未使用waiter）

### 元: メモ・注意点（完了済み・時点情報）

- ggExametrika v1.0.0 CRAN審査中（2026-03-25 時点）。未実装プロットがある場合は exametrika の `plot()` にフォールバックする
- README.md の Phase 2 ステータスを更新済み（LCA, LRA, Biclustering, IRM 完了を反映、2026-02-28）

---

## 2026-06-15 — exametrika 1.14.0 動作確認

- exametrika v1.14.0 が 2026-06-14 に CRAN 受理・公開されたのを受け，shinyExametrika が
  新バージョンで問題なく動くか確認。
- ローカルに exametrika 1.14.0.9000（1.14.0 の全変更を含む dev 版，CRAN ミラー未同期のため
  本体ソースから install）+ ggExametrika 1.1.1 を入れたペアに対して `devtools::test()` を実行。
- 結果: **FAIL 0 | WARN 0 | SKIP 0 | PASS 183** で全通過。コード変更は不要。
- shinyExametrika は CRAN 登録対象外（shinyapps.io / shinylive 配布）なので，動作確認のみで完了。

## 2026-06-03（夕） — shinyapps.io デプロイ詰まり + webR/Shinylive スパイク（GO 確定）

### shinyapps.io 再デプロイの停止と対応

- `rsconnect::deployApp` がファイル選別（29ファイル＝R/・inst/・app.R・DESCRIPTION・NAMESPACE のみ，
  CLAUDE.md/NEWS/WORKLOG/dev/tests は ignore で除外）は正常に通ったが，**renv の pre-flight
  validation で停止**。原因はローカルの **exametrika 1.14.0 / ggExametrika 1.1.1 が「unknown source」**
  （CRANにも GitHub remote metadata にも無い＝ローカル R CMD INSTALL 由来）で，renv が
  shinyapps.io 側での復元元を決められないため。
- DESCRIPTION には既に `Remotes: kosugitti/exametrika, kosugitti/ggExametrika` と
  `exametrika (>= 1.10.0)` あり。**対応方針（殿の選択）: CRAN版へ下げる**
  （`install.packages(c("exametrika","ggExametrika"))` で 1.13.1 / 1.1.0 → R再起動 → 再デプロイ）。
- 互換性確認: shinyExametrika が呼ぶ ggExametrika 関数は全て 1.1.0 に存在，`plotArray_gg(r)` は
  位置引数のみで 1.1.1 のリネーム(Clusterd→Clustered)・border 引数は未使用。exametrika 関数も全て
  1.13.1 に存在（1.14.0 はバグ修正のみで新規関数なし）。**CRAN版でフル動作する**。
- 注意: グローバルライブラリを下げるため，お遍路さん(BMS2026 Glasso)・A3論文(IRM二値欠測NaN)等の
  1.14.0 依存に影響。デプロイ後 `remotes::install_github` で戻すのが安全。あるいは 6/15 CRAN
  1.14.0/1.1.1 受理を待てば下げ不要。

### webR/Shinylive 実現可能性スパイク（実ブラウザ end-to-end，GO 確定）

殿が Shinylive 案を再検討。横断メモ(`exametrika-dev/notes/2026-06-03_*.md`)で「未検証の本丸=
exametrika(Rcpp)が webR でビルドできるか」が分岐点だったので決着をつけた。

- **repo.r-wasm.org（webR公式・CRANを自動WASMミラー）に exametrika が C++込みでビルド済み**を発見
  （R4.5枠 exametrika 1.11.0，バイナリ HTTP 200）。Rcpp 1.1.1 も有。webR CI がCRAN全体をWASM化。
- exametrika は webR向きの素性: `LinkingTo: Rcpp` のみ，src の4cppは純粋数学コードで
  thread/fstream/system/fork/OpenMP 地雷ゼロ。
- shinyExametrika の依存15個（exametrika/ggExametrika/shiny/bslib/DT/shinyjs/shinyWidgets/
  shiny.i18n/golem/htmltools/jsonlite/ggplot2/igraph/Rcpp/config）が**全部 webR リポジトリに有**。
- **実証**: 最小Shinyliveアプリを `shinylive::export`→Python COOP/COEP配信→chromote起動。
  webR(R 4.5.1)が起動し **IRT(純R)動作(15×2) + GRM(grm_analytical.cpp/C++)が収束**
  (`iter20 final 6008.30 converged`, `class=c("exametrika","GRM")`)。初回ロード ~36秒
  （コールド，以降 Service Worker キャッシュ）。
- **結論: フル機能(全モデル)の shinyExametrika を Shinylive 化可能。Shinylive が shinyapps.io を
  置き換えうる**（無料・サーバレス・時間/メモリ無制限）。

### 次回（順次エクスポート）

1. golem構造の平坦化（shinyExametrika 自身は webR リポジトリに無い → app ディレクトリ化 or
   ローカルパッケージ同梱）。
2. 自作JS / shinyjs::runjs / shiny.i18n の DOM翻訳が Shinylive の iframe 内で動くか確認。
3. 最重量モデル(IRM Gibbs)の体感速度測定。
4. ホスティング=さくら + `.htaccess`(COOP/COEP)。GitHub Pages 不可。
- 検証手段: `chromote` で実ブラウザ駆動。Shinylive アプリは **iframe 内**で動くため
  `iframe.contentDocument` 経由でDOMを読む。COOP/COEP ヘッダ必須。

---

## 2026-06-03 — UX 洗練（優先A）+ データタブ刷新 + 実ブラウザ検証で3バグ修正

exametrika-dev 横断メモ（`notes/2026-06-03_shinylive_and_shiny_refinement.md`）の
「Shinylive 化より先に三男を洗練する」方針に沿って，優先A〜の改善に着手。途中で
殿の実機テストから複数バグが発覚し，chromote で実ブラウザ検証して根治した。

### 優先A（広く効く・低コスト）

- **A-1 pre-check 統一**: `R/fct_precheck.R` 新設。`check_data_requirement()` /
  `precheck_banner()`。全10分析タブ冒頭に `uiOutput(ns("precheck"))` を配置し，
  データ未設定/型不一致を黄色アラートで明示（従来は silent な `req()` のみ）。
  型要件: binary=CTT/IRT/LCA/LRA/Biclustering/IRM/BNM/LDLRA，ordinal+rated=GRM，
  any=Descriptives。i18n（en/ja）2文字列追加。
- **A-2 パラメータ guidance**: `R/fct_param_help.R` 新設。`param_label()` で
  ラベルに (?) ホバーツールチップ（説明＋推奨/既定値）。IRT 2PL/3PL/4PL，LRA
  GTM/SOM・MIC，Biclustering ncls/nfld/method/MIC，IRM gamma_c/gamma_f/seed，
  BNM/LDLRA の analysis_mode/max_parents/population/mutation_rate/pbil_alpha 等。
  i18n 18文字列追加。
- **A-3 モデル help**: `R/fct_modelhelp.R` 新設。`model_help_block()`（native
  `<details>`，既定で折りたたみ）を全10タブへ。mod_guide の既存翻訳を再利用し
  新規 i18n は3文字列に抑制。

### ガイドページのカード潰れ修正

- `mod_guide.R` の外枠が `layout_column_wrap(heights_equal = "row")` で，
  page_navbar の fill レイアウト下では各カードを画面高に等分割→ overflow:hidden で
  内容クリップ。通常の縦積み（`d-flex flex-column gap-3`）＋各カード `fill = FALSE`
  に変更し，内容高さで描画・ページスクロールに。

### データタブ刷新

- **列選択UI**: ID列をプルダウン（列名）＋分析変数を複数選択（`selectizeInput`）に。
  複数ID列（ID+GID 等）で余分な列が分析に混入し nominal 誤判定になる殿の事例を解消。
  既定=先頭列ID・残りを分析変数，選択ID は自動除外，変数0個は警告。
- **取得方法をラジオ1段に**: 「データの取得方法」ラジオ（アップロード/サンプル）＋
  `conditionalPanel` で該当入力のみ表示。二段スタックを解消。

### タブのデータ準備ゲーティング + データセット表示

- `analysis_tab_requirements()`（`fct_precheck.R`）で tab値→必要型を定義。
  app_server の `observe` が `shinyjs::addClass/removeClass(selector=)` で
  `.nav-disabled` をトグル。dataFormat 前は全分析タブ無効，整形後かつ型一致時のみ
  有効化（binaryでGRM無効，ordinalでDescriptives+GRMのみ）。現タブ無効化時は
  データタブへ誘導（`isolate`）。
- ナビヘッダ左に現読み込みデータセットを赤字常時表示（`● k2022.csv [binary, 20×6]`）。
  `mod_data_upload_server` の戻り値を `list(data, name)` に拡張。

### 実ブラウザ（chromote）検証で根治した3バグ

殿の実機で「Format Data 成功でも分析タブが有効化しない」「サンプル選択でエラー」
が再発。headless では再現せず，`chromote`（新規インストール）で実アプリを起動し
`console.error`/例外/通知を捕捉して特定:

1. **タブ有効化が無反応**: 当初の自作カスタムJSメッセージハンドラ（head の inline
   `Shiny.addCustomMessageHandler`）が Shiny 初期化とレースし，enable が無反応に。
   → **shinyjs::addClass/removeClass** に置換して根治（無効化は効くが有効化が死ぬ症状）。
2. **i18n コンソールエラー**（`Unexpected input value mode: '[object Object]'`）:
   `shiny.i18n::update_lang()` が shiny.i18n 0.3.0 の `#i18n-state` 入力バインディング
   経由で往復し shiny 1.13 と非互換。起動時（observer の init 発火）＋トグル毎に発生。
   → update_lang をやめ，言語 observer で **shinyjs::runjs により `.i18n` span を
   `window.i18n_translations` 辞書から直接書き換え** ＋ `ignoreInit = TRUE`。
   また `i18n$use_js()` を app_ui 冒頭で呼ぶ順序にして全静的ラベル（約340個）を
   `.i18n` span 化（属性文脈は `R/utils_i18n.R` の `t_plain()` でプレーン文字列に）。
3. **サンプル読み込み `unused argument (envir = env)`**: `mod_data_upload.R` の
   `get(input$sample_data, envir = env)` の **無修飾 `get()`** が，殿のセッションに
   attached な「envir 引数を持たない `get`」にマスクされていた（RStudio Run App は
   コンソールセッションを継承）。マスクされた get を再現し殿のエラーと完全一致を確認。
   → マスク不可能な **`env[[input$sample_data]]`** に置換。

### 新規ファイル / テスト

- 新規: `R/fct_precheck.R`，`R/fct_param_help.R`，`R/fct_modelhelp.R`，`R/utils_i18n.R`
- テスト: `test-fct_precheck.R`，`test-fct_modelhelp.R`，`test-i18n.R`，
  `test-mod_data_upload.R`（pre-check/型要件/ゲーティング/i18n use_js/t_plain/
  列選択subset/サンプル読み込み env[[]] 回帰 など）。**全151 PASS**。
- i18n（en/ja）: 計28文字列追加（pre-check 2 + model help 3 + param tooltip 18 +
  data source/analysis vars 等 5）。

### 恒久メモ
- shiny.i18n 0.3.0 の `update_lang` は shiny 1.x で壊れる→ `.i18n` span を
  shinyjs::runjs で直接差し替える。クライアント側クラス操作はカスタムJSハンドラ
  より shinyjs（初期化レース回避）。UIバグは chromote で実検証。
  （メモリ `shiny-i18n-incompatibility.md` にも記録）

---

## 2026-06-02 — i18n 表記統一: GRM/BNM/LDLRA 進捗メッセージのスペース挿入

殿から「exametrika 一家のユーザ可視文字列に他に typo がないか総点検」
の指示で，Explore エージェントが `inst/i18n/translation.json` の日本語訳
で英字直後にスペースが入っていない 6 行を発見。他の 8 行 (CTT/IRT/LCA/
LRA/Biclustering/IRM など) は全てスペース挿入済みで，明らかな表記不統一。

### 修正対象 (6 行)

すべて `inst/i18n/translation.json`:

- L101 `"GRM分析を実行中..."` -> `"GRM 分析を実行中..."`
- L254 `"BNM分析を実行中..."` -> `"BNM 分析を実行中..."`
- L255 `"BNM_GA構造学習を実行中..."` -> `"BNM_GA 構造学習を実行中..."`
- L256 `"BNM_PBIL構造学習を実行中..."` -> `"BNM_PBIL 構造学習を実行中..."`
- L281 `"LDLRA分析を実行中..."` -> `"LDLRA 分析を実行中..."`
- L282 `"LDLRA_PBIL構造学習を実行中..."` -> `"LDLRA_PBIL 構造学習を実行中..."`

### スコープ判断

- 英字以外 (typo そのもの) は本プロジェクトでは見つからなかった
- 専門用語 (biclustering, ranklustering, IRM, LDLRA, BINET 等) は造語のため
  確信度 low として保留したが，どれも実在の用語で要確認案件なし
- 関連修正: 親パッケージ exametrika 側の R/*.R で 4 件の文字列 typo を
  同時修正 (詳細は `~/Dropbox/Git/exametrika/WORKLOG.md` 2026-06-02 PM)

### 確認

`grep '(GRM|BNM|LDLRA).*分析を実行中' translation.json` でスペースなし
パターンが 0 件であることを確認。R CMD check / shiny.i18n 経由の動作確認
は未実施 (JSON の最小修正のみのため不要と判断)。

## 2026-05-20 — PR#14 マージ (DAG plot height slider)

arimune-san からの PR#14 "feat: DAG plot height slider for BNM and LDLRA"
(branch `feature/dag-plot-improvement`, commit 57f4b95) を smoke test 後
develop に squash マージ (merge commit 8d8297c)。

### 変更内容

- `R/mod_bnm.R`, `R/mod_ldlra.R`: DAG plot にスライダー (400–1200px, step 50,
  default 600) を追加し，`renderPlot()` の `height = function() {...}` で
  DAG 選択時のみ slider 値を採用，それ以外は 600 にフォールバック。
- `R/mod_*.R`: Download Plot ボタンを plot の **上** に移動 (重なり防止)。
- `inst/i18n/translation.json`: "Plot Height (px)" / "プロットの高さ (px)"
  を追加。
- `NEWS.md`: 変更履歴追記。

### smoke test

Newton で実施:

```r
devtools::load_all(".")          # OK
devtools::test()                 # 67/67 PASS (filter dag: 57/57 含む)
# translation.json の "Plot Height (px)" キーを jsonlite::fromJSON で検証 → 通過
```

依存パッケージ `config / golem / shiny.i18n / shinyjs / shinyWidgets` は
Newton に未インストールだったので install.packages で導入してから
load_all 通過確認。

### レビュー判断

- 既存挙動を破壊しない条件付き height (`input$plot_type == "DAG" &&
  !is.null(input$plot_height)`) で，DAG 以外の plot や UI 初期化前は 600 維持。
- LDLRA は plot_options_ui の if 分岐で IRP/TRP/LRD/RMP のときには slider
  を出さないように設計されており，PR description の Test plan と整合。
- UI 動作確認は arimune-san が手元で済ませている (PR description) ので
  コードレビューと自動テストで十分と判断し，approve + squash merge。

### 残課題

- LDB / BINET タブ実装 (12/14 タブ実装済)。
- v1.13.1 exametrika との互換性は ggExametrika の smoke test と
  共通で確認済 (BNM / LDLRA の API 変更なし)。

## 2026-06-07 使い方動画 (日英) + shinylive 4アプリ分割

### 使い方動画

- 録画先行方式。先生が英語UIで画面録画 (`docs/video/movie.mov`, 282秒) を撮り，
  それに合わせてナレーションを配置 (write to picture)。
- `docs/video/build_video.py` を拡張:
  - エンドカードを無音化し，ナレーション (シーン9) は本編の最後に重ねて流す
    →音声が終わってから無音エンドカードを出す方式に変更 (`card_silent`)。
  - `_body_end` で本編をトリム (エンドカードを早く出す)。
  - `bgm` コマンド新設: BGM を小音量ループ＋末尾フェードアウト＋ナレーション中の
    ダッキング (sidechaincompress)。`start` 秒で BGM 開始を遅らせ，タイトルコール中は
    無音・本編開始でBGMイン (フェードインなし)。
  - シーン配置は `anchors_<lang>.json` (録画本編内の開始秒，`_body_end` 付き)。
- 日本語ナレーション (`narration_ja.md`): #5 を尺に合わせて短縮 (枠25秒に対し
  元36.7秒→24秒)。読み修正は build_video.py の `PRON_MAP_JA`。
  VOICEVOX 四国めたん (speaker=2)。
- 英語ナレーション (`narration_en.md`): 全面書き直し (日本語版の構成に一致)。
  同じ録画 (UIは英語) に当てるためアンカーは日本語版と同じ。OpenAI gpt-4o-mini-tts
  voice="ash" (陽気な口調)。キーは `~/Dropbox/.openai_key`。
- BGM: Pixabay の Maksym Malko (cute happy kids)。エンドカードにテキストクレジット，
  YouTube 説明欄用にHTMLリンク版クレジットを `youtube_descriptions.txt` に同梱。
- 公開: 日本語 https://youtu.be/q5I25ttD_Bs / 英語 https://youtu.be/dKi-vMs1iYQ
- exametrika Discussions #31 / shinyExametrika Discussions #15 (Discussions を
  有効化してから) で日英まとめて告知。
- `docs/video/*.mov` は .gitignore。動画・音声 (mp4/wav/mp3) も従来どおり除外。

### shinylive 4アプリ分割

- `app_ui()` / `app_server()` に `tabs` 引数を追加 (既定 NULL = 全タブ。本体アプリ不変)。
  部分集合を渡すとそのタブのパネルだけ構築し，対応するモジュールサーバだけ起動。
  `tab_guide` / `tab_data` は常に含む。タブゲーティングも build 内のタブのみ対象。
- `dev/build_shinylive.R`: 4アプリを生成して1サイトに同居エクスポート。
  - ctt = Descriptives + CTT / irt = IRT + GRM / lca = LCA + LRA /
    bicl = Biclustering + IRM。各アプリに Guide + Data (dataFormat) を内包。
  - 各 app dir に共有ヘルパー (fct_*) + 必要モジュール + `inst/` をコピーし，
    `app.R` を生成 (`app_ui(request, tabs=...)` / `app_server(..., tabs=...)`)。
  - `shinylive::export(appdir, destdir, subdir=name)` で `shinylive/site/<name>` へ。
- 実ブラウザ (chromote) で4アプリとも webR 起動・正しいタブ構成を確認。先生が
  各分析を実際に回してエラーなしを確認。
- 配信の罠 (メモリ `shinylive-serving-gotchas` 記録):
  - COOP/COEP を自前サーバで付ける場合，COEP は `credentialless` (require-corp だと
    webR の repo.r-wasm.org からの cross-origin パッケージ取得がブロックされる)。
  - サイトルートの `shinylive-sw.js` は全サブアプリを制御する本体。消すと起動しない。
  - エクスポート版は `#root` 内の iframe (`app-frame`) でアプリが走る。
  - webR の exametrika は repo.r-wasm.org の wasm 版 = 1.11.0 (ローカル 1.13.1 より遅れる)。
- 生成物 `shinylive/` は .gitignore。再生成は `Rscript dev/build_shinylive.R`。

### CI 修正 (R CMD check warnings → 緑)

- `error_on: "warning"` の R-CMD-check が 3 warnings で落ち続けていた (6/3 由来、6/7 の
  変更が原因ではない)。3 件を解消して緑化:
  - 非ASCII (R/ の `×`/`●`) → `×`/`●` エスケープ (app_server.R)。
  - `htmltools::HTML()` (R コード) → shiny 再エクスポートの `HTML()` (app_ui.R)。
  - tests の `htmltools::renderTags` 未宣言 → DESCRIPTION の Suggests に htmltools 追加。
- 結果 0 errors / 0 warnings / 2 notes で success。commit `c6c2a0b`。

### shinylive 公開の判断 (2026-06-07)

- GitHub Pages への一般公開は見送り。理由: shinyapps.io でフル機能版が稼働中で、
  shinylive 版は4アプリ=機能の部分集合 (BNM/LDLRA は webR で重く除外) のため置換に
  ならない。二重メンテ・初回ロード重・wasm版1.11.0のバージョン遅れ等のコストに対し、
  常時公開の意義が現時点では薄い。
- ビルド資産は温存 (`dev/build_shinylive.R` で1コマンド再生成、ローカル動作確認済み)。
- 将来の公開トリガー: (a) 動画公開でアクセスが伸び shinyapps.io 無料枠 (月25時間) の
  上限が問題化, (b) データをサーバに出さないプライバシー重視の配布が必要, (c) 教材への
  iframe 埋め込み。いずれか発生時に gh-pages へ出す (サブパス配信でも SW パスは相対計算で動く)。

### このセッションの区切り

- 当初3目標 (shinyapps.io 公開 / 使い方動画 / shinylive 化) は完了。動画は日英 YouTube 公開・
  両リポジトリ Discussions 告知済み。shinylive は実装・検証済みで公開は条件付き保留。
- shinyExametrika プロジェクトは一旦この区切りで完了とする。

### 残課題 (将来)

- shinylive 公開 (上記トリガー発生時)。
- LDB / BINET タブ実装 (引き続き)。
- 優先B の UX 洗練 (長時間計算の時間目安+タイムアウト等)。

## CLAUDE.mdからの退避 (2026-07-17)

ホーム索引(~/Dropbox/CLAUDE.md)のステータスセル圧縮時の退避(退避時点の全文):

golem 製 Shiny GUI(12/14タブ実装，LDB/BINET はプレースホルダー)。shinyapps.io 公開・使い方動画(日英)・shinylive 4アプリ分割の当初3目標完了(2026-06-07)で一区切り。exametrika 1.14.0 動作確認済(6/15, 183 tests pass・CRAN登録不要)。次の作業は優先B UX洗練 または LDB/BINET 実装。詳細→Git/shinyExametrika/{CLAUDE,WORKLOG}.md


## 2026-08-20 exametrika 2.0.0 対応

`safe_field(result, new_name, old_name, default)` から旧名引数を落とし，
`safe_field(result, field, default)` に簡素化(`e9fb8ea`)。

exametrika 2.0.0 が `Nclass` / `Nfield` / `Nrank` / `N_Cycle` を削除した。
これらは 1.8.0 から非推奨で，本パッケージは `exametrika (>= 1.10.0)` を要求するため，
**旧名を試す経路はどのサポート版でも到達不能**だった。呼び出し 2 箇所
(`mod_ldlra.R`)を更新し，フォールバック挙動を検査していたテストは
既定値の検査に置き換えた。

exametrika 2.0.0(提出した実物)をインストールした環境で **183 件全通過**。
