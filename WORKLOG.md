# shinyExametrika Work Log

Per-session internal narrative for shinyExametrika. User-facing changes
go in `NEWS.md`; this file captures *why* and *what was investigated*.
Entries are newest-first.

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

### 残課題

- shinylive サイトの公開先配置 (さくら + .htaccess 等。SW が COOP/COEP を付与するので
  素の静的ホスティングで可)。
- LDB / BINET タブ実装 (引き続き)。
