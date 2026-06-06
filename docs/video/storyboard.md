# shinyExametrika 使い方動画 — ストーリーボード / 録画ガイド

対象アプリ: https://kosugitti.shinyapps.io/shinyExametrika/

このファイルは 3 役を兼ねる:

1. 録画ガイド（先生が画面録画するときの操作手順とシーンごとの目安秒数）
2. ナレーション台本（日本語 / 英語）
3. 音声合成の入力元（`build_video.py` がここからシーン台本を読む）

方針:

- 映像トラックは先生が QuickTime 等で画面録画（無音でよい）。
- 音声トラックは AI 音声合成（日本語 = VOICEVOX，英語 = 別エンジン）で生成。
- 各シーンの「目安秒数」は合成音声の尺に合わせてある。録画時はこの秒数を目安に操作を進めると後合成が楽。
- 日本語版・英語版の 2 本を作る。英語版はアプリ右上トグルで UI を English に切り替えて録画する。

録画解像度: 1920×1080（フルHD）推奨。ブラウザはタブ・ブックマークバーを隠してアプリだけ映す。

---

## Scene 0 — タイトル（目安 6 秒）

画面: タイトルカード（`build_video.py` が静止画を自動挿入。録画不要）

- JA: shinyExametrika。コードを書かずに，テストデータ分析をブラウザで。
- EN: shinyExametrika. Test data analysis in your browser — no coding required.

## Scene 1 — アプリを開く / Guide タブ（目安 18 秒）

画面操作: ブラウザでアプリ URL を開く。Guide タブが最初に表示される。上から下へゆっくりスクロールし，4 ステップの説明を見せる。

- JA: これは exametrika パッケージの機能を，ブラウザだけで使える Web アプリ，shinyExametrika です。最初に開く Guide タブに，分析の進め方が 4 ステップでまとまっています。データを読み込み，型を整え，分析を実行し，結果を見る。この流れを順に追っていきましょう。
- EN: This is shinyExametrika, a web app that brings the exametrika package to your browser. The Guide tab you land on lays out the workflow in four steps: load your data, format it, run an analysis, and view the results. Let's walk through them.

## Scene 2 — データ読み込み（目安 22 秒）

画面操作: Data タブをクリック。左サイドバーの「データソース」で "Use sample data" を選び，ドロップダウンから `J15S500` を選択。右側にデータプレビューが出る。

- JA: まず Data タブです。自分の CSV をアップロードすることもできますが，今回は組み込みのサンプルデータを使います。J15S500，15 項目・500 人の二値データを選びます。右側に読み込んだデータのプレビューが表示されました。
- EN: First, the Data tab. You can upload your own CSV file, but here we'll use a built-in sample: J15S500, a binary dataset of fifteen items and five hundred examinees. The preview of the loaded data appears on the right.

## Scene 3 — データの整形（目安 18 秒）

画面操作: ID 列のプルダウンと分析変数の選択を見せる（サンプルは自動判定済み）。「Format Data」ボタンをクリック。ナビ左の現データセット表示が `● J15S500 [binary, 15 × 500]` のように赤字で点く。上部の分析タブが有効化（点灯）するのを見せる。

- JA: 次にデータを整えます。ID の列と，分析に使う項目を指定して，Format Data ボタンを押します。すると画面の左上に，今読み込んでいるデータの形が表示されます。同時に，このデータ型に合う分析タブだけが有効になります。二値データなので，二値向けの分析が選べるようになりました。
- EN: Next we format the data. We tell the app which column is the ID and which items to analyze, then click Format Data. The header now shows the active dataset and its shape, and only the analysis tabs that fit this data type light up. Since this is binary data, the binary-compatible analyses become available.

## Scene 4 — 記述統計（目安 16 秒）

画面操作: Descriptives タブをクリック。実行ボタンを押し，項目ごとの通過率・得点分布などのテーブルとプロットを見せる。

- JA: まずは記述統計です。Descriptives タブで実行すると，項目ごとの通過率や，合計得点の分布が表示されます。データの素性をここで掴みます。
- EN: Let's start with descriptive statistics. The Descriptives tab shows each item's pass rate and the distribution of total scores — a quick feel for the data.

## Scene 5 — 項目応答理論（IRT）（目安 26 秒）

画面操作: IRT タブをクリック。サイドバーでモデル（2PL など）を選び，パラメータの ? ツールチップに軽く触れる。実行ボタンを押す。適合度指標テーブル → 項目パラメータ（識別力・困難度）テーブル → 項目特性曲線（ICC）プロットの順に見せる。Download Plot ボタンも指す。

- JA: 本格的な分析の例として，項目応答理論を見てみましょう。IRT タブでモデルを選びます。各パラメータには，ハテナマークにカーソルを合わせると説明が出ます。実行すると，モデルの適合度，項目ごとの識別力と困難度，そして項目特性曲線が描かれます。曲線が右にあるほど難しい項目です。プロットやテーブルはボタンからダウンロードできます。
- EN: For a fuller analysis, let's look at item response theory. In the IRT tab we choose a model. Hovering the question mark next to each parameter shows an explanation. Running it gives the model fit, each item's discrimination and difficulty, and the item characteristic curves. Curves further to the right mark harder items. You can download any plot or table with the buttons provided.

## Scene 6 — もう一つのモデル（Biclustering）（目安 20 秒）

画面操作: Biclustering タブをクリック。クラス数・フィールド数を設定して実行。array plot（並べ替えられたヒートマップ）を見せる。

- JA: shinyExametrika には他にも，潜在クラス分析やバイクラスタリングなど，多くのモデルが揃っています。例えばバイクラスタリングでは，受験者と項目を同時にグループ分けし，並べ替えた反応パターンをヒートマップで可視化できます。
- EN: shinyExametrika offers many more models, from latent class analysis to biclustering. Biclustering, for instance, groups examinees and items at the same time and visualizes the reordered response pattern as a heatmap.

## Scene 7 — 言語切替・まとめ（目安 16 秒）

画面操作: 右上の EN / JA トグルを切り替えて UI 言語が変わるのを見せる。Guide タブに戻る。

- JA: 画面右上のトグルで，日本語と英語をいつでも切り替えられます。コードを書かずに，アップロードして，ボタンを押すだけ。shinyExametrika で，テストデータ分析を始めてみてください。
- EN: The toggle in the top right switches between English and Japanese at any time. No code — just upload your data and click. Give shinyExametrika a try for your own test data analysis.

## Scene 8 — エンドカード（目安 6 秒）

画面: エンドカード（静止画。URL と GitHub を表示。録画不要）

- JA: shinyExametrika。kosugitti.shinyapps.io/shinyExametrika
- EN: shinyExametrika. kosugitti.shinyapps.io/shinyExametrika

---

合計目安: 約 2 分 50 秒（タイトル・エンドカード込み）。
