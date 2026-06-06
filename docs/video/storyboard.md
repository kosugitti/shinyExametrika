# shinyExametrika 使い方動画 — 録画ガイド / ストーリーボード

対象アプリ（最新・本番）: https://kosugitti.shinyapps.io/shinyExametrika/

## 制作方式（record-first）

1. 先生が、この shot list に沿って画面録画する（無音でOK、尺は気にしない）。
2. 受け取った録画を Claude が見て、実際の画と尺に合わせて日本語ナレーション原稿を書く。
3. 音声合成（日本語＝VOICEVOX 四国めたん／英語＝OpenAI TTS ash・明るめ口調）。
4. `build_video.py assemble` でタイトル/エンドカードを前後に付け、ナレーションを重畳して mp4 化。
5. 日本語版・英語版の2本（英語版は右上トグルで UI を English にして別途録画）。

録画: QuickTime（Cmd+Shift+5）、できれば 1920×1080。ブックマークバー・余計なタブは隠す。
各結果画面で2〜3秒止まると、後でナレーションを乗せやすい。

デモ用データ（ローカルからアップロードする）: `docs/video/demo_data/`
- `demo_binary.csv`（ID, Group, Item01–10, 一部 99 欠測）
- `demo_ordinal.csv`（ID, Group, Q01–08, 1–5）
- `demo_nominal.csv`（ID, Group, V01–06, 1–4）
- `demo_rated.csv`（ID, Group, Item01–08, 多肢選択1–4）＋ 正答キー `demo_rated_CA.txt`（CA欄に入力）

---

## Shot list

### S1. タイトル（カード・録画不要）
Claude が静止画を前置。

### S2. サイトを開く / Guide をなめる
- ブラウザでアプリ URL を開く。最初に出る Guide タブを上から下へゆっくりスクロール。
- 4ステップ（Load → Format → Run → Results）が書いてあるのを見せる。
- ナレーション意図: 「使い方はこの表紙ページに全部書いてあります」。

### S3. まずサンプルデータでひと回し（軽く）
- Data タブ → データソース「サンプル」→ `J15S500` を選択 → プレビュー表示。
- ナレーション意図: 「すぐ試すなら組み込みサンプルが使えます」。

### S4. dataFormat を丁寧に（ローカルCSVのアップロード）★ここが主役
binary を例に、設定の各項目を順に見せる:
- データソース「アップロード」→ `demo_binary.csv` を選ぶ。
- ID列 = ID を選ぶ。分析変数から余分な `Group` 列を外す（ID/グループ列の除外を実演）。
- 欠測値コードに `99` を入れる。
- Response Type は Auto-detect のまま（自動判定を見せる）。
- Format Data → 左上に `● demo_binary [binary, ...]`、タブ点灯。
- ナレーション意図: 「ID・分析する列・欠測の指定。型は自動判定」。

### S5. 4つの回答タイプを順に
- ordinal: `demo_ordinal.csv` をアップ → Group 除外 → Format（型 = ordinal）。
- nominal: `demo_nominal.csv` → Format（型 = nominal）。
- rated: `demo_rated.csv` → Response Type で「Rated」を選ぶ → 「正答キー（CA）」欄に
  `demo_rated_CA.txt` の並び（例 1, 2, 1, 1, 4, 3, 4, 1）を入力 → Format（型 = rated）。
- ナレーション意図: 「二値・順序・名義・採点（正答つき）に対応」。

### S6. 分析を実行（IRT）
- binary データに戻す（再アップ or サンプル J15S500）→ IRT タブ → モデル選択 → Run。
- 適合度・項目母数・被験者母数のテーブル、Plots で ICC を見せる（?ツールチップに触れても良い）。

### S7. 出力（今回の新機能）
- IRT 実行後、左サイドバー Run の下の出力ボタン群を見せる。
- 「分析結果を一括（Excel）」を押してダウンロード（複数シートの xlsx）。
- 「Rスクリプト」を押してダウンロード（操作を再現する R スクリプト）。
- ナレーション意図: 「結果はCSV／Excel一括で保存でき、操作を再現するRスクリプトも出せます」。

### S8. もう一つのモデル（Biclustering）
- Biclustering タブ → クラス数・フィールド数 → Run → array ヒートマップ。

### S9. 言語切替
- 右上 EN/JA トグルを切り替え、UI が切り替わるのを見せる。

### S10. エンドカード（録画不要）
Claude が静止画を後置（URL・GitHub）。

---

## 旧・参考ナレーション（draft）

S2以前の版で書いた `narration_{ja,en}.md` は下書き。最終原稿は録画を見てから
画に合わせて書き直す。
