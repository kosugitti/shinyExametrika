#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
shinyExametrika 使い方動画ビルダー（日英）

方式: 先生が画面録画（無音）し，AI 音声合成のナレーションを重ねる。

エンジン:
  - 日本語: VOICEVOX（http://127.0.0.1:50021，要起動）。話者 = 四国めたん(ノーマル) id=2
  - 英語  : macOS say（Samantha）

サブコマンド:
  audio  <lang>            シーンごとに音声合成 → narration_<lang>.wav に結合 + timing_<lang>.md 出力
  cards                    タイトル / エンドカード PNG を生成（cards/title.png, cards/end.png）
  assemble <lang> <rec>    録画 <rec> + narration_<lang>.wav → final_<lang>.mp4
                           （タイトル6s + 録画[ナレ重畳] + エンド6s）

依存: ffmpeg, Pillow, （日本語のみ）VOICEVOX 起動 + requests

推奨ワークフロー:
  1. python3 build_video.py cards
  2. VOICEVOX 起動 → python3 build_video.py audio ja
  3.                  python3 build_video.py audio en
  4. narration_<lang>.wav を再生しながら，timing_<lang>.md の尺を目安にアプリを操作して画面録画
  5. python3 build_video.py assemble ja rec_ja.mov  /  assemble en rec_en.mov
"""

import os
import re
import sys
import json
import glob
import shutil
import subprocess
import tempfile

BASE = os.path.dirname(os.path.abspath(__file__))
AUDIO = os.path.join(BASE, "audio")
CARDS = os.path.join(BASE, "cards")
os.makedirs(AUDIO, exist_ok=True)
os.makedirs(CARDS, exist_ok=True)

VOICEVOX_URL = "http://127.0.0.1:50021"
VOICEVOX_SPEAKER = 2          # 四国めたん（ノーマル）。3=ずんだもん, 8=春日部つむぎ

# VOICEVOX はローマ字綴りを正しく読めない。日本語合成の前に固有名詞を
# 読み（カタカナ）に置換する。長い綴りから先に置換すること。
PRON_MAP_JA = [
    ("shinyExametrika", "シャイニーエグザメトリカ"),
    ("Exametrika", "エグザメトリカ"),
    ("exametrika", "エグザメトリカ"),
    ("kosugitti", "コスギッチ"),
    ("shinyapps.io", "シャイニーアップスドットアイオー"),
    ("4 つ", "よっつ"),
    ("4つ", "よっつ"),
    ("500人", "ごひゃくにん"),
    ("15項目", "じゅうごこうもく"),
    ("二値", "にち"),
    ("母数", "ぼすう"),
    ("評定", "ひょうてい"),
    ("右上", "みぎうえ"),
    ("GitHub", "ギットハブ"),
    ("Issue", "イシュー"),
    ("Discussion", "ディスカッション"),
    ("Enjoy", "エンジョイ"),
]


def apply_pron_ja(text):
    for a, b in PRON_MAP_JA:
        text = text.replace(a, b)
    # 英単語(ラテン)と日本語の間のスペースは VOICEVOX で不自然な間になる。
    # 両隣が ASCII のときだけ残し（"Format Data" 等）、片方でも日本語なら詰める。
    text = re.sub(r'(?<=[^\x00-\x7f]) +', '', text)
    text = re.sub(r' +(?=[^\x00-\x7f])', '', text)
    return text

# 英語は OpenAI TTS（gpt-4o-mini-tts, voice=ash, 明るめ口調指定）
OPENAI_KEY_FILE = os.path.expanduser("~/Dropbox/.openai_key")
OPENAI_VOICE = "ash"
OPENAI_INSTRUCTIONS = ("Speak like a cheerful, upbeat and friendly tutorial host. "
                       "Warm, lively, and smooth, with natural phrasing. Not rushed.")
SAY_VOICE = "Aaron"           # フォールバック用 macOS en_US 男性
W, H = 1920, 1080
SCENE_GAP = 1.2              # シーン間の無音(秒)
CARD_SEC = 6                 # タイトル/エンドカードの尺(秒)
END_CARD_SEC = 5.0           # エンドカード(無音)の尺(秒)


# ---- ナレーション原稿のパース --------------------------------------------
def parse_narration(lang):
    path = os.path.join(BASE, f"narration_{lang}.md")
    scenes = {}
    cur = None
    buf = []
    for line in open(path, encoding="utf-8"):
        s = line.rstrip("\n")
        if s.startswith("## "):
            if cur is not None:
                scenes[cur] = " ".join(x.strip() for x in buf if x.strip())
            try:
                cur = int(s[3:].strip())
                buf = []
            except ValueError:
                cur = None
        elif cur is not None:
            buf.append(s)
    if cur is not None:
        scenes[cur] = " ".join(x.strip() for x in buf if x.strip())
    # 説明用の見出し(## N でない部分)で cur=None のものは捨てられる
    return {k: v for k, v in scenes.items() if v}


# ---- 音声合成 -------------------------------------------------------------
def synth_voicevox(text, out_wav):
    import requests
    q = requests.post(f"{VOICEVOX_URL}/audio_query",
                      params={"text": text, "speaker": VOICEVOX_SPEAKER}, timeout=30)
    q.raise_for_status()
    r = requests.post(f"{VOICEVOX_URL}/synthesis",
                      params={"speaker": VOICEVOX_SPEAKER},
                      headers={"Content-Type": "application/json"},
                      data=json.dumps(q.json()), timeout=60)
    r.raise_for_status()
    with open(out_wav, "wb") as f:
        f.write(r.content)


def synth_openai(text, out_wav):
    import urllib.request
    key = open(OPENAI_KEY_FILE).read().strip()
    payload = json.dumps({
        "model": "gpt-4o-mini-tts",
        "voice": OPENAI_VOICE,
        "input": text,
        "instructions": OPENAI_INSTRUCTIONS,
        "response_format": "wav",
    }).encode("utf-8")
    req = urllib.request.Request(
        "https://api.openai.com/v1/audio/speech", data=payload,
        headers={"Authorization": f"Bearer {key}", "Content-Type": "application/json"})
    with urllib.request.urlopen(req, timeout=120) as r:
        data = r.read()
    raw = out_wav + ".raw.wav"
    with open(raw, "wb") as f:
        f.write(data)
    subprocess.run(["ffmpeg", "-y", "-loglevel", "error", "-i", raw,
                    "-ar", "44100", "-ac", "2", out_wav], check=True)
    os.remove(raw)


def synth_say(text, out_wav):
    with tempfile.TemporaryDirectory() as td:
        aiff = os.path.join(td, "x.aiff")
        subprocess.run(["say", "-v", SAY_VOICE, "-o", aiff, text], check=True)
        subprocess.run(["ffmpeg", "-y", "-loglevel", "error", "-i", aiff,
                        "-ar", "44100", "-ac", "2", out_wav], check=True)


PAUSE_PER = 0.6   # 「…」1文字ぶんの実無音(秒)。「……」=1.2秒


def synth_scene(text, out_wav, synth):
    """シーン本文を合成。本文中の「…」連続を，その文字数に比例した実無音に置き換える。"""
    tokens = re.split(r'(…+)', text)
    if not any("…" in t for t in tokens):
        synth(text, out_wav)
        return
    with tempfile.TemporaryDirectory() as td:
        segs = []
        for i, t in enumerate(tokens):
            if not t:
                continue
            if set(t) <= {"…"}:
                dur = len(t) * PAUSE_PER
                sil = os.path.join(td, f"s{i}.wav")
                subprocess.run(["ffmpeg", "-y", "-loglevel", "error", "-f", "lavfi",
                                "-i", "anullsrc=r=44100:cl=stereo", "-t", str(dur), sil], check=True)
                segs.append(sil)
            elif t.strip():
                raw = os.path.join(td, f"t{i}.wav")
                synth(t.strip(), raw)
                norm = os.path.join(td, f"t{i}n.wav")
                subprocess.run(["ffmpeg", "-y", "-loglevel", "error", "-i", raw,
                                "-ar", "44100", "-ac", "2", norm], check=True)
                segs.append(norm)
        listf = os.path.join(td, "l.txt")
        with open(listf, "w") as f:
            for s in segs:
                f.write(f"file '{s}'\n")
        subprocess.run(["ffmpeg", "-y", "-loglevel", "error", "-f", "concat",
                        "-safe", "0", "-i", listf, "-c", "copy", out_wav], check=True)


def wav_duration(path):
    out = subprocess.run(
        ["ffprobe", "-v", "error", "-show_entries", "format=duration",
         "-of", "default=nw=1:nk=1", path],
        capture_output=True, text=True).stdout.strip()
    return float(out) if out else 0.0


def cmd_audio(lang):
    scenes = parse_narration(lang)
    synth = synth_voicevox if lang == "ja" else synth_openai
    if lang == "ja":
        import requests
        try:
            requests.get(f"{VOICEVOX_URL}/version", timeout=3)
        except Exception:
            sys.exit("VOICEVOX が起動していません（http://127.0.0.1:50021）。アプリを起動してください。")

    parts = []
    timing = []
    t = 0.0
    silence = os.path.join(AUDIO, "_gap.wav")
    subprocess.run(["ffmpeg", "-y", "-loglevel", "error", "-f", "lavfi",
                    "-i", f"anullsrc=r=44100:cl=stereo", "-t", str(SCENE_GAP),
                    silence], check=True)

    for n in sorted(scenes):
        seg = os.path.join(AUDIO, f"{lang}_{n:02d}.wav")
        text = apply_pron_ja(scenes[n]) if lang == "ja" else scenes[n]
        synth_scene(text, seg, synth)
        # 統一フォーマットへ
        norm = os.path.join(AUDIO, f"{lang}_{n:02d}_n.wav")
        subprocess.run(["ffmpeg", "-y", "-loglevel", "error", "-i", seg,
                        "-ar", "44100", "-ac", "2", norm], check=True)
        d = wav_duration(norm)
        timing.append((n, t, d))
        t += d + SCENE_GAP
        parts += [norm, silence]

    # 結合
    listf = os.path.join(AUDIO, f"_concat_{lang}.txt")
    with open(listf, "w") as f:
        for p in parts[:-1]:   # 末尾の無音は落とす
            f.write(f"file '{p}'\n")
    out = os.path.join(BASE, f"narration_{lang}.wav")
    subprocess.run(["ffmpeg", "-y", "-loglevel", "error", "-f", "concat",
                    "-safe", "0", "-i", listf, "-c", "copy", out], check=True)
    total = wav_duration(out)
    print(f"[{lang}] narration_{lang}.wav  ({total:.1f}s, {len(scenes)} scenes)")

    # タイミングガイド出力
    tg = os.path.join(BASE, f"timing_{lang}.md")
    with open(tg, "w", encoding="utf-8") as f:
        f.write(f"# 録画タイミングガイド ({lang})\n\n")
        # カードは先頭(0)と末尾(最大番号)。録画はその間のシーンのみ。
        card_scenes = {0, max(scenes)}
        f.write("narration を再生しながら，各シーンの開始時刻を目安に操作してください。\n")
        body_scenes = [nn for nn in sorted(scenes) if nn not in card_scenes]
        f.write(f"（シーン 0=タイトル, {max(scenes)}=エンドは録画不要。"
                f"録画は Scene {body_scenes[0]}〜{body_scenes[-1]} を通しで。）\n\n")
        f.write("| Scene | 開始(録画基準) | 尺 |\n|---|---|---|\n")
        # 録画は最初の本編シーンから始まる(タイトルカードは後で前置)ので，それを0:00基準に再計算
        base1 = next((s for (nn, s, d) in timing if nn == body_scenes[0]), 0.0)
        for (nn, s, d) in timing:
            if nn in card_scenes:
                continue
            rel = s - base1
            f.write(f"| {nn} | {int(rel//60)}:{rel%60:04.1f} | {d:.1f}s |\n")
    print(f"[{lang}] timing guide -> timing_{lang}.md")


# ---- カード生成 -----------------------------------------------------------
def _font(size):
    from PIL import ImageFont
    for p in [
        "/System/Library/Fonts/ヒラギノ角ゴシック W6.ttc",
        "/System/Library/Fonts/Hiragino Sans GB.ttc",
        "/Library/Fonts/Arial.ttf",
        "/System/Library/Fonts/Helvetice.ttc",
    ]:
        if os.path.exists(p):
            try:
                return ImageFont.truetype(p, size)
            except Exception:
                pass
    return ImageFont.load_default()


def _center(draw, text, font, y, fill):
    bb = draw.textbbox((0, 0), text, font=font)
    w = bb[2] - bb[0]
    draw.text(((W - w) / 2, y), text, font=font, fill=fill)


def cmd_cards():
    from PIL import Image, ImageDraw
    # タイトル
    img = Image.new("RGB", (W, H), (24, 38, 64))
    d = ImageDraw.Draw(img)
    _center(d, "shinyExametrika", _font(140), 360, (255, 255, 255))
    _center(d, "Test data analysis in your browser", _font(56), 560, (170, 200, 240))
    _center(d, "no coding required", _font(48), 650, (140, 170, 210))
    img.save(os.path.join(CARDS, "title.png"))
    # エンド
    img = Image.new("RGB", (W, H), (24, 38, 64))
    d = ImageDraw.Draw(img)
    _center(d, "Try it now", _font(104), 250, (255, 255, 255))
    _center(d, "kosugitti.shinyapps.io/shinyExametrika", _font(50), 440, (170, 200, 240))
    _center(d, 'Also an R package:  install.packages("exametrika")', _font(44), 560, (200, 220, 250))
    _center(d, "github.com/kosugitti/shinyExametrika", _font(40), 660, (140, 170, 210))
    _center(d, "Music by Maksym Malko from Pixabay", _font(30), 1010, (110, 130, 165))
    img.save(os.path.join(CARDS, "end.png"))
    print("cards -> cards/title.png, cards/end.png")


# ---- 組み立て -------------------------------------------------------------
def cmd_assemble(lang, rec):
    title = os.path.join(CARDS, "title.png")
    end = os.path.join(CARDS, "end.png")
    narr = os.path.join(BASE, f"narration_{lang}.wav")
    for p in (title, end, narr, rec):
        if not os.path.exists(p):
            sys.exit(f"見つかりません: {p}（先に cards / audio / 録画 を用意してください）")

    with tempfile.TemporaryDirectory() as td:
        # 1) 録画にナレーションを重畳（録画の音声は無視）。録画尺に合わせる。
        body = os.path.join(td, "body.mp4")
        subprocess.run([
            "ffmpeg", "-y", "-loglevel", "error",
            "-i", rec, "-i", narr,
            "-map", "0:v:0", "-map", "1:a:0",
            "-vf", f"scale={W}:{H}:force_original_aspect_ratio=decrease,"
                   f"pad={W}:{H}:(ow-iw)/2:(oh-ih)/2,setsar=1,fps=30",
            "-c:v", "libx264", "-pix_fmt", "yuv420p", "-c:a", "aac",
            "-shortest", body], check=True)
        # 2) タイトル/エンドカードを動画化（無音）
        def card_clip(png, out):
            subprocess.run([
                "ffmpeg", "-y", "-loglevel", "error", "-loop", "1", "-t", str(CARD_SEC),
                "-i", png, "-f", "lavfi", "-i", "anullsrc=r=44100:cl=stereo",
                "-vf", f"scale={W}:{H},setsar=1,fps=30",
                "-c:v", "libx264", "-pix_fmt", "yuv420p", "-c:a", "aac",
                "-shortest", out], check=True)
        tclip = os.path.join(td, "t.mp4"); card_clip(title, tclip)
        eclip = os.path.join(td, "e.mp4"); card_clip(end, eclip)
        # 3) 連結
        listf = os.path.join(td, "list.txt")
        with open(listf, "w") as f:
            for p in (tclip, body, eclip):
                f.write(f"file '{p}'\n")
        out = os.path.join(BASE, f"final_{lang}.mp4")
        subprocess.run(["ffmpeg", "-y", "-loglevel", "error", "-f", "concat",
                        "-safe", "0", "-i", listf, "-c", "copy", out], check=True)
    print(f"[{lang}] -> final_{lang}.mp4  ({wav_duration(os.path.join(BASE, f'final_{lang}.mp4')):.1f}s)")


def cmd_assemble2(lang, rec, anchors_path):
    """録画 + シーン音声を anchors の時刻に配置 + タイトル/エンドカード → final_<lang>.mp4

    anchors_<lang>.json: {"1": 秒, ... "7": 秒, "9": 秒}（録画本編内の開始秒）。
    シーン9も本編に重ねる（録画の最後まで音声を流す）。
    タイトルカード=シーン0音声。エンドカードは無音で音声の後に出す。
    """
    raw = json.load(open(anchors_path))
    anchors = {int(k): float(v) for k, v in raw.items()
               if k.lstrip("-").isdigit()}
    title = os.path.join(CARDS, "title.png")
    end = os.path.join(CARDS, "end.png")
    for p in (title, end, rec):
        if not os.path.exists(p):
            sys.exit(f"見つかりません: {p}")
    D = wav_duration(rec)
    # _body_end: 本編をこの秒数で切り詰める（エンドカードを早く出す用）
    body_end = raw.get("_body_end")
    if body_end is not None:
        D = min(D, float(body_end))

    body_scenes = sorted(anchors)
    clips = [os.path.join(AUDIO, f"{lang}_{n:02d}_n.wav") for n in body_scenes]
    for c in clips:
        if not os.path.exists(c):
            sys.exit(f"シーン音声がありません: {c}（先に audio {lang}）")

    with tempfile.TemporaryDirectory() as td:
        # 1) 本編ナレーション音声を録画尺の無音上に各シーンを配置して合成
        inputs = []
        for c in clips:
            inputs += ["-i", c]
        fc = [f"anullsrc=r=44100:cl=stereo:d={D:.2f}[base]"]
        labels = ["[base]"]
        for i, n in enumerate(body_scenes):
            ms = int(anchors[n] * 1000)
            fc.append(f"[{i}:a]adelay={ms}:all=1[a{i}]")
            labels.append(f"[a{i}]")
        fc.append("".join(labels) + f"amix=inputs={len(clips)+1}:normalize=0:dropout_transition=0[aout]")
        body_audio = os.path.join(td, "body.wav")
        subprocess.run(["ffmpeg", "-y", "-loglevel", "error", *inputs,
                        "-filter_complex", ";".join(fc), "-map", "[aout]",
                        "-t", f"{D:.2f}", body_audio], check=True)

        # 2) 録画映像 + 本編音声
        body = os.path.join(td, "body.mp4")
        subprocess.run([
            "ffmpeg", "-y", "-loglevel", "error", "-i", rec, "-i", body_audio,
            "-map", "0:v:0", "-map", "1:a:0",
            "-vf", f"scale={W}:{H}:force_original_aspect_ratio=decrease,"
                   f"pad={W}:{H}:(ow-iw)/2:(oh-ih)/2,setsar=1,fps=30",
            "-c:v", "libx264", "-pix_fmt", "yuv420p", "-c:a", "aac", "-ar", "44100",
            "-t", f"{D:.2f}", body], check=True)

        # 3) カード
        def card_with_audio(png, audio, out):
            # 音声つき（音声長+0.6秒）。タイトルカード用。
            dur = wav_duration(audio) + 0.6
            subprocess.run([
                "ffmpeg", "-y", "-loglevel", "error", "-loop", "1", "-i", png, "-i", audio,
                "-t", f"{dur:.2f}", "-vf", f"scale={W}:{H},setsar=1,fps=30",
                "-c:v", "libx264", "-pix_fmt", "yuv420p", "-c:a", "aac", "-ar", "44100", "-ac", "2",
                out], check=True)

        def card_silent(png, dur, out):
            # 無音カード。エンドカード用（ナレーションが終わってから出す）。
            subprocess.run([
                "ffmpeg", "-y", "-loglevel", "error", "-loop", "1", "-i", png,
                "-f", "lavfi", "-i", "anullsrc=r=44100:cl=stereo",
                "-t", f"{dur:.2f}", "-vf", f"scale={W}:{H},setsar=1,fps=30",
                "-c:v", "libx264", "-pix_fmt", "yuv420p", "-c:a", "aac", "-ar", "44100", "-ac", "2",
                out], check=True)
        tclip = os.path.join(td, "t.mp4")
        card_with_audio(title, os.path.join(AUDIO, f"{lang}_00_n.wav"), tclip)
        eclip = os.path.join(td, "e.mp4")
        card_silent(end, END_CARD_SEC, eclip)

        # 4) 連結（再エンコードして揃える）
        listf = os.path.join(td, "list.txt")
        with open(listf, "w") as f:
            for p in (tclip, body, eclip):
                f.write(f"file '{p}'\n")
        out = os.path.join(BASE, f"final_{lang}.mp4")
        subprocess.run(["ffmpeg", "-y", "-loglevel", "error", "-f", "concat", "-safe", "0",
                        "-i", listf, "-c:v", "libx264", "-pix_fmt", "yuv420p", "-c:a", "aac",
                        out], check=True)
    print(f"[{lang}] -> final_{lang}.mp4  ({wav_duration(os.path.join(BASE, f'final_{lang}.mp4')):.1f}s)")


def cmd_bgm(in_mp4, music, out_mp4, vol=0.12, start=0.0):
    """既存の最終動画に BGM を焼き込む。
    BGM を小音量でループ＋末尾フェードアウト＋ナレーション中は自動で音量を下げる
    （ダッキング）。start 秒だけ BGM 開始を遅らせる（タイトルカード中は無音にする用）。
    フェードインはなし（start の地点からそのまま始まる）。in_mp4 の映像はそのままコピー。
    """
    for p in (in_mp4, music):
        if not os.path.exists(p):
            sys.exit(f"見つかりません: {p}")
    D = wav_duration(in_mp4)
    fade_out_st = max(0.0, D - 3.0)
    # [1:a]=BGM（-stream_loop でループ済み）, [0:a]=ナレーション
    mus_chain = [f"volume={vol}"]
    if start > 0:
        mus_chain.append(f"adelay={int(start * 1000)}:all=1")
    mus_chain.append(f"afade=t=out:st={fade_out_st:.2f}:d=3")
    fc = (
        f"[1:a]{','.join(mus_chain)}[mus];"
        f"[mus][0:a]sidechaincompress=threshold=0.04:ratio=6:attack=5:release=350[musd];"
        f"[0:a][musd]amix=inputs=2:normalize=0:duration=first[aout]"
    )
    subprocess.run([
        "ffmpeg", "-y", "-loglevel", "error",
        "-i", in_mp4,
        "-stream_loop", "-1", "-i", music,
        "-filter_complex", fc,
        "-map", "0:v:0", "-map", "[aout]",
        "-c:v", "copy", "-c:a", "aac", "-ar", "44100",
        "-t", f"{D:.2f}", out_mp4], check=True)
    print(f"bgm -> {out_mp4}  ({wav_duration(out_mp4):.1f}s, vol={vol})")


if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    cmd = sys.argv[1]
    if cmd == "audio":
        cmd_audio(sys.argv[2])
    elif cmd == "cards":
        cmd_cards()
    elif cmd == "assemble":
        cmd_assemble(sys.argv[2], sys.argv[3])
    elif cmd == "assemble2":
        cmd_assemble2(sys.argv[2], sys.argv[3], sys.argv[4])
    elif cmd == "bgm":
        # bgm <in_mp4> <music> <out_mp4> [volume] [start_sec]
        cmd_bgm(sys.argv[2], sys.argv[3], sys.argv[4],
                float(sys.argv[5]) if len(sys.argv) > 5 else 0.12,
                float(sys.argv[6]) if len(sys.argv) > 6 else 0.0)
    else:
        sys.exit(__doc__)
