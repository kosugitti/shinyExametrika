# 使い方動画の dataFormat デモ用ダミーデータ生成
# binary / ordinal / nominal / rated の4種。各 ID 列 + 余分な Group 列(GID)入り
# （ID選択・分析変数の取捨・欠測コードの説明を映せるように）。

set.seed(20260606)
DIR <- "docs/video/demo_data"
dir.create(DIR, showWarnings = FALSE, recursive = TRUE)

n <- 40L                                  # 受験者数（小さめ＝表が見やすい）
id <- sprintf("S%03d", seq_len(n))
grp <- sample(c("A", "B"), n, replace = TRUE)   # 余分な列（分析からは外す想定）

inject_na <- function(mat, p = 0.02, code = 99) {
  k <- floor(length(mat) * p)
  if (k > 0) mat[sample(length(mat), k)] <- code
  mat
}

# ---- binary: 10項目 0/1, 欠測99あり（na=99 の説明用） ----
J <- 10L
theta <- rnorm(n)
b <- seq(-1.5, 1.5, length.out = J)
U <- sapply(seq_len(J), function(j) as.integer(plogis(theta - b[j]) > runif(n)))
colnames(U) <- sprintf("Item%02d", seq_len(J))
U <- inject_na(U, p = 0.02, code = 99)
write.csv(data.frame(ID = id, Group = grp, U, check.names = FALSE),
          file.path(DIR, "demo_binary.csv"), row.names = FALSE)

# ---- ordinal: 8項目 1-5 Likert ----
J <- 8L
eta <- rnorm(n)
Q <- sapply(seq_len(J), function(j) {
  pr <- eta + rnorm(n, 0, 0.8) + (j - J / 2) * 0.1
  as.integer(cut(pr, breaks = quantile(pr, probs = seq(0, 1, length.out = 6)),
                 include.lowest = TRUE))
})
colnames(Q) <- sprintf("Q%02d", seq_len(J))
write.csv(data.frame(ID = id, Group = grp, Q, check.names = FALSE),
          file.path(DIR, "demo_ordinal.csv"), row.names = FALSE)

# ---- nominal: 6項目, カテゴリ 1-4（順序なし） ----
J <- 6L
N <- sapply(seq_len(J), function(j) sample.int(4, n, replace = TRUE))
colnames(N) <- sprintf("V%02d", seq_len(J))
write.csv(data.frame(ID = id, Group = grp, N, check.names = FALSE),
          file.path(DIR, "demo_nominal.csv"), row.names = FALSE)

# ---- rated: 8項目の多肢選択(カテゴリ1-4) + 正答キー(CA) ----
# rated は「多肢選択の回答＋正答キー」型。値は選択肢カテゴリ、CA で各項目の正答を与える。
J <- 8L
CA <- sample.int(4, J, replace = TRUE)        # 各項目の正答カテゴリ
abil <- rnorm(n)
Rt <- sapply(seq_len(J), function(j) {
  p_correct <- plogis(abil - rnorm(1, 0, 0.6)) # 能力が高いほど正答しやすい
  correct <- p_correct > runif(n)
  wrong_opts <- setdiff(1:4, CA[j])
  ans <- ifelse(correct, CA[j], wrong_opts[sample.int(3, n, replace = TRUE)])
  as.integer(ans)
})
colnames(Rt) <- sprintf("Item%02d", seq_len(J))
write.csv(data.frame(ID = id, Group = grp, Rt, check.names = FALSE),
          file.path(DIR, "demo_rated.csv"), row.names = FALSE)
# 正答キーを動画用にメモ（CA入力欄にこの並びを打ち込む）
writeLines(c(
  "demo_rated.csv の正答キー (CA) — データタブの「正答キー(CA)」欄にこの順で入力:",
  paste(CA, collapse = ", ")
), file.path(DIR, "demo_rated_CA.txt"))

cat("written to", DIR, ":\n")
for (f in list.files(DIR, pattern = "demo_.*csv$", full.names = TRUE)) {
  d <- read.csv(f, check.names = FALSE)
  cat(sprintf("  %-18s %d x %d\n", basename(f), nrow(d), ncol(d)))
}
