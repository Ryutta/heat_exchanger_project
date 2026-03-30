# M2PI4410W.PV（ジャケット圧）の純粋な取得値と、df_raw内のjacket_Pの値を比較するスクリプト

source("pi_utils.R")
library(dplyr)
library(tidyr)
library(lubridate)

# --- 1. Analysis_1st.Rmd と同じ処理で df_raw を構築 ---
start_time <- "2021-02-01T00:00:00Z"
tags <- c("M2FC4403.PV", "M2TI4405.PV", "M2PI4410W.PV", "M2PI4407.PV", "M2PI4411W.PV")

all_data <- data.frame()
for (tag in tags) {
  web_id <- get_web_id(tag)
  if (!is.null(web_id)) {
    d <- get_interpolated_data(web_id, tag, start_time = start_time, end_time = "*", interval = "24h")
    if (!is.null(d)) {
      all_data <- bind_rows(all_data, d)
    }
  }
}

df_wide <- all_data |>
  mutate(Timestamp = with_tz(Timestamp, tzone = "UTC")) |>
  mutate(Timestamp = floor_date(Timestamp, "day")) |>
  group_by(Timestamp, Tag) |>
  summarize(Value = mean(Value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = Tag, values_from = Value)

df_calc <- df_wide |>
  rename(
    steam_flow = `M2FC4403.PV`,
    t1 = `M2TI4405.PV`,
    jacket_P = `M2PI4410W.PV`,
    pump_inlet_raw = `M2PI4407.PV`,
    pump_outlet_raw = `M2PI4411W.PV`
  )

df_raw_jacket_p <- df_calc |> select(Timestamp, jacket_P)

# --- 2. 純粋に M2PI4410W.PV のみを取得 ---
target_tag <- "M2PI4410W.PV"
target_web_id <- get_web_id(target_tag)
pure_d <- get_interpolated_data(target_web_id, target_tag, start_time = start_time, end_time = "*", interval = "24h")

# 比較のためにUTCの0時(日単位)に合わせるのみ（平均等の集計はしない）
pure_d_aligned <- pure_d |>
  mutate(Timestamp = with_tz(Timestamp, tzone = "UTC")) |>
  mutate(Timestamp = floor_date(Timestamp, "day")) |>
  select(Timestamp, Pure_Value = Value)

# --- 3. 比較 ---
comparison <- pure_d_aligned |>
  full_join(df_raw_jacket_p, by = "Timestamp") |>
  mutate(Diff = Pure_Value - jacket_P)

# 差分がある行を抽出
diffs <- comparison |> filter(abs(Diff) > 1e-6 | is.na(Diff))

print("=== 比較結果 ===")
if (nrow(diffs) > 0) {
  print(paste("差分が見つかりました（", nrow(diffs), "件）:"))
  print(head(diffs, 20))

  # 差分をCSVに保存
  write.csv(comparison, "jacket_p_comparison.csv", row.names = FALSE)
  print("詳細な比較結果を 'jacket_p_comparison.csv' に保存しました。")
} else {
  print("値は完全に一致しています（差分はありません）。")
  print("サンプルの比較データ:")
  print(head(comparison, 5))
}
