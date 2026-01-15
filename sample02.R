# 'df'は元のデータフレーム名に置き換えてください
df_end <- Excel_DF |>
  filter(!is.na(pump_outlet)) |>
  group_by(Period) |> # 期間別にグループ化
  filter(
    # 最終日 (max(Date)) から1か月前までの期間に絞り込む
    Timestamp >= (max(Timestamp) %m-% months(1)) & Timestamp <= max(Timestamp)
  ) |>
  summarize(
    # X列の最大値を取得 (データがない場合は NA を返すように処理)
    Max_P = if_else(
      all(is.na(pump_outlet)), 
      NA_real_, 
      max(pump_outlet, na.rm = TRUE)
    ),
    # 基準となった日付を記録 (最終日)
    Reference_Date = max(Timestamp)
  ) |>
  mutate(
    Type = "End" # 終了日基準であることを示す列を追加
  )

# 'df'は元のデータフレーム名に置き換えてください
df_beginning <- Excel_DF |>
  group_by(Period) |> # 期間別にグループ化
  filter(
    # 最初の日 (min(Date)) から1か月後までの期間に絞り込む
    Timestamp >= min(Timestamp) & Timestamp <= (min(Timestamp) %m+% months(1))
  ) |>
  summarize(
    # X列の最大値を取得 (データがない場合は NA を返すように処理)
    Max_P = if_else(
      all(is.na(pump_outlet)), 
      NA_real_, 
      max(pump_outlet, na.rm = TRUE)
    ),
    # 基準となった日付を記録 (最初の日)
    Reference_Date = min(Timestamp),
    .groups = 'drop'
  ) |>
  mutate(
    Type = "Beginning" # 開始日基準であることを示す列を追加
  )

final_result <- bind_rows(df_end, df_beginning)

Excel_DoS2 <- Excel_DoS |>
  mutate(
    PeriodType = str_c(Period, `b/e`, sep = "_")
  )

df_nos <- Excel_DoS2 |>
  select(NoS,PeriodType)

df_final <- left_join(final_result2, df_nos, by = "PeriodType")

df_final2 <- df_final |>
  filter(!is.na(NoS))

df_final2$UA <- df_final2$Max_U * 367

x <- as.matrix(df_final2 |> select(Max_P))
y <- as.matrix(df_final2 |> select(NoS))
reg <- lm(y ~ x)
reg
summary(reg)

df_final2 |>
  ggplot(aes(x = Max_P, y = NoS)) + # aes() を ggplot() に移動すると、geom_pointとgeom_smoothの両方に適用される
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") + # 線形近似直線を追加。se=TRUEで信頼区間を表示
  labs(x = "Max_pump_outlet", y = "NoS") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

library(ggplot2)

# 1. サンプルデータ作成 (mtcarsを使用)
df <- mtcars
df$car_name <- rownames(mtcars)

# 2. 回帰分析と95%信頼区間の計算
# ※interval = "confidence" は平均値の信頼区間
model <- lm(mpg ~ wt, data = df)
ci_data <- predict(model, interval = "confidence", level = 0.95)

# 元のデータに計算結果（fit, lwr, upr）を結合
df_final <- cbind(df, ci_data)

# 3. 信頼区間外のデータだけを抽出したサブセットを作成
outliers <- df_final[df_final$mpg < df_final$lwr | df_final$mpg > df_final$upr, ]

# 4. 描画
ggplot(df_final, aes(x = wt, y = mpg)) +
  # 全体の散布図
  geom_point(color = "black") +
  # 信頼区間付きの回帰直線
  geom_smooth(method = "lm", level = 0.95, fill = "gray80", alpha = 0.5) +
  # 外れた点にのみ赤丸をつける（強調）
  geom_point(data = outliers, color = "red", size = 2) +
  # 外れた点にのみテキストを表示
  # nudge_y でラベルの位置を少し上にずらす, check_overlap = TRUE で重なりを最小限にする
  geom_text(data = outliers, 
            aes(label = car_name), 
            vjust = -1,      # 点の上に配置
            size = 3.5, 
            check_overlap = TRUE) + 
  theme_minimal() +
  ylim(min(df_final$mpg), max(df_final$mpg) + 2) # ラベルがはみ出ないよう上を少し広げる

library(dygraphs)
library(xts)
library(dplyr)

# 1. 表示したい数値列を選択し、Timestampを軸にしたxtsオブジェクトに変換
# ここでは NoS, PI, Flow, production, jacket_P, pump_outlet を対象とします
df_xts <- df_raw %>%
  select(Timestamp, jacket_P) %>%
  # xts化（Timestamp列を除いたデータを値に、Timestampを時間に設定）
  { xts(select(., -Timestamp), order.by = .$Timestamp) }

# 2. dygraphで表示
dygraph(df_xts, main = "df_raw Time Series Analysis") %>%
  dyRangeSelector() %>%             # 下部のスライダーを追加
  dyOptions(connectSeparatedPoints = TRUE, # NA（jacket_Pなど）があっても線を繋ぐ
            drawPoints = TRUE,             # データ点も表示
            pointSize = 2) %>%
  dyAxis("y", label = "Value") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE)

