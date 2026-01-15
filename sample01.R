Excel_DF <- read_xlsx("data/20251210_u_value.xlsx", sheet = 1)
Excel_DoS <- read_xlsx("data/NoS_20251201.xlsx", sheet = 1)

Excel_DF2 <- Excel_DF |>
  select(-Timestamp)

install.packages("pacman")
install.packages("GGally")
install.packages("ggplot2")
pacman::p_load(GGally)
ggpairs(Excel_DF2, mapping=aes(color=Period))

Excel_DF2

pacman::p_load(GGally)
ggpairs(iris, mapping=aes(color=Species))



# 1. ライブラリの読み込み

install.packages("e1071")
library(e1071)

# 3. 期間ごとにグループ化し、平均値と分散を計算
summary_table <- Excel_DF2  |>
  group_by(Period) |>                # Period列の各期間でグループ分け
  summarise(
    mean = mean(U),                 # 各グループのXの平均値を計算
    var = var(U),                     # 各グループのXの分散を計算
    skew = skewness(U),
    Kurt = kurtosis(U),
    meanP = mean(jacket_P),
    NoS = sum(NoS, na.rm = TRUE)
  )

select.table <- summary_table |>
  slice_tail(n =6)

x <- as.matrix(summary_table |> select(meanUA))
y <- as.matrix(summary_table |> select(NoS))

reg1 <- lm(y ~ x)
reg1
summary(reg1)

summary_table |>
  ggplot(aes(x = meanUA, y = NoS)) + # aes() を ggplot() に移動すると、geom_pointとgeom_smoothの両方に適用される
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") + # 線形近似直線を追加。se=TRUEで信頼区間を表示
  labs(x = "UA", y = "NoS") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


x <- as.matrix(select.table |> select(meanP))
y <- as.matrix(select.table |> select(NoS))



result |>
  ggplot() +
  geom_point(aes(x = Max_P_in_Last_Month, y = NoS, size = 20)) +
  labs(x = "max_pumpP", y = "NoS") +
  theme_bw() +
# ここにテーマの変更を加える
  theme(
    # 軸ラベル（"U" と "NoS"）の文字の大きさを変更
    axis.title = element_text(size = 14),
    
    # 軸の目盛りラベル（数値やカテゴリ名）の文字の大きさを変更
    axis.text = element_text(size = 12),
    
    # プロット全体の文字の基準サイズを変更
    # (これにより、他の要素（例: タイトル、凡例）のサイズも相対的に変わります)
    # base_size = 20
    
    # プロットのタイトルがある場合、その文字の大きさを変更
    # plot.title = element_text(size = 16)
  )


library(dplyr)
library(lubridate) # 日付計算（%m-% や months()）のために必要

# 処理の実行
result <- Excel_DF |>
  group_by(Period) |> # 1. Period別にグループ化
  filter(
    # 2. 日付（Date）が「グループ内の最終日(max(Date))」から
    #    「最終日から1か月引いた日」の間にある行のみを抽出
    Timestamp >= (max(Timestamp) %m-% months(1)) & Timestamp <= max(Timestamp)
  ) |>
  summarize(
    # 3. 絞り込んだデータ（直近1か月分）からXの最大値を取得
    Max_P_in_Last_Month = max(pump_outlet, na.rm = TRUE),
    NoS = sum(NoS, na.rm = TRUE)
  ) |>
  ungroup() # グループ化を解除

print(result)

x <- as.matrix(result |> select(Max_P_in_Last_Month))
y <- as.matrix(result |> select(NoS))

reg1 <- lm(y ~ x)
reg1
summary(reg1)

# 処理の実行
result2 <- Excel_DF |>
  group_by(Period) |> # 1. Period別にグループ化
  filter(
    # 2. 日付（Date）が「グループ内の最終日(max(Date))」から
    #    「最終日から1か月引いた日」の間にある行のみを抽出
    Timestamp >= (max(Timestamp) %m-% months(1)) & Timestamp <= max(Timestamp),
    !is.na(jacket_P)
  ) |>
  summarize(
    # 3. 絞り込んだデータ（直近1か月分）からXの最大値を取得
    Max_P_in_Last_Month = max(jacket_P, na.rm = TRUE),
    NoS = sum(NoS, na.rm = TRUE)
  ) |>
  ungroup() # グループ化を解除

print(result2)

x <- as.matrix(result2 |> select(Max_P_in_Last_Month))
y <- as.matrix(result2 |> select(NoS))

reg2 <- lm(y ~ x)
reg2
summary(reg2)

result$UA <- result$Max_U_in_Last_Month * 367

summary_table$meanUA <- summary_table$mean * 367

final_result2 <- final_result |>
  mutate(
    PeriodType = str_c(Period, Type, sep = "_")
  )
final_result

head(df_raw)
