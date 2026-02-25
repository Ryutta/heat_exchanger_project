# PI Data Analysis Project

このプロジェクトは Power BI (.pbip) を使用して PI System からデータを取得・可視化するためのものです。
`.pbip` ファイルの構造解析結果および、R言語を使用して同様のデータを取得する方法について記述します。

## プロジェクト構造解析

`data_from_pi.pbip` は Power BI Developer Mode のプロジェクトファイルです。
データ取得のロジックは `data_from_pi.SemanticModel` ディレクトリ内の TMDL ファイルに定義されています。

### データソース

データは **PI Web API** を介して取得されています。
解析の結果、以下のロジックでデータ取得が行われていることが判明しました。

*   **定義ファイル**: `data_from_pi.SemanticModel/definition/tables/PIデータ取得.tmdl`
*   **使用API**:
    *   WebId取得: `/piwebapi/points?path=\\{ServerName}\{TagName}`
    *   データ取得: `/piwebapi/streams/{WebId}/{mode}` (modeは `recorded` または `interpolated`)

### パラメータ設定

`data_from_pi.SemanticModel/definition/expressions.tmdl` にて以下のパラメータが定義されています。

| パラメータ名 | 設定値の例 | 説明 |
| :--- | :--- | :--- |
| `PI_System_URL` | `https://hinuta105.ccit.ad.sharedom.net` | PI Web API のベースURL |
| `PI_Server_Name` | `HINUTA101C` | PI Data Archive サーバー名 |
| `PI_Tag` | `ADAC1A.PV,ADAC1B.PV` | 取得対象のタグ名（カンマ区切り） |
| `recorded_or_interpolated` | `recorded` | 取得モード (`recorded`: 実測値, `interpolated`: 補間値) |
| `start_time` | `*-3d` | 取得開始時間 (PI Time Format) |
| `end_time` | `*-4h` | 取得終了時間 (PI Time Format) |
| `interval` | `1h` | データ間隔（補間値取得時などに使用） |

## R からのデータ取得

PI Web API は標準的な REST API であるため、R言語からも `httr` や `jsonlite` パッケージを使用することでデータを取得可能です。

### 前提条件

1.  **ネットワーク**: Rを実行する環境から `PI_System_URL` にアクセス可能であること。
2.  **認証**: 適切な権限を持つユーザーアカウント（Basic認証 または Kerberos/Windows統合認証）。
3.  **ライブラリ**: 以下のRパッケージがインストールされていること。
    *   `httr`: HTTPリクエスト用
    *   `jsonlite`: JSONパース用

### 認証設定のセットアップ (重要)

セキュリティ上の理由から、ユーザー名とパスワードはスクリプト内に直接記述せず、別の設定ファイルで管理します。

1. `auth_config_template.R` をコピーし、ファイル名を `auth_config.R` に変更してください。
2. `auth_config.R` を開き、`AUTH_USER` と `AUTH_PASS` に自身の認証情報を入力して保存してください。
   ```r
   AUTH_USER <- "your_username"
   AUTH_PASS <- "your_password"
   ```
3. `auth_config.R` は `.gitignore` に登録されているため、Gitリポジトリにはコミットされません。

### R サンプルコード

以下は、Power BI で行われているデータ取得プロセスを R で再現するサンプルコードです。

```r
# 必要なライブラリのロード
if (!require("httr")) install.packages("httr")
if (!require("jsonlite")) install.packages("jsonlite")

library(httr)
library(jsonlite)

# --- 設定 ---
base_url <- "https://hinuta105.ccit.ad.sharedom.net" # PI_System_URL
pi_server <- "HINUTA101C"                             # PI_Server_Name
tags <- c("ADAC1A.PV", "ADAC1B.PV")                   # PI_Tag (配列で指定)
start_time <- "*-3d"
end_time <- "*-4h"
mode <- "recorded"                                    # recorded_or_interpolated

# 認証設定
if (file.exists("auth_config.R")) {
  source("auth_config.R")
  auth <- authenticate(AUTH_USER, AUTH_PASS)
} else {
  stop("auth_config.R が見つかりません。auth_config_template.R をコピーして作成してください。")
}


# --- データ取得関数 ---
get_pi_data <- function(tag_name) {
  # 1. WebId の取得
  # URL: /piwebapi/points?path=\\Server\Tag
  path_param <- paste0("\\\\", pi_server, "\\", tag_name)
  url_point <- paste0(base_url, "/piwebapi/points")

  resp_point <- GET(url_point, query = list(path = path_param), auth)

  if (status_code(resp_point) != 200) {
    warning(paste("Failed to get WebId for tag:", tag_name))
    return(NULL)
  }

  content_point <- fromJSON(content(resp_point, "text", encoding = "UTF-8"))
  web_id <- content_point$WebId

  # 2. ストリームデータの取得
  # URL: /piwebapi/streams/{WebId}/{mode}
  url_stream <- paste0(base_url, "/piwebapi/streams/", web_id, "/", mode)

  # クエリパラメータの設定 (Power BIのロジックに準拠)
  query_params <- list(
    startTime = start_time,
    endTime = end_time
  )

  # 補間値の場合は interval が必要
  if (mode == "interpolated") {
    query_params$interval <- "1h" # 必要に応じて変更
  }

  resp_data <- GET(url_stream, query = query_params, auth)

  if (status_code(resp_data) != 200) {
    warning(paste("Failed to get data for tag:", tag_name))
    return(NULL)
  }

  content_data <- fromJSON(content(resp_data, "text", encoding = "UTF-8"))

  # データフレームに変換
  df <- content_data$Items
  if (!is.null(df)) {
    df$Tag <- tag_name
    # 必要な列を選択・整理
    if("Value" %in% names(df) && is.data.frame(df$Value)){
        # Valueがレコードの場合の処理が必要かもしれませんが、通常は単純な値か、詳細オブジェクト
        # ここでは簡易的にそのまま扱います
    }
    return(df)
  } else {
    return(NULL)
  }
}

# --- 実行と結合 ---
all_data <- do.call(rbind, lapply(tags, get_pi_data))

# 結果の確認
if (!is.null(all_data)) {
  print(head(all_data))
  # 必要に応じてCSV保存など
  # write.csv(all_data, "pi_data.csv", row.names = FALSE)
} else {
  print("データが取得できませんでした。")
}
```
