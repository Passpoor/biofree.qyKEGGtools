# ============================================================
# 🌐 自动检测包更新
# ============================================================
#' @export
check_biofree_update <- function(repo = "biofree-lab/biofree.qyKEGGtools", auto_install = TRUE) {
  if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
  if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
  current_ver <- as.character(utils::packageVersion("biofree.qyKEGGtools"))
  message("📦 当前版本: ", current_ver)
  url <- paste0("https://api.github.com/repos/", repo, "/releases/latest")
  res <- httr::GET(url)
  if (httr::status_code(res) != 200) {
    warning("⚠️ 无法访问 GitHub release 信息。")
    return(invisible(NULL))
  }
  latest <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
  latest_ver <- gsub("^v", "", latest$tag_name)
  message("🌐 最新版本: ", latest_ver)
  if (utils::compareVersion(latest_ver, current_ver) > 0) {
    message("🚀 发现新版本，可更新至 ", latest_ver)
    if (auto_install) {
      if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
      remotes::install_github(repo, upgrade = "never")
      message("✅ 已更新至最新版本 ", latest_ver)
    }
  } else {
    message("✅ 已是最新版本。")
  }
}