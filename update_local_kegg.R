# ============================================================
# 🔄 自动更新 KEGG 数据库
# ============================================================
#' @export
update_local_kegg <- function(species = "hsa", db_dir = "~/biofree_KEGG_mirror", max_age_days = 30, force = FALSE) {
  if (!requireNamespace("createKEGGdb", quietly = TRUE)) stop("❌ 请先安装 createKEGGdb。")
  if (!dir.exists(db_dir)) dir.create(db_dir, recursive = TRUE)
  db_path <- file.path(db_dir, paste0("KEGG_", species, ".sqlite"))
  need_update <- force
  if (file.exists(db_path)) {
    mod_time <- file.info(db_path)$mtime
    age_days <- as.numeric(difftime(Sys.time(), mod_time, units = "days"))
    message("📅 ", species, " 数据库更新时间: ", format(mod_time, "%Y-%m-%d"), "（", round(age_days, 1), " 天前）")
    if (age_days > max_age_days) {
      message("🔄 超过 ", max_age_days, " 天，准备更新。")
      need_update <- TRUE
    }
  } else {
    message("⚠️ 未发现数据库，将新建。")
    need_update <- TRUE
  }
  
  if (need_update) {
    tmp_tar <- createKEGGdb::create_kegg_db(species)
    untar_dir <- tempfile()
    untar(tmp_tar, exdir = untar_dir)
    sqlite_file <- list.files(untar_dir, pattern = "[.]sqlite$", recursive = TRUE, full.names = TRUE)[1]
    if (is.na(sqlite_file)) stop("❌ 数据库更新失败。")
    file.copy(sqlite_file, db_path, overwrite = TRUE)
    message("✅ 已更新至最新 KEGG 数据库：", db_path)
  } else {
    message("✅ 数据库仍为最新，无需更新。")
  }
}