# ============================================================
# 🧩 KEGG 数据加载模块
# ============================================================

#' @export
load_local_kegg <- function(species = "hsa", db_dir = "~/biofree_KEGG_mirror", rebuild = FALSE) {
  if (!dir.exists(db_dir)) dir.create(db_dir, recursive = TRUE)
  db_path <- file.path(db_dir, paste0("KEGG_", species, ".sqlite"))
  
  if (!file.exists(db_path) || rebuild) {
    message("[INFO] 未检测到本地 KEGG 数据库，正在构建物种: ", species)
    if (!requireNamespace("createKEGGdb", quietly = TRUE)) {
      stop('❌ 需要安装 createKEGGdb 包: install.packages("createKEGGdb")')
    }
    tmp_tar <- createKEGGdb::create_kegg_db(species)
    untar_dir <- tempfile()
    untar(tmp_tar, exdir = untar_dir)
    sqlite_file <- list.files(untar_dir, pattern = "[.]sqlite$", recursive = TRUE, full.names = TRUE)[1]
    if (is.na(sqlite_file) || !file.exists(sqlite_file)) {
      stop("❌ 构建 KEGG 数据库失败，请检查网络或物种代号。")
    }
    file.copy(sqlite_file, db_path, overwrite = TRUE)
    message("✅ 已构建并保存至: ", db_path)
  } else {
    message("✅ 检测到本地数据库: ", db_path)
  }
  
  db <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
  tables <- DBI::dbListTables(db)
  message("[INFO] 已连接数据库 (", species, ")。包含表: ", paste(tables, collapse = ", "))
  return(db)
}