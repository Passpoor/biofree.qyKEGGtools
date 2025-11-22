# ============================================================
# 🚀 KEGG 富集分析模块
# ============================================================
#' @export
enrich_local_KEGG <- function(gene, species = "hsa", db_dir = "~/biofree_KEGG_mirror",
                              pCutoff = 0.05, qCutoff = 0.2) {
  if (missing(gene) || length(gene) == 0) stop("❌ gene 列表不能为空。")
  db <- load_local_kegg(species = species, db_dir = db_dir)
  path2gene <- DBI::dbGetQuery(db, "SELECT * FROM pathway2gene")
  path2name <- DBI::dbGetQuery(db, "SELECT * FROM pathway2name")
  
  res <- clusterProfiler::enricher(
    gene = gene,
    TERM2GENE = path2gene,
    TERM2NAME = path2name,
    pvalueCutoff = pCutoff,
    qvalueCutoff = qCutoff
  )
  RSQLite::dbDisconnect(db)
  message("🎯 ", species, " 的 KEGG 富集分析完成，共检测到 ", nrow(res@result), " 个通路。")
  return(res)
}