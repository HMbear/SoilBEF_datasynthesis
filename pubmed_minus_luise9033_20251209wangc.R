rm(list = ls())
setwd("F:\\Literature\\WOS_pubmed")
getwd()

big_file   <- "pubmed_merged_dedup.ris"       
small_file <- "merged_sum_dedup_luise9033.ris" 
out_file   <- "pubmed_merged_dedup_minus9033.ris" 

#1. read RIS
big_lines   <- readLines(big_file,   encoding = "UTF-8")
small_lines <- readLines(small_file, encoding = "UTF-8")

#2. Records split by ER
split_ris_records <- function(lines) {
  er_idx  <- grep("^ER  -", lines)
  starts  <- c(1, er_idx[-length(er_idx)] + 1)
  recs <- mapply(function(s, e) lines[s:e], s = starts, e = er_idx, SIMPLIFY = FALSE)
  list(records = recs, starts = starts, ends = er_idx)
}

big_s   <- split_ris_records(big_lines)
small_s <- split_ris_records(small_lines)

big_recs   <- big_s$records
small_recs <- small_s$records

#3. Extract 'Unique ID': Prefer using DOI; if there is no DOI, use TI+PY
get_id <- function(rec) {
  di <- rec[grepl("^DO  -", rec)]
  if (length(di) > 0) {
    return(sub("^DO  -\\s*", "", di[1]))
  } else {
    ti <- rec[grepl("^TI  -", rec)][1]
    py <- rec[grepl("^PY  -", rec)][1]
    ti <- if (!is.na(ti)) sub("^TI  -\\s*", "", ti) else "NO_TITLE"
    py <- if (!is.na(py)) sub("^PY  -\\s*", "", py) else "NO_YEAR"
    return(paste0(ti, "|", py))
  }
}

big_id   <- vapply(big_recs,   get_id, character(1))
small_id <- vapply(small_recs, get_id, character(1))

#4. Find the records to be deleted from the large file
rm_idx <- which(big_id %in% small_id)   
keep_idx <- !(big_id %in% small_id)

n_before <- length(big_recs)
n_remove <- length(rm_idx)
n_after  <- sum(keep_idx)

cat("Original record count: ", n_before, "\n")#41590
cat("Number of records to delete: ", n_remove, "\n")#2475
cat("Number of records after deletion: ", n_after, "\n\n")#39115

#5. View the line numbers of deleted records in the original file

doi_line <- sapply(rm_idx, function(i) {
  s <- big_s$starts[i]
  e <- big_s$ends[i]
  rec_lines <- big_lines[s:e]
  pos <- grep("^DO  -", rec_lines)
  if (length(pos) == 0) return(NA_integer_) 
  s + pos[1] - 1   
})

deleted_info <- data.frame(
  record_index = rm_idx,
  start_line   = big_s$starts[rm_idx],
  end_line     = big_s$ends[rm_idx],
  doi_line     = doi_line,    
  id           = big_id[rm_idx],
  row.names    = NULL
)

print(head(deleted_info, 10)) #first 10

write.csv(deleted_info, "deleted_records_line_info.csv", row.names = FALSE)

#6. write new ris
new_recs  <- big_recs[keep_idx]
new_lines <- unlist(lapply(new_recs, function(x) c(x, "")))
writeLines(new_lines, out_file, useBytes = TRUE)#merged_sum_dedup_minus106.ris

cat("\n A new file has been generated：", out_file, "\n")

#--------------------------------------------------------check the final numbers------------

file <- "pubmed_merged_dedup_minus9033.ris"
lines <- readLines(file, encoding = "UTF-8")

n_records <- sum(grepl("^ER  -", lines))

cat("pubmed_merged_dedup_minus9033.ris records: ", n_records, "\n")#39115



