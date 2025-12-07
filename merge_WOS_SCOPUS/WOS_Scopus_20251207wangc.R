rm(list = ls())
#-------------------------------------------------Scopus_merged_75347.ris------------------------
setwd("F:\\Literature\\Scopus")#All 5 RIS files
getwd()
files <- list.files(pattern = "\\.ris$", full.names = TRUE)
start_num <- as.numeric(sub(".*_(\\d+)_.*", "\\1", files))
files_sorted <- files[order(start_num)]
files_sorted
merged_content <- unlist(lapply(files, function(f) c(readLines(f, encoding = "UTF-8"), "")))
writeLines(merged_content, "merged_75347.ris", useBytes = TRUE)
#--------------------------------------------------------check_numbers??------------------
setwd("F:\\Literature\\Scopus")
getwd()
files <- list.files(pattern = "\\.ris$", full.names = TRUE)

count_ris_records <- function(f) {
  lines <- readLines(f, encoding = "UTF-8")
  sum(grepl("^ER  -", lines))   
}

n_per_file <- sapply(files, count_ris_records)

ris_summary <- data.frame(
  file = basename(files),
  n_records = n_per_file,
  row.names = NULL
)

print(ris_summary)

#---------------------------------------------------------------------------------------------
#-------------------------------------------------merged WOS Scopus------------------------
setwd("F:\\Literature\\WOS_Scopus")# Two summary files for WOS and Scopus，Remember to change the file names
getwd()

## Set function
## Split the RIS file into individual records based on the ER
split_ris_records <- function(lines) {
  er_idx <- grep("^ER  -", lines)
  if (length(er_idx) == 0) stop("ER - not found please check the RIS format")
  starts <- c(1, er_idx[-length(er_idx)] + 1)
  records <- mapply(function(s, e) lines[s:e], s = starts, e = er_idx, SIMPLIFY = FALSE)
  list(records = records, starts = starts, er_idx = er_idx)
}

## Generate a unique ID using DO / TI / PY.
get_id <- function(rec) {
  # 1) use DOI：DO  -
  di <- rec[grepl("^DO  -", rec)]
  if (length(di) > 0) {
    doi <- sub("^DO  -\\s*", "", di[1])
    if (nzchar(doi)) return(doi)
  }
  
  # 2) OR TI  - AND PY  -
  ti <- rec[grepl("^TI  -", rec)][1]
  py <- rec[grepl("^PY  -", rec)][1]
  ti <- if (!is.na(ti)) sub("^TI  -\\s*", "", ti) else "NO_TITLE"
  py <- if (!is.na(py)) sub("^PY  -\\s*", "", py) else "NO_YEAR"
  
  paste0(ti, "|", py)
}



dedup_ris <- function(ris_file, out_file, dup_csv = NULL) {
  cat("Starting processing:", ris_file, "\n")
  
  lines <- readLines(ris_file, encoding = "UTF-8")
  s <- split_ris_records(lines)
  records <- s$records
  starts  <- s$starts
  er_idx  <- s$er_idx
  
  IDs <- vapply(records, get_id, character(1))
  
  cat("Total number of records:", length(records), "\n")
  cat("Number of records with missing ID:", sum(is.na(IDs) | IDs == ""), "\n")
  
  # Find duplicate IDs
  dup_ids <- unique(IDs[duplicated(IDs)])
  duplicate_map <- lapply(dup_ids, function(id) which(IDs == id))
  names(duplicate_map) <- dup_ids
  
  # if needed, export the duplicate list for comparison in Excel
  if (!is.null(dup_csv)) {
    dup_table <- data.frame(
      doi_no       = seq_along(dup_ids),
      doi          = dup_ids,
      record_index = sapply(duplicate_map, function(idx) paste(idx, collapse = "; ")),
      stringsAsFactors = FALSE
    )
    dup_table$start_line <- sapply(duplicate_map, function(idx) {
      paste(starts[idx], collapse = "; ")
    })
    dup_table$end_line <- sapply(duplicate_map, function(idx) {
      paste(er_idx[idx], collapse = "; ")
    })
    
    first_index <- as.numeric(sub(";.*", "", dup_table$record_index))
    dup_table <- dup_table[order(first_index), ]
    
    write.csv(dup_table, dup_csv, row.names = FALSE)
    cat("Duplicate record details have been exported.", dup_csv, "\n")
  }
  
  # deduplication
  dup_idx  <- which(duplicated(IDs))
  cat("number of duplicates detected based on DOI or TI+PY:", length(dup_idx), "\n")
  
  keep_idx   <- !duplicated(IDs)
  dedup_recs <- records[keep_idx]
  
  dedup_lines <- unlist(lapply(dedup_recs, function(x) c(x, "")))
  writeLines(dedup_lines, out_file, useBytes = TRUE)
  
  cat("Number of records after deduplication:", length(dedup_recs), "\n")
  cat("Deduplicated file has been generated:", out_file, "\n\n")
  
  invisible(list(records = dedup_recs, IDs = IDs[keep_idx]))
}

##1.perform deduplication separately for WOS and Scopus
res_wos <- dedup_ris(
  ris_file = "savedrecs_WOS.ris",
  out_file = "savedrecs_WOS_dedup.ris",
  dup_csv  = "savedrecs_WOS_duplicates.csv"
)

res_scopus <- dedup_ris(
  ris_file = "scopus_export.ris",
  out_file = "scopus_export_dedup.ris",
  dup_csv  = "scopus_export_duplicates.csv"
)


## 2.Read the two already deduplicated files.
wos_lines    <- readLines("savedrecs_WOS_dedup.ris", encoding = "UTF-8")
scopus_lines <- readLines("scopus_export_dedup.ris", encoding = "UTF-8")

wos_s    <- split_ris_records(wos_lines)
scopus_s <- split_ris_records(scopus_lines)

wos_recs    <- wos_s$records
scopus_recs <- scopus_s$records

wos_ids    <- vapply(wos_recs,    get_id, character(1))
scopus_ids <- vapply(scopus_recs, get_id, character(1))

cat("WOS (deduplicated) record count:", length(wos_recs), "\n")
cat("Scopus (deduplicated) record count:", length(scopus_recs), "\n")

## Use WOS as the primary source: keep all WOS records and add only those Scopus records not present in WOS.
keep_scopus <- !(scopus_ids %in% wos_ids)
cat("Number of additional records contributed from Scopus:", sum(keep_scopus), "\n")

merged_recs <- c(wos_recs, scopus_recs[keep_scopus])

merged_lines <- unlist(merged_recs, use.names = FALSE)
writeLines(merged_lines, "merged_WOSplusScopus_dedup.ris", useBytes = TRUE)

cat("Final merged record count:", length(merged_recs), "\n")
cat("Final file: merged_WOSplusScopus_dedup.ris\n")


