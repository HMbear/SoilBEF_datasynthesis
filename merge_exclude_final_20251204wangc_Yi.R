rm(list = ls())

#------------------------------------------------------------------#
#----1. Merged all ris.files ------
# articles: Cong, Hui, Huimin 
# early access + data papers 

# Where cong downloaded
setwd("C:/nextcloud/SoilBEF_Synthesis/Literature_pool/cong")
getwd()
files_cong <- list.files(pattern = "\\.ris$", full.names = TRUE)
start_num <- as.numeric(sub(".*savedrecs_([0-9]+)_.*", "\\1", files_cong))
files_sorted_cong <- files_cong[order(start_num)]
files_sorted_cong
merged_content_cong <- unlist(lapply(files_cong, function(f) c(readLines(f, encoding = "UTF-8"), "")))
#writeLines(merged_content_cong, "merged_1_25000.ris", useBytes = TRUE)

#Where Hui downloaded 
setwd("C:/nextcloud/SoilBEF_Synthesis/Literature_pool/hui")
getwd()
files_hui <- list.files(pattern = "\\.ris$", full.names = TRUE)
start_num <- as.numeric(sub(".*savedrecs_([0-9]+)_.*", "\\1", files_hui))
files_sorted_hui <- files_hui[order(start_num)]
files_sorted_hui
merged_content_hui <- unlist(lapply(files_hui, function(f) c(readLines(f, encoding = "UTF-8"), "")))
#writeLines(merged_content_hui, "merged_25001_50000.ris", useBytes = TRUE)

#where Huimin downlaod 
setwd("C:/nextcloud/SoilBEF_Synthesis/Literature_pool/huimin")
getwd()
files_huimin <- list.files(pattern = "\\.ris$", full.names = TRUE)
start_num <- as.numeric(sub(".*savedrecs_([0-9]+)_.*", "\\1", files_huimin))
files_sorted_huimin <- files_huimin[order(start_num)]
files_sorted_huimin
merged_content_huimin <- unlist(lapply(files_huimin, function(f) c(readLines(f, encoding = "UTF-8"), "")))
#writeLines(merged_content_Huimin, "merged_50001_76192.ris", useBytes = TRUE)

# combine everything in the literature pool
setwd("C:/nextcloud/SoilBEF_Synthesis/Literature_pool/Early_access_data_paper")
getwd()
files_ED <- list.files(pattern = "\\.ris$", full.names = TRUE)
merged_content_ED <- unlist(lapply(files_ED, function(f) c(readLines(f, encoding = "UTF-8"), "")))
#writeLines(merge


# combine everything in the literature pool
setwd("C:/nextcloud/SoilBEF_Synthesis/Literature_pool")
getwd()
merged_content <- c(
  merged_content_cong,
  merged_content_hui,
  merged_content_huimin,
  merged_content_ED
)

#check the length 
sum(grepl("^TY  -", merged_content_ED))
#merged_content_ED for 84+651 -> 735
sum(grepl("^TY  -", merged_content_cong))
#25000
sum(grepl("^TY  -", merged_content_hui))
#25000
sum(grepl("^TY  -", merged_content_huimin))
#26192
sum(grepl("^TY  -", merged_content))
# 25000+25000+26192+735 -> 76927
# 76927 
sum(grepl("^ER  -", merged_content))
#76927

#-----------------------------------------------------#
#---- 2. Remove the duplication -----

#1. Records split by ER
split_ris_records <- function(lines) {
  er_idx  <- grep("^ER  -", lines)
  starts  <- c(1, er_idx[-length(er_idx)] + 1)
  recs <- mapply(function(s, e) lines[s:e], s = starts, e = er_idx, SIMPLIFY = FALSE)
  list(records = recs, starts = starts, ends = er_idx)
}

merged_s   <- split_ris_records(merged_content)

er_idx <- merged_s$ends 
starts <- merged_s$starts 
records <- merged_s$records 

#2. Extract 'Unique ID': Prefer using DOI; if there is no DOI, use TI+PY
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


IDs <- vapply(records, get_id, character(1))

cat("sum_records：", length(records), "\n") # sum_records： 76927
cat("Number of missing unique IDs：", sum(is.na(IDs)), "\n")


###----2.1 find the replicates ----
dup_ids <- unique(IDs[duplicated(IDs)]) #686 replicates
duplicate_map <- lapply(dup_ids, function(id) which(IDs == id))
names(duplicate_map) <- dup_ids

# duplicate_map: list of record_index vectors
# dup_ids: names(duplicate_map)

dup_table <- data.frame(
  doi_no = seq_along(dup_ids),
  doi = dup_ids,
  record_index = sapply(duplicate_map, function(idx) paste(idx, collapse = "; ")),
  stringsAsFactors = FALSE
)
# 
# #starts: start numbers in merged_content 
# dup_table$start_line <- sapply(duplicate_map, function(idx) {
#   paste(starts[idx], collapse = "; ")
# })
# 
# #er_idx the orders in merged_content 
# dup_table$end_line <- sapply(duplicate_map, function(idx) {
#   paste(er_idx[idx], collapse = "; ")
# })
# 
# first_index <- as.numeric(sub(";.*", "", dup_table$record_index))
# dup_table <- dup_table[order(first_index), ]

# write.csv(dup_table, "merged_sum_duplicates.csv", row.names = FALSE)#use EXCEL open merged_sum.ris to check line number
# 
# dup_table

###----2.2 remove the duplicates----
#IDs is the doi for each records in merged_content
dup_idx <- which(duplicated(IDs))
which(IDs %in% dup_ids)

cat("Number of duplicate records (by DOI or TI+PY):", length(dup_idx), "\n")

#records the lists_include all of the inform in merged_content 
keep_idx   <- !duplicated(IDs)
dedup_recs <- records[keep_idx] #with number of 76232 = 76927 + 695
remove_recs <- records[dup_idx] #the lines we removed, 695 lines

##final results
merged_content_dedup <- unlist(lapply(dedup_recs, function(x) c(x, "")))

# writeLines(dedup_lines, out_file, useBytes = TRUE)
# 
# cat("Number of records after deduplication：", length(dedup_recs), "\n")
# cat("File has been generated：", out_file, "\n")
# 

#----------------------------------------------------------#
#----3. Remove 106 lines -----

#big_file   <- "merged_sum_dedup.ris"       
small_file <- "savedrecs_106_forLuise.ris" 

#1. read RIS
#big_lines   <- readLines(big_file,   encoding = "UTF-8")
small_lines <- readLines(small_file, encoding = "UTF-8")


big_s   <- split_ris_records(merged_content_dedup)
small_s <- split_ris_records(small_lines)

big_recs   <- big_s$records
small_recs <- small_s$records

#take the doi out 
big_id   <- vapply(big_recs,   get_id, character(1))
small_id <- vapply(small_recs, get_id, character(1))

#4. Find the records to be deleted from the large file
rm_idx <- which(big_id %in% small_id)   
#keep_idx is repeated with previous one you used before
keep_idx_luis <- which(!(big_id %in% small_id))

# n_before <- length(big_recs)
# n_remove <- length(rm_idx)
# n_after  <- sum(keep_idx)

cat("Original record count: ", length(big_recs), "\n")#76232
cat("Number of records to delete: ", length(rm_idx), "\n")#106
cat("Number of records after deletion: ", length(keep_idx_luis), "\n\n")#76126

#5. View the line numbers of deleted records in the original file

doi_line <- sapply(rm_idx, function(i) {
  s <- big_s$starts[i]
  e <- big_s$ends[i]
  rec_lines <- merged_content_dedup[s:e]
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

#write.csv(deleted_info, "deleted_records_line_info.csv", row.names = FALSE)

#6. write new ris
sum_dedup_luis_recs  <- big_recs[keep_idx_luis]
merged_sum_dedup_minus106 <- unlist(lapply(sum_dedup_luis_recs, function(x) c(x, "")))
sum(grepl("^ER  -", merged_sum_dedup_minus106)) #76126
##out the final files 

writeLines(merged_sum_dedup_minus106, 
           file ="merged_sum_dedup_minus106.ris" , 
           useBytes = TRUE)#merged_sum_dedup_minus106.ris
write.csv(dup_table, file = "inform_duplication.csv")

cat("\n A new file has been generated：", out_file, "\n")


#savedrecs_data paper.ris for 84
#savedrecs_early assess.ris for 651
#merged_1_76192.ris for 76192
#merged_sum.ris for 76927
#merged_sum_dedup.ris for 76232
#merged_sum_dedup_minus106.ris for 76126









