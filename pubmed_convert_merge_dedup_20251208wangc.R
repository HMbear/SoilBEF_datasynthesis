rm(list = ls())
setwd("F:\\Literature\\Pubmed")

convert_dir <- "convert"   # Folder storing the TXT files

# Find all TXT files in the convert directory
txt_files <- list.files(convert_dir,
                        pattern = "\\.txt$",
                        full.names = TRUE)

cat("Detected files:\n")
print(basename(txt_files))

# -----------------------------
# Start processing each file one by one
# -----------------------------

for(in_file in txt_files){
  
  cat("\n==============================\n")
  cat("Processing file:", basename(in_file), "\n")
  cat("==============================\n")
  
  # Output filename (same name but extension replaced with .ris)
  out_file <- file.path(
    convert_dir,
    paste0(tools::file_path_sans_ext(basename(in_file)), ".ris")
  )
  
  lines <- readLines(in_file, encoding = "UTF-8")
  
  # --- The following is your original code: unchanged ---
  
  pmid_idx <- grep("^PMID- ", lines)
  starts   <- pmid_idx
  ends     <- c(pmid_idx[-1] - 1, length(lines))
  
  records <- mapply(function(s, e) lines[s:e],
                    s = starts, e = ends, SIMPLIFY = FALSE)
  
  get_field <- function(rec, tag){
    hit <- rec[grepl(paste0("^", tag, "  - "), rec)]
    if(length(hit) == 0) return(NA_character_)
    sub(paste0("^", tag, "  - "), "", hit[1])
  }
  
  get_abstract <- function(rec){
    idx <- grep("^AB  - ", rec)
    if(length(idx) == 0) return(NA_character_)
    first <- sub("^AB  - ", "", rec[idx[1]])
    i <- idx[1] + 1
    extra <- character(0)
    while(i <= length(rec) && grepl("^      ", rec[i])){
      extra <- c(extra, trimws(rec[i]))
      i <- i + 1
    }
    paste(c(first, extra), collapse = " ")
  }
  
  get_authors <- function(rec){
    hits <- rec[grepl("^AU  - ", rec)]
    if(length(hits) == 0) return(character(0))
    authors <- sub("^AU  - ", "", hits)
    paste0("AU  - ", authors)
  }
  
  get_doi <- function(rec){
    hits <- rec[grepl("\\[doi\\]", rec, ignore.case = TRUE)]
    if(length(hits) == 0) return(NA_character_)
    lid_hits <- hits[grepl("^LID\\s*-\\s+", hits)]
    aid_hits <- hits[grepl("^AID\\s*-\\s+", hits)]
    if(length(lid_hits) > 0){
      line <- lid_hits[1]
    } else if(length(aid_hits) > 0){
      line <- aid_hits[1]
    } else {
      line <- hits[1]
    }
    x <- sub("^(AID|LID)\\s*-\\s+", "", line)
    x <- sub("\\s*\\[doi\\].*$", "", x, ignore.case = TRUE)
    trimws(x)
  }
  
  record_to_ris <- function(rec){
    pmid   <- get_field(rec, "PMID")
    title  <- get_field(rec, "TI")
    journal<- get_field(rec, "JT")
    dp     <- get_field(rec, "DP")
    year   <- if(!is.na(dp)) substr(dp, 1, 4) else NA_character_
    vol    <- get_field(rec, "VI")
    issue  <- get_field(rec, "IP")
    pages  <- get_field(rec, "PG")
    ab     <- get_abstract(rec)
    doi    <- get_doi(rec)
    aus    <- get_authors(rec)
    
    sp <- ep <- NA_character_
    if(!is.na(pages)){
      if(grepl("-", pages)){
        parts <- strsplit(pages, "-")[[1]]
        sp <- parts[1]
        ep <- parts[length(parts)]
      } else {
        sp <- pages
      }
    }
    
    ris <- c(
      "TY  - JOUR",
      if(!is.na(pmid))   paste0("ID  - ", pmid)  else NA,
      if(!is.na(title))  paste0("TI  - ", title) else NA,
      aus,
      if(!is.na(year))   paste0("PY  - ", year)  else NA,
      if(!is.na(journal))paste0("JO  - ", journal) else NA,
      if(!is.na(vol))    paste0("VL  - ", vol)   else NA,
      if(!is.na(issue))  paste0("IS  - ", issue) else NA,
      if(!is.na(sp))     paste0("SP  - ", sp)    else NA,
      if(!is.na(ep))     paste0("EP  - ", ep)    else NA,
      if(!is.na(ab))     paste0("AB  - ", ab)    else NA,
      if(!is.na(doi))    paste0("DO  - ", doi)   else NA,
      "ER  - ",
      ""
    )
    
    ris[!is.na(ris)]
  }
  
  ris_list <- lapply(records, record_to_ris)
  
  writeLines(unlist(ris_list), out_file)
  
  cat("RIS file generated:", out_file, "\n")
}

cat("\n All PubMed files have been successfully converted!\n")

#-----------------------------------
# Merge all RIS files
#-----------------------------------

convert_dir <- "F:\\Literature\\Pubmed\\convert"

# Read all RIS files (excluding dedup and duplicate files)
ris_files <- list.files(
  convert_dir,
  pattern = "pubmed-multidiver-set.*\\.ris$",
  full.names = TRUE
)

cat("Detected RIS files:\n")
print(basename(ris_files))

merged_ris_file <- file.path(convert_dir, "pubmed_all_converted.ris")

all_ris_lines <- unlist(lapply(ris_files, function(f){
  readLines(f, encoding = "UTF-8")
}))

writeLines(all_ris_lines, merged_ris_file, useBytes = TRUE)
cat("Merged RIS file generated:", merged_ris_file, "\n")

#----------------------------------------
# Deduplication after merging
#----------------------------------------

all_lines <- readLines(merged_ris_file, encoding = "UTF-8")
er_idx <- grep("^ER  -", all_lines)

starts <- c(1, er_idx[-length(er_idx)] + 1)
ends   <- er_idx

ris_records <- mapply(function(s, e) all_lines[s:e],
                      s = starts, e = ends, SIMPLIFY = FALSE)

get_ris_field <- function(rec, tag){
  pattern <- paste0("^", tag, "  - ")
  hit <- rec[grepl(pattern, rec)]
  if (length(hit) == 0) return(NA_character_)
  sub(pattern, "", hit[1])
}

# Create deduplication key (prefer DOI; fallback TI+PY)
# Create deduplication key with priority: PMID → DOI → TI+PY
ids <- sapply(ris_records, function(rec){
  
  pmid <- get_ris_field(rec, "ID")   # ID  field we created for PMID
  doi  <- get_ris_field(rec, "DO")
  ti   <- get_ris_field(rec, "TI")
  py   <- get_ris_field(rec, "PY")
  
  # Priority 1: PMID
  if (!is.na(pmid) && pmid != "") {
    return(paste0("PMID:", pmid))
  }
  
  # Priority 2: DOI
  if (!is.na(doi) && doi != "") {
    return(paste0("DOI:", doi))
  }
  
  # Priority 3: TI + PY (fallback)
  return(paste0("TI:", ti, "_PY:", py))
})


total_n  <- length(ids)
tab_id   <- table(ids)

unique_n     <- length(tab_id)
n_dup_ids    <- sum(tab_id > 1)
n_dup_records <- total_n - unique_n

cat("\n----- Deduplication Report -----\n")
cat("Total merged records:", total_n, "\n")
cat("Records after deduplication:", unique_n, "\n")
cat("Number of duplicated IDs:", n_dup_ids, "\n")
cat("Duplicate records removed:", n_dup_records, "\n")

#----------------------------------------
# Save deduplicated file
#----------------------------------------

dedup_ris_file <- file.path(convert_dir, "pubmed_merged_dedup.ris")
unique_ids <- unique(ids)
dedup_records <- ris_records[match(unique_ids, ids)]

writeLines(unlist(dedup_records),
           dedup_ris_file,
           useBytes = TRUE)

cat("Deduplicated RIS saved:", dedup_ris_file, "\n")
#----------------------------------------
# Save duplicate records (only removed entries)
#----------------------------------------

dup_ris_file <- file.path(convert_dir, "pubmed_merged_duplicates.ris")

# First occurrence positions for each unique ID
first_pos <- match(unique_ids, ids)

# All positions
all_pos <- seq_along(ids)

# Removed positions = all occurrences except the first one
removed_pos <- setdiff(all_pos, first_pos)

dup_records <- ris_records[removed_pos]

writeLines(unlist(dup_records),
           dup_ris_file,
           useBytes = TRUE)

cat("Duplicate-record RIS saved (only removed entries):", dup_ris_file, "\n")


#----------------------------------------
# Count all RIS files in folder (including merged & dedup results)
#----------------------------------------

count_ris_records <- function(ris_path){
  lines <- readLines(ris_path, encoding = "UTF-8")
  er_idx <- grep("^ER  -", lines)
  length(er_idx)
}

all_ris_files <- list.files(
  convert_dir,
  pattern = "\\.ris$",
  full.names = TRUE
)

ris_count2 <- data.frame(
  ris_file  = basename(all_ris_files),
  n_records = sapply(all_ris_files, count_ris_records),
  stringsAsFactors = FALSE
)

cat("\nFinal RIS record counts (including merged & dedup files):\n")
print(ris_count2)


#sort(table(ids), decreasing = TRUE)[1:20]
#ids[removed_pos][1:20]
#table(table(ids))

#----------------------------------------
## Find records with missing DOI
#----------------------------------------
dedup_file <- "F:\\Literature\\Pubmed\\convert\\pubmed_merged_dedup.ris"

lines <- readLines(dedup_file, encoding = "UTF-8")

er_idx <- grep("^ER  -", lines)
starts <- c(1, er_idx[-length(er_idx)] + 1)
ends   <- er_idx

records <- mapply(function(s, e) lines[s:e],
                  s = starts, e = ends, SIMPLIFY = FALSE)

get_field <- function(rec, tag){
  pattern <- paste0("^", tag, "  - ")
  hit <- rec[grepl(pattern, rec)]
  if (length(hit) == 0) return(NA_character_)
  sub(pattern, "", hit[1])
}

# take DOI
dois <- sapply(records, function(rec){
  doi <- get_field(rec, "DO")
  ifelse(is.na(doi) || doi == "", NA, doi)
})


idx_no_doi <- which(is.na(dois))

cat("Number of records with missing DOI:", length(idx_no_doi), "\n")

