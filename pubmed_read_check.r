
###How to read file
#install.packages("bibliometrix")   # run once
library(bibliometrix)
library(dplyr)
library(revtools)
library(tidyverse)

# 1. Read the PubMed txt file
setwd("C:/nextcloud/SoilBEF_Synthesis/Literature_pool/Pubmed")
getwd()
# put the file in your working directory or give full path
files <- list.files(pattern = "^pubmed-.*\\.txt$")
files
# 2. Read + convert all in one go
pubmed_list <- lapply(files, function(f) {
  convert2df(file = f, dbsource = "pubmed", format = "pubmed")
})

pubmed_all <- bind_rows( pubmed_list)

lines_2011_2015 <- readLines("pubmed-multidiver-set_2011_2015.txt")
lines_2019_2020 <- readLines("pubmed-multidiver-set_2019_2020.txt")
lines_2021_2022 <- readLines("pubmed-multidiver-set_2021_2022.txt")

##  By using another method
files2 <- list.files(pattern = "^pubmed-.*\\.txt$")
pm_bib2 <- read_bibliography(files2, return_df = FALSE)
write_bibliography(pm_bib2, "pubmed_all.ris", format = "ris")



#Check the csv files 
csv_files <- list.files(pattern = "^csv-.*\\.csv$")
csv_list <- lapply(csv_files, read.csv)


csv_1952_2010 <- read_csv("csv-multidiver-set1952_2010.csv")
csv_2011_2015 <- read_csv("csv-multidiver-set2011_2015.csv")
csv_2016_2018 <- read_csv("csv-multidiver-set2016_2018.csv")
csv_2019_2020 <- read_csv("csv-multidiver-set_2019_2020.csv")
csv_2021_2022 <- read_csv("csv-multidiver-set_2021_2022.csv")
csv_2023_2024 <- read_csv("csv-multidiver-set_2023_2024.csv")
csv_2025_2026 <- read_csv("csv-multidiver-set2025_2026.csv")
# TRUE/FALSE for each row

csv_all_rows <- rbind(csv_1952_2010 ,
                      csv_2011_2015 ,
                      csv_2016_2018 ,
                      csv_2019_2020 ,
                      csv_2021_2022 ,
                      csv_2023_2024 ,
                      csv_2025_2026 
                      )

dup_flag <- duplicated(csv_all_rows$DOI)
csv_duplicates <- csv_all_rows[duplicated(csv_all_rows$DOI), ]
csv_unique <-  csv_all_rows[!duplicated(csv_all_rows$DOI), ]

#For luise
setwd("C:/nextcloud/SoilBEF_Synthesis/Literature_pool/Luise")
getwd()
files_luise <- list.files(pattern = "\\.ris$", full.names = TRUE)
start_num <- as.numeric(sub(".*savedrecs_([0-9]+)_.*", "\\1", files_luise))
files_sorted_luise <- files_luise[order(start_num)]
files_sorted_luise
merged_content_luise <- unlist(lapply(files_luise, function(f) c(readLines(f, encoding = "UTF-8"), "")))
#writeLines(merged_content_hui, "merged_25001_50000.ris", useBytes = TRUE)
#sum(grepl("^TY  -", merged_content_luise))
split_ris_records <- function(lines) {
  er_idx  <- grep("^ER  -", lines)
  starts  <- c(1, er_idx[-length(er_idx)] + 1)
  recs <- mapply(function(s, e) lines[s:e], s = starts, e = er_idx, SIMPLIFY = FALSE)
  list(records = recs, starts = starts, ends = er_idx)
}

merged_s   <- split_ris_records(merged_content_luise)

er_idx <- merged_s$ends 
starts <- merged_s$starts 
records <- merged_s$records 


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


IDs_luise <- vapply(records, get_id, character(1))

IDs_noLuise <- csv_unique[!csv_unique$DOI %in% IDs_luise, ] 


setwd("C:/nextcloud/SoilBEF_Synthesis/Literature_pool")
getwd()
merged_all_WOC <- unlist(lapply("merged_sum_dedup_minus106.ris", 
                                function(f) c(readLines(f, encoding = "UTF-8"), "")))

merged_all_woc   <- split_ris_records(merged_all_WOC)
records_all_woc <- merged_all_woc$records 
IDs_all_woc <- vapply(records_all_woc, get_id, character(1))

IDs_noLuise[!IDs_noLuise$DOI %in% IDs_all_woc, ] %>% write.csv(file = "file_added_pubmed.csv")

IDs_noLuise[!IDs_noLuise$DOI %in% IDs_all_woc, ]%>% filter(
grel("DOI", DOI) == TRUE
)

##extract all of the DOIs 

##Exclude papers that Luise used for her own data 

##compared THE DOIs with the WOS 

