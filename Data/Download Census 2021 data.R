# PURPOSE: To download ALL of the Topic Summary (TS) tables from the 2021 Census available on NOMIS,
#          including the version of each table available for each spatial scale (because disclosure
#          control measures mean that OA-level results will not sum to the same total as that provided
#          in higher level versions of the same table)

# File structure of download assumes a root directory with Code and Data folders, with the initial
# working directory being the Code folder. Data are downloaded into the Data/TS folder, with
# separate sub-folders for each table (e.g. Data/TS/ts001) and with a further subfolder for the
# table-specific metadata provided by ONS (e.g. Data/TS/ts001/metadata)


# Based on original code by Alex Singleton (https://github.com/alexsingleton/Census_2021_Output_Areas)

library(tidyverse)

# the rvest and vroom packages are installed by default as part of the tidyverse,
# but not loaded by the library(tidyverse) command
library(rvest) 
library(vroom) 

# library(magrittr) # Pipe
#library(arrow) # Apache Arrow

# Read the Nomis Topic Summaries Bulk Download HTML page
html_page <- read_html("https://www.nomisweb.co.uk/sources/census_2021_bulk")

# Get census table zip file names
zip_urls <- html_page %>% 
  html_nodes("a") %>%
  html_attr("href") %>%
  str_subset(pattern = ".zip$")

# Make zip file names into a full URL
zip_urls <- paste0("https://www.nomisweb.co.uk",zip_urls)


# For each dataset URL in turn, download and unzip the data
# into a table-specific folder

# Note: when run this code generated 7 unzip warning messages.
# Of these 6 relate to the three tables with an ordinary and 'asp' version
# (ts037, ts038 and ts039) with the metadata folder of the ordinary table
# (e.g. ts037/metadata) ending up containing a copy of the metadata .txt file
# for both ts037 AND  ts037asp. (The asp version of a table presents results
# as percentages rather than counts.)  The 7th warning related ts040, the table
# immediately following ts039asp. However, despite these warnings all data appear
# to have been downloaded correctly, the only apparent issue being the spurious
# metadata files the ts037, ts038 and ts039 metadata folders.

for (dataset_url in zip_urls) {
  
  # Identify root name of TS table associated with this dataset
  # (i.e. strip out URL and and '-extra' part of URL name)
  
  table_name <- str_match(dataset_url, "(?<=-).*(?=\\.)") %>%
    str_remove(., "-extra")
  
  # Create output directory for current TS table (if it doesn't already exist)
  
  table_dir <- paste0("../Data/TS/",table_name)
  
  if (!dir.exists(table_dir)){
    dir.create(table_dir, recursive = TRUE)
  }

  # Download and unzip the current table-specific zip file
  f <- curl::curl_download(dataset_url, tempfile(fileext = ".zip")) 
  unzip(f,  exdir=table_dir) # Unzip
  
} # Next dataset_url


# Correct file names for files which, once unzipped, end in ..csv instead of .csv
# At time of writing this applied only to ts002 lsoa and msoa files, but check here
# covers all tables downloaded

for (table_name in table_names) {
  
  # Declare path to table-specific folder 
  table_dir <- paste0("../Data/TS/",table_name)
  
  # Obtain list of files in table-specific folder that end with ..csv
  file_names_old <- list.files(table_dir, pattern="\\.\\.csv")
  
  # Report progress to screen, flagging any 'incorrect' file names identified
  print(table_name)
  print(file_names_old)
  
  # Amend any incorrect names to end in .csv instead of ..csv
  file_names_new <- str_replace(file_names_old, pattern = "\\.\\.", "\\.")
  
  # Change the names of the files as stored on you computer's hard-drive
  file.rename(paste0(table_dir,"/",file_names_old), paste0(table_dir,"/",file_names_new))
  
}