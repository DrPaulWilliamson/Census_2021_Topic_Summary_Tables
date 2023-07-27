# PURPOSE: To create a metadata file covering all of the TS tables downloadable from NOMIS

# Based on original code by Alex Singleton (https://github.com/alexsingleton/Census_2021_Output_Areas)

library(tidyverse)

# the vroom package is installed by default as part of the tidyverse,
# but not loaded by the library(tidyverse) command
library(vroom) 

# Get list of TS tables downloaded from Nomis and saved into the ../Data/TS folder
table_dir <- "../Data/TS/"
table_names <- list.files( table_dir, recursive = FALSE )

# Create ordered geography list
geography_order <-  c("ctry", "rgn", "utla", "ltla", "msoa",
                      "ward", "lsoa", "oa") 

# Create a vector of lists to store the table-specific results
# (To feed into bind_rows to avoid using rbind in a loop)
result <- vector("list", length = length(table_names))


# For each table in turn:
#  (a) Identify the geographic levels for which it is available
#  (b) Create table metadata, including table code, name, category names and frequency counts

for (i in 1:length(table_names)) {
  
  #i <- 8
  table_name <- table_names[i]
  print(table_name)
  
  # Obtain list of files in table-specific folder

  table_dir <- paste0("../Data/TS/",table_name)
  
  file_names <- list.files(table_dir, pattern="*.csv")
  
  # Extract and retain the geographic levels for which files are available
  geographies <- str_match(file_names, paste0("(?<=",table_name,"-).*$" )  ) %>%
    str_remove(., ".csv") %>%
    discard(., is.na)
  
  # Read in the ctry level version of the table [available for all TS tables]
  # and keep only the counts for the England & Wales row, unless table has been
  # released for Wales only, in which case keep the Wales row
  tb <- vroom(paste0(table_dir,"/census2021-",table_name,"-ctry.csv"),
              show_col_types = FALSE )
  if (table_name %in% c("ts032", "ts033", "ts034", "ts035", "ts036", "ts076")) {
    tb <- tb %>% filter(`geography code` == "W92000004") # Keep only Wales row
  } else {
    tb <- tb %>% filter(`geography code` == "K04000001") # Keep only England & Wales row
  }
  
  tb <- tb %>% select(-date, -geography, -`geography code`) # Keep only cols containing numeric counts

  
  # Calculate values that need to be known in advance of creation of tibble, as they are used
  # to help calculate other values
  Cell_Label <- colnames(tb) %>%
    str_remove( pattern = "[^:]*:" ) %>% # Remove ONS prefix from Cell_Label
    str_remove(  pattern = "; measures: Value" ) %>% # Remove measure suffix because this element not consistently provided for all tables
    str_trim() # Remove leading and any trailing spaces

  
  # Read in ONS metadata file for current table, and calculate any values 
  
  
  # [Due to the unzipping issue flagged above, the standard (non-asp) versions of 
  # Tables 37, 38 and 39 have two metadata files, an ordinary and an asp version.
  # Hence the code below specifies use of the first of any metadata files in the
  # table's metadata folder]
  
  ons_metadata_filename <- list.files( paste0(table_dir,"/metadata/"), pattern="*.txt")[1]
  
  # If metadata file exists, read file in and extract required information
  if ( file.exists(paste0(table_dir,"/metadata/",ons_metadata_filename) ) ) {
    
    # Read in ONS metadata file [This generates parsing warnings which can be ignored]
    tb_m <- vroom(paste0(table_dir,"/metadata/",ons_metadata_filename),
                  show_col_types = FALSE, delim = ":" )
    
    Table_Label <- colnames(tb_m)[2]
    
    Unit <- pull( tb_m[ which(tb_m[ , 1] == "Unit of measure"), 2 ] )
    
    Variable_ID <- str_remove(tb_m[ which(tb_m[ , 1] == "Keywords"), 2], pattern = ".* ") %>%
      str_remove(pattern = "]") %>%
      str_replace(pattern=",", replacement = "; ")

    # Calculate Variables to enable calculation of Variable_Label for 2D tables
    Variables <- length( unlist( str_split(Variable_ID, pattern=";") ) )
        
    # Calculate Variable_Label now due to to two-step calculation process
    Variable_Label  <- str_split_fixed(tb_m[ which(tb_m[ , 1] == "Label")[2], 2], pattern = " \\(", n=2)[1]
    
    # For two-dimensional tables, extract and add name of second variable to Variable_Label
    if ( Variables == 2) {
      Variable_Label <- paste(Variable_Label, 
                               str_split_fixed(tb_m[ which(tb_m[ , 1] == "Label")[3], 2], pattern = " \\(", n=2)[1],
                               sep="; ")
    }
  
    # Calculate Categories in advance because it is a two stage process
    #ts006 and ts041 meta data do not identify number of categories associated with variable
    if (table_name %in% c("ts006", "ts041") ) {
      Categories <- "1"
    } else {
      Categories <- pull( tb_m[which(tb_m[ , 1] == "Number Of Options")[2], 2] )
    }
    
    # Calculate Interior_Cells in advance because it is a two stage process
    Interior_Cells <- as.numeric(Categories)
    
    # If table contains 2 variables, add count of categories for second variable and
    # update Interior cell count to = Var 1 Categories x Var 2 categories
    if (Variables == 2) {
      Categories <- paste(Categories,
                          pull( tb_m[which(tb_m[ , 1] == "Number Of Options")[3], 2] ),
                          sep="; " )
      Interior_Cells <- Interior_Cells * as.numeric( pull( tb_m[which(tb_m[ , 1] == "Number Of Options")[3], 2] ) )
    }
    
  }
    
  # Make amendments to meta data provided by ONS to cover omissions, inconsistencies etc.
    
  if (table_name == "ts006") {
    Variable_ID <- "pop_density" # Not supplied by ONS
    Variable_Label <- "Population density"  # Supplied by ONS, but without field separator required to
                                            # permit automated extraction
  }
    
  if (table_name == "ts007") {
    Variable_Label <- "Age (single year)" # ONS label: Age (101 Categories)
  }
    
  # No meta-data file supplied by ONS for ts007a
  if (table_name == "ts007a") {
    Table_Label <- "Age (18 categories)"
    Unit  <- "Person" 
    Variable_ID <- "resident_age_18a"
    Variable_Label <- "Age"
    Variables <- 1
    Categories <- "18"
    Interior_Cells <- 18
    Cells <- ncol(tb)
  }
    
  if (table_name %in% c("ts012", "ts013", "ts022", "ts024", "ts028", "ts031", "ts032",
                        "ts070", "ts079") ) {
    Variable_Label <- paste0(Variable_Label," (detailed)") # Adds the (detailed) for consistency
  }
    
  if (table_name %in% c("ts037asp", "ts039asp") ) {
    Variable_Label <- paste0(Variable_Label," (age standardised)") # To distinguish from non-asp version of table
  }

  if (table_name == "ts038asp") {
    Variable_Label <- "Disability (age standardised)" # Drop the term 'Equality Act' from Variable Label
                                                      # for consistency with ts038; add (age standarised)
                                                      # to distinguish from ts038
  }
    
  if (table_name == "ts041") {
    Variable_ID <- "households" # Not supplied by ONS
    Variable_Label <- "Number of households"  # Supplied by ONS, but without field separator required to
                                              # permit automated extraction
  }

  if (table_name == "ts063") {
    Variable_Label <- paste0(Variable_Label, "(major group)")  # For clarification
  }

  if (table_name == "ts064") {
    Variable_Label <- paste0(Variable_Label, "(minor group)")  # For clarification
  }

  if (table_name == "ts75") {
    Variable_Label <- paste0("Religion (",Variable_Label, ")")  # For consistency and clarity
  }

  
  
  # Create table-specific metadata which includes recording which geographic levels
  # data are available for
  
  #tb_metadata
  result[[i]] <- tibble(  
    
    Table_ID = table_name,
    
    Table_Label = Table_Label,
    
    Unit = Unit,
    
    Measure =  if( str_detect(table_name, "asp") ) {
      "Age-standardised proportions"
    } else if(table_name == "ts006") {
      "Usual residents per square kilometre"
    } else {
      "Count"
    },
    
    Spatial_Coverage = if (table_name %in% c("ts032", "ts033", "ts034", "ts035", "ts036", "ts076")) {
      "Wales"
    } else {
      "England & Wales"
    },
    
    Lowest_Geography = geography_order[ max( which( (geography_order %in% geographies ) == TRUE) ) ],
    
    Tiers = max(lengths(str_split(Cell_Label, pattern = ":"))),
    
    Variables = Variables,
    
    Variable_ID = Variable_ID,
    
    Variable_Label = Variable_Label,
    
    Categories = Categories, 
    
    Cells = ncol(tb),
    
    Interior_Cells = Interior_Cells,
    
    Cell_ID = paste0( table_name, sprintf( "%04d", seq_along(1:ncol(tb)) ) ),
    
    Cell_Label_ONS_Prefix = str_remove(colnames(tb), pattern = ":.*"),
    
    Cell_Label_ONS = str_trim( str_remove(colnames(tb), pattern="[^:]*:") ),
    
    Cell_Label = Cell_Label,
    
    EW_Count = as.numeric( tb[1, ] ),
    
    EW_Pct = as.numeric( round( tb[1, ] / tb[1, ][[1]] * 100, digits = 2) ),
    
    ctry = "ctry" %in% geographies,
    
    rgn = "rgn" %in% geographies,
    
    utla = "utla" %in% geographies,
    
    ltla = "ltla" %in% geographies,
    
    msoa = "msoa" %in% geographies,
    
    ward = "ward" %in% geographies,
    
    lsoa = "lsoa" %in% geographies,
    
    oa = "oa" %in% geographies
    
  )
  
} # next table

# Combine results from each table from a list of results into one tibble
TS_Cell_Metadata <- bind_rows(result)


# Where required, correct/amend/clarify ONS Cell Labels for the table cells 
# containing totals in order to provide consistency, accuracy and full information

# Replace 'Total' and 'All persons'
TS_Cell_Metadata$Cell_Label[ TS_Cell_Metadata$Cell_ID %in% c("ts0010001", "ts0040001", "ts0050001",
                                     "ts0070001", "ts007a0001", "ts0080001", "ts0090001") ]  <-
  "Total: All usual residents"

# Replace 'Total'
TS_Cell_Metadata$Cell_Label[ TS_Cell_Metadata$Cell_ID %in% c("ts0020001") ]  <-
  "Total: All usual residents aged 16 years and over"

# Replace 'Total'
TS_Cell_Metadata$Cell_Label[ TS_Cell_Metadata$Cell_ID %in% c("ts0100001") ]  <-
  "Total: All usual household residents aged 16 years and over"

# Replace 'Total', 'Total: All household spaces' and 'Number of households' 
TS_Cell_Metadata$Cell_Label[ TS_Cell_Metadata$Cell_ID %in% c("ts0030001", "ts0170001", 
                                                           "ts0410001", "ts0750001") ]  <-
  "Total: All households"

# Replace 'Persons per square kilometre'
TS_Cell_Metadata$Cell_Label[ TS_Cell_Metadata$Cell_ID %in% c("ts0060001") ]  <-
  "Usual residents per square kilometre"

# Replace 'Total: all usual residents'
TS_Cell_Metadata$Cell_Label[ TS_Cell_Metadata$Cell_ID %in% c("ts0190001") ]  <-
  "Total: All usual residents aged 1 year and over"

# Replace 'All persons'
TS_Cell_Metadata$Cell_Label[ TS_Cell_Metadata$Cell_ID %in% c("ts0200001") ] <-
  "Total: All non-UK short-term residents"

# Replace 'Total: All usual residents aged 3 years and over'
TS_Cell_Metadata$Cell_Label[ TS_Cell_Metadata$Cell_ID %in% c("ts0320001", "ts0330001", "ts0340001",
                                                           "ts0350001", "ts0360001", "ts0760001") ]  <-
  "Total: All usual residents in Wales aged 3 years and over"

# Replace 'Total: all usual residents'
TS_Cell_Metadata$Cell_Label[ TS_Cell_Metadata$Cell_ID %in% c("ts0550001") ]  <-
  "Total: All usual residents with a second address"

# Replace 'Total: all usual residents'
TS_Cell_Metadata$Cell_Label[ TS_Cell_Metadata$Cell_ID %in% c("ts0710001") ]  <-
  "Total: All usual residents aged 16 years and over"

# Replace 'Total'
TS_Cell_Metadata$Cell_Label[ TS_Cell_Metadata$Cell_ID %in% c("ts0730001") ]  <-
  "Total: All usual residents who have previously served in the armed forces"

# Replace 'Total: all households'
TS_Cell_Metadata$Cell_Label[ TS_Cell_Metadata$Cell_ID %in% c("ts0740001") ]  <-
  "Total: All Household Reference Persons who have previously served in the armed forces"

# Disambiguate ts0270007 and ts0270017 cell labels
TS_Cell_Metadata$Cell_Label[ TS_Cell_Metadata$Cell_ID == "ts0270007" ] <-
  "All other combinations of only UK identities"

TS_Cell_Metadata$Cell_Label[ TS_Cell_Metadata$Cell_ID == "ts0270014 "] <-
  "Any other combination of only UK identities"


# Reduce to Table metadata file (keeping only first row/cell of each table),
# add population base (Pop_Base) based on Cell_Label for first cell in table...
TS_Table_Metadata <- TS_Cell_Metadata %>%
  filter(str_detect(str_sub(Cell_ID, -4, -1), "0001")) %>%
  mutate(Pop_Base = str_remove(Cell_Label, ".*[Tt]otal: "))
# ...then amend Pop_Base information where required
TS_Table_Metadata$Pop_Base[TS_Table_Metadata$Table_ID == "ts006"] <-
  "All usual residents" # Not 'Population density'
TS_Table_Metadata$Pop_Base[TS_Table_Metadata$Table_ID %in% c("ts037asp", "ts038asp")] <-
  "All usual residents"
TS_Table_Metadata$Pop_Base[TS_Table_Metadata$Table_ID == "ts039asp"] <- 
  "All usual residents aged 5 years and over"
TS_Table_Metadata$Pop_Base[TS_Table_Metadata$Table_ID == "ts031"] <- 
  "All usual residents" # ONS version capitalised each word


# Finalise TS Table Metadata by dropping unnecessary variables from pop_base,
# renaming EW_Count to EW_Value, and re-order columns as required
TS_Table_Metadata <- TS_Table_Metadata %>%
  select( - Cell_ID, -Cell_Label_ONS_Prefix, -Cell_Label_ONS, -Cell_Label, -EW_Pct) %>%
  rename( EW_Value = EW_Count) %>%
  relocate(Table_ID, Table_Label, Spatial_Coverage, Lowest_Geography,
           Unit, Measure, Pop_Base,
           Tiers,
           Variables, Variable_ID, Variable_Label,
           Categories, Cells, Interior_Cells,
           EW_Value,
           ctry:oa)

# Output the TS Table Metadata
write_csv( TS_Table_Metadata, "../Data/TS_Table_Metadata.csv")


# Finalise TS Cell Metadata by adding the Table's Pop_Base to each cell 
# and ensuring only required columns are in retained, in desired order
TS_Cell_Metadata <- left_join(TS_Cell_Metadata, 
                              TS_Table_Metadata[ , c("Table_ID", "Pop_Base")]) %>%
  relocate(Table_ID, Table_Label,
           Spatial_Coverage, Lowest_Geography,
           Unit, Measure, Pop_Base,
           Tiers,
           Variables, Variable_ID, Variable_Label, 
           Categories, Cells, Interior_Cells,
           Cell_ID,
           Cell_Label_ONS_Prefix,
           Cell_Label_ONS,
           Cell_Label,
           EW_Count,
           EW_Pct,
           ctry:oa)

# Output the TS_Cell_Metadata
write_csv( TS_Cell_Metadata, "../Data/TS_Cell_Metadata.csv")


