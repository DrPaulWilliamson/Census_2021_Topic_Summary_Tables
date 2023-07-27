# Extract all cell counts relevant to a specific tier of a TS table's cell hierarchy
#
# N.B. If request is for a tier higher than exists in table, then highest possible
# table tier will be extracted instead. E.g. If a table only has one tier, then a
# request for Tier 5 cell counts would extract Tier 1 cell counts.


# 1. Load relevant libraries

library(tidyverse)
library(vroom)


# 2. Read in TS_Cell_Mapping

# Read in TS_Cell_Mapping.csv
mapping_filename <- ("../Data/TS_Cell_Mapping.csv")
TS_Cell_Mapping <- vroom(mapping_filename)


# 3. Specify the TS table and geographic level for which Census data are required
table_name <- "ts002"
geography <- "ctry"


# 4. Illustrate the concept of cell hierarchies within a table

## Table ts002 contains three tiers of cell hierarchy.
## The following section of code is not needed to extract the cell counts
## required for a given tier, but is provided here to illustrate
## the concept of cell hierarchies

# List ALL of the cells in the table (regardless of which Tier they belong to)
TS_Cell_Mapping %>%
  filter(Table_ID == table_name) %>%
  select(Cell_Label)

# List ALL of the cells that belong in Tier 1 of the table, plus
# associated 'long' and 'short' versions of Tier 1 label
TS_Cell_Mapping %>%
  filter(Table_ID == table_name & !is.na(Cell_Label_Tier1) ) %>%
  select(Cell_Label, Cell_Label_Tier1, Cell_Label_Short_Tier1  )

# List ALL of the cells that belong in Tier 2 of the table, plus
# associated 'long' and 'short' versions of Tier 2 label
TS_Cell_Mapping %>%
  filter(Table_ID == table_name & !is.na(Cell_Label_Tier2) ) %>%
  select(Cell_Label, Cell_Label_Tier2, Cell_Label_Short_Tier2  )

# List ALL of the cells that belong in Tier 3 of the table, plus
# associated 'long' and 'short' versions of Tier 3 label
TS_Cell_Mapping %>%
  filter(Table_ID == table_name & !is.na(Cell_Label_Tier3) ) %>%
  select(Cell_Label, Cell_Label_Tier3, Cell_Label_Short_Tier3  )

# List ALL of the cells that belong in Tier 4 of the table, plus
# associated 'long' and 'short' versions of Tier 4 label
# [For ts002 Tier 4 cells are the same as the Tier 3 because
#  ts002 only contains three tiers of cell hierarchy]
TS_Cell_Mapping %>%
  filter(Table_ID == table_name & !is.na(Cell_Label_Tier3) ) %>%
  select(Cell_Label, Cell_Label_Tier3, Cell_Label_Short_Tier3  )


# 5. Extract census data for the relevant table, geographic level and cell tier

# Specify the table tier required (from Tier1 to Tier5)
Tier <- "Tier1"

# Specify and save names of required Tier label columns
Tier_label <- paste0("Cell_Label_",Tier)
Tier_label_short <- paste0("Cell_Label_Short_",Tier)

# Extract Cell IDs for all cells that belong to the Tier and convert into cell numbers
cell_IDs <- 
  TS_Cell_Mapping %>% 
  filter(Table_ID == table_name & !is.na( .data[[Tier_label]] ) ) %>%
  select(Cell_ID) %>%
  unlist()
cell_numbers <- cell_IDs %>%
  str_sub(-4) %>%
  as.numeric()

# Read in TS table counts for specified level of geography
data_filename <- paste0("../Data/TS/",
                        table_name,"/",
                        "census2021-",table_name,"-ctry.csv")
table_data <- vroom(data_filename)

# Extract cell counts for specified table, geography and table tier
# for each area (row) in the file
Tier1_table_data <- table_data[ , c(1:3, cell_numbers+3)]
Tier1_table_data

# If desired, replace descriptive column name for cell count with Cell_ID
names(Tier1_table_data)[4:(length(cell_numbers)+3)] <- cell_IDs
Tier1_table_data
