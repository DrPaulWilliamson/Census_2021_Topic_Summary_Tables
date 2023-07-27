# 1. Create mapping of TS table cell hierarchy
# 2. Count number of interior cell counts per table
# 3. Add count of interior cells to TS_Table_Metadata file

### NOTE: This code takes a LONG time to run (c. 1-2 mins per table)
### This appears to be due at least in part to the memory bloat associated
### with R maintaining multiple copies of the same data object when certain
### actions are performed on it. The code below has been 'cludged' to
### work around the worst of this.

library(tidyverse)

# the vroom package is installed by default as part of the tidyverse,
# but not loaded by the library(tidyverse) command
library(vroom)


table_dir <- "../Data/TS_Metadata/"

if (!dir.exists(table_dir)){
  dir.create(table_dir, recursive = TRUE)
}


# Read in TS_Cell_Metadata
Cell_Metadata_file_name <- paste0("../Data/TS_Cell_Metadata.csv")
meta_data_table <- vroom(Cell_Metadata_file_name, show_col_types = FALSE )

# Extract list of names of tables to be processed
# [needed to initialise table loop]
table_names <- unique(meta_data_table$Table_ID)

# Delete meta_data_table data (to aid memory management)
rm(meta_data_table)


#for (i in 1:length(table_names)) {
for (i in 61:77) {  
  
  # Read in table meata data and find table_names again
  # [doing this inside loop greatly aids memory management]
  meta_data_table <- vroom(Cell_Metadata_file_name, show_col_types = FALSE )
  table_names <- unique(meta_data_table$Table_ID)

  #i <- 28
  table_name <- table_names[i]
  #table_name <- "ts021"

  # Extract specific table
  table_data <- meta_data_table %>%
    filter(Table_ID == table_name) %>%
    select(Cell_ID, Cell_Label, EW_Count)
  
  # Delete data that will not be used in rest of loop, to help
  # avoid memory bloat
  rm(meta_data_table)
  rm(table_names)
  
    
  print(table_name)
  
  #write_csv(table_data, file = '../Data/temp.csv')
  
  # Extract Table_ID from Cell_ID
  table_data$Table_ID <- str_sub(table_data$Cell_ID, 1, -5)
  
  
  # If table == ts003, 'correct' label for cell 9 to make consistent with
  # labels for other cells in table
  if (table_name == "ts003") {
    table_data$Cell_Label[ table_data$Cell_ID == "ts0030009"] <- 
      "Single family household: Married or civil partnership couple: With dependent children"
  }
  
  
  # If table == ts007 ONS did not include tier 1 labels as part of tier 2 labels,
  # so these need to be added
  
  if (table_name == "ts007") {
  
    # Add relevant Tier 1 category label to relevant Tier 2 cells
    # (a) Place copy of Tier 1 labels in variable ts007_Tier1_Cell_Labels
    ts007_Tier1_Cells <- c(1, 2, 8, 14, 21, 26, 32, 43, 59, 75, 86, 97)
    ts007_Tier1_Cell_Labels <- NULL
    ts007_Tier1_Cell_Labels[1:nrow(table_data)] <- NA
    ts007_Tier1_Cell_Labels[ ts007_Tier1_Cells ] <-   table_data$Cell_Label[ ts007_Tier1_Cells ]
    # (b) Assign all Tier 2 cells the relevant Tier 1 label
    ts007_Tier1_Cell_Labels <- unlist( as_tibble(ts007_Tier1_Cell_Labels) %>% fill( value ) )
    # (c) Replace Tier 2 Cell_Label with colon-separated Tier 1 and Tier 2 label
    table_data$Cell_Label[-ts007_Tier1_Cells] <- str_c(ts007_Tier1_Cell_Labels[-ts007_Tier1_Cells], table_data$Cell_Label[-ts007_Tier1_Cells], sep=": " )
  
  }
  

  # If table == ts009, remove redundant Age and Sex prefixes.
  # Also remove 'All person' cell labels and counts (set to NA) to avoid double-counting
  # since these counts are simply the totals of the sex-specific counts
  # Finally, supply the missing Tier 1 Age information in Tier 2 Age labels
  if (table_name == "ts009") {
  
    # Remove the redundant text '; Age'
    table_data$Cell_Label <- 
      str_remove(table_data$Cell_Label, pattern="; Age")
    
    # Rename cell 1 to make consistent with naming strategy used by ONS for rest of table
    table_data$Cell_Label[1] <- "All persons: Total"
    
    # Extract the Sex and Age parts of the cell label as separate variables
    ts009_Sex_Cell_Labels <- 
      sapply( strsplit(table_data$Cell_Label,"[:]"), `[`, 1) # Sex
    ts009_Age_Cell_Labels <- 
      sapply( strsplit(table_data$Cell_Label,"[:]"), `[`, 2) #Age
    
    # Make a copy of the Age part of the Cell label, keep only the Tier 1 part of each cell Age label
    # then copy the Tier 1 Age label to all of the associated Tier 2 cells
    ts009_Tier1_Age_Cell_Labels <- ts009_Age_Cell_Labels
    ts009_Tier1_Age_Cell_Labels[ !str_detect(ts009_Age_Cell_Labels, "to|and|Total" ) ] <- NA
    ts009_Tier1_Age_Cell_Labels[ str_detect(ts009_Age_Cell_Labels, "90") ] <- NA
    ts009_Tier1_Age_Cell_Labels <- unlist( as_tibble(ts009_Tier1_Age_Cell_Labels) %>% fill( value ) )
    
    # Make a copy of the Age part of the cell label, keeping only the Tier2 part of each cell Age label
    ts009_Tier2_Age_Cell_Labels <- ts009_Age_Cell_Labels
    ts009_Tier2_Age_Cell_Labels[ str_detect(ts009_Age_Cell_Labels, "to|and|Total") ] <- NA
    ts009_Tier2_Age_Cell_Labels[ str_detect(ts009_Age_Cell_Labels, "90") ] <- 
      ts009_Age_Cell_Labels[ str_detect(ts009_Age_Cell_Labels, "90") ] #Add back the 90 years and over Tier 2 category
    
    # Combine the Sex and Tier 1 labels
    table_data$Cell_Label <- str_c( ts009_Sex_Cell_Labels,
                                    ts009_Tier1_Age_Cell_Labels,
                                    sep=":" )
    
    # For Tier 2 cells, add in the Tier2 label
    Tier2_Age_Cells <- !is.na(ts009_Tier2_Age_Cell_Labels)
    table_data$Cell_Label[ Tier2_Age_Cells ] <- 
      str_c( ts009_Sex_Cell_Labels[ Tier2_Age_Cells ],
             ts009_Tier1_Age_Cell_Labels[ Tier2_Age_Cells ],
             ts009_Tier2_Age_Cell_Labels[ Tier2_Age_Cells ],
             sep=":")

    # If table == ts009, set the 'All person' cell labels and counts to NA
    # to avoid double-counting alongside the supplied labels and counts for sex by age.
    # [first renaming first Cell label to All usual residents to ensure that the
    #  overall table total count is retained]
    # BUT keep copy of the All person cell labels for reinsertation into Cell_Label
    # column once Cell_Tier labels have been added
    ts009_Tidy_Cell_Label <- table_data$Cell_Label[2:103]
    table_data$Cell_Label[1] <- "Total: All usual residents"
    table_data[ str_detect(table_data$Cell_Label, pattern="persons"),
                c("Cell_Label", "EW_Count") ] <- NA
    
  } # If ts009

  
  # If table == ts027, rename the Tier 2 category 'Any other combination of only UK identities' to 
  # 'All other combinations of UK only identities' to make it different from Tier 1 category of same name
  # Then add missing Tier 1 labels to some Tier 2 cells
  # Also, cells need sorting into hierarchical order if hierarchy identification algorithm is to work correctly.
  # Do this by changing cells IDs here, and by then changing back to correct cells IDs once hierarchy labels
  # have been created.
  if (table_name == "ts027") {
    # Rename Tier 2 labels that has same name as Tier 1 label
    table_data$Cell_Label[14] <- "All other combinations of UK only identities"
    # Add Tier 1 labels to start of Tier 2 Cell labels
    table_data$Cell_Label[8:14] <- str_c(table_data$Cell_Label[7],table_data$Cell_Label[8:14], sep=": ")
    table_data$Cell_Label[15] <- str_c(table_data$Cell_Label[19],table_data$Cell_Label[15], sep=": ")
    table_data$Cell_Label[17] <- str_c(table_data$Cell_Label[19],table_data$Cell_Label[17], sep=": ")
    table_data$Cell_Label[16] <- str_c(table_data$Cell_Label[20],table_data$Cell_Label[16], sep=": ")
    table_data$Cell_Label[18] <- str_c(table_data$Cell_Label[20],table_data$Cell_Label[18], sep=": ")

    # Rename Cell_ID to reflect actual hierarchy order
    table_data$Cell_ID[15:20] <- c("ts0270016", "ts0270019", "ts0270017", "ts0270020", "ts0270015", "ts0270018")
    table_data <- table_data %>% arrange(Cell_ID)
  }  
  
  
  # If table == ts039, ONS-supplied labels failed to include Tier 1 label in some Tier 2 cells
  if (table_name == "ts039") {
    # Add Tier 1 labels to start of Tier 2 Cell labels
    table_data$Cell_Label[4] <- str_c(table_data$Cell_Label[3],table_data$Cell_Label[4], sep=": ")
    table_data$Cell_Label[5] <- str_c(table_data$Cell_Label[3],table_data$Cell_Label[5], sep=": ")
    table_data$Cell_Label[7] <- str_c(table_data$Cell_Label[6],table_data$Cell_Label[7], sep=": ")
    table_data$Cell_Label[8] <- str_c(table_data$Cell_Label[6],table_data$Cell_Label[8], sep=": ")
  } 

      
  # If table == ts047 or ts048, correct inconsistencies in ONS-supplied spacing after colons
  # so that there is ALWAYS a space after a colon
  if ( table_name %in% c("ts047", "ts048") ) {
    table_data$Cell_Label <- str_replace_all(table_data$Cell_Label, pattern = ": ", replace=":") %>%
      str_replace_all(., pattern = ":", replace=": ")
  }  
  
  
  # If table == ts052 or ts053, change the colon in middle of Tier 1 Cell name for a doule hyphen
  # so that colons are reserved in Cell Labels for denoting changes in table hierarchy
  if ( table_name %in% c("ts052", "ts053") ) {
    table_data$Cell_Label <- str_replace(table_data$Cell_Label, "rooms: ", "rooms -- ")
  }
  
  
  # If table == ts060, ONS omitted to include Tier 1 labels for Tier 2 cells, so these need to be added.
  # between them. To help with this process the colons placed in the middle of Tier 1 labels
  # are replaced with hyphens, to ensure that colons in Cell Labels denote change in hierarchy.
  # In addition, to maintain consistency the 'missing' colon in the R, S, T, U Other Cell Label
  # is added back, in the form of a hyphen. Finally, a stray space and semi-colon in the label for
  # Category E are 'corrected'.
  if (table_name == "ts060") {
    # Identify the labels associated with Tier 1 cells
    ts060_Tier1_Cells <- c(2, 6, 12, 37, 39, 44, 46, 51, 57, 60,
                           67, 71, 73, 81, 88, 90, 92, 96)
    # Replace colons in Tier 1 labels with hyphens, including adding a hyphen to
    # the R, S, T, U Other category to make consistent with other categories
    table_data$Cell_Label[ts060_Tier1_Cells] <- 
      str_replace( table_data$Cell_Label[ ts060_Tier1_Cells ], ": ", " - ")
    table_data$Cell_Label[96] <- str_replace( table_data$Cell_Label[96], 
                                              "U Other", "U - Other")
###    # Amend the text label for Category E to remove stray space and convert semi-colon to a comma
###    table_data$Cell_Label[39] <- str_replace( table_data$Cell_Label[39], 
###                                              " Water supply;", "Water supply,")
    # Add relevant Tier 1 category label to relevant Tier 2 cells
    table_data$Cell_Label[3:5] <-  
      str_c(table_data$Cell_Label[2], table_data$Cell_Label[3:5], sep=": ")
    table_data$Cell_Label[7:11] <-  
      str_c(table_data$Cell_Label[6], table_data$Cell_Label[7:11], sep=": ")
    table_data$Cell_Label[13:36] <-  
      str_c(table_data$Cell_Label[12], table_data$Cell_Label[13:36], sep=": ")
    table_data$Cell_Label[38] <-  
      str_c(table_data$Cell_Label[37], table_data$Cell_Label[38], sep=": ")
    table_data$Cell_Label[40:43] <-  
      str_c(table_data$Cell_Label[39], table_data$Cell_Label[40:43], sep=": ")
    table_data$Cell_Label[45] <-  
      str_c(table_data$Cell_Label[44], table_data$Cell_Label[45], sep=": ")
    table_data$Cell_Label[47:50] <-  
      str_c(table_data$Cell_Label[46], table_data$Cell_Label[47:50], sep=": ")
    table_data$Cell_Label[52:56] <-  
      str_c(table_data$Cell_Label[51], table_data$Cell_Label[52:56], sep=": ")
    table_data$Cell_Label[58:59] <-  
      str_c(table_data$Cell_Label[57], table_data$Cell_Label[58:59], sep=": ")
    table_data$Cell_Label[61:66] <-  
      str_c(table_data$Cell_Label[60], table_data$Cell_Label[61:66], sep=": ")
    table_data$Cell_Label[68:70] <-  
      str_c(table_data$Cell_Label[67], table_data$Cell_Label[68:70], sep=": ")
    table_data$Cell_Label[72] <-  
      str_c(table_data$Cell_Label[71], table_data$Cell_Label[72], sep=": ")
    table_data$Cell_Label[74:80] <-  
      str_c(table_data$Cell_Label[73], table_data$Cell_Label[74:80], sep=": ")
    table_data$Cell_Label[82:87] <-  
      str_c(table_data$Cell_Label[81], table_data$Cell_Label[82:87], sep=": ")
    table_data$Cell_Label[89] <-  
      str_c(table_data$Cell_Label[88], table_data$Cell_Label[89], sep=": ")
    table_data$Cell_Label[91] <-  
      str_c(table_data$Cell_Label[90], table_data$Cell_Label[91], sep=": ")
    table_data$Cell_Label[93:95] <-  
      str_c(table_data$Cell_Label[92], table_data$Cell_Label[93:95], sep=": ")
    table_data$Cell_Label[97:106] <-  
      str_c(table_data$Cell_Label[96], table_data$Cell_Label[97:106], sep=": ")
  }
  
  
  # If table == ts065, remove ' Not in employment: ' from the beginning of cells labels
  # (once their original beginning up to first colon has already been removed). This text appears in all
  # labels except for the first (total) cell, and is redundant because it does not identify a sub-category
  # of the total cell. Rather the total cell label includes the same information, but in a different way.
  if (table_name == "ts065") {
    table_data$Cell_Label <- str_remove(table_data$Cell_Label, "Not in employment: ")
  }
  
  
  # If table == ts066, ONS-supplied labels various do or don't include a space after colons. Make
  # consistent by (a) removing any spaces after colons; (b) adding a space after all colons
  if (table_name == "ts066") {
    table_data$Cell_Label <- str_replace_all(table_data$Cell_Label, pattern = ": ", replace=":") %>%
      str_replace_all(., pattern = ":", replace=": ")
  }
  
  
  # If table == ts076, remove redundant '; Age' text from ONS-supplied cell labels, both to avoid
  # inclusion of variable name in cell label and to avoid a redundant Tier 2 label.
  if (table_name == "ts076") {
    table_data$Cell_Label <- str_remove(table_data$Cell_Label, pattern="; Age")
  }
  
  
  # Split Cell Label into constituent levels (tiers) of table hierarchy
  # [TS Tables can have up to 5 levels off table hierarchy]
  table_data$Tier1 <-
    sapply( strsplit(table_data$Cell_Label,"[:]"), `[`, 1)
  table_data$Tier2 <-
    sapply( strsplit(table_data$Cell_Label,"[:]"), `[`, 2)
  table_data$Tier3 <-
    sapply( strsplit(table_data$Cell_Label,"[:]"), `[`, 3)
  table_data$Tier4 <-
    sapply( strsplit(table_data$Cell_Label,"[:]"), `[`, 4)
  table_data$Tier5 <-
    sapply( strsplit(table_data$Cell_Label,"[:]"), `[`, 5)
  
  
  # Initial Tier 1 Cell Label = Tier 1 text
  table_data$Cell_Label_Tier1 <- table_data$Tier1
  
  
  # Combine any Tier 2 label with associated Tier 1 label
  table_data$Cell_Label_Tier2 <- 
    ifelse( is.na(table_data$Tier2), 
            table_data$Tier2,
            paste( table_data$Cell_Label_Tier1, table_data$Tier2, sep = ":")
    )
  
  
  # Combine any Tier 3 label with associated Tier 2 label
  table_data$Cell_Label_Tier3 <- 
    ifelse( is.na(table_data$Tier3), 
            table_data$Tier3,
            paste( table_data$Cell_Label_Tier2, table_data$Tier3, sep = ":")
    )
  
  
  # Combine any Tier 4 label with associated Tier 3 label
  table_data$Cell_Label_Tier4 <- 
    ifelse( is.na(table_data$Tier4), 
            table_data$Tier4,
            paste( table_data$Cell_Label_Tier3, table_data$Tier4, sep = ":")
    )
  
  
  # Combine any Tier 5 label with associated Tier 4 label
  table_data$Cell_Label_Tier5 <- 
    ifelse( is.na(table_data$Tier5), 
            table_data$Tier5,
            paste( table_data$Cell_Label_Tier4, table_data$Tier5, sep = ":")
    )
  
  
  # If Tier 1 label is unique, propogate to Tier 2 where a Tier 2 label does not exit
  
  #write_csv(table_data, file= '../Data/temp.csv')
  
  # Count number of instances of each Tier 1 Category label
  # [There will be multiple instances if the category is sub-divided
  #  in lower tiers of the table; only 1 if not]
  table_data <- table_data %>%
    group_by(Cell_Label_Tier1) %>%
    summarise(Cell_Label_Tier1_n = n()) %>%
    right_join(table_data) %>%
    arrange(Cell_ID)
  
  # If only one instance of Tier 1 category, and no associated Tier 2 sub-category,
  # propogate this category to Tier 2
  valid_cells <- (table_data$Cell_Label_Tier1_n == 1) & 
    (is.na(table_data$Cell_Label_Tier2))
  table_data$Cell_Label_Tier2[ valid_cells ] <- table_data$Cell_Label_Tier1[ valid_cells ]
  
  # Count number of instances of each Tier 2 Category label
  table_data <- table_data %>%
    group_by(Cell_Label_Tier2) %>%
    summarise(Cell_Label_Tier2_n = n()) %>%
    right_join(table_data) %>%
    arrange(Cell_ID)
  
  # If only one instance of Tier 2 category, and no associated Tier 3 sub-category,
  # propogate this category to Tier 3
  valid_cells <- table_data$Cell_Label_Tier2_n == 1  & is.na(table_data$Cell_Label_Tier3)
  table_data$Cell_Label_Tier3[ valid_cells ] <- table_data$Cell_Label_Tier2[ valid_cells ]
  
  
  # Count number of instances of each Tier 3 Category label
  table_data <- table_data %>%
    group_by(Cell_Label_Tier3) %>%
    summarise(Cell_Label_Tier3_n = n()) %>%
    right_join(table_data) %>%
    arrange(Cell_ID)
  
  # If only one instance of Tier 3 category, and no associated Tier 4 sub-category,
  # propogate this category to Tier 4
  valid_cells <- table_data$Cell_Label_Tier3_n == 1  & is.na(table_data$Cell_Label_Tier4)
  table_data$Cell_Label_Tier4[ valid_cells ] <- table_data$Cell_Label_Tier3[ valid_cells ]
  
  # Count number of instances of each Tier 4 Category label
  table_data <- table_data %>%
    group_by(Cell_Label_Tier4) %>%
    summarise(Cell_Label_Tier4_n = n()) %>%
    right_join(table_data) %>%
    arrange(Cell_ID)
  
  # If only one instance of Tier 4 category, and no associated Tier 4 sub-category,
  # propogate this category to Tier 5
  valid_cells <- table_data$Cell_Label_Tier4_n == 1  & is.na(table_data$Cell_Label_Tier5)
  table_data$Cell_Label_Tier5[ valid_cells ] <- table_data$Cell_Label_Tier4[ valid_cells ]
  
  
  # Delete any duplicate categories in Tiers 1 to 5
  table_data$Cell_Label_Tier1[ duplicated(table_data$Cell_Label_Tier1) ] <- NA
  table_data$Cell_Label_Tier2[ duplicated(table_data$Cell_Label_Tier2) ] <- NA
  table_data$Cell_Label_Tier3[ duplicated(table_data$Cell_Label_Tier3) ] <- NA
  table_data$Cell_Label_Tier4[ duplicated(table_data$Cell_Label_Tier4) ] <- NA
  table_data$Cell_Label_Tier5[ duplicated(table_data$Cell_Label_Tier5) ] <- NA
  
  
  # Remove 'Total' and 'All usual residents' as a valid category from Tiers 1 to 5, since the
  # Tier categories should sum to the table total [i.e. a total is a table margin, not a table cell]
  table_data$Cell_Label_Tier1[ str_detect(table_data$Cell_Label_Tier1, 
                                          pattern="(?i)total|all usual residents") ] <- NA
  table_data$Cell_Label_Tier2[ str_detect(table_data$Cell_Label_Tier2, 
                                          pattern="(?i)total|all usual residents") ] <- NA
  table_data$Cell_Label_Tier3[ str_detect(table_data$Cell_Label_Tier3, 
                                          pattern="(?i)total|all usual residents") ] <- NA
  table_data$Cell_Label_Tier4[ str_detect(table_data$Cell_Label_Tier4, 
                                          pattern="(?i)total|all usual residents") ] <- NA
  table_data$Cell_Label_Tier5[ str_detect(table_data$Cell_Label_Tier5, 
                                          pattern="(?i)total|all usual residents") ] <- NA

  
  # Create an abridged label for each category in each Tier, based on the minimum text required
  # to uniquely identify the category WITHIN that tier (N.B. This may well involve loss of
  # important higher-tier contextual information required to fully understand what a cell is counting)
  
  # Assume that Tier 1 Category labels are by definition unique
  table_data$Cell_Label_Short_Tier1 <- table_data$Cell_Label_Tier1
  
  
  #Create shorter version of Cell_Label_Tier2
  table_data$Cell_Label_Short_Tier2 <- str_remove(table_data$Cell_Label_Tier2, pattern= ".*:")
  
  # Count number of instances of each shortened Tier 2 Category label
  table_data <- table_data %>%
    group_by(Cell_Label_Short_Tier2) %>%
    summarise(Cell_Label_Short_Tier2_n = n()) %>%
    right_join(table_data) %>%
    arrange(Cell_ID)
  
  # If shortened Tier 2 category has duplicates, revert to original longer Tier 2 Cell Label
  table_data$Cell_Label_Short_Tier2[ table_data$Cell_Label_Short_Tier2_n >1 ] <-
    table_data$Cell_Label_Tier2[ table_data$Cell_Label_Short_Tier2_n >1 ]
  
  
  #Create shorter version of Cell_Label_Tier3 based on Tier 3 only by dropping text before last colon
  table_data$Cell_Label_Short_Tier3_3 <- str_remove(table_data$Cell_Label_Tier3, pattern= ".*:")
  
  #Create shorter version of Cell_Label_Tier3 based on Tiers 2 and 3 only by dropping text before first colon
  table_data$Cell_Label_Short_Tier3_23 <- str_remove(table_data$Cell_Label_Tier3, pattern= "^[^:]*:")
  
  # Count number of instances of each possible shorter Tier 3 Category label
  table_data <- table_data %>%
    group_by(Cell_Label_Short_Tier3_23) %>%
    summarise(Cell_Label_Short_Tier3_23_n = n()) %>%
    right_join(table_data)
  table_data <- table_data %>%
    group_by(Cell_Label_Short_Tier3_3) %>%
    summarise(Cell_Label_Short_Tier3_3_n = n()) %>%
    right_join(table_data) %>%
    arrange(Cell_ID)
  
  # Assign shortest possible unique Tier3 label
  table_data$Cell_Label_Short_Tier3 <- table_data$Cell_Label_Tier3 # Direct copy of existing Tier 3 label
  table_data$Cell_Label_Short_Tier3[table_data$Cell_Label_Short_Tier3_23_n == 1] <-
    table_data$Cell_Label_Short_Tier3_23[table_data$Cell_Label_Short_Tier3_23_n == 1] # Unique two element label, if any
  table_data$Cell_Label_Short_Tier3[table_data$Cell_Label_Short_Tier3_3_n == 1] <-
    table_data$Cell_Label_Short_Tier3_3[table_data$Cell_Label_Short_Tier3_3_n == 1] # Unique one element label, if any
  
  #Create shorter version of Cell_Label_Tier4 based on Tier 4 only by dropping text before last colon
  table_data$Cell_Label_Short_Tier4_4 <- str_remove(table_data$Cell_Label_Tier4, pattern= ".*:")
  
  #Create shorter version of Cell_Label_Tier4 based on Tiers 2, 3 and 4 only by dropping text before first colon
  table_data$Cell_Label_Short_Tier4_234 <- str_remove(table_data$Cell_Label_Tier4, pattern= "^[^:]*:")
  
  #Create shorter version of Cell_Label_Tier4 based on Tiers 3 and 4 only by dropping text before first and second colon
  table_data$Cell_Label_Short_Tier4_34 <- str_remove(table_data$Cell_Label_Short_Tier4_234, pattern= "^[^:]*:")
  
  # Count number of instances of each possible shorter Tier 4 Category label
  table_data <- table_data %>%
    group_by(Cell_Label_Short_Tier4_4) %>%
    summarise(Cell_Label_Short_Tier4_4_n = n()) %>%
    right_join(table_data)
  table_data <- table_data %>%
    group_by(Cell_Label_Short_Tier4_34) %>%
    summarise(Cell_Label_Short_Tier4_34_n = n()) %>%
    right_join(table_data)
  table_data <- table_data %>%
    group_by(Cell_Label_Short_Tier4_234) %>%
    summarise(Cell_Label_Short_Tier4_234_n = n()) %>%
    right_join(table_data) %>%
    arrange(Cell_ID)  # This seemed to make memory jump
  
  # Assign shortest possible unique Tier4 label
  table_data$Cell_Label_Short_Tier4 <- table_data$Cell_Label_Tier4 # Direct copy of existing Tier 4 label
  table_data$Cell_Label_Short_Tier4[table_data$Cell_Label_Short_Tier4_234_n == 1] <-
    table_data$Cell_Label_Short_Tier4_234[table_data$Cell_Label_Short_Tier4_234_n == 1] # Unique three element label, if any
  table_data$Cell_Label_Short_Tier4[table_data$Cell_Label_Short_Tier4_34_n == 1] <-
    table_data$Cell_Label_Short_Tier4_34[table_data$Cell_Label_Short_Tier4_34_n == 1] # Unique two element label, if any
  table_data$Cell_Label_Short_Tier4[table_data$Cell_Label_Short_Tier4_4_n == 1] <-
    table_data$Cell_Label_Short_Tier4_34[table_data$Cell_Label_Short_Tier4_4_n == 1] # Unique one element label, if any  
  
  # Assign shortest possible unique Tier5 label
  ### NOTE: since only TS012 has 5 levels, all of which are unique, automated code to check each table
  ### for shortest possible unique Tier5 label not implemented here. Instead unique Tier 5 labels for TS012
  ### are hard-coded here, with all other tables inheriting their shortest Tier4 labels as their
  ### shortest possible Tier 5 label
  
  if (table_name == "ts012") {
    table_data$Cell_Label_Short_Tier5 <- str_remove(table_data$Cell_Label_Tier5, pattern= ".*:")
  } else {
    table_data$Cell_Label_Short_Tier5 <- table_data$Cell_Label_Short_Tier4 # Direct copy of existing Short Tier 4 label
  }
  

  
  # Make corrections to Tier 1 labels necessitated by ONS failing to provide a cell count for one or more
  # Tier 1 categories. (E.g. ts075 contains 'Multi-person household' category but provides no associated cell count.
  # Instead it simply provides counts for the Tier 2 categories of this Tier 1 category.) Hence solution is to
  # make the Tier 1 categories the same as the Tier 2 categories so that they match the cell counts actually provided
  # by ONS.
  if (table_name %in% c("ts038asp", "ts075")) {
    table_data$Cell_Label_Tier1 <- table_data$Cell_Label_Tier2
    table_data$Cell_Label_Short_Tier1 <- table_data$Cell_Label_Short_Tier2
  }
  
  #write_csv(table_data, file='../Data/temp.csv')
  
  # Correct ts027 Cell IDs back to names and order as originally supplied by ONS
  if (table_name == "ts027") {
    # # Make sure data sorted in Cell_ID order
    # table_data <- table_data %>% arrange(Cell_ID) 
    # Change Cell_IDs back to originals supplied by ONS
    table_data$Cell_ID[15:20] <- c("ts0270019", "ts0270015", "ts0270017", "ts0270020", "ts0270016", "ts0270018")
    # Sort data back into original Cell_ID order
    table_data <- table_data %>% arrange(Cell_ID) 
  }
  
  # write out object, delete and read back in, in an attempt
  # to reduce memory usage
  
  #write_csv(table_data, file = '../Data/temp.csv')
  #rm(table_data)
  #table_data <- vroom('../Data/temp.csv')
  
  
  # If table contains only 1 row (e.g. TS041 'Number of households') then at this stage all Tier labels = NA
  # Set all Tier labels to ONS-supplied Cell_Label
  if (nrow(table_data) == 1) {
    table_data[ , str_detect(names(table_data), "Label")] <- table_data$Cell_Label
  }

  # If table ts009, add back the temporarily removed tidy cell labels for 'All person' rows
  if (table_name == "ts009") {
    table_data$Cell_Label[2:103] <- ts009_Tidy_Cell_Label
  }
  
  # Find Table and Tier totals [they should all be the same]
  table_data[ , c("Table_total", "Tier1_total", "Tier2_total", "Tier3_total",
                  "Tier4_total", "Tier5_total",
                  "Tier1_short_total", "Tier2_short_total", "Tier3_short_total",
                  "Tier4_short_total", "Tier5_short_total") ] <- NA
  
  # Table total is normally given in first row of table, but in some cases (e.g. asp tables),
  # treat the 'table total' as simply the sum of the table cells
  table_data$Table_total[1] <- if (str_detect(table_data$Cell_Label[1], "Total") == TRUE) {
    table_data$EW_Count[1]
  } else  {
    sum( table_data$EW_Count ) 
  }
  
  print("Find Tier totals")
  table_data$Tier1_total[1] <- sum( table_data$EW_Count[ !is.na(table_data$Cell_Label_Tier1) ] )
  table_data$Tier2_total[1] <- sum( table_data$EW_Count[ !is.na(table_data$Cell_Label_Tier2) ] )
  table_data$Tier3_total[1] <- sum( table_data$EW_Count[ !is.na(table_data$Cell_Label_Tier3) ] )
  table_data$Tier4_total[1] <- sum( table_data$EW_Count[ !is.na(table_data$Cell_Label_Tier4) ] )
  table_data$Tier5_total[1] <- sum( table_data$EW_Count[ !is.na(table_data$Cell_Label_Tier5) ] )
    
  table_data$Tier1_short_total[1] <- sum( table_data$EW_Count[ !is.na(table_data$Cell_Label_Tier1) ] )
  table_data$Tier2_short_total[1] <- sum( table_data$EW_Count[ !is.na(table_data$Cell_Label_Tier2) ] )
  table_data$Tier3_short_total[1] <- sum( table_data$EW_Count[ !is.na(table_data$Cell_Label_Tier3) ] )
  table_data$Tier4_short_total[1] <- sum( table_data$EW_Count[ !is.na(table_data$Cell_Label_Tier4) ] )
  table_data$Tier5_short_total[1] <- sum( table_data$EW_Count[ !is.na(table_data$Cell_Label_Tier5) ] )  
  
  print("total_check")
  # Save check that all table totals are the same
  table_data$total_check <- 
    table_data %>% 
      select( contains("total")) %>%
      slice(1) %>% # Use only first row of table_data, where table totals are stored
      mutate(total_check = if_all(Tier1_total:Tier5_short_total, `==`, Table_total)) %>%
      select(total_check) %>%
      unlist() # Without unlist, results in new column called total_check$total_check
  
  if (table_data$total_check[1] == FALSE) {
    warning("Tier totals differ for table ", table_name)
  }
  
  print("Drop no longer needed columns")
  # Drop no longer needed columns, ungroup, and sort into cell order
  table_data <- table_data %>%
    select(Table_ID,
           Cell_ID, Cell_Label,
           Cell_Label_Tier1, Cell_Label_Tier2, Cell_Label_Tier3, Cell_Label_Tier4, Cell_Label_Tier5,
           Cell_Label_Short_Tier1, Cell_Label_Short_Tier2, Cell_Label_Short_Tier3, 
           Cell_Label_Short_Tier4, Cell_Label_Short_Tier5,
           EW_Count, 
           #Table_total, Tier1_total, Tier2_total, Tier3_total, Tier4_total, Tier5_total,
           #Tier1_short_total, Tier2_short_total, Tier3_short_total, 
           #Tier4_short_total, Tier5_short_total, total_check
           ) #%>%
    #arrange(Cell_ID)
  
  print("Convert NAs")
  # Convert NAs in Cell Labels to null characters to aid easier reading [i.e. "" ]
  table_data <- 
    mutate(table_data,
           across(where(is.character), ~replace_na(.x, "")) #Apply to character columns only
    )
  
  #write_csv(table_data, file= '../Data/temp.csv')
  

  
  # table_data$n_Tiers <- n_Tiers
  n_Tiers <- -9

  # Count and save number of interior (i.e. non-marginal) cells in table
  table_data$n_Interior_Cells <- sum(table_data$Cell_Label_Tier5 != "")

  print("Write out table_data to file")
  #timings <- system.time(
    #write_csv(table_data, file= '../Data/temp.csv')
    write_csv( table_data, 
               paste0("../Data/TS_Metadata/",table_name,"_metadata.csv") )
    # vroom_write(table_data, 
    #            paste0("../Data/TS_Metadata/",table_name,"_metadata.csv"),
    #            delim=",")
  #)
  #print(timings)
  
  rm(table_data)
  
} #next table_name

# Read in table-specific data and combine
table_dir <- "../Data/TS_Metadata/"
filenames <- list.files( table_dir, recursive = FALSE )
filenames <- str_c(table_dir, filenames)
TS_Cell_Mapping <- vroom(filenames)
nrow(TS_Cell_Mapping)

# Read in existing Table metadata, add and rename n_Tiers and n_Interior_Cells, and save updated file
file_name <- "../Data/TS_Table_Metadata.csv"
TS_Table_Metadata <- vroom(file_name, show_col_types = FALSE ) %>%
  mutate(Tiers = n_Tiers, Interior_Cells=Interior_cell)

# Identify which cell in each table is the first cell, to aid
# selection of only one row per table from TS_Cell_Mapping
Table_0001_cells <- str_sub(TS_Cell_Mapping$Cell_ID, -4) == "0001"

# Add Interior_Cells from TS_Cell_Mapping onto TS_Table
TS_Table_Metadata <- TS_Cell_Mapping %>%
  filter(Table_0001_cells==TRUE) %>% 
  select(Table_ID, Interior_Cells) %>%
  right_join(TS_Table_Metadata)

write_csv(TS_Table_Metadata, "../Data/TS_Table_Metadata.csv") #Overwrite existing file

# Save cell mappings results as a .csv file
write_csv(TS_Cell_Mapping, paste0("../Data/","TS_Cell_Mapping.csv"))
