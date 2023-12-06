########
# Project: Anonymized
# Purpose: Clean and merge data
# Author: Davi Ferreira Veronese
# Date: 17th November 2023
########

# This script performs simple cleaning and fuzzy merging processes. 
# As it was developed for an ongoing project, all references to real data, paths, and names were removed. 
# Thus, it is an anonymized script with the exclusive purpose of presenting some of the R codings I have been working on. 
# Please do not circulate it.

# Load packages
library(tidyverse)
library(tidylog)
library(haven)
library(readxl)
library(fedmatch)
library(stringr)

##### Import data #####

# Import data_1
data_1 <- readRDS("data.rds")

# Import data_2
path_1 <- "path_1"
data_2_1 <- read.csv(path_1)

path_2 <- "path_2"
data_2_2 <- read.csv(path_2)

path_3 <- "path_3"
data_2_3 <- read.csv(path_3)
  
##### Clean data #####

# Create cleaning function
clean <- function(df) {
  df %>%
    select(-`S.No.`) %>%
    mutate_all(~tolower(.)) %>%
    rename_all(~str_to_lower(gsub("\\.{1,3}", "_", .))) %>%
    rename(state = state_state,
           district = district_district,
           block = block_block,
           gp = gp_gp,
           village = village_village) %>% 
    mutate_all(~ gsub("^ ", "", .)) 
}

data_2_1 <- clean(data_2_1)
data_2_2 <- clean(data_2_2)
data_2_3 <- clean(data_2_3)

# Bind
data_2 <- bind_rows(data_2_1, data_2_2, data_2_3)

# Remove missing values
data_1 <- data_1 %>%
  filter(!is.na(code_1))

# Prepare data for merge

## Create a unique ID
data_2 <- data_2 %>%
  arrange(district, block, village) %>%
  mutate(code_2 = group_indices(., district, block, village))

## Keep only village-level observations
data_2 <- data_2 %>%
  distinct(district, block, village, .keep_all = TRUE) %>%
  select(district, block, village, code_2)

# Keep only variables that are necessary to merge
data_1 <- data_1 %>%
  select(district, block, village, code_1)
  
##### Fuzzy merge - data_1 and data_2 #####

# Merge 
merge <- merge_plus(data_1, data_2, 
                              match_type = "multivar",
                              by = c("block", "village"),
                              suffixes = c("_1", "_2"),
                              unique_key_1 = "code_1",
                              unique_key_2 = "code_2",
                              multivar_settings = build_multivar_settings(
                                compare_type = c("stringdist", "stringdist"), 
                                top = 1, 
                                blocks = c("district"), 
                                wgts = c(0.5, 1)))

match1 <- merge$matches

# Identify wrong matches
match1 <- match1 %>% 
  select(code_1, code_2, village_1, village_2, village_compare, everything()) %>% 
  arrange(village_compare)
  
wrong_1 <- c(9, 12, 8, 10, 44, 76)

match1_wrong <- match1 %>% 
  filter(code_1 %in% wrong_1)

# Now drop them from the match1 dataset
match1 <- match1 %>% 
  filter(!(code_1 %in% wrong_1))

# Create crosswalk 
crosswalk <- match %>% 
  select(code_1, code_2, district, block_1, block_2, village_1, village_2, district_1, district_2)

##### Merge data_1 and data_2 #####
crosswalk <- crosswalk %>%
  select(code_1, code_2)

data <- data_1 %>%
  left_join(crosswalk, by = "code_1") %>%
  left_join(data_2, by = "code_2") %>% 
  select(code_1, code_2, state, district, subdistrict, block, gp, village,
         everything()) %>%
  mutate_all(~ ifelse(. %in% c("", " ", "  "), NA_character_, .)) # and make final clean

# Save data
saveRDS(data, "data.rds")

# Clean environment
rm(list = ls())


