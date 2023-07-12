###
# Project: NESP 2.1
# Data:    Spatially-balanced BOSS and BRUV habitat annotations
# Task:    Consolidate data
# author:  Claude Spencer
# date:    July 2023
##

# Clear the environment of any large files
rm(list = ls())

# Load libraries
library(tidyverse)

# Make a function to read in all the habitat data in the directory
read_files_csv <- function(flnm) {
  flnm %>%
    readr::read_csv(col_types = readr::cols(.default = "c")) %>%
    GlobalArchive::ga.clean.names()
}

# Read in the habitat data
habitat <- list.files(path = "data/staging",
                      recursive = T,
                      pattern = "_broad.habitat.csv",
                      full.names = T) %>%
  purrr::map_dfr(~read_files_csv(.)) %>%
  dplyr::select(campaignid, sample, latitude, longitude,                        # Metadata
                starts_with("broad."),                                          # Habitat columns
                -c(broad.reef, broad.kelps, total.sum,                          # Remove unnecessary columns
                   broad.total.points.annotated, broad.unknown)) %>%
  dplyr::filter(!sample %in% "NA") %>%                                          # One NA sample in the FRDC campaign
  mutate(across(starts_with("broad"), as.numeric)) %>%                          # Change back to numeric values
  mutate(across(starts_with("broad"), ~replace_na(.,0))) %>%
  dplyr::mutate(broad.total.points.annotated = rowSums(.[,6:ncol(.)],           # Generate total points annotated to standardise 
                                                       na.rm = T)) %>%
  dplyr::filter(broad.total.points.annotated > 0) %>%
  glimpse()

# Check the correct campaigns have been included
unique(habitat$campaignid)

# Write out the tidy data
write.csv(habitat, file = "data/tidy/nesp-2.1_WA_habitat-data.csv",
          row.names = F)
