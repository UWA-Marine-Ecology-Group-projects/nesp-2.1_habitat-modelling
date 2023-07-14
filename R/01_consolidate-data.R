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
  dplyr::filter(campaignid %in% c("2020-06_south-west_stereo-BRUVs", 
                                  "2020-10_south-west_stereo-BRUVs",
                                  # "2020-10_south-west_BOSS",
                                  "2021-03_West-Coast_BOSS" )) %>%
  dplyr::filter(!sample %in% "NA") %>%                                          # One NA sample in the FRDC campaign
  mutate(across(starts_with("broad"), as.numeric)) %>%                          # Change back to numeric values
  mutate(across(starts_with("broad"), ~replace_na(.,0))) %>%
  dplyr::mutate(broad.total.points.annotated = rowSums(.[,5:ncol(.)],           # Generate total points annotated to standardise 
                                                       na.rm = T)) %>%
  dplyr::filter(broad.total.points.annotated > 0) %>%                           # Remove unsuccessful samples
  dplyr::mutate(sessile.inverts = broad.sponges + broad.stony.corals + broad.bryozoa +
                  broad.crinoids + broad.hydroids + broad.invertebrate.complex + 
                  broad.true.anemones + broad.octocoral.black) %>%
  glimpse()

# Read in the metadata
metadata <- list.files(path = "data/raw",
                       recursive = T,
                       pattern = "_Metadata.csv",
                       full.names = T) %>%
  purrr::map_dfr(~read_files_csv(.)) %>%
  dplyr::filter(campaignid %in% c("2020-06_south-west_stereo-BRUVs", 
                                  "2020-10_south-west_stereo-BRUVs",
                                  # "2020-10_south-west_BOSS",
                                  "2021-03_West-Coast_BOSS" )) %>%
  dplyr::select(campaignid, sample, site, planned.or.exploratory) %>%
  dplyr::filter(!is.na(sample)) %>%
  dplyr::mutate(sample = ifelse(campaignid %in% "2020-06_south-west_stereo-BRUVs" 
                                & str_length(.$sample) == 1, 
                                str_pad(.$sample, width = 2, 
                                        pad = "0", side = "left"), 
                                sample)) %>%
  distinct() %>%                                                                # Duplicated row
  glimpse()

test <- metadata %>%
  dplyr::filter(is.na(planned.or.exploratory))

# missing.sites <- metadata %>%
#   dplyr::filter(campaignid %in% "2020-10_south-west_stereo-BRUVs") %>%
#   dplyr::mutate(sample = str_replace_all(.$sample, "IO", "")) %>%
#   dplyr::select(-campaignid) %>%
#   glimpse()
# 
# missing.mbh <- metadata %>%
#   dplyr::filter(is.na(planned.or.exploratory)) %>%
#   dplyr::select(-c(planned.or.exploratory, site)) %>%
#   dplyr::left_join(missing.sites) %>%
#   glimpse()
# 
# test <- missing.mbh %>%
#   dplyr::filter(is.na(planned.or.exploratory))

# Join habitat data with cluster ID (site) and mark type (MBH or other)

tidy.habitat <- habitat %>%
  left_join(metadata) %>%
  dplyr::mutate(planned.or.exploratory = ifelse(is.na(planned.or.exploratory),
                                                "Captains pick", planned.or.exploratory)) %>%
  dplyr::mutate(clustered = ifelse(campaignid %in% "2021-03_West-Coast_BOSS", FALSE, TRUE),
                method = ifelse(str_detect(.$campaignid, "BRUV"), "BRUV", "drop.cam")) %>%
  glimpse()                                                                     # Number of obs matches original

test <- tidy.habitat %>%
  dplyr::filter(is.na(planned.or.exploratory) & isTRUE(clustered))

# Write out the tidy data
write.csv(habitat, file = "data/tidy/nesp-2.1_WA_habitat-data.csv",
          row.names = F)
