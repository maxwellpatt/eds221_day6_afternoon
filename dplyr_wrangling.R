#-------------- Section 1: Filter ---------------#

library(tidyverse)
library(palmerpenguins)
library(lterdatasampler)
library(ggplot2)

# Look for an exact match: ==

penguins_biscoe <- penguins %>%
  filter(island == "Biscoe")

adelie_torgersen <- penguins %>%
  filter(species == "Adelie" & island == "Torgersen")
  # could also write this as filter(species == "Adelie", island == "Torgersen)

# Create a subset from penguins that only contains Gentoo penguins in 2008

gentoo_2008 <- penguins %>%
  filter(species =="Gentoo", year == 2008)

# Create a subset that contains Gentoos and Adelies
gentoo_adelie <- penguins %>% filter(species == "Gentoo" | species == "Adelie")

# Create a subset that contains observations where the island is Dream OR the
# year is 2009
dream_or_2009 <- penguins %>% filter(island == "Dream" | year == 2009)

# Make a ggplot chart of water temp vs crab size
ggplot(data = pie_crab, aes(x = water_temp, y = size)) + geom_point()

# Keep observations for sites NIB, ZI, DB, JC
# We can use the %in% operator to ask: does the value in our column match
# any of the values IN this vector?
pie_sites <- pie_crab %>% filter(site %in% c("NIB", "ZI", "DB", "JC"))

# Create a subset using the %in% operator that includes sites PIE, ZI, NIB,
# BB and CC
valid_sites = c("PIE", "ZI", "NIB", "BB", "CC")
pie_sites_2 <- pie_crab %>% filter(site %in% valid_sites)

# Excluding filter statements

# != : asks is this NOT equal to that value

exclude_zi <- pie_crab %>% filter(site != "ZI")

# What if I want to exclude sites BB, CC and PIE
exclude_bb_cc_pie <- pie_crab %>% filter(!site %in% c("BB", "CC", "PIE"))

# Create a subset from pie_crab that only contains obs from NIB, CC, and ZI,
# for crabs with carapace size exceeding 13
pie_crab_large <- pie_crab %>% filter(site %in% c("NIB", "CC", "ZI"), size > 13)



#----------- Selecting columns -----------#

# Select individual columns by name, separate them by a comma
crabs_subset <- pie_crab %>% select(latitude, size, water_temp)

# Select a range (and individual column) of columns using :
crabs_subset2 <- pie_crab %>% select(site:air_temp, name)

#------------ Mutate -------------------#

# Use dplyr::mutate() to add or update a column, while keeping all existing cols
crabs_cm <- pie_crab %>%
  mutate(size_cm = size / 10)

# What happens if I use mutate to add a new column containing the mean of the size column?
crabs_mean <- pie_crab %>%
  mutate(mean_size = mean(size, na.rm = TRUE))

# Reminder: group_by + summarize
mean_size_by_site <- pie_crab %>%
  group_by(site) %>%  # saying under the hood that different sites exist
  summarize(mean_size = mean(size, na.rm = TRUE))

# What about a group_by then mutate?
group_mutate <- pie_crab %>%
  group_by(site) %>%
  mutate(mean_size = mean(size, na.rm = TRUE))

# What if I want to create a new column in pie_crab that contains "giant" if
# the size is greater than 35, or "not giant" if the size is less than or equal to 35?

# Use dplyr::case_when() to write if-else statements more easily!
crabs_bin <- pie_crab %>%
  mutate(size_binned = case_when(
    size > 20 ~ "giant",
    size <= 20 ~ "not giant"
  ))

sites_binned <- pie_crab %>%
  mutate(region = case_when(
    site %in% c("ZI", "CC", "PIE") ~ "Low",
    site %in% c("BB", "NIB") ~ "Middle",
    TRUE ~ "High"
  ))




























