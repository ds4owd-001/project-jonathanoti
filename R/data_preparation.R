# description ---------------------------------------------------------------

# R script to extract from jmpwash package, read in additional csv files and clean the data. 
# The tidy data is then saved as a CSV file.

# packages ------------------------------------------------------------------

#install.packages("devtools")
#devtools::install_github("WASHNote/jmpwashdata")

library(jmpwashdata)
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
library(readr)
library(janitor)

# =====================================code==================================

# Data acquisition ----------------------------------------------------------

# extract data of interest from the jmpwash package

jmp_schools_wld_hygiene <-  jmpwashdata::jmp_schools_wld_hygiene
jmp_schools_wld_sanitation <-  jmpwashdata::jmp_schools_wld_sanitation
jmp_schools_wld_water <-  jmpwashdata::jmp_schools_wld_water


# save data as csv into the data/raw folder

write_csv(jmp_schools_wld_hygiene, here::here("data/raw/jmp_schools_hygiene.csv"))
write_csv(jmp_schools_wld_water, here::here("data/raw/jmp_schools_water.csv"))
write_csv(jmp_schools_wld_sanitation, here::here("data/raw/jmp_schools_sanitation.csv"))


# Data Cleaning -------------------------------------------------------------

# Import data

data_hygi <- read_csv(here::here("data/raw/jmp_schools_hygiene.csv"), col_select = 1:25)
data_sanit <- read_csv(here::here("data/raw/jmp_schools_sanitation.csv"), col_select = 1:25)
data_water <- read_csv(here::here("data/raw/jmp_schools_water.csv"), col_select = 1:25)
jmp2022 <- read_csv(here::here("data/raw/JMP_2022_Schools_WASH.csv"))


# the jmp2022 data is a full set of the data downloaded from the JMP website. 
# It has additional information which is needed for the analysis. 
# The interested section will be extracted.


# combine the service type data sets into one

data_merge_1 <- cbind.data.frame(data_hygi, data_sanit[,-c(1:7)])
data_merge_full <- cbind.data.frame(data_merge_1, data_water[,-c(1:7)])



# Rename the headings for some columns 

processed_v1 <- data_merge_full |> 
  rename(country = "name",
         total_population = "schoolagepop_nat",
         urban_popu_percent = "prop_urb",
         pre_school_popu_percent = "prop_pre",
         prim_school_popu_percent = "prop_pri",
         secon_school_popu_percent = "prop_sec") |> 
  mutate(rural_popu_percent = 100 - urban_popu_percent) |> 
  relocate(rural_popu_percent, .after = 4)


# remove the section/rows that are not of interest from the country column
processed_v1 <- processed_v1 |> 
  filter(country != "WORLD")


# Merge with the data from jmp2022 ------------------------------------------

# select the data from JMP2022 and merge

SDGregion <- jmp2022 |> 
  select(`SDG region`) 
iso3 <-  jmp2022 |> 
  select(`iso3 code`)
country <- jmp2022 |> 
  select(`COUNTRY, AREA OR TERRITORY`)

sub_regions <- cbind.data.frame(SDGregion,iso3,country)
sub_regions <-  drop_na(sub_regions)

# convert to unique values and merge
uniq_sub_regions <- unique(sub_regions)


# join the sub_regions with the larger data (processed_v1) using left_join 
combined_final <- left_join(processed_v1, uniq_sub_regions, by = "country") |> 
  relocate(c(iso3, SDGregion))


# Pivoting ------------------------------------------------------------------

# it turns out there are negative values of -999 in the data which does not fit 
# our purpose so we replace it with NAs before pivoting

processed_v2 <- combined_final |> 
  mutate_all(~ ifelse(. == -999, NA, .))

# we check to see if the values have been removed.
which(processed_v2 == -999, arr.ind = TRUE)


# now conduct the pivot

# pivot v1: combine the coverage of service levels under one variable and their corresponding values
processed_v2_longer <- processed_v2 |> 
  pivot_longer(cols = c(11:64),
               values_to = "percent_coverage",
               names_to = "service_levels")


# creating new columns for the service types, residential types and service levels
processed_v2_longer <- 
  processed_v2_longer |> 
  mutate(residence_school_level = factor(case_when(
    str_detect(service_levels, "pre") ~ "Pre-primary",
    str_detect(service_levels, "pri") ~ "Primary",
    str_detect(service_levels, "sec") ~ "Secondary",
    str_detect(service_levels, "urb") ~ "Urban residence",
    str_detect(service_levels, "rur") ~ "Rural residence",
    str_detect(service_levels, "nat") ~ "National level")), 
    service_level = factor(case_when(
      str_detect(service_levels, "bas") ~ "Basic service",
      str_detect(service_levels, "lim") ~ "Limited service",
      str_detect(service_levels, "none") ~ "No service")),
    service_types = as.factor(case_when(
      str_detect(service_levels, "hyg") ~ "Hygiene",
      str_detect(service_levels, "wat") ~ "Water supply",
      str_detect(service_levels, "san") ~ "Sanitation",
      TRUE ~ "Other")))


# pivot v2: combine population proportions under one variable
processed_v2_longer_v2 <- 
  processed_v2_longer |> 
  select(-service_levels) |>  #removes the old service_levels column
  pivot_longer(cols = c(6:10),
               names_to = "population_category",
               values_to = "population_share") 




# Check for NAs and fix it --------------------------------------------------

# count the number of observations
processed_v2_longer_v2

# check number of observations with missing data
sum(is.na.data.frame(processed_v2_longer_v2))

# there are 729,310 rows with missing data

# Remove the missing data from the column of interest
processed_v2_longer_v3 <- processed_v2_longer_v2 |> 
  filter(!is.na(percent_coverage))


# checking for other NAs to confirm
which(is.na(processed_v2_longer_v3), arr.ind = TRUE)

processed_v2_longer_v3[76506:79005,1:3]
# some NAs revealed in the iso3 column. it seems to be from Côte d’Ivoire. 


# Let's fill it
processed_v2_longer_v4 <- processed_v2_longer_v3 |> 
  mutate(
    iso3 = case_when(
      is.na(iso3) & country == "Côte d’Ivoire" ~ "CIV", TRUE ~ iso3),
    SDGregion = case_when(
      is.na(SDGregion) & country == "Côte d’Ivoire" ~ "Sub-Saharan Africa", TRUE ~ SDGregion))


# lets check again for the nas
which(is.na(processed_v2_longer_v4), arr.ind = TRUE)

# lets confirm
processed_v2_longer_v4[76506:77005,1:3]

# Now everything is fine. There are no Nas



# Extract the final data for the analysis (data for sub-Sahara Africa) -------

Final_processed_data <- processed_v2_longer_v4 |>
  filter(SDGregion == "Sub-Saharan Africa")


# Save tidy data --------------------------------------------------------------

# save data in the processed folder
write_csv(Final_processed_data, here::here("data/processed/final_capstone_data.csv"))


