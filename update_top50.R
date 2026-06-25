# ------------------------------------------------------------------
# Update top-50 aggregate tables for year window: 2019 + 2021-2024
# (excludes 2017, 2018, 2020)
#
# Regenerates, faithfully reusing mastercode.R methodology, only the
# year set changes:
#   data/master_fatality_50.csv
#   data/master_fatality.csv                       (full table, source of top30)
#   Additional_plot_August2025/master_fatality_cbsa50.csv  (mirror copy)
#   Additional_plot_August2025/youth_msa_county_wide_modified.csv
#
# Reuses existing data/mastersheetFARS.csv (no duckdb rebuild needed).
# Population pulled live from ACS1 via tidycensus (needs CENSUS_API_KEY).
# ------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidycensus)
  library(tidyverse)
  library(readxl)
})

yrs <- c(2019, 2021, 2022, 2023, 2024)
cat("Year window:", paste(yrs, collapse = ", "), "\n")

# ====================== Part A: master_fatality_50 =================

master <- read.csv("data/mastersheetFARS.csv") %>%
  filter(YEAR %in% yrs)
master <- master %>%
  mutate(county_code = sprintf("%02d%03d", STATE, COUNTY))

children_f <- master %>%
  filter(AGE_CATEGORY < 5)          # under 18

master <- master %>%
  group_by(county_code) %>%
  summarise(fatality_count = sum(fatality_count))

children_f <- children_f %>%
  group_by(county_code) %>%
  summarise(fatality_count = sum(fatality_count))

# aggregate fatality number by MSA
msa <- read_excel("data/cbsa_list.xlsx") %>%
  filter(`Metropolitan/Micropolitan Statistical Area` == "Metropolitan Statistical Area")

msa$`FIPS State Code`  <- as.numeric(msa$`FIPS State Code`)
msa$`FIPS County Code` <- as.numeric(msa$`FIPS County Code`)
msa$GEOID <- sprintf("%02d%03d", msa$`FIPS State Code`, msa$`FIPS County Code`)

msa <- msa %>%
  rename("name" = `CBSA Title`, "cbsa_code" = `CBSA Code`) %>%
  select(name, cbsa_code, GEOID)

msa_fatality <- master %>%
  left_join(msa, by = c("county_code" = "GEOID")) %>%
  filter(!is.na(cbsa_code)) %>%
  group_by(cbsa_code, name) %>%
  summarise(fatality_count = sum(fatality_count), .groups = "drop") %>%
  rename("fatality_overall" = "fatality_count")

msa_fatality_children <- children_f %>%
  left_join(msa, by = c("county_code" = "GEOID")) %>%
  filter(!is.na(cbsa_code)) %>%
  group_by(cbsa_code, name) %>%
  summarise(fatality_count = sum(fatality_count), .groups = "drop") %>%
  rename("fatality_children" = "fatality_count")

master_fatality <- left_join(msa_fatality, msa_fatality_children, by = c("cbsa_code" = "cbsa_code")) %>%
  mutate(fatality_children = ifelse(is.na(fatality_children), 0, fatality_children)) %>%
  select(cbsa_code, name.x, fatality_overall, fatality_children) %>%
  rename("name" = "name.x")

# ---- total population (B01001_001), summed across years ----
get_tot <- function(year) {
  get_acs(
    geography = "Metropolitan Statistical Area/Micropolitan Statistical Area",
    variables = c(tot_pop = "B01001_001"),
    year = year, survey = "acs1"
  )
}
pop <- bind_rows(lapply(yrs, get_tot)) %>%
  group_by(GEOID) %>%
  summarise(tot_pop = sum(estimate))

master_fatality <- left_join(master_fatality, pop, by = c("cbsa_code" = "GEOID")) %>%
  mutate(rate_overall = fatality_overall / tot_pop * 100000) %>%
  filter(!is.na(rate_overall))

# ---- children population under 18 (B09001_001), summed across years ----
get_child <- function(year) {
  get_acs(
    geography = "Metropolitan Statistical Area/Micropolitan Statistical Area",
    variables = "B09001_001E",
    year = year, survey = "acs1"
  )
}
children_pop <- bind_rows(lapply(yrs, get_child)) %>%
  group_by(GEOID) %>%
  summarise(child_pop = sum(estimate)) %>%
  filter(GEOID %in% master_fatality$cbsa_code)

master_fatality <- left_join(master_fatality, children_pop, by = c("cbsa_code" = "GEOID")) %>%
  mutate(rate_children = fatality_children / child_pop * 100000)

master_fatality_50 <- master_fatality %>%
  arrange(desc(tot_pop)) %>%
  head(50)

write.csv(master_fatality_50, "data/master_fatality_50.csv")
write.csv(master_fatality,    "data/master_fatality.csv")
write.csv(master_fatality_50, "Additional_plot_August2025/master_fatality_cbsa50.csv")
cat("Wrote master_fatality_50.csv (", nrow(master_fatality_50), "rows ) and master_fatality.csv\n")

# ====================== Part B: youth_msa_county_wide ==============

master_county <- read.csv("data/mastersheetFARS.csv") %>%
  filter(YEAR %in% yrs)
fatality_county <- master_county %>%
  mutate(GEOID = sprintf("%02d%03d", STATE, COUNTY)) %>%
  mutate(FIPS.State.Code  = substr(GEOID, 1, nchar(GEOID) - 3),
         FIPS.County.Code = substr(GEOID, 3, nchar(GEOID))) %>%
  select(-X)

county_list <- read.csv("./data/metro_fips_codes.csv") %>%
  mutate(GEOID = str_pad(FIPS.Code, 5, pad = "0")) %>%
  mutate(FIPS.State.Code  = substr(GEOID, 1, nchar(GEOID) - 3),
         FIPS.County.Code = substr(GEOID, 3, nchar(GEOID)))

county_fatality <- fatality_county %>%
  filter(GEOID %in% msa$GEOID)

# county total population (B01003_001), kept per-year (wide)
get_county_pop <- function(year) {
  get_acs(
    geography = "county",
    variables = "B01003_001",
    year = year, survey = "acs1",
    output = "wide"
  ) %>%
    mutate(year = year)
}
pop <- bind_rows(lapply(yrs, get_county_pop)) %>%
  rename(pop = "B01003_001E") %>%
  select(NAME, GEOID, pop, year) %>%
  mutate(GEOID = str_pad(GEOID, 5, pad = "0")) %>%
  mutate(FIPS.State.Code  = substr(GEOID, 1, nchar(GEOID) - 3),
         FIPS.County.Code = substr(GEOID, 3, nchar(GEOID)))

# rate = mean of yearly (fatalities / pop * 100000); youth = under 20 (<6)
youth_tot_county_wide <- county_fatality %>%
  left_join(pop, by = c("GEOID", "YEAR" = "year")) %>%
  filter(AGE_CATEGORY < 6) %>%
  group_by(GEOID, YEAR, NAME) %>%
  summarise(fatsum = sum(fatality_count), pop = first(pop), .groups = "drop") %>%
  group_by(GEOID, NAME) %>%
  summarise(rate_youth = mean(fatsum / pop * 100000), .groups = "drop") %>%
  left_join(
    county_fatality %>%
      left_join(pop, by = c("GEOID", "YEAR" = "year")) %>%
      group_by(GEOID, YEAR, NAME) %>%
      summarise(fatsum = sum(fatality_count), pop = first(pop), .groups = "drop") %>%
      group_by(GEOID, NAME) %>%
      summarise(rate_total = mean(fatsum / pop * 100000), .groups = "drop"),
    by = c("GEOID", "NAME")
  ) %>%
  mutate(Main = ifelse(GEOID %in% county_list$GEOID, "Main", "Adjacent")) %>%
  drop_na()

youth_tot_county_wide$NAME <- gsub("(?i)county", "", youth_tot_county_wide$NAME, perl = TRUE)

youth_msa_county_wide <- youth_tot_county_wide %>%
  left_join(msa) %>%
  rename(CBSA = name, County = NAME) %>%
  filter(CBSA %in% master_fatality_50$name) %>%
  drop_na()

write.csv(youth_msa_county_wide,
          "Additional_plot_August2025/youth_msa_county_wide_modified.csv",
          row.names = FALSE)
cat("Wrote youth_msa_county_wide_modified.csv (", nrow(youth_msa_county_wide), "rows )\n")
cat("DONE\n")
