# ──────────────────────────────────────────────────────────────────────
# 01_build_data.R
# Build yearly fatality-rate panels (2015–2024) for the 10 transit MSAs
# and their 10 central counties. Counts are pulled from data/raw_data.duckdb
# (FARS person tables), population from ACS via tidycensus.
#
# Outputs (written next to this script):
#   msa_fatality_2015_2024.csv
#   county_fatality_2015_2024.csv
# ──────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(tidyverse)
  library(duckdb)
  library(DBI)
  library(tidycensus)
})

here <- "top_10 transit_city"
db_path <- "data/raw_data.duckdb"

# ── Targets ───────────────────────────────────────────────────────────
msa_targets <- tribble(
  ~cbsa_code, ~name,
  "35620", "New York-Newark-Jersey City, NY-NJ",
  "31080", "Los Angeles-Long Beach-Anaheim, CA",
  "16980", "Chicago-Naperville-Elgin, IL-IN",
  "47900", "Washington-Arlington-Alexandria, DC-VA-MD-WV",
  "37980", "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD",
  "14460", "Boston-Cambridge-Newton, MA-NH",
  "41860", "San Francisco-Oakland-Fremont, CA",
  "42660", "Seattle-Tacoma-Bellevue, WA",
  "12580", "Baltimore-Columbia-Towson, MD",
  "38300", "Pittsburgh, PA"
)

county_targets <- tribble(
  ~county_code, ~name,
  "36061", "New York County, NY",
  "06037", "Los Angeles County, CA",
  "17031", "Cook County, IL",
  "11001", "District of Columbia",
  "42101", "Philadelphia County, PA",
  "25025", "Suffolk County, MA",
  "06075", "San Francisco County, CA",
  "53033", "King County, WA",
  "24510", "Baltimore City, MD",
  "42003", "Allegheny County, PA"
)

years <- 2015:2024

# ── 1. Yearly fatality counts by county from FARS (INJ_SEV = 4) ───────
con <- dbConnect(duckdb(), dbdir = db_path, read_only = TRUE)

union_sql <- paste(
  sprintf(
    "SELECT %d AS YEAR, STATE, COUNTY, INJ_SEV FROM person%d",
    years, years
  ),
  collapse = "\n  UNION ALL\n  "
)

q <- sprintf("
  SELECT YEAR,
         printf('%%02d%%03d', STATE, COUNTY) AS county_code,
         COUNT(*) AS fatality_count
  FROM (
    %s
  ) t
  WHERE INJ_SEV = 4
  GROUP BY YEAR, county_code
", union_sql)

county_year <- dbGetQuery(con, q) |> as_tibble()
dbDisconnect(con, shutdown = TRUE)

# ── 2. Aggregate county counts to the 10 MSAs ─────────────────────────
suppressPackageStartupMessages(library(readxl))
cbsa_xwalk <- read_excel("data/cbsa_list.xlsx") |>
  filter(`Metropolitan/Micropolitan Statistical Area` == "Metropolitan Statistical Area") |>
  mutate(
    GEOID = sprintf(
      "%02d%03d",
      as.integer(`FIPS State Code`),
      as.integer(`FIPS County Code`)
    )
  ) |>
  select(cbsa_code = `CBSA Code`, GEOID) |>
  filter(cbsa_code %in% msa_targets$cbsa_code)

msa_counts <- county_year |>
  inner_join(cbsa_xwalk, by = c("county_code" = "GEOID")) |>
  group_by(cbsa_code, YEAR) |>
  summarise(fatality_count = sum(fatality_count), .groups = "drop") |>
  rename(year = YEAR)

# ── 3. ACS populations ─────────────────────────────────────────────────
# ACS 1-year is suspended for 2020; use ACS 5-year (2016–2020) as proxy.

get_pop_msa <- function(yr) {
  if (yr == 2020) {
    message("MSA pop: 2020 (decennial P1)")
    return(
      get_decennial(
        geography = "Metropolitan Statistical Area/Micropolitan Statistical Area",
        variables = "P1_001N",
        year      = 2020,
        sumfile   = "pl"
      ) |>
        transmute(cbsa_code = GEOID, year = 2020, population = value)
    )
  }
  message("MSA pop: ", yr, " (acs1)")
  get_acs(
    geography = "Metropolitan Statistical Area/Micropolitan Statistical Area",
    variables = "B01001_001",
    year      = yr,
    survey    = "acs1"
  ) |>
    transmute(cbsa_code = GEOID, year = yr, population = estimate)
}

get_pop_county <- function(yr) {
  if (yr == 2020) {
    message("County pop: 2020 (decennial P1)")
    return(
      get_decennial(
        geography = "county",
        variables = "P1_001N",
        year      = 2020,
        sumfile   = "pl"
      ) |>
        transmute(county_code = GEOID, year = 2020, population = value)
    )
  }
  message("County pop: ", yr, " (acs1)")
  get_acs(
    geography = "county",
    variables = "B01003_001",
    year      = yr,
    survey    = "acs1"
  ) |>
    transmute(county_code = GEOID, year = yr, population = estimate)
}

msa_pop    <- bind_rows(lapply(years, get_pop_msa)) |>
  filter(cbsa_code %in% msa_targets$cbsa_code)

county_pop <- bind_rows(lapply(years, get_pop_county)) |>
  filter(county_code %in% county_targets$county_code)

# ── 4. Build final panels and write CSVs ──────────────────────────────
msa_panel <- msa_targets |>
  crossing(year = years) |>
  left_join(msa_counts, by = c("cbsa_code", "year")) |>
  left_join(msa_pop,    by = c("cbsa_code", "year")) |>
  mutate(
    fatality_count = replace_na(fatality_count, 0L),
    rate = fatality_count / population * 1e5
  )

county_panel <- county_targets |>
  crossing(year = years) |>
  left_join(county_year |> rename(year = YEAR),
            by = c("county_code", "year")) |>
  left_join(county_pop, by = c("county_code", "year")) |>
  mutate(
    fatality_count = replace_na(fatality_count, 0L),
    rate = fatality_count / population * 1e5
  )

write_csv(msa_panel,    file.path(here, "msa_fatality_2015_2024.csv"))
write_csv(county_panel, file.path(here, "county_fatality_2015_2024.csv"))

cat("\nMSA panel:\n");    print(msa_panel    |> arrange(name, year), n = 20)
cat("\nCounty panel:\n"); print(county_panel |> arrange(name, year), n = 20)
