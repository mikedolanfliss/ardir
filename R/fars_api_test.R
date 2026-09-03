# FARS API details: https://crashviewer.nhtsa.dot.gov/crashviewer/crashapi
# Query builder: https://cdan.dot.gov/query 

library(tidyverse)

# Injury severity table ####
# "https://crashviewer.nhtsa.dot.gov/crashviewer/CrashAPI/analytics/GetInjurySeverityCounts?fromCaseYear=2014&toCaseYear=2015&state=1&format=csv" |> read_csv()

# Summary counts ####
# "https://crashviewer.nhtsa.dot.gov/crashviewer/CrashAPI/analytics/GetInjurySeverityCounts?state=1&fromCaseYear=2014&toCaseYear=2018&state=1&format=csv" |> read_csv()

# Raw FARS data by table ####
# fars_tbl = "https://crashviewer.nhtsa.dot.gov/crashviewer/CrashAPI/FARSData/GetFARSData?dataset=Accident&FromYear=2014&ToYear=2016&state=1&format=csv" |> read_csv() # Example 1
# "https://crashviewer.nhtsa.dot.gov/crashviewer/CrashAPI/FARSData/GetFARSData?dataset=DRIMPAIR&FromYear=2014&ToYear=2016&state=1&format=csv" |> read_csv() # Example 2
# Tables include: ACCIDENT, CEVENT, CRASHRF (2020 Onwards), DAMAGE, DISTRACT, DRIMPAIR, DRIVERRF (2020 Onwards), DRUGS, FACTOR, MANEUVER, NMCRASH, NMDISTRACT (2019 Onwards), NMIMPAIR, NMPRIOR, PARKWORK, PBTYPE, PERSON, PERSONRF (2020 Onwards), PVEHICLESF (2020 Onwards), RACE, SAFETYEQ, TRAILERVINDERIVED, VEHICLE, VEHICLESF (2020 Onwards), VEVENT, VINDECODE, VINDERIVED, VIOLATION, VISION, VSOE, WEATHER (2020 Onwards)
# state=* DOES work to bring in all state data. Must take care to ask for one year at a time, avoid hammering

# Test extract ####
fars_data_base_request = "https://crashviewer.nhtsa.dot.gov/crashviewer/CrashAPI/FARSData/GetFARSData?dataset=DATASET_HERE&FromYear=2014&ToYear=2014&state=1&format=csv"
fars_test_tbl = tibble(dataset = c("ACCIDENT", "DRUGS", "DRIMPAIR", "PERSON", "FACTOR")) |> # Get many tables at once
  mutate(request = fars_data_base_request |> str_replace("DATASET_HERE", dataset)) |> 
  mutate(data = request |> map(read_csv)) # TODO wrap this read_csv() call with a 1 second wait to be nice to API.

fars_test_tbl
fars_test_tbl |> pull(dataset)
fars_test_tbl$data |> map(head)
fars_test_tbl |> filter(dataset == "DRIMPAIR") |> pull(data)
fars_test_tbl |> filter(dataset == "DRUGS") |> pull(data) |> head()

## FULL Person Table ####
fars_person_tbl = fars_test_tbl |> filter(dataset == "PERSON") |> unnest(data)
fars_person_sm_tbl = fars_person_tbl |> select(matches("st_case|statename|caseyear|age|alc|sex|harm|sev")) # seems like what we need?
# age, sexname, alc_res\name, 

# Promote >0.08 BAC to a crash attribute
fars_crashbac_tbl = fars_person_sm_tbl |> 
  mutate(direct_alc_attributable = case_when(
      alc_res > 99 ~ NA_integer_, # Could also regex this, something like: "0.0[89]|0.[12345]"
      alc_res > 8 ~ TRUE, # "Directly Alcohol Attributable"
      T ~ FALSE)) |> # "Other / Mixed Cause"
  group_by(st_case) |> 
  summarize(direct_alc_attributable = any(direct_alc_attributable))

get_mvc_age_group = function(x){  
  age_tbl = tribble(
    ~age, ~age_group,
    0:14, "0-14",
    15:19, "15–19",
    20:24, "20–24",
    25:34, "25–34",    
    35:44, "35–44",    
    45:54, "45-54",     
    55:64, "55-64",     
    65:120, "65+",
    NA_integer_, NA_character_,
  ) |> unnest(age) |> 
    mutate(across(age_group, as_factor))
  
  tbl_to_return = tibble(age = x) |> left_join(age_tbl)
  # TODO if (verbose)... can print the above table
  return(tbl_to_return |> pull(age_group))
}
# tibble(x = sample(1:100, 10)) |> mutate(age_group = x |> get_mvc_age_group())

## Create person BAC % table ####
fars_bac_pct_tbl = fars_person_sm_tbl |> 
  left_join(fars_crashbac_tbl) |> # join was crash alcohol attributable (>0.08 BAC)
  filter(inj_sevname |> str_detect("(K)")) |> 
  mutate(age_group = age |> get_mvc_age_group()) |> 
  group_by(year = caseyear, state = statename, age_group, sex = sexname) |> 
  count(direct_alc_attributable, .drop = FALSE)  |> ungroup() |> 
  arrange(year, state, age_group, sex) |> 
  complete(year, state, age_group, sex, direct_alc_attributable, fill = list(n = 0))

fars_bac_pct_tbl = fars_bac_pct_tbl |> 
  left_join(
    fars_bac_pct_tbl |> 
      group_by(year, state, age_group, sex) |> 
      summarize(total_n = sum(n)) ) |> 
  filter(direct_alc_attributable) |> 
  mutate(pct_alc_attributable = n / total_n)

fars_bac_pct_tbl |> saveRDS("data/fars_bac_pct_tbl.rds")
# TODO question - drop missing age -> missing age group?
