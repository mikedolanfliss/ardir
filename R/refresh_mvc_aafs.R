# Get state-specific alcohol-attributable fatal MVCs from FARS API
# FARS API details: https://crashviewer.nhtsa.dot.gov/crashviewer/crashapi
# Query builder: https://cdan.dot.gov/query 

library(tidyverse)
library(tictoc)
source("R/ardi_setup.R")
# refresh_fars_person_tbl (optional state)
# prepare_fars_person_tbl(^)
# make_crash_bac_pct_tbl(^)
# make_mvc_aaf_tbl(^)

# API REQUEST ####
# TODO write an API friendly read_csv() that waits a second beteween calls
# TODO save original file to temp directory locally (not github). 
# TODO make this a function, like "refresh_mvc_aaf" with a flexible / hard coded date
# TODO SAVE, SHRINK, and REREAD! if this file is in data, don't redownload?
# Could consider dropping in a .ignore folder.

refresh_fars_person_tbl = function(state = "*"){
  print("Reading all FARS person data from API for all years - can take 10-15m for all US")
  fars_request_tbl = tibble(
    year = 2010:2024 |> as.character(), # TODO flex this date or accept as parameter
    base_request = "https://crashviewer.nhtsa.dot.gov/crashviewer/CrashAPI/FARSData/GetFARSData?dataset=PERSON&FromYear=REQUESTYEAR&ToYear=REQUESTYEAR&state=STATE&format=csv") |> 
    mutate(request = base_request |> 
      str_replace_all("REQUESTYEAR", year) |> 
      str_replace_all("STATE", state)) |>  
    mutate(data = request |> map(\(x){read_csv(x, col_types = cols(.default = "c"))}))
  fars_request_tbl = fars_request_tbl |> 
    unnest(data) |> 
    mutate(across(c(caseyear, st_case, age, alc_res), as.integer))
  return(fars_request_tbl)
}
# save the API return in temp as an RDS, then delete it?

# CREATE & RECODE PERSON TABLE ####
# Can save intermediate tables to data/ here.
prepare_fars_person_tbl = function(full_fars_person_tbl){
  # Shrink
  fars_person_sm_tbl = full_fars_person_tbl |> 
    select(year = caseyear, state = statename, st_case, # Crash info
      age, agename, sex = sexname, # Demographics
      inj_kabco = inj_sevname, # Severity
      alc_bac_result = alc_res, alc_bac_text = alc_resname)# Alcohol and BAC info
  # Recode ####
  fars_person_sm_tbl = fars_person_sm_tbl |> 
    filter(sex %in% c("Male", "Female"), !is.na(sex), age < 120, !is.na(age)) |> 
    mutate(sex = sex |> str_to_lower() |> as_factor()) |> 
    mutate(alc_bac_result = alc_bac_text |> str_extract("^[0-9\\.]+ ") |> as.numeric()) |> # Overwrite built in number variable
    mutate(alc_bac_result = case_when(
      alc_bac_text |> str_detect("% BAC|or Greater") ~ alc_bac_result, # Could also regex this, something like: "0.0[89]|0.[12345]"
      T ~  NA_integer_))  |> 
    mutate(direct_alc_attributable = case_when( # ID >=0.08 BAC CRASHES ####
        alc_bac_result >= 0.08 ~ TRUE, # "Directly Alcohol Attributable"
        T ~ FALSE)) # "Other / Mixed Cause"

  fars_crashbac_tbl = fars_person_sm_tbl |>
    group_by(year, state, st_case) |> 
    summarize(crash_direct_alc_attributable = any(direct_alc_attributable))

  fars_person_sm_tbl = fars_person_sm_tbl |> 
    left_join(fars_crashbac_tbl) # join was crash alcohol attributable (>0.08 BAC)
  # TODO question - drop missing age -> missing age group?
}
# Hand check the BAC vars reveals issue: 0.84 and 0.084 both get "84" in number var.
# fars_person_sm_tbl |> count(alc_bac_result, alc_bac_text) |> arrange(desc(alc_bac_result)) |> View() # Could save and quarto a table like this. 

# Create & rejoin crash BAC % table ####

# fars_person_sm_tbl |> filter(st_case |> str_detect("^1000[123]$")) |> arrange(st_case)

fars_person_sm_tbl |> saveRDS("data/fars_person_sm_tbl.rds")

make_crash_bac_pct_tbl = function(fars_person_sm_tbl){
  fars_bac_pct_tbl = fars_person_sm_tbl |>   
    filter(inj_kabco |> str_detect("(K)")) |> 
    mutate(age_range = age |> recode_mvc_age_group()) |> 
    group_by(year, state, age_range, sex) |> 
    count(direct_alc_attributable, .drop = FALSE)  |> ungroup() |> 
    arrange(year, state, age_range, sex) |> 
    complete(year, state, age_range, sex, direct_alc_attributable, fill = list(n = 0))
  return(fars_bac_pct_tbl)
}

make_mvc_aaf_tbl = function(fars_bac_pct_tbl){
  mvc_aaf_tbl = fars_bac_pct_tbl |> 
    left_join( # Join total counts
      fars_bac_pct_tbl |> 
        group_by(year, state, age_range, sex) |> 
        summarize(total_n = sum(n)) ) |> 
    filter(direct_alc_attributable) |> 
    select(-direct_alc_attributable) |> 
    rename(n_alc_att = n) |> 
    mutate(aaf_num = n_alc_att / total_n) |> 
    mutate(cod_long = "Motor-vehicle traffic crashes")
  return(mvc_aaf_tbl)
}
# 

# full_fars_person_tbl = refresh_fars_person_tbl() # fars_request_tbl = refresh_fars_person_tbl(state = "North%20Carolina") # Doesn't work yet
# full_fars_person_tbl |> saveRDS("temp/fars_request_tbl.rds") # 86 megs, 1.3M rows x 162 variables
full_fars_person_tbl = readRDS("temp/fars_request_tbl.rds") # 86 megs, 1.3M rows x 162 variables. 10s to read
fars_person_sm_tbl = prepare_fars_person_tbl(full_fars_person_tbl)
# Save & pickup analysis here (TODO split these tasks into different functions)
# fars_person_sm_tbl |> saveRDS("data/fars_person_sm_tbl_orig.rds")
# fars_person_sm_tbl = readRDS("data/fars_person_sm_tbl_orig.rds")
fars_bac_pct_tbl = make_crash_bac_pct_tbl(fars_person_sm_tbl)
mvc_aaf_tbl = make_mvc_aaf_tbl(fars_bac_pct_tbl)
mvc_aaf_tbl |> saveRDS("data/mvc_aaf_tbl.rds")
mvc_aaf_tbl |> saveRDS("data/mvc_aaf_tbl.csv")

# TODO subfolder under data (FARS, BRFSS, etc.
# TODO calculate US-wide? Or just join into national data allowing state variation
# TODO project today()'s yearly AAFs by duplicating prior year or a simple linear / loess type projection?