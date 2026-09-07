# TODO across the board check naming conventions - ardi_cod_long is probably ideal, for example

workbook_url = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQIiNisnSOi-NJbf8ZP9BnaF_p5en2yiTsSc-UNmYrwpN9vw-eMIAfu7ix_zD915jMdYDCgVmPIgZse/pub?gid={sheetid}&single=true&output=csv"

# GENERIC HELPFUL FUNCTIONS ####
expand_age_range = function(this_tbl){ # TODO make this lazy eval
  # TODO check that there's a variable called age rather than assume
  # TODO / FYI - currently must be contiguous age ranges. If NA, could expand from 0-120 for a left_join.
  return_tbl = this_tbl |> 
    #separate_wider_delim(age_range, delim = "[:\\|]", names = c("age_range_low", "age_range_high")) |> 
    mutate(across(age_range, str_squish)) |> 
    mutate(across(age_range, \(x){x |> str_replace("\\+", "-115")})) |> 
    mutate(age_range_low = age_range |> str_extract("^[0-9]+")) |> # ^ More robust than separate wider
    mutate(age_range_high = age_range |> str_extract("[0-9]+$")) |>
    mutate(age_list = map2(age_range_low, age_range_high, \(l, h){l:h})) |> 
    select(-age_range_low, -age_range_high) |> 
    unnest(age_list) |> 
    rename(age = age_list)
  return(return_tbl)
}

expand_sex_list = function(this_tbl){
  return_tbl = this_tbl |> 
    mutate(sex_list = sex_list |> str_split("\\|")) |>
    unnest(sex_list) |> 
    rename(sex = sex_list)    
}

## Expand ARDI COD for left_joining 
expand_sex_and_age = function(this_tbl){
  # Expand the ardi case table into row-specifics for cod_long, 
  tbl_to_return = this_tbl |> 
    expand_age_range() |> 
    expand_sex_list()    
  return(this_tbl)
}

# COD HELPFUL FUNCTIONS ####

## Create sythetic death records for testing 
create_synth_death_data = function(n_records = 1000){
  tbl_to_return = tibble(
    age = sample(1:115, n_records, replace = T),
    sex = sample(c("male", "female"), n_records, replace = T),
    icd_cod = paste0(
      sample(LETTERS, n_records, replace = T),
      sample(0:9, n_records, replace = T),
      sample(0:9, n_records, replace = T),
      sample(c(0:9, rep("", 3)), n_records, replace = T))
  ) 
  return(tbl_to_return)
}
# create_synth_death_data(10) # Example

## Helper function, leverage vec_case_when 
recode_values_regex = function(x, from_regex, to){
  # Extend recode_values() function to accept regular expressions
 # Thanks to this dplyr issue submitted by MDF: https://github.com/tidyverse/dplyr/issues/7846
  vctrs::vec_case_when(
    conditions = purrr::map(from_regex, function(from_regex) str_detect(x, from_regex)),
    values = as.list(to)
  )
}
 
## Main function 
recode_values_regex_tbl = function(x, regex_tbl){
  # Wrap recode_values_regex to accept a table directly
  tbl_to_return = tibble(x) |>
    mutate(label = x |>
      recode_values_regex(from_regex = regex_tbl$regex, to = regex_tbl$label))
  return(tbl_to_return |> pull(label))
}

## Clean ARDI cod table (or your own) 
clean_cod_tbl = function(this_tbl){
  # if age_range exists, if sex_list exists, etc.
  tbl_to_return = this_tbl |>     
    mutate(age_range = case_when(
      is.na(age_range) | age_range == "" | age_range == "*" ~ "1-115",
      T ~ age_range
    )) |> 
    mutate(across(sex_list, str_to_lower)) |> 
    mutate(sex_list = case_when(
      is.na(sex_list) | sex_list == "" ~ "male|female",
    T ~ sex_list)
    )
}

## Get ARDI COD table from online 
fetch_ardi_cod_tbl = function(){
  print("Reading ARDI COD tbl from google doc location; typically very fast")
  ardi_case_tbl = read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQIiNisnSOi-NJbf8ZP9BnaF_p5en2yiTsSc-UNmYrwpN9vw-eMIAfu7ix_zD915jMdYDCgVmPIgZse/pub?gid=1878924397&single=true&output=csv")
  ardi_case_tbl = ardi_case_tbl |> clean_cod_tbl()
  # TODO clean ardi_case_tbl here. 
  # TODO fill age range
  return(ardi_case_tbl)
}

# tidy_ardi_cod_tbl = get_ardi_cod_tbl() |> 
#   expand_age_range() |> 
#   expand_sex_list()

assign_ardi_cods = function(cod_tbl, ardi_cod_lookup_tbl){
  regex_lookup_tbl = ardi_cod_lookup_tbl |> select(regex = icd10_code_regex, label = cod_long)
  tidy_lookup_tbl = ardi_cod_lookup_tbl |>
    select(cod_long, sex_list, age_range) |> 
    expand_sex_and_age() |> 
    mutate(case_def_eligible = T)

  tbl_to_return = cod_tbl |> 
    mutate(cod_long = icd_cod |> recode_values_regex_tbl(lookup_tbl)) |> # classify ARDI CODs)
    left_join(tidy_lookup_tbl) |> # left_join for eligibility |> 
    mutate(cod_long = case_when(
      is.na(case_def_eligible) ~ "",
      case_def_eligible ~ cod_long)) |> 
    select(-case_def_eligible, -sex_list, -age_range)
}

# MVC / FARS FUNCTIONS ####
recode_mvc_age_group = function(x){  
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
  ) |> unnest(age)
  
  tbl_to_return = tibble(age = x) |> left_join(age_tbl)
  # TODO if (verbose)... can print the above table
  return(tbl_to_return |> pull(age_group))
}
# tibble(x = sample(1:100, 10)) |> mutate(age_group = x |> get_mvc_age_group())

fetch_direct_aaf_tbl = function(){
  tbl_to_return = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQIiNisnSOi-NJbf8ZP9BnaF_p5en2yiTsSc-UNmYrwpN9vw-eMIAfu7ix_zD915jMdYDCgVmPIgZse/pub?gid=354728235&single=true&output=csv" |> read_csv()
  return(tbl_to_return)
}
# expand_aaf_tbl = function(this_aaf_tbl){
#   if(!("age" %in% names(this_aaf_tbl))){
#     this_aaf_tbl = this_aaf_tbl |> 
#       mutate()
#   }
# }

# PREVALENCE FUNCTIONS ####
calc_indirect_aaf = function(prev_tbl, rr_tbl){
  # TODO test/kickback if expected variables aren't prepped
  aaf_indirect_tbl = prev_tbl |> 
    full_join(rr_tbl) |> # Join RRs in
    mutate( # calculate total alcohol and excessive alcohol attributable fractions
      total_product_sum = prev_low*(RR_low-1) + prev_high*(RR_high-1) + prev_high*(RR_high-1),
      indirect_total_aaf = total_product_sum / (1 + total_product_sum),
      RS2 = RR_medium / RR_low, 
      RS3 = RR_high / RR_low,
      excess_product_sum = prev_medium*(RS2 - 1) + prev_high*(RS3 - 1),
      indirect_excess_aaf = excess_product_sum / (1 + excess_product_sum))
  return(aaf_indirect_tbl)
}
#TODO ^ this is odd w protective RRs.

fetch_rr_wide_tbl = function(){
  rr_wide_tbl = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQIiNisnSOi-NJbf8ZP9BnaF_p5en2yiTsSc-UNmYrwpN9vw-eMIAfu7ix_zD915jMdYDCgVmPIgZse/pub?gid=2127475538&single=true&output=csv" |> read_csv()  
  return(rr_wide_tbl)
}
pivot_rr_tbl_long = function(rr_wide_tbl){
  rr_tbl = rr_wide_tbl |> 
    select(matches("cause|male|female|prev_type")) |> 
    pivot_longer(cols = matches("female|male"), values_to = "RR", names_to = "consumption_sex") |> 
    mutate(consumption_sex = consumption_sex |> str_remove("^RR_")) |> 
    separate(consumption_sex, into = c("consumption", "sex")) |> 
    pivot_wider(names_from = c("consumption"), values_from = "RR", names_prefix = "RR_")
  return(rr_tbl)
}
# DATA VALIDATION TESTS ####