library(tidyverse)
library(readxl)
library(purrr)

# TABLE RECODE FUNCTIONS ####
## Helper function, leverage vec_case_when ####
recode_values_regex = function(x, from_regex, to){
  # Extend recode_values() function to accept regular expressions
 # Thanks to this dplyr issue submitted by MDF: https://github.com/tidyverse/dplyr/issues/7846
  vctrs::vec_case_when(
    conditions = purrr::map(from_regex, function(from_regex) str_detect(x, from_regex)),
    values = as.list(to)
  )
}
 
## Main function ####
recode_values_regex_tbl = function(x, regex_tbl){
  # Wrap recode_values_regex to accept a table directly
  tbl_to_return = tibble(x) |>
    mutate(label = x |>
      recode_values_regex(from_regex = regex_tbl$regex, to = regex_tbl$label))
  return(tbl_to_return |> pull(label))
}

# Get and clean up ARDI COD tbl ####
get_ardi_cod_tbl = function(){
  ardi_case_tbl = read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQIiNisnSOi-NJbf8ZP9BnaF_p5en2yiTsSc-UNmYrwpN9vw-eMIAfu7ix_zD915jMdYDCgVmPIgZse/pub?gid=1878924397&single=true&output=csv")
  ardi_case_tbl = ardi_case_tbl |> 
    mutate(age_range = case_when(
      is.na(age_range) | age_range == "" | age_range == "*" ~ "1-115",
      T ~ age_range
    )) |> 
    mutate(across(sex_list, str_to_lower)) |> 
    mutate(sex_list = case_when(
      is.na(sex_list) | sex_list == "" ~ "male|female",
    T ~ sex_list)
    )
  # TODO clean ardi_case_tbl here. 
  # TODO fill age range
  return(ardi_case_tbl)
}
# tidy_ardi_cod_tbl = get_ardi_cod_tbl() |> 
#   expand_age_range() |> 
#   expand_sex_list()

 