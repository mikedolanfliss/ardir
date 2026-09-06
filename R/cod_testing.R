library(tidyverse)

source("R/ardi_setup.R")

ardi_cod_lookup_tbl = fetch_ardi_cod_tbl() # Retrieve from online / TODO Testing

cod_test_tbl = create_synth_death_data(1000000) |> # Create 1M fake records
  assign_ardi_cods(ardi_cod_lookup_tbl) # Label 1M ICD codes. Seconds.
  
cod_test_tbl |> arrange(desc(cod_long))
cod_test_tbl |> count(cod_long)

# TODO check naming - ardi_cod_long, for example

# Join direct AAFs
aaf_direct_tbl = fetch_direct_aaf_tbl() |> select(cod_long, direct_aaf_num = aaf_num)
cod_test_tbl2 = cod_test_tbl |> left_join(aaf_direct_tbl) |> replace_na(list(direct_aaf_num = 0))

# Join direct MVC AAFs
mvc_aaf_tbl = readRDS("data/mvc_aaf_tbl.rds") |>   # TODO wrap in a nicer function
  filter(state == "North Carolina") |> 
  expand_age_range() |> 
  filter(year == max(year)) |> 
  select(cod_long, age, sex, mvc_aaf_num = aaf_num) # could do year-specific
cod_test_tbl3 = cod_test_tbl2 |> left_join(mvc_aaf_tbl) |> replace_na(list(mvc_aaf_num = 0))

cod_test_aafsum_tbl = cod_test_tbl3 |> 
  mutate(aaf_sum = direct_aaf_num + mvc_aaf_num) |> 
  arrange(desc(aaf_sum))
  # mutate(aaf_sum = direct_aaf_num + mvc_aaf_num)
cod_test_aafsum_tbl |> 
  group_by(cod_long) |> 
  summarize(across(aaf_sum, sum)) |> 
  arrange(desc(aaf_sum))
