library(tidyverse)
source("R/ardi_setup.R")

ardi_cod_lookup_tbl = fetch_ardi_cod_tbl() # Retrieve from online / TODO Testing
ardi_cod_lookup_tbl |> print(n=Inf) # 58 causes. ages are inferred.

cod_test_tbl = create_synth_death_data(1000000) |> # Create 1M fake records
  assign_ardi_cods(ardi_cod_lookup_tbl) # Label 1M ICD codes with CoD. Seconds.
cod_test_tbl  

# cod_test_tbl |> arrange(desc(cod_long))
# cod_test_tbl |> count(cod_long)

# FIRST: Join direct AAFs
aaf_direct_tbl = fetch_direct_aaf_tbl() |> select(cod_long, direct_aaf_num = aaf_num)
cod_test_tbl2 = cod_test_tbl |> 
  left_join(aaf_direct_tbl) |> replace_na(list(direct_aaf_num = 0))

# SECOND: Join direct MVC AAFs
mvc_aaf_tbl = readRDS("data/mvc_aaf_tbl.rds") |>   # TODO wrap this in a nicer function perhaps
  filter(state == "North Carolina") |> 
  expand_age_range() |> 
  filter(year == max(year)) |> 
  select(cod_long, age, sex, mvc_aaf_num = aaf_num) # could do year-specific
cod_test_tbl3 = cod_test_tbl2 |> left_join(mvc_aaf_tbl) |> replace_na(list(mvc_aaf_num = 0))

# THIRD: Create and join indirect AAFs from RRs/prevalence
rr_tbl = fetch_rr_wide_tbl() |> pivot_rr_tbl_long()
brfss_alcohol_prev_tbl = readRDS("data/brfss_alcohol_prev_tbl.rds") |> 
  filter(state == "North Carolina") |> 
  filter(year == max(year))
indirect_aaf_tbl = calc_indirect_aaf(rr_tbl, brfss_alcohol_prev_tbl)
indirect_aaf_tbl |> select(cod_long = cause, year, sex, indirect_total_aaf, indirect_excess_aaf)
cod_test_tbl4 = cod_test_tbl3 |> 
  left_join(indirect_aaf_tbl |> 
    filter(year == max(year)) |> 
    select(cod_long = cause, year, sex, indirect_total_aaf, indirect_excess_aaf)) |> 
  replace_na(list(indirect_total_aaf = 0, indirect_excess_aaf = 0))

# Combine all AAFs
cod_test_aafsum_tbl = cod_test_tbl4 |> 
  mutate(aaf_sum = direct_aaf_num + mvc_aaf_num + indirect_total_aaf) |> 
  arrange(desc(aaf_sum))

cod_test_aafsum_tbl |> arrange(desc(indirect_excess_aaf))

#Example aggregate table
cod_test_aafsum_tbl |> 
  group_by(cod_long) |> 
  summarize(
    cods = paste(icd_cod, collapse = ", ") |> str_trunc(width = 20),
    n_records = n(),
    across(aaf_sum, sum)) |> 
  arrange(desc(aaf_sum))
