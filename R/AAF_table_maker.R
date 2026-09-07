# AAF reader
library(tidyverse)
# library(validate) # for table integrity testing TODO
# Should some of this be git-ignored?


# Cause table ####
workbook_url = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQIiNisnSOi-NJbf8ZP9BnaF_p5en2yiTsSc-UNmYrwpN9vw-eMIAfu7ix_zD915jMdYDCgVmPIgZse/pub?gid={sheetid}&single=true&output=csv"
cause_tbl = workbook_url |> str_glue(sheetid = 1878924397) |> read_csv()
aaf_direct_tbl = workbook_url |> str_glue(sheetid = 354728235) |> read_csv()

# RR table ####

# tidy-long for now, may want to split by sex only and go wide with consumption
rr_tbl = fetch_rr_wide_tbl() |> pivot_rr_tbl_long()

# Prev ####
prev_tbl_wide = workbook_url |> str_glue(sheetid = 61349445) |> read_csv()
prev_tbl = prev_tbl_wide |> 
  pivot_longer(cols = matches("year_"), names_to = "year", values_to = "prev") |> 
  mutate(year = year |> str_remove("year_") |> as.integer()) |> 
  pivot_wider(names_from = c("consumption"), values_from = "prev", names_prefix = "prev_")
prev_tbl

# AAF indirect table ####
aaf_indirect_tbl = calc_indirect_aaf(prev_tbl, rr_tbl)
  
# FARS BAC % table ####
# %alcohol attributable (>=.08 BAC by anyone in crash) by year, state, age_group, sex
mvc_aaf_tbl = readRDS("data/mvc_aaf_tbl.RDS")
mvc_aaf_tidy_tbl = mvc_aaf_tbl |> 
  mutate(ardi_cod_long = "Motor-vehicle traffic crashes") |> 
  expand_age_range()

# IDEA: Can left_join the AAF to deaths in "sections"

# Table join tests ####
## Expansion join ####
# Because a "join" approach requires matching keys, this may be a useful data maneuver. 
# Could also segment join (and then sum across them, e.g. cause only, cause + age, cause + sex, etc.
pivot_test_tbl = workbook_url |> str_glue(sheetid = 1564529729) |> read_csv()

pivot_test_tbl |> expand_age_range()


# AAF table = 
# [DIRECT] + (...add years)
# [INDIRECT] + (=RR + Prev)
# [FARS] +
# [VDRS or SUDORS] (optional)


ardi_cod_lookup_tbl = fetch_ardi_cod_tbl() # Retrieve from online / TODO Testing

aaf_direct_tbl = fetch_direct_aaf_tbl()


