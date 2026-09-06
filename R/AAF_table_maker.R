# AAF reader
library(tidyverse)
# library(validate) # for table integrity testing TODO
# Should some of this be git-ignored?


# Cause table ####
workbook_url = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQIiNisnSOi-NJbf8ZP9BnaF_p5en2yiTsSc-UNmYrwpN9vw-eMIAfu7ix_zD915jMdYDCgVmPIgZse/pub?gid={sheetid}&single=true&output=csv"
cause_tbl = workbook_url |> str_glue(sheetid = 1878924397) |> read_csv()
aaf_direct_tbl = workbook_url |> str_glue(sheetid = 354728235) |> read_csv()

# RR table ####
rr_tbl_wide = workbook_url |> str_glue(sheetid = 2127475538) |> read_csv()
rr_tbl = rr_tbl_wide |> 
  select(matches("cause|male|female|prev_type")) |> 
  pivot_longer(cols = matches("female|male"), values_to = "RR", names_to = "consumption_sex") |> 
  mutate(consumption_sex = consumption_sex |> str_remove("^RR_")) |> 
  separate(consumption_sex, into = c("consumption", "sex")) |> 
  pivot_wider(names_from = c("consumption"), values_from = "RR", names_prefix = "RR_")
rr_tbl # tidy-long for now, may want to split by sex only and go wide with consumption


# Prev ####
prev_tbl_wide = workbook_url |> str_glue(sheetid = 61349445) |> read_csv()
prev_tbl = prev_tbl_wide |> 
  pivot_longer(cols = matches("year_"), names_to = "year", values_to = "prev") |> 
  mutate(year = year |> str_remove("year_") |> as.integer()) |> 
  pivot_wider(names_from = c("consumption"), values_from = "prev", names_prefix = "prev_")
prev_tbl

# AAF indirect table ####
aaf_indirect_tbl = prev_tbl |> 
  full_join(rr_tbl) |> # Join RRs in
  mutate( # calculate total alcohol and excessive alcohol attributable fractions
    total_product_sum = prev_low*(RR_low-1) + prev_high*(RR_high-1) + prev_high*(RR_high-1),
    total_aaf = total_product_sum / (1 + total_product_sum),
    RS2 = RR_medium / RR_low, 
    RS3 = RR_high / RR_low,
    excess_product_sum = prev_medium*(RS2 - 1) + prev_high*(RS3 - 1),
    excess_aaf = excess_product_sum / (1 + excess_product_sum))
aaf_indirect_tbl
# aaf_indirect_tbl |> View()
  
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


