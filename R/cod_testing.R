library(tidyverse)

source("R/cod_code.R")
source("R/create_synth_death_data.R")


ardi_cod_tbl = get_ardi_case_tbl() 
ardi_cod_tbl |> count(icd10_codes_text, sort = T) # Should be all 1.
# TODO need to split / expand this table by cod_sex and maybe age. Can recode regex en mass but need to left-join as well. A little awkward.

lookup_tbl = ardi_cod_tbl |> select(regex = icd10_code_regex, label = cod_long) |> print(n=Inf)

cod_test_tbl = create_synth_death_data(1000000) |> # Label 1M ICD codes
  mutate(cod_long = cod |> recode_values_regex_tbl(lookup_tbl)) |>
cod_test_tbl |> count(cod_long)

