library(tidyverse)
library(readxl)
library(janitor)

# Test read extract ####
# read_excel("data/CrashReport_2026.08.25.xlsx", n_max = 10)
# YIKES this format is brutal
new_fars_names = c(
  "year", "state", "sex", 
  c("bac" |> paste0(c("_00", "_0107", "_08up", "_01up", "_15", "_05up")), "total") |> map(\(x){paste0(x, c("_n", "_pct"))}) |> unlist())

fars_tbl = read_excel("data/CrashReport_2026.08.25.xlsx", skip = 8, col_names = new_fars_names) |> fill(c(year, state), .direction = "down")
fars_tbl 

fars_ardi_tbl = fars_tbl |> select(year, state, sex, bac_08up_n, bac_08up_pct, total_n)
fars_ardi_tbl |> filter(state == "North Carolina") |> arrange(desc(year), state)

