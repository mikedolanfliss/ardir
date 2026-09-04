# Get state-specific alcohol-attributable fatal MVCs from FARS API
# FARS API details: https://crashviewer.nhtsa.dot.gov/crashviewer/crashapi
# Query builder: https://cdan.dot.gov/query 

library(tidyverse)
library(tictoc)

# API REQUEST ####
# TODO write an API friendly read_csv() that waits a second beteween calls
# TODO switch to all states. Consider making this a function so you can ask for your state of interest. 
# TODO save original file to temp directory locally (not github). 
tic("Read all FARS person data from API")
fars_request_tbl = tibble(
  year = 2010:2024 |> as.character(),
  base_request = "https://crashviewer.nhtsa.dot.gov/crashviewer/CrashAPI/FARSData/GetFARSData?dataset=PERSON&FromYear=REQUESTYEAR&ToYear=REQUESTYEAR&state=*&format=csv") |> 
  mutate(request = base_request |> str_replace_all("REQUESTYEAR", year)) |>  
  mutate(data = request |> map(\(x){read_csv(x, col_types = cols(.default = "c"))}))
toc() # ~40 seconds for 2010-2024, 1 state; 10-15min for every state (state=*)
# TODO SAVE, SHRINK, and REREAD! if this file is in data, don't redownload?
# Could consider dropping in a .ignore folder.
# fars_request_tbl |> saveRDS("temp/fars_request_tbl.rds") # 86 megs, 1.3M rows x 162 variables

# CREATE & RECODE PERSON TABLE ####
fars_person_tbl = fars_request_tbl |> 
  unnest(data) |> 
  mutate(across(c(caseyear, st_case, age, alc_res), as.integer))

# Shrink to just vars we need ####
fars_person_sm_tbl = fars_person_tbl |> 
  select(year = caseyear, state = statename, st_case, age, agename, sex = sexname, inj_kabco = inj_sevname, alc_bac_result = alc_res, alc_bac_text = alc_resname) # seems like what we need?
# TODO Save here
fars_person_sm_tbl |> saveRDS("data/fars_person_sm_tbl_orig.rds")

# Pick up analysis here
fars_person_sm_tbl = readRDS("data/fars_person_sm_tbl_orig.rds")

get_mvc_age_group = function(x){  # Helper function for age group recoding - external table is cleaner
  age_tbl = tribble(
    ~age, ~age_group,
    0:14, "0-14", # TODO is there value later in the analysis to 
    15:19, "15–19", # ... build this "expanded" lookup table from raw char age ranges?
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

fars_person_sm_tbl = fars_person_sm_tbl |> 
  filter(sex %in% c("Male", "Female"), !is.na(sex), age < 120, !is.na(age)) |> 
  mutate(sex = sex |> as_factor()) |> 
  mutate(alc_bac_result = alc_bac_text |> str_extract("^[0-9\\.]+ ") |> as.numeric()) |> # Overwrite built in number variable
  mutate(alc_bac_result = case_when(
    alc_bac_text |> str_detect("% BAC|or Greater") ~ alc_bac_result, # Could also regex this, something like: "0.0[89]|0.[12345]"
    T ~  NA_integer_))  |> 
  mutate(direct_alc_attributable = case_when( # ID >=0.08 BAC CRASHES ####
      alc_bac_result >= 0.08 ~ TRUE, # "Directly Alcohol Attributable"
      T ~ FALSE)) # "Other / Mixed Cause"
# TODO question - drop missing age -> missing age group?

# Hand check the BAC vars reveals issue: 0.84 and 0.084 both get "84" in number var.
# fars_person_sm_tbl |> count(alc_bac_result, alc_bac_text) |> arrange(desc(alc_bac_result)) |> View() # Could save and quarto a table like this. 

# Create crash BAC % table ####
fars_crashbac_tbl = fars_person_sm_tbl |>   
  group_by(year, state, st_case) |> 
  summarize(crash_direct_alc_attributable = any(direct_alc_attributable))
fars_person_sm_tbl = fars_person_sm_tbl |> 
  left_join(fars_crashbac_tbl) # join was crash alcohol attributable (>0.08 BAC)
# fars_person_sm_tbl |> filter(st_case |> str_detect("^1000[123]$")) |> arrange(st_case)

fars_person_sm_tbl |> saveRDS("data/fars_person_sm_tbl.rds")

# Calculate person AAF ####
fars_bac_pct_tbl = fars_person_sm_tbl |>   
  filter(inj_kabco |> str_detect("(K)")) |> 
  mutate(age_group = age |> get_mvc_age_group()) |> 
  group_by(year, state, age_group, sex) |> 
  count(direct_alc_attributable, .drop = FALSE)  |> ungroup() |> 
  arrange(year, state, age_group, sex) |> 
  complete(year, state, age_group, sex, direct_alc_attributable, fill = list(n = 0))

mvc_aaf_tbl = fars_bac_pct_tbl |> 
  left_join( # Join total counts
    fars_bac_pct_tbl |> 
      group_by(year, state, age_group, sex) |> 
      summarize(total_n = sum(n)) ) |> 
  filter(direct_alc_attributable) |> 
  select(-direct_alc_attributable) |> 
  rename(n_alc_att = n) |> 
  mutate(pct_alc_attributable = n_alc_att / total_n)
mvc_aaf_tbl
mvc_aaf_tbl |> saveRDS("data/mvc_aaf_tbl.RDS")
mvc_aaf_tbl |> write_csv("data/mvc_aaf_tbl.csv")

# Quick testing (should be external report or file)
mvc_aaf_tbl |> arrange(state, year, sex, age_group) |> filter(state == "North Carolina", year == 2024)
ggplot(mvc_aaf_tbl |> filter(state == "North Carolina"), aes(x = age_group, color = year, y = pct_alc_attributable, group = year))+
  geom_line(stat = "smooth", se = F, alpha = 0.7)+
  facet_grid(.~sex)+
  theme_minimal()+
  labs(title = "NC MVC AAFs")
ggsave("testing/MVC AAFs - North Carolina Graph.png")

ggplot(mvc_aaf_tbl |> filter(state == "Rhode Island"), aes(x = age_group, color = year, y = pct_alc_attributable, group = year))+
  geom_line(stat = "smooth", se = F, alpha = 0.7)+
  facet_grid(.~sex)+
  theme_minimal()+
  labs(title = "Rhode Island MVC AAFs")
ggsave("testing/MVC AAFs - Rhode Island Graph.png")

# TODO subfolder under data (FARS, BRFSS, etc.
# TODO will need to expand this for age (not age_group) joining. This table will be row_bound to the other documented ones. 
# TODO calculate US-wide? Or just join into national data allowing state variation
# TODO project today()'s yearly AAFs by duplicating prior year or a simple linear / loess type projection?

# fars_person_sm_tbl |> filter(state == "North Carolina", inj_kabco |> str_detect("K")) |> arrange(age) |> View()
# fars_bac_pct_tbl |> filter(state == "North Carolina") |> 
#   ggplot(aes(x = year, y = pct_alc_attributable, color = age_group)) +
#   facet_wrap(~sex)+
#   geom_smooth(se = F)

