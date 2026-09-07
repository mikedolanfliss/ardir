library(tidyverse)
library(httr2)
library(tictoc)
# About data: https://data.cdc.gov/Behavioral-Risk-Factors/Behavioral-Risk-Factor-Surveillance-System-BRFSS-H/dttw-5yxu/about_data 
# Full data CSV endpoint. Probably BIG. # https://data.cdc.gov/api/v3/views/dttw-5yxu/query.csv (3M records unless filtered)
# Details: https://dev.socrata.com/foundry/data.cdc.gov/dttw-5yxu

# brfss_request_url example = "https://data.cdc.gov/api/v3/views/dttw-5yxu/query.csv?app_token=YOUR_APP_TOKEN"
# Pulled from: https://data.cdc.gov/Behavioral-Risk-Factors/Behavioral-Risk-Factor-Surveillance-System-BRFSS-P/dttw-5yxu/explore/query/SELECT%0A%20%20%60year%60%2C%0A%20%20%60locationabbr%60%2C%0A%20%20%60locationdesc%60%2C%0A%20%20%60class%60%2C%0A%20%20%60topic%60%2C%0A%20%20%60question%60%2C%0A%20%20%60response%60%2C%0A%20%20%60break_out%60%2C%0A%20%20%60break_out_category%60%2C%0A%20%20%60sample_size%60%2C%0A%20%20%60data_value%60%2C%0A%20%20%60confidence_limit_low%60%2C%0A%20%20%60confidence_limit_high%60%2C%0A%20%20%60display_order%60%2C%0A%20%20%60data_value_unit%60%2C%0A%20%20%60data_value_type%60%2C%0A%20%20%60data_value_footnote_symbol%60%2C%0A%20%20%60data_value_footnote%60%2C%0A%20%20%60datasource%60%2C%0A%20%20%60classid%60%2C%0A%20%20%60topicid%60%2C%0A%20%20%60locationid%60%2C%0A%20%20%60breakoutid%60%2C%0A%20%20%60breakoutcategoryid%60%2C%0A%20%20%60questionid%60%2C%0A%20%20%60responseid%60%2C%0A%20%20%60geolocation%60%0AORDER%20BY%0A%20%20%60year%60%20DESC%20NULL%20FIRST%2C%0A%20%20%60locationabbr%60%20ASC%20NULL%20LAST%2C%0A%20%20%60display_order%60%20ASC%20NULL%20LAST/page/filter
# read_csv("creds/cred_tbl.csv") # cred test
brfss_request_url = "https://data.cdc.gov/api/v3/views/dttw-5yxu/query.csv?query=SELECT%0A%20%20%60year%60%2C%0A%20%20%60locationabbr%60%2C%0A%20%20%60locationdesc%60%2C%0A%20%20%60class%60%2C%0A%20%20%60topic%60%2C%0A%20%20%60question%60%2C%0A%20%20%60response%60%2C%0A%20%20%60break_out%60%2C%0A%20%20%60break_out_category%60%2C%0A%20%20%60sample_size%60%2C%0A%20%20%60data_value%60%2C%0A%20%20%60confidence_limit_low%60%2C%0A%20%20%60confidence_limit_high%60%2C%0A%20%20%60display_order%60%2C%0A%20%20%60data_value_unit%60%2C%0A%20%20%60data_value_type%60%2C%0A%20%20%60data_value_footnote_symbol%60%2C%0A%20%20%60data_value_footnote%60%2C%0A%20%20%60datasource%60%2C%0A%20%20%60classid%60%2C%0A%20%20%60topicid%60%2C%0A%20%20%60locationid%60%2C%0A%20%20%60breakoutid%60%2C%0A%20%20%60breakoutcategoryid%60%2C%0A%20%20%60questionid%60%2C%0A%20%20%60responseid%60%2C%0A%20%20%60geolocation%60%0AWHERE%0A%20%20caseless_one_of(%60class%60%2C%20%22Alcohol%20Consumption%22)%0A%20%20AND%20caseless_one_of(%0A%20%20%20%20%60break_out_category%60%2C%0A%20%20%20%20%22Age%20Group%22%2C%0A%20%20%20%20%22Overall%22%2C%0A%20%20%20%20%22Sex%22%0A%20%20)%0AORDER%20BY%0A%20%20%60year%60%20DESC%20NULL%20FIRST%2C%0A%20%20%60locationabbr%60%20ASC%20NULL%20LAST%2C%0A%20%20%60display_order%60%20ASC%20NULL%20LAST"
brfss_alcohol_question_tbl = brfss_request_url |> 
  str_replace("YOUR_APP_TOKEN", read_csv("creds/cred_tbl.csv") |> pull(app_token)) |> 
  read_csv()
brfss_alcohol_question_tbl |> saveRDS("data/brfss_alcohol_question_tbl.rds")
# brfss_alcohol_question_tbl = readRDS("data/brfss_alcohol_question_tbl.rds")

brfss_alcohol_question_tbl |> count(topic)
brfss_alcohol_question_tbl |> count(topic, response)
brfss_alcohol_question_tbl |> arrange(year, locationabbr, break_out, response, class, topic)


brfss_alcohol_prev_tbl = brfss_alcohol_question_tbl |> 
  filter(break_out_category == "Sex") |> 
  mutate(prev_type = case_when(
    topic == "Alcohol Consumption" & response == "No" ~ "non_drinker",
    topic == "Alcohol Consumption" & response == "Yes" ~ "all_drinkers",    
    topic == "Binge Drinking" & response == "Yes" ~ "binge",
    topic == "Heavy Drinking" & response == "Yes" ~ "heavy",
    topic == "Heavy Drinking" & response |> str_detect("^Meet criteria") ~ "heavy",
    T ~ ""
  )) |> 
  filter(prev_type != "") |> 
  select(year, state = locationdesc, prev_type, break_out, data_value) |> 
  pivot_wider(names_from = prev_type, values_from = data_value)  |> 
  mutate(low_drinkers = all_drinkers - binge - heavy) |> 
  mutate(across(break_out, str_to_lower)) |>   
  select(year, state, sex = break_out, prev_low = low_drinkers, prev_medium = binge, prev_high = heavy) |> 
  mutate(across(matches("^prev_"), \(x){x/100}))
  
brfss_alcohol_prev_tbl
brfss_alcohol_prev_tbl |> saveRDS("data/brfss_alcohol_prev_tbl.rds")

# For BRFSS, the variables we need are: survey year, sex, _age80, alcday5, avedrnk3, drnk3ge5, maxdrnks, _rfbing6, drnkany5, _sststr, _strwt and _llcpwt