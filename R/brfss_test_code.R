# brfssr is a community package. Could rely on it
# Could try to directly ping the API as well

# BRFSSR ####
# https://github.com/bcapistrant/brfssR - 
# "This package contains five datasets provided by the Centers for Disease Control and 
# Prevention Behavioral Risk Factor Surveillance System (BRFSS) between 2014-2020:"
# Defunct? Last edit 5 years ago.

# Other packages ####
# Other potential useful packages if working w raw data for weights: survey, srvyr, haven.

# BRFSS API / data.cdc.gov ####
# https://data.cdc.gov/Behavioral-Risk-Factors/Behavioral-Risk-Factor-Surveillance-System-BRFSS-A/d2rk-yvas/about_data
# Seems like one could build a query and then attach an API endpoint call to it using the Socrata interface. Socrata package may also help.
# https://data.cdc.gov/api/v3/views/d2rk-yvas/query.csv?query=SELECT%0A%20%20%60year%60%2C%0A%20%20%60locationabbr%60%2C%0A%20%20%60locationdesc%60%2C%0A%20%20%60class%60%2C%0A%20%20%60topic%60%2C%0A%20%20%60question%60%2C%0A%20%20%60response%60%2C%0A%20%20%60break_out%60%2C%0A%20%20%60break_out_category%60%2C%0A%20%20%60sample_size%60%2C%0A%20%20%60data_value%60%2C%0A%20%20%60confidence_limit_low%60%2C%0A%20%20%60confidence_limit_high%60%2C%0A%20%20%60display_order%60%2C%0A%20%20%60data_value_unit%60%2C%0A%20%20%60data_value_type%60%2C%0A%20%20%60data_value_footnote_symbol%60%2C%0A%20%20%60data_value_footnote%60%2C%0A%20%20%60datasource%60%2C%0A%20%20%60classid%60%2C%0A%20%20%60topicid%60%2C%0A%20%20%60locationid%60%2C%0A%20%20%60breakoutid%60%2C%0A%20%20%60breakoutcategoryid%60%2C%0A%20%20%60questionid%60%2C%0A%20%20%60responseid%60%2C%0A%20%20%60geolocation%60%0AWHERE%20caseless_one_of(%60class%60%2C%20%22Alcohol%20Consumption%22)%0AORDER%20BY%20%60year%60%20DESC%20NULL%20FIRST%2C%20%60locationabbr%60%20ASC%20NULL%20LAST
# https://github.com/Chicago/RSocrata

# library - RSocrata # FYI, out of date for this version of R.

## Polite BRFSS call (attempt) #### 
# Developed the "gimme alcohol consumption data" pull here: 
# https://data.cdc.gov/Behavioral-Risk-Factors/Behavioral-Risk-Factor-Surveillance-System-BRFSS-A/d2rk-yvas/explore/query/SELECT%0A%20%20%60year%60%2C%0A%20%20%60locationabbr%60%2C%0A%20%20%60locationdesc%60%2C%0A%20%20%60class%60%2C%0A%20%20%60topic%60%2C%0A%20%20%60question%60%2C%0A%20%20%60response%60%2C%0A%20%20%60break_out%60%2C%0A%20%20%60break_out_category%60%2C%0A%20%20%60sample_size%60%2C%0A%20%20%60data_value%60%2C%0A%20%20%60confidence_limit_low%60%2C%0A%20%20%60confidence_limit_high%60%2C%0A%20%20%60display_order%60%2C%0A%20%20%60data_value_unit%60%2C%0A%20%20%60data_value_type%60%2C%0A%20%20%60data_value_footnote_symbol%60%2C%0A%20%20%60data_value_footnote%60%2C%0A%20%20%60datasource%60%2C%0A%20%20%60classid%60%2C%0A%20%20%60topicid%60%2C%0A%20%20%60locationid%60%2C%0A%20%20%60breakoutid%60%2C%0A%20%20%60breakoutcategoryid%60%2C%0A%20%20%60questionid%60%2C%0A%20%20%60responseid%60%2C%0A%20%20%60geolocation%60%0AWHERE%20caseless_one_of%28%60class%60%2C%20%22Alcohol%20Consumption%22%29%0AORDER%20BY%20%60year%60%20DESC%20NULL%20FIRST%2C%20%60locationabbr%60%20ASC%20NULL%20LAST/page/filter

# This would be the polite way (per the website note), but it doesn't work right now / I've done it wrong:
# https://support.socrata.com/hc/en-us/articles/202949268-How-to-query-more-than-1000-rows-of-a-dataset #UGH
# download_file_with_wait = function(x, sleep_seconds = 1){
#   Sys.sleep(sleep_seconds); return(read_csv(x)) # wait a second so we aren't hammering the API
# }
# request_tbl = tibble(request = paste0(base_request, "&$limit=500", paste0("&$offset=", seq(0, 5000, 500)))) 
# request_tbl = request_tbl |> 
#   mutate(response = map(request, download_file_with_wait))
# brfss_tbl = request_tbl |> 
#   select(response) |> 
#   unnest(response) |> 
#   distinct() # In case API calls overlap

# Rude BRFSS call / test - just ask ###
library(tidyverse)
base_request_url = "https://data.cdc.gov/api/v3/views/d2rk-yvas/query.csv?query=SELECT%0A%20%20%60year%60%2C%0A%20%20%60locationabbr%60%2C%0A%20%20%60locationdesc%60%2C%0A%20%20%60class%60%2C%0A%20%20%60topic%60%2C%0A%20%20%60question%60%2C%0A%20%20%60response%60%2C%0A%20%20%60break_out%60%2C%0A%20%20%60break_out_category%60%2C%0A%20%20%60sample_size%60%2C%0A%20%20%60data_value%60%2C%0A%20%20%60confidence_limit_low%60%2C%0A%20%20%60confidence_limit_high%60%2C%0A%20%20%60display_order%60%2C%0A%20%20%60data_value_unit%60%2C%0A%20%20%60data_value_type%60%2C%0A%20%20%60data_value_footnote_symbol%60%2C%0A%20%20%60data_value_footnote%60%2C%0A%20%20%60datasource%60%2C%0A%20%20%60classid%60%2C%0A%20%20%60topicid%60%2C%0A%20%20%60locationid%60%2C%0A%20%20%60breakoutid%60%2C%0A%20%20%60breakoutcategoryid%60%2C%0A%20%20%60questionid%60%2C%0A%20%20%60responseid%60%2C%0A%20%20%60geolocation%60%0AWHERE%20caseless_one_of(%60class%60%2C%20%22Alcohol%20Consumption%22)%0AORDER%20BY%20%60year%60%20DESC%20NULL%20FIRST%2C%20%60locationabbr%60%20ASC%20NULL%20LAST"
brfss_alcohol_tbl = base_request_url |> read_csv() # This ALSO works, without the limit / offset. Website says it won't.
brfss_alcohol_tbl |> count(break_out) # where are the breakouts?
brfss_alcohol_tbl |> count(locationabbr) # 54 locations
brfss_alcohol_tbl |> count(year) # 2011-2024 at the moment (Aug 2026).
brfss_alcohol_tbl |> count(question)
brfss_alcohol_tbl |> count(question) |> View()

brfss_alcohol_tbl