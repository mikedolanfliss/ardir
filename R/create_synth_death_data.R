library(tidyverse)

create_synth_death_data = function(n_records = 1000){
  tbl_to_return = tibble(
    age = sample(1:115, n_records, replace = T),
    sex = sample(c("male", "female"), n_records, replace = T),
    cod = paste0(
      sample(LETTERS, n_records, replace = T),
      sample(0:9, n_records, replace = T),
      sample(0:9, n_records, replace = T),
      sample(c(0:9, rep("", 3)), n_records, replace = T))
  ) 
  return(tbl_to_return)
}
# create_synth_death_data(10) # Example


