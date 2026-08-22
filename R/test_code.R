# This R file is full of test, but poorly formatted (for pack) functions and scripts.

library(tidyverse)
add_ardi_age_groups = function(x){
  # This function could work by hard code. Could use a case_when here, 
  # or just join x to a tibble using expand_grid, 
  # or reference a package-specific actual tibble that could edited elsewhere.
  # Note that this TIBBLE could just exist (e.g. age_group_tbl) and let the user left_join themselves.
  # Here we get to QC, however.

  # TODO Could QC the age a bit, warn, and print here, e.g. ages < 0, greater than 120, etc.
  age_tbl = tribble(
    ~age, ~age_group,
    0, "<1",
    1:4, "1-4",
    5:9, "5-9",
    10:14, "10-14",
    15:19, "15–19",
    20:24, "20–24",
    25:29, "25–29",
    30:34, "30–34",
    35:39, "35–39",
    40:44, "40–44",
    45:49, "45-49", 
    50:54, "50–54",
    55:59, "55-59", 
    60:64, "60–64",
    65:69, "65–69",
    70:74, "70–74",
    75:79, "75–79",
    80:84, "80–84", 
    85:120, "85+",
    NA_integer_, NA_character_,
  ) |> unnest(age)
  
  tbl_to_return = tibble(age = x) |> left_join(age_tbl)
  # TODO if (verbose)... can print the above table
  return(tbl_to_return |> pull(age_group))
}

# Example - could use a set of fake / example death records saved in the package.
tibble(age = sample(1:120, size = 20, replace = T)) |> 
  mutate(age_group = age |> add_ardi_age_groups())

