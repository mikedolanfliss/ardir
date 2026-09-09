
# Quick testing (should be external report or file)
library(tidyverse)

mvc_aaf_tbl = readRDS("data/mvc_aaf_tbl.RDS")

mvc_aaf_tbl |> arrange(state, year, sex, age_range) |> filter(state == "North Carolina", year == 2024)

# Plots for testing
ggplot(mvc_aaf_tbl |> filter(state == "North Carolina"), aes(x = age_range, color = year, y = aaf_num, group = year))+
  geom_line(stat = "smooth", se = F, alpha = 0.7)+
  facet_grid(.~sex)+
  theme_minimal()+
  labs(title = "NC MVC AAFs")
ggsave("testing/MVC AAFs - North Carolina Graph.png")

ggplot(mvc_aaf_tbl |> filter(state == "Rhode Island"), aes(x = age_range, color = year, y = aaf_num, group = year))+
  geom_line(stat = "smooth", se = F, alpha = 0.7)+
  facet_grid(.~sex)+
  theme_minimal()+
  labs(title = "Rhode Island MVC AAFs")
ggsave("testing/MVC AAFs - Rhode Island Graph.png")

# Multi-year aggregation example

# TODO this helper function could be made generic

make_multiyear_mvc_aafs = function(mvc_aaf_tbl, n_years = 2){
  # Expect certain structure, for now; later, test.
  avail_fars_years = mvc_aaf_tbl |> distinct(year) |> pull(year)
  mvc_aaf_multi_tbl = tibble(
    end_year = mvc_aaf_tbl |> distinct(year) |> pull(year), n_years = n_years) |> 
    filter((end_year - n_years + 1) %in% avail_fars_years) |> 
    mutate(year = map2(end_year, n_years, \(x, y){seq(x - y + 1, x)})) |> 
    unnest(year) |> 
    mutate(data = map(year, \(x){mvc_aaf_tbl |> filter(year == x) |> select(-year)})) |> 
    unnest(data) |> 
    group_by(end_year, n_years, state, age_range, sex) |> 
    summarize(across(c(n_alc_att, total_n), sum), .groups = "drop") |> 
    mutate(aaf_num = n_alc_att / total_n) |> 
    mutate(year_desc = str_glue("{end_year-n_years+1}-{end_year}"))
  return(mvc_aaf_multi_tbl)
}

make_multiyear_mvc_aafs(mvc_aaf_tbl, 2)
make_multiyear_mvc_aafs(mvc_aaf_tbl, 2) |> write_csv("testing/2yr_mvc_aaf_tbl.csv")
make_multiyear_mvc_aafs(mvc_aaf_tbl, 3)
  


 