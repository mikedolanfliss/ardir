
# Quick testing (should be external report or file)
library(tidyverse)
source("R/ardi_setup.R")

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


make_multiyear_mvc_aafs(mvc_aaf_tbl, 2)
make_multiyear_mvc_aafs(mvc_aaf_tbl, 3)
make_multiyear_mvc_aafs(mvc_aaf_tbl, 2) |> write_csv("testing/2yr_mvc_aaf_tbl.csv")
make_multiyear_mvc_aafs(mvc_aaf_tbl, 2) |> 
  filter(end_year == 2021, state == "North Carolina")
  


 