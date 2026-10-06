library(tidyverse)
source("R/ardi_setup.R")

fetch_ardi_cod_tbl()
direct_aaf_tbl = fetch_direct_aaf_tbl()
wide_rr_tbl = fetch_rr_wide_tbl()

source_vintage_tbl = direct_aaf_tbl |> 
  filter(aaf_num < 1) |> 
  select(cod = cod_long, source = aaf_source) |> 
  mutate(table = "Direct AAF") |> 
  bind_rows(
    wide_rr_tbl |> 
      select(cod = cause, source = RR_source) |> 
      mutate(table = "RR table")
  ) |> 
  mutate(year = source |> str_extract("[0-9]{4}") |> as.integer()) |>   
  mutate(cod = cod |> fct_reorder2(table, year))
  
source_vintage_tbl

ggplot(source_vintage_tbl, aes(year, cod)) +
  geom_point()+  
  geom_segment(aes(y = cod, yend = cod, x = year, xend = today() |> year()))+
  geom_label(aes(label = year))+
  scale_x_continuous(limits = c(NA, today() |> year()))+
  geom_vline(xintercept = today() |> year())+
  annotate("text", x = today() |> year(), y = 10, label = "TODAY")+
  facet_grid(table ~ ., scales = "free_y")+
  labs(title = "ARDI literature sources")
ggsave("graphs/ardi_lit_vintage.png", width = 8, height = 20)
