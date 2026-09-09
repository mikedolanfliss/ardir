library(tidyverse)
# library(tessaract) # for tesseract::ocr_data(). Not avail for this version of R...?
library(tabulapdf)

# Extract causes from Esser et al ####
supplement_pdf = "manuscripts/Esser et al. - 2022 - Estimated Deaths Attributable to Excessive Alcohol Use Among US Adults Aged 20 to 64 Years, 2015 to.pdf"
pdftools::pdf_convert(supplement_pdf, pages = 2:5, filenames = "temp/esser_2022_table" |> paste0(2:5))
tbl_extract = tabulapdf::extract_tables(supplement_pdf) |> bind_rows()
tbl_extract |> write_csv("temp/esser_2022_table_messy.csv")

# Extract per capita consumption from NIAAA ####
# download.file("https://www.niaaa.nih.gov/sites/default/files/surveillance-report121.pdf", "manuscripts/niaaa_report_2024.pdf") # Done once.
# ^ File is malformed, creating tabula error "page tree root must be a directory", required printing to PDF again outside R.
niaaa_report_list = tabulapdf::extract_tables("manuscripts/niaaa_report_2024_v2.pdf")
niaaa_report_list = tabulapdf::extract_tables("manuscripts/surveillance-report122.Per-Capita-Consumption.pdf")

# niaaa_report_list[[9]] # MESSY
# niaaa_report_list[[10]]

clean_niaaa_table = function(x){
  new_x = x |> 
    set_names(c("state_year", "percap_beer", "percap_wine_spirits", "percap_all", "decile")) |> 
    filter(!(state_year |> str_detect("geographic"))) |> 
    mutate(
      row_type = if_else(state_year |> str_detect("19|20"), "num", "text"),  
      state = if_else(row_type == "num", NA_character_, state_year),
      year = if_else(row_type == "num", state_year |> str_remove_all("\\.") |> str_trim(), NA_character_)) |> 
    fill(state, .direction = "down") |> 
    separate(percap_wine_spirits, into = c("percap_wine", "percap_spirits"), sep = " ") |> 
    filter(row_type == "num")
  return(new_x)
}

niaaa_report_tbl = tibble(page_num = 1:length(niaaa_report_list)) |> 
  mutate(page_table = map(page_num, \(x) niaaa_report_list[[x]])) |> 
  slice_tail(n = -8) |> # In 2024 and 2025 NIAAA report, relevant table starts with #9
  mutate(clean_table = map(page_table, clean_niaaa_table)) |> 
  select(-page_table) |> 
  unnest(clean_table) |> 
  fill(state, .direction = "down") |> # need to fill across pages
  select(state, year, matches("^per"), decile)
  
niaaa_report_tbl
niaaa_report_tbl |> count(state)
niaaa_report_tbl |> write_csv("data/niaaa_report_tbl.csv")
niaaa_report_tbl |> saveRDS("data/niaaa_report_tbl.rds")
