library(tidyverse)
library(tessaract)
library(tabulapdf)

tesseract::ocr_data()


if(!dir.exists("temp")) dir.create("temp/")
supplement_pdf = "manuscripts/Esser et al. - 2022 - Estimated Deaths Attributable to Excessive Alcohol Use Among US Adults Aged 20 to 64 Years, 2015 to.pdf"
pdftools::pdf_convert(supplement_pdf, pages = 2:5, filenames = "temp/esser_2022_table" |> paste0(2:5))

tbl_extract = tabulapdf::extract_tables(supplement_pdf) |> bind_rows()
tbl_extract |> write_csv("temp/esser_2022_table_messy.csv")