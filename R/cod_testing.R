library(tidyverse)
x = c("F103", "F106", "F109", "D120")
x |> str_detect("F10[3-9]")

x = c("K71", "K704", "K705", "K709")
x |> str_detect("K70[0-49]")

