# FARS API details: https://crashviewer.nhtsa.dot.gov/crashviewer/crashapi
# Query builder: https://cdan.dot.gov/query 

library(tidyverse)

# Injury severity table ####
"https://crashviewer.nhtsa.dot.gov/crashviewer/CrashAPI/analytics/GetInjurySeverityCounts?fromCaseYear=2014&toCaseYear=2015&state=1&format=csv" |> read_csv()

# Raw FARS data ####
fars_tbl = "https://crashviewer.nhtsa.dot.gov/crashviewer/CrashAPI/FARSData/GetFARSData?dataset=Accident&FromYear=2014&ToYear=2016&state=1&format=csv" |> read_csv()
"https://crashviewer.nhtsa.dot.gov/crashviewer/CrashAPI/FARSData/GetFARSData?dataset=DRIMPAIR&FromYear=2014&ToYear=2016&state=1&format=csv" |> read_csv()
# Tables: ACCIDENT, CEVENT, CRASHRF (2020 Onwards), DAMAGE, DISTRACT, DRIMPAIR, DRIVERRF (2020 Onwards), DRUGS, FACTOR, MANEUVER, NMCRASH, NMDISTRACT (2019 Onwards), NMIMPAIR, NMPRIOR, PARKWORK, PBTYPE, PERSON, PERSONRF (2020 Onwards), PVEHICLESF (2020 Onwards), RACE, SAFETYEQ, TRAILERVINDERIVED, VEHICLE, VEHICLESF (2020 Onwards), VEVENT, VINDECODE, VINDERIVED, VIOLATION, VISION, VSOE, WEATHER (2020 Onwards)
fars_data_base_request = "https://crashviewer.nhtsa.dot.gov/crashviewer/CrashAPI/FARSData/GetFARSData?dataset=DATASET_HERE&FromYear=2014&ToYear=2016&state=1&format=csv"
fars_test_tbl = tibble(dataset = c("DRUGS", "DRIMPAIR", "PERSON", "FACTOR")) |> 
  mutate(request = fars_data_base_request |> str_replace("DATASET_HERE", dataset)) |> 
  mutate(data = request |> map(read_csv))
fars_test_tbl |> filter(dataset == "DRIMPAIR") |> pull(data)

# By Person ####
# Get crashes by person seems useful, has drinking and multiple alcohol variables
fars_person_tbl = "https://crashviewer.nhtsa.dot.gov/crashviewer/CrashAPI/crashes/GetCrashesByPerson?age=30&sex=2&seatPos=11&injurySeverity=2&fromCaseYear=2014&toCaseYear=2015&state=1&includeOccupants=true&includeNonOccupants=true&format=csv" |> read_csv()
"https://crashviewer.nhtsa.dot.gov/crashviewer/CrashAPI/crashes/GetCrashesByPerson?age=30&sex=2&seatPos=11&injurySeverity=2&fromCaseYear=2014&toCaseYear=2015&state=1&includeOccupants=true&includeNonOccupants=true&format=csv" |> read_csv() |> head(1) |> t()

fars_person_tbl

fars_person_tbl |> names()
fars_person_tbl |> select(matches("age|pos")) # looking for the right variables... ?
fars_person_tbl |> View()
# 0-14, 15-19, 20-24, 25-34, 35-44, 45-54, 55-64, 65+