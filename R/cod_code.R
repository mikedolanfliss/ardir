library(tidyverse)
library(readxl)
library(purrr)

# HARD CODE ICD ASSIGNMENT FUNCTIONS ####
assign_death_mechanism = function(icd_str){
  tbl_to_return = tibble(icd_str) |>
    mutate(mech = case_when(
      icd_str |> str_detect("^(W2[5-9]|W4[56]|X78|X99|Y28)|Y354") ~ "CUT/PIERCE", #initials + date? if we do the function version
      icd_str |> str_detect("^W(6[0-9]|7[0-4])|^X71|^X92|^Y21") ~ "DROWNING/SUBMERSION",
      icd_str |> str_detect("^W[01][0-9]|^X80|^Y01|^Y30") ~ "FALL",
      icd_str |> str_detect("^X([01][0-9]|7[67]|9[89])|^Y2[67]|Y363|U013") ~ "FIRE/BURN",
      icd_str |> str_detect("^(W3[234]|X7[234]|X9[345]|Y2[234])|Y350|YU014") ~ "FIREARM",
      icd_str |> str_detect("^(W24|W3[01])") ~ "MACHINERY",
      icd_str |> str_detect("^(V[3-7][4-9]|V8[3-6][0-3]|V2[0-8][3-9]|V29[4-9]|V1[234][3-9]|V19[4-6]|V0[234][19]|V092|V80[245]|V8[12]1|V87[0-8]|V892)") ~ "MVT", # TODO incomplete. MDF has MVT regex code on Katie Harmon project
      icd_str |> str_detect("V1[01]|V1[234][012]|V1[5678]|V19[012389]") ~ "MVT: PEDAL CYCLIST OTHER",
      icd_str |> str_detect("V01|V0[234]0|V0[56]|V09[0139]") ~ "MVT: PEDESTRIAN OTHER",
      icd_str |> str_detect("V2[0-8][012]|V[2-7][0-9][0123]|V80[0126789]|V8[12][023456789]|V8[3-6][4-9]|V879|V88[0-9]|V89[0139]|X82|Y03|Y32") ~ "MVT: OTHER LAND TRANSPORT", # ? V80 includes 1; V81 does not?
      icd_str |> str_detect("^(V9[0-9]|Y361|U011)") ~ "MVT: OTHER TRANSPORT", # SAS seems to have an error here, first3 in Y61 / U011. Around line 620
      icd_str |> str_detect("^(W4[23]|W5[3-9]|W6[0-4]|W9[2-9]|X[23][0-9]|X5[1-7])") ~ "NATURAL/ENVIRONMENTAL",
      icd_str |> str_detect("^X50") ~ "OVEREXERTION",
      icd_str |> str_detect("^(X[46]|X8[5-9]|X90|Y1|Y352|U01[67])") ~ "POISONING",
      icd_str |> str_detect("^(W[25][012]|X79|Y00|Y04|Y29|Y353)") ~ "STRUCK BY/AGAINST",
      icd_str |> str_detect("W7[5-9]|W8[0-4]|X70|X91|Y20") ~ "SUFFOCATION",
      icd_str |> str_detect("W23|W44|W49|Y85|X75|X81|X96|Y02|Y25|Y31|Y35[15]|Y36[02]|U010|U01[25]|U030|W3[5-9]|W4[01]|W8[5-9]|W9[01]|Y0[567]|Y36[4-8]") ~ "OTHER SPECIFIED/CLASSIFIABLE", # 85/850?
      icd_str |> str_detect("X58|X83|Y08|Y33|Y356|Y89[01]|U018|U02|Y8[67]") ~ "OTHER SPECIFIED/NEC",
      icd_str |> str_detect("X59|X84|Y09|Y34|Y899|Y357|Y369|U019|U039") ~ "UNSPECIFIED INJURY",
      icd_str |> str_detect("Y[4567][0-9]|Y8[0-4]|Y88") ~ "ADVERSE EFFECTS",
      # End Injury codes
      icd_str |> str_detect("[AB][0-9]") ~ "A&B: INFECTIONS/PARASITES",
      icd_str |> str_detect("C3[0-9]") ~ "C3: CANCER (LUNG)",
      icd_str |> str_detect("C22|K70") ~ "C22|K70: CANCER & CIRRHOSIS (LIVER)",
      icd_str |> str_detect("C") ~ "C3*: CANCER (OTHER)",
      icd_str |> str_detect("D") ~ "D*: BLOOD & IMMUNE DISEASES",
      icd_str |> str_detect("E") ~ "D*: ENDOCRINE, NUTRITION, METABOLIC DISEASES",
      icd_str |> str_detect("F") ~ "F*: MENTAL & BEHAVIORAL DISORDERS",
      icd_str |> str_detect("G") ~ "G*: NERVOUS SYSTEM DISEASES",
      icd_str |> str_detect("H[0-5]") ~ "H0-5: EYE DISEASES",
      icd_str |> str_detect("H[6-9]") ~ "H6-9: EAR & JAW DISEASES",
      icd_str |> str_detect("I") ~ "I: HEART & CIRCULATORY DISEASES",
      icd_str |> str_detect("J") ~ "J: RESPIRATORY DISEASES",
      icd_str |> str_detect("K") ~ "K: DIGESTIVE DISEASES",
      icd_str |> str_detect("L") ~ "L: SKIN TISSUE DISEASES",
      icd_str |> str_detect("M") ~ "M: MUSCULOSKELETAL DISEASES",
      icd_str |> str_detect("N") ~ "N: GENITOURINARY DISEASES",
      icd_str |> str_detect("O") ~ "O: PREGNANCY/CHILDBIRTH",
      icd_str |> str_detect("P") ~ "P: PERINATAL CONDITIONS",
      icd_str |> str_detect("Q[0-9A]") ~ "Q: CONGENITAL MALFORMATIONS / GENETIC DISORDERS",
      icd_str |> str_detect("R99") ~ "R99: PENDING COD",
      icd_str |> str_detect("R") ~ "R: OTHER UNCLASSIFIED SYMPTOMS",
      icd_str |> str_detect("ST") ~ "ST: OTHER INJURY CODES",
      icd_str |> str_detect("U") ~ "U: SPECIAL PURPOSE CODES",
      icd_str |> str_detect("V") ~ "V: EXTERNAL CAUSE CODES",
      icd_str |> str_detect("Z") ~ "Z: SOCIAL DRIVERS",
      T ~ str_glue("{icd_str} (OTHER)")
    ))
  return(tbl_to_return |> pull(mech))
}


assign_death_intent = function(icd_str){
    tbl_to_return = tibble(icd_str) |>
    mutate(intent = case_when(
      icd_str |> str_detect("V0[1-9]|V[1-9]|W[0-9]|X[0-5][0-9]|Y8[56]") ~ "UNINTENTIONAL",
      icd_str |> str_detect("X[67]|X8[0-4]|U03|Y870") ~ "SELF-INFLICTED",
      icd_str |> str_detect("X8[5-9]|X[6-9]|Y0[0-9]|Y871|U0[12]") ~ "ASSAULT", # MDF Not sure le Y09 is refactored correctly
      icd_str |> str_detect("Y[12][0-9]|Y3[0-4]|Y872|Y899") ~ "UNDETERMINED",
      icd_str |> str_detect("Y3[56]|Y89[01]") ~ "LEGAL INTERVENTION",
      icd_str |> str_detect("Y[4567][0-9]|Y8[0-4]|Y88") ~ "OTHER",
      T ~ "" # Should this be missing?
    ))
  return(tbl_to_return |> pull(intent))
}



# TABLE RECODE FUNCTIONS ####
recode_values_regex = function(x, from_regex, to){
  # Extend recode_values() function to accept regular expressions
 # Thanks to this dplyr issue submitted by MDF: https://github.com/tidyverse/dplyr/issues/7846
  vctrs::vec_case_when(
    conditions = purrr::map(from_regex, function(from_regex) str_detect(x, from_regex)),
    values = as.list(to)
  )
}
 
recode_values_regex_tbl = function(x, regex_tbl){
  # Wrap recode_values_regex to accept a table directly
  tbl_to_return = tibble(x) |>
    mutate(label = x |>
      recode_values_regex(from_regex = regex_tbl$regex, to = regex_tbl$label))
  return(tbl_to_return |> pull(label))
}
 
ardi_case_tbl = read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQIiNisnSOi-NJbf8ZP9BnaF_p5en2yiTsSc-UNmYrwpN9vw-eMIAfu7ix_zD915jMdYDCgVmPIgZse/pub?gid=1878924397&single=true&output=csv")
# Example file (moved to repo as test, June 2026)
# TODO leave a test icd table / sample in the I drive, possibly in repo.

 