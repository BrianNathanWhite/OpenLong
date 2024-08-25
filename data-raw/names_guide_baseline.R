## code to prepare `names_guide_baseline` dataset goes here

names_guide_baseline <- readr::read_csv("data-raw/names-guide-baseline.csv")

usethis::use_data(names_guide_baseline, overwrite = TRUE)
