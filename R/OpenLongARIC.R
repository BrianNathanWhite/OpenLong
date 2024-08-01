OpenLongARIC <- S7::new_class(
  name = "OpenLongARIC",
  package = 'OpenLong',
  parent = OpenLongData,
  validator = function(self) {

    if(length(self@filepath) == 1){

      if("Main_Study" %nin% list.files(self@filepath)){
        paste0(
          "Main_Study directory not found in filepath: \'", self@filepath, "\'.",
          "\n- filepath should be the location of",
          " BioLincc ARIC data on your device."
        )
      }

    }

  }
)

S7::method(read_baseline, OpenLongARIC) <- function(x){
  folder_V1 <- file.path(x@filepath, 'Main_Study', 'v1','CSV')
  folder_V2 <- file.path(x@filepath,'Main_Study', 'v2','CSV')
  folder_AFU <- file.path(x@filepath, 'Main_Study','AFU','CSV')
  folder_V5 <- file.path(x@filepath, 'Main_Study', 'v5','CSV')
  folder_V3 <- file.path(x@filepath, 'Main_Study','v3','CSV')

  # Read the CSV files from respective folders
  derive <- readr::read_csv(file = file.path(folder_V1, 'derive13.csv'), show_col_types = FALSE, guess_max = Inf)
  ftra02 <- readr::read_csv(file = file.path(folder_V1, 'ftra02.csv'), show_col_types = FALSE, guess_max = Inf)
  fhxa <- readr::read_csv(file = file.path(folder_V2, 'fhxa.csv'), show_col_types = FALSE, guess_max = Inf)
  mcups1 <- readr::read_csv(file = file.path(folder_AFU, 'mcups1.csv'), show_col_types = FALSE, guess_max = Inf)
  nhx <- readr::read_csv(file = file.path(folder_V5, 'nhx.csv'), show_col_types = FALSE, guess_max = Inf)
  phxa04 <- readr::read_csv(file = file.path(folder_V3, 'phxa04.csv'), show_col_types = FALSE, guess_max = Inf)
  hom <- readr::read_csv(file = file.path(folder_V1, 'hom.csv'), show_col_types = FALSE, guess_max = Inf)
  amha02 <- readr::read_csv(file = file.path(folder_V3, 'amha02.csv'), show_col_types = FALSE, guess_max = Inf)

  # # Combine the read data into a list
  # input_base <- list(derive = derive,
  #                    ftra02 = ftra02,
  #                    fhxa = fhxa,
  #                    mcups1 = mcups1,
  #                    nhx = nhx,
  #                    phxa04 = phxa04,
  #                    hom = hom,
  #                    amha02 = amha02)

  # Return the list of data frames
  input_base <- list(derive = derive,
                     ftra02 = ftra02,
                     fhxa = fhxa,
                     mcups1 = mcups1,
                     nhx = nhx,
                     phxa04 = phxa04,
                     hom = hom,
                     amha02 = amha02)

  input_base

}


S7::method(read_longitudinal, OpenLongARIC) <- function(x){
  folder_v1 <- file.path(x@filepath,'Main_Study', 'v1' ,'CSV')
  folder_v2 <- file.path(x@filepath, 'Main_Study', 'v2','CSV')
  folder_v3 <- file.path(x@filepath, 'Main_Study', 'v3','CSV')
  folder_v4 <- file.path(x@filepath, 'Main_Study', 'v4','CSV')
  folder_v5 <- file.path(x@filepath, 'Main_Study', 'v5','CSV')
  folder_v6 <- file.path(x@filepath, 'Main_Study', 'v6','CSV')
  folder_v7 <- file.path(x@filepath, 'Main_Study', 'v7','CSV')

  # Read the CSV files from respective folders
  derive1 <- readr::read_csv(file = file.path(folder_v1, 'derive13.csv'), show_col_types = FALSE, guess_max = Inf)
  derive2 <- readr::read_csv(file = file.path(folder_v2, 'derive2_10.csv'), show_col_types = FALSE, guess_max = Inf)
  derive3 <- readr::read_csv(file = file.path(folder_v3, 'derive37.csv'), show_col_types = FALSE, guess_max = Inf)
  derive4 <- readr::read_csv(file = file.path(folder_v4, 'derive47.csv'), show_col_types = FALSE, guess_max = Inf)
  derive5 <- readr::read_csv(file = file.path(folder_v5, 'derive51.csv'), show_col_types = FALSE, guess_max = Inf)
  derive6 <- readr::read_csv(file = file.path(folder_v6, 'derive61.csv'), show_col_types = FALSE, guess_max = Inf)
  derive7 <- readr::read_csv(file = file.path(folder_v7, 'derive71.csv'), show_col_types = FALSE, guess_max = Inf)

  bpua03 <- readr::read_csv(file = file.path(folder_v3, 'bpua03.csv'), show_col_types = FALSE, guess_max = Inf)
  bpub04 <- readr::read_csv(file = file.path(folder_v4, 'bpub04.csv'), show_col_types = FALSE, guess_max = Inf)

  paq <- readr::read_csv(file = file.path(folder_v6, 'paq.csv'), show_col_types = FALSE, guess_max = Inf)
  paqa04 <- readr::read_csv(file = file.path(folder_v4, 'paqa04.csv'), show_col_types = FALSE, guess_max = Inf)

  # Combine the read data into a list
  longitudinal_data <- list(
    derive1 = derive1,
    derive2 = derive2,
    derive3 = derive3,
    derive4 = derive4,
    derive5 = derive5,
    derive6 = derive6,
    derive7 = derive7,
    bpua03 = bpua03,
    bpub04 = bpub04,
    paq = paq,
    paqa04 = paqa04
  )

  # Return the list of data frames
  longitudinal_data
}

S7::method(derive_baseline, OpenLongARIC) <- function(x){
  tibble::tibble()
}


## didn't derive anything for baseline

S7::method(derive_longitudinal, OpenLongARIC) <- function(x){

}


S7::method(clean_baseline, OpenLongARIC) <- function(x){

  data_use_ARIC <- x@baseline

  if(is_empty(data_use_ARIC)){
    data_use_ARIC <- x@components$baseline %>%
      purrr::reduce(.f = dplyr::left_join)
  }

    # ARIC_baseline <- data_to_use$derive %>%
    #   dplyr::left_join(data_to_use$fhxa, by = "ID_C") %>%
    #   dplyr::left_join(data_to_use$ftra02, by = "ID_C") %>%
    #   dplyr::left_join(data_to_use$mcups1, by = "ID_C") %>%
    #   dplyr::left_join(data_to_use$nhx, by = "ID_C") %>%
    #   dplyr::left_join(data_to_use$hom, by = "ID_C") %>%
    #   dplyr::left_join(data_to_use$amha02, by = "ID_C") %>%
    #   dplyr::left_join(data_to_use$phxa04, by = "ID_C")

  data_use_ARIC%>%
    dplyr::mutate(race_variable = dplyr::case_when(
      RACEGRP == "A" ~ "Asian",
      RACEGRP == "B" ~ "Black",
      RACEGRP == "I" ~ "American Indian or Alaskan Native",
      RACEGRP == "W" ~ "White",
      TRUE ~ as.character(RACEGRP)
      )) %>%
    dplyr::mutate(gender_variable = dplyr::case_when(
      GENDER == "F" ~ "Female",
      GENDER == "M" ~ "Male",
      TRUE ~ as.character(GENDER)
      )) %>%
    dplyr::mutate(hypertension_medication = dplyr::case_when(
      HYPTMD01 == 0 ~ "No",
      HYPTMD01 == 1 ~ "Yes",
      TRUE ~ as.character(HYPTMD01)
      )) %>%
    dplyr::mutate(current_drinker = dplyr::case_when(
      CURDRK02 == 0 ~ "No",
      CURDRK02 == 1 ~ "Yes",
      TRUE ~ as.character(CURDRK02)
      )) %>%
    dplyr::mutate(marital_status = dplyr::case_when(
      FHXA01 == "W" ~ "Widowed",
      FHXA01 == "D" ~ "Divorced",
      FHXA01 == "S" ~ "Single",
      FHXA01 == "M" ~ "Married",
      TRUE ~ as.character(FHXA01)
      )) %>%
    dplyr::mutate(past_heart_failure = dplyr::case_when(
      MCU6 == "N" ~ "No",
      MCU6 == "Y" ~ "Yes",
      TRUE ~ as.character(MCU6)
      )) %>%
    dplyr::mutate(past_cerebrovascular_disease = dplyr::case_when(
      NHX4D == "N" ~ "No",
      NHX4D == "Y" ~ "Yes",
      TRUE ~ as.character(NHX4D)
      )) %>%
    dplyr::mutate(previous_depression = dplyr::case_when(
      NHX6B == "N" ~ "No",
      NHX6B == "Y" ~ "Yes",
      TRUE ~ as.character(NHX6B)
      )) %>%
    dplyr::mutate(income_variable = dplyr::case_when(
      PHXA60 == "0" ~ "NA",
      PHXA60 == "A" ~ "Under $5,000",
      PHXA60 == "B" ~ "$5,000 - $7,999",
      PHXA60 == "C" ~ "$8,000 - $11,999",
      PHXA60 == "D" ~ "$12,000 - $15,999",
      PHXA60 == "E" ~ "$16,000 - $24,999",
      PHXA60 == "F" ~ "$25,000 - $34,999",
      PHXA60 == "G" ~ "$35,000 - $49,999",
      PHXA60 == "H" ~ "$50,000 - $74,999",
      PHXA60 == "I" ~ "$75,000 - $99,999",
      PHXA60 == "J" ~ "$100,000 and over",
      TRUE ~ as.character(PHXA60)
      )) %>%
    dplyr::mutate(previous_cancer = dplyr::case_when(
      HOM10F == "N" ~ "No",
      HOM10F == "Y" ~ "Yes",
      TRUE ~ as.character(HOM10F)
      )) %>%
    dplyr::mutate(cancer_incidence_plus_therapy = dplyr::case_when(
      AMHA18 == "Y" & AMHA20 == "Y" ~ "Breast Cancer Diagnosis and Chemo/Radiation",
      AMHA18 == "Y" & AMHA20 == "N" ~ "Breast Diagnosis",
      AMHA18 == "N" & AMHA20 == "Y" ~ "Chemo/Radiation Only",
      AMHA18 == "N" & AMHA20 == "N" ~ "None"
    ))
  }

# S7::method(clean_longitudinal, OpenLongMesa) <- function(x){
#
#   data_to_use <- x@components$baseline$input_base
#
#
# }




