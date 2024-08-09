OpenLongMesa <- S7::new_class(
  name = "OpenLongMesa",
  package = 'OpenLong',
  parent = OpenLongData,
  validator = function(self) {

    if(length(S7::prop(self, "filepath")) == 1){

      if("Primary" %nin% list.files(S7::prop(self, "filepath"))){
        paste0(
          "Primary directory not found in filepath: \'", S7::prop(self, "filepath"), "\'.",
          "\n- filepath should be the location of",
          " BioLincc MESA data on your device."
        )
      }

    }

  }
)

S7::method(read_baseline, OpenLongMesa) <- function(x){
  input_mesa1 <- readr::read_csv(file = file.path(S7::prop(x, "filepath"),
                                                  "Primary",
                                                  "Exam1",
                                                  "Data",
                                                  "mesae1dres20220813.csv"),
                                 show_col_types = FALSE,
                                 guess_max = Inf)

  list(input_mesa1 = input_mesa1)

}

S7::method(read_longitudinal, OpenLongMesa) <- function(x){
  fnames <- c("mesae2dres06222012.csv", "mesae3dres06222012.csv",
              "mesae4dres06222012.csv", "mesae5_drepos_20220820.csv")
  data_directory <- c("Exam2", "Exam3", "Exam4", "Exam5")
  longitudinal_data <- purrr::map2(
    .x = purrr::set_names(fnames),
    .y = data_directory,
    .f = ~ readr::read_csv(file = file.path(S7::prop(x, "filepath"),
                                            "Primary",
                                            .y,
                                            "Data",
                                            .x),
                           show_col_types = FALSE,
                           guess_max = Inf)
  )

  S7::prop(x, "components")$longitudinal <- longitudinal_data

}

S7::method(derive_baseline, OpenLongMesa) <- function(x){

  data_to_use <- S7::prop(x, "baseline")

  if(is_empty(data_to_use)){

    data_to_use <- S7::prop(x, "components")$baseline$input_mesa1

  }

  data_to_use %>%
    dplyr::mutate(
      total_cigarettes = compute_total_cigarettes(cig_per_day = CIGSDAY1,
                                                  age_started = AGESMK1,
                                                  age_quit = AGEQUIT1)
    )

}

S7::method(derive_longitudinal, OpenLongMesa) <- function(x){

  tibble::tibble()

}

S7::method(clean_baseline, OpenLongMesa) <- function(x){
  data_to_use <- S7::prop(x, "baseline")

  if(is_empty(data_to_use)){
    data_to_use <- S7::prop(x, "components")$baseline$input_mesa1
  }

  data_to_use %>%
    dplyr::mutate(across(c("HISP1", "MEXICAN1", "DOMINIC1", "PUERTO1", "CUBAN1", "OTHHISP1"), ~ifelse(.x == 1, "Yes", "No"))) %>%
    dplyr::mutate(RACE1C = dplyr::case_when(
      RACE1C == 1 ~ "Caucasian",
      RACE1C == 2 ~ "Chinese",
      RACE1C == 3 ~ "African American",
      RACE1C == 4 ~ "Hispanic",
      TRUE ~ as.character(RACE1C)
    )) %>%
    dplyr::mutate(GENDER1 = dplyr::case_when(
      GENDER1 == 0 ~ "Female",
      GENDER1 == 1 ~ "Male",
      TRUE ~ as.character(GENDER1)
    )) %>%
    dplyr::mutate(MARITAL1 = dplyr::case_when(
      MARITAL1 == 1 ~ "Married/ Living as Married",
      MARITAL1 == 2 ~ "Widowed",
      MARITAL1 == 3 ~ "Divorced",
      MARITAL1 == 4 ~ "Separated",
      MARITAL1 == 5 ~ "Never Married",
      MARITAL1 == 6 ~ "Prefer Not to Say",
      TRUE ~ as.character(MARITAL1)
    )) %>%
    dplyr::mutate(EDUC1 = dplyr::case_when(
      EDUC1 == 0 ~ "No Schooling",
      EDUC1 == 1 ~ "Grades 1-8",
      EDUC1 == 2 ~ "Grades 9-11",
      EDUC1 == 3 ~ "Completed High School/GED",
      EDUC1 == 4 ~ "Some College but No Degree",
      EDUC1 == 5 ~ "Technical School Certificate",
      EDUC1 == 6 ~ "Associate Degree",
      EDUC1 == 7 ~ "Bachelor's Degree",
      EDUC1 == 8 ~ "Graduate/Professional School",
      TRUE ~ as.character(EDUC1)
    )) %>%
    dplyr::mutate(INCOME1 = dplyr::case_when(
      INCOME1 == 1 ~ "<$5,000",
      INCOME1 == 2 ~ "$5,000 - $7,000",
      INCOME1 == 3 ~ "$8,000 - $11,999",
      INCOME1 == 4 ~ "$12,000 - $15,999",
      INCOME1 == 5 ~ "$16,000 - $19,999",
      INCOME1 == 6 ~ "$20,000 - $24,999",
      INCOME1 == 7 ~ "$25,000 - $29,999",
      INCOME1 == 8 ~ "$30,000 - $34,999",
      INCOME1 == 9 ~ "$35,000 - $39,999",
      INCOME1 == 10 ~ "$40,000 - $49,999",
      INCOME1 == 11 ~ "$50,000 - $74,999",
      INCOME1 == 12 ~ "$75,000 - $99,999",
      INCOME1 == 13 ~ "$100,000+",
      TRUE ~ as.character(INCOME1)
    )) %>%
    dplyr::mutate(across(c("HIGHBP1", "CANCER1", "COLONCN1", "NMSKNCN1", "OTHCN1", "BRSTCN1", "PROSTCN1"), ~dplyr::case_when(
      .x == 0 ~ "No",
      .x == 1 ~ "Yes",
      .x == 9 ~ "Don't Know",
      TRUE ~ as.character(.x)
    ))) %>%
    dplyr::mutate(across(c("MAJOR21", "MAJOR31"), ~ifelse(.x == 1, "Existing Abnormalities", NA_character_)))
}

S7::method(clean_longitudinal, OpenLongMesa) <- function(x){

  data_to_use <- S7::prop(x, "baseline")

  if(is_empty(S7::prop(x, "components")$longitudinal)){
    stop("Logintudinal data not found")
  }

  # Extract the longitudinal data
  longitudinal_data <- S7::prop(x, "components")$longitudinal
  mesa_one <- S7::prop(x, "components")$baseline$input_mesa1
  mesa_two <- longitudinal_data[[1]]
  mesa_three <- longitudinal_data[[2]]
  mesa_four <- longitudinal_data[[3]]
  mesa_five <- longitudinal_data[[4]]


  mesa_one <- mesa_one %>%
    dplyr::rename(ID = MESAID, BMI = BMI1C, Age = AGE1C, Glucose = GLUCOS1C) %>%
    dplyr::mutate(hypertension_med = HTNMED1C, hypertension_his = HTN1C)

  mesa_two <- mesa_two %>%
    dplyr::rename(ID = mesaid, BMI = bmi2c, Age = age2c, Glucose = glucos2c) %>%
    dplyr::mutate(hypertension_med = htnmed2c, hypertension_his = htn2c)

  mesa_three <- mesa_three %>%
    dplyr::rename(ID = mesaid, BMI = bmi3c, Age = age3c, Glucose = glucos3c) %>%
    dplyr::mutate(hypertension_med = htnmed3c, hypertension_his = htn3c)

  mesa_four <- mesa_four %>%
    dplyr::rename(ID = mesaid, BMI = bmi4c, Age = age4c, Glucose = glucos4c) %>%
    dplyr::mutate(hypertension_med = htnmed4c, hypertension_his = htn4c)

  mesa_five <- mesa_five %>%
    dplyr::rename(ID = MESAID, BMI = BMI5C, Age = AGE5C, Glucose = GLUCOSE5) %>%
    dplyr::mutate(hypertension_med = HTNMED5C, hypertension_his = HTN5C)

  # Ensure all datasets have a column to identify the exam
  mesa_one <- mesa_one %>% dplyr::mutate(Exam = 1)
  mesa_two <- mesa_two %>% dplyr::mutate(Exam = 2)
  mesa_three <- mesa_three %>% dplyr::mutate(Exam = 3)
  mesa_four <- mesa_four %>% dplyr::mutate(Exam = 4)
  mesa_five <- mesa_five %>% dplyr::mutate(Exam = 5)

  # Combine the datasets
  mesa_combined <- dplyr::bind_rows(mesa_one, mesa_two, mesa_three, mesa_four, mesa_five)

  # Select the relevant columns
  mesa_longitudinal <- mesa_combined %>%
    dplyr::select(ID, Exam, Age, BMI, Glucose, hypertension_med, hypertension_his)
}

