

mesa_init <- OpenLongMesa(filepath = here::here('data-raw/sensitive/MESA'))

mesa_loaded <- data_load(mesa_init)

run_tests_type_stability <- function(x){

  cmp <- get_components(x)

  # assert each component is a list
  expect_true(is.list(cmp$baseline))
  expect_true(is.list(cmp$longitudinal))

  # assert each item in the baseline components is a dataframe
  for(i in seq_along(cmp$baseline)){
    expect_s3_class(cmp$baseline[[i]], class = 'data.frame')
  }
  # same assertion for longitudinal components
  for(i in seq_along(cmp$longitudinal)){
    expect_s3_class(cmp$longitudinal[[i]], class = 'data.frame')
  }

}

test_that(
  desc = "assert type stability for MESA",
  code = {
    run_tests_type_stability(mesa_loaded)
  }
)


aric_init <- OpenLongAric(filepath = here::here('data-raw/sensitive/ARIC'))

aric_loaded <- data_load(aric_init)

test_that(
  desc = "assert type stability for ARIC",
  code = {
    run_tests_type_stability(aric_loaded)
  }
)



