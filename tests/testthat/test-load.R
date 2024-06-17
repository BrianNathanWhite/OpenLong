

mesa_init <- OpenLongMesa(filepath = 'data-raw/sensitive/MESA')

test_that(
  desc = "loaded data",
  code = {
    expect_no_error(load(mesa_init))
  }
)
