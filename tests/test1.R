library(testthat)

library(finalassignment)

testthat::expect_that(make_filename(2012),
                      is_identical_to("accident_2012.csv.bz2"))

testthat::expect_that(make_filename("2017"),
                      is_identical_to("accident_2017.csv.bz2"))