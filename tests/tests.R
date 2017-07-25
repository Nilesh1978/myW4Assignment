library(testthat)

# check if the correct file name is returned
expect_that(make_filename(2013), matches("accident_2013.csv.bz2"))

# check if the correct data type is returned
expect_that(fars_read(make_filename(2013)), is_a("data.frame"))

# do some more elaborate testing here...