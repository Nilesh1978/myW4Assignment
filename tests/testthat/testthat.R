#' run some tests
#'
#' @examples
#' \dontrun{
#' fars_map_state(1, 2013)
#' }
#'
#' @importFrom maps map
#' @importFrom graphics points

library(testthat)

# set path to data files
mypath <- system.file("extdata", package = "myW4Assignment")
setwd(mypath)

# check if the correct file name is returned is_a
expect_that(make_filename(2013), matches("accident_2013.csv.bz2"))

# check if the correct data type is returned
expect_that(fars_read(make_filename(2013)), is_a("data.frame"))

# do some more elaborate testing here...
