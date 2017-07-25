## ----gh-installation, eval = FALSE---------------------------------------
#  # install.packages("devtools")
#  devtools::install_github('monstrap/myW4Assignment')

## ----options, results='hide'---------------------------------------------
mypath <- system.file("extdata", package = "myW4Assignment")
knitr::opts_knit$set(root.dir = mypath)

## ----map_example, eval=FALSE---------------------------------------------
#  knitr::opts_knit$set(root.dir = "../inst/extdata")
#  fars_map_state(1, 2013)

## ----summary_example, eval=FALSE-----------------------------------------
#  fars_summarize_years(2013)

