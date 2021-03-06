% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{create a map with the Fatality Analysis Reporting System data}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{Unique state number as in the Fatality Analysis
Reporting System data.}

\item{year}{Year for which the plot should be produces.}
}
\value{
None.
}
\description{
This function loads the data from the Fatality Analysis Reporting
System and creates a plot with the map of the given state and the
fatalities of the given year.
}
\note{
This function will create an error if the file for the given
      year does not exist or if the state number is invalid.
}
\examples{
\dontrun{
fars_map_state(1, 2013)
}

}
