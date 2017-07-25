#' read Fatality Analysis Reporting System
#' 
#' This function reads a file from the Fatality Analysis Reporting 
#' System and returns its content as a data frame.
#' 
#' @param filename A valid file name to read the data from.
#'
#' @return This function returns the content of the read file in as a
#'         data frame.
#'
#' @note This function will create an error if the file does not exist.
#'
#' @examples
#' fars_data <- fars_read("accident_2013.csv.bz2")
#' fars_data <- fars_read(make_filename(2013))
#' 
#' @importFrom dplyr tbl_df
#' 
#' @export
#' 
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' make a valid file name
#' 
#' This function creates a valid Fatality Analysis Reporting System 
#' file name for a given year.
#'
#' @param year Year for which the file name should be created.
#'
#' @return This function returns a string containing the file name.
#'
#' @examples
#' fars_file_name <- make_filename(2013)
#' 
#' @export
#' 
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' read multiple Fatality Analysis Reporting System files
#' 
#' This function reads multiple files from the Fatality Analysis 
#' Reporting System. The files are organized in such a way that each
#' file contains the data of one year. 
#'
#' @param years Scalar year or vector of years to be read.
#'
#' @return This function returns a list (or a list of lists in the case
#'         that a vector of years is passed as a parameter) containing
#'         the data read from the corresponding files.
#' 
#' @examples
#' fars_data <- fars_read_years(2013)
#' fars_data <- fars_read_years(c(2013, 2014))
#' 
#' @importFrom dplyr mutate select 
#' @importFrom magrittr %>%
#' 
#' @export
#' 
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>% 
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' create a summary of multiple Fatality Analysis Reporting System files
#' 
#' This function loads the relevant Fatality Analysis Reporting System 
#' files and creates a summary of its data.
#'
#' @param years Scalar year or vector of years to be summarized.
#'
#' @return This function returns a data frame with a summary of the
#'         Fatality Analysis Reporting System data for the given years.
#'
#' @examples
#' fars_data <- fars_summarize_years(2013)
#' fars_data <- fars_summarize_years(c(2013, 2014))
#' 
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom magrittr %>%
#' @importFrom tidyr spread
#' 
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' create a map with the Fatality Analysis Reporting System data
#' 
#' This function loads the data from the Fatality Analysis Reporting 
#' System and creates a plot with the map of the given state and the 
#' fatalities of the given year.
#'
#' @param state.num Unique state number as in the Fatality Analysis 
#'                  Reporting System data.
#' @param year Year for which the plot should be produces.
#'
#' @return None.
#'
#' @note This function will create an error if the file for the given 
#'       year does not exist or if the state number is invalid.
#' 
#' @examples
#' fars_map_state(1, 2013)
#' 
#' @importFrom maps map
#' @importFrom graphics points
#' 
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
