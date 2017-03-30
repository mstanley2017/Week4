library(dplyr)
library("tidyr")
library(maps)
setwd("C:\\Users\\RR500204\\Desktop\\data\\Assignment 4_Resubmit")
#'Displaying year-wise fatality summaries and representing them visually in the form of maps by
#'state
#'
#'The dataset used data from the US National Highway Traffic Safety Administration's
#'     Fatality Analysis Reporting System, which is a nationwide census providing the
#'     American public yearly data regarding fatal injuries suffered in motor vehicle
#'     traffic crashes
#'
#'  The function fars_read is used to evaluate the existance of a file and read in the
#'  information into a dataframe if the file exists
#'
#'
#'  @param to print the message saying the input file name does not exist if the file does not
#'  @param exist else it reads in csv file that is provided as input. All messages pertaining
#'    to the progress of the reading in of the csv file is suppresed. A dataframe is created to
#'    save the input that is read in
#'
#'  @return This function either returns a message the filename does not exist if the file name
#'     name does not exist or a dataframe containing the information if the file exists
#'
#'
#'
#'  @examples
#'  fars_read("accident_2013.csv.bz2")
#'  fars_read("accident_2017.csv.bz2")
#'
#'
#'  The make_filename function is used to create a filename using the year as an input and
#'  contantenating the text "accident",the year that was inputted and the extensions ".csv.bz2"
#'
#'  @param to print the filename by contantenating the text "accident",the numeric value of the
#'    year that was inputted and the extensions ".csv.bz2"
#'
#'  @return This function returns a character vector containing the formatted combination of text
#'     and variable values
#'
#'  @examples
#'  make_filename(2013)
#'  make_filename(2014)
#'
#'
#' The function fars_read_years is used to take the year as an input and create a filename
#' using make_filename function. The file is then read using the fars_read function. To which a
#' column called year is added using the mutate function. To use the mutate function the dplyr
#' package needs to be installed.The data is then subset by month and
#' year using the dplyr::select function. Trycatch provides a mechanism for handling unusual
#' conditions. If an error is generated, the error handler picks up the error generated and
#' displays a warning message "invalid year: " with the invalid year information. If no error is
#' generated, it displays "NULL". The entire function of creating the files, reading in the
#' details, adding a year column, subsetting the information is applied to all elements of the
#' list and returns a list using the lapply function
#'
#' @param takes a year as input,creates a filename, reads the information, adds a column for year then
#'    subsets the information, validates if there is an error and appropriately displays a warning
#'    message if an error occurs else returns "NULL"
#'
#'
#' @return If an error is generated a warning message "invalid year: " with
#'    the invalid year information is displayed. If no error is generated it displays "NULL"
#'
#' @examples
#' fars_read_years(2013)
#' fars_read_years(2014)
#'
#'
#' The fars_summarize_years function takes a year as an input and summarizes the number of
#' accidents by month within the input year specified
#'
#' @param to display a monthwise summary of the number of fatal injuries for the year
#'  specified
#'
#' @return A table displaying a month number and the consolidated number of fatal injuries for
#' each month of the specified year
#'
#' @examples
#' fars_summarize_years(2013)
#' fars_summarize_years(2014)
#'
#'
#'The fars_map_state function takes the state number and year as an input and displays a plot
#'of the state with the number of fatalaties mapped as dots at the appropriate location of the
#'incident. To be able to use this function ensure the maps package is installed
#'
#' @param To display a map plot of the state for the year specified with the dots for each fatal
#'    injury incident. Each state has a state number corresponding to it. The input to this
#'    function is the state number and the year. The data for the state is filtered and the
#'    number of rows is determined. Each fatal injury incident is plotted as dot within the map
#'    at their corresponding latitude and longitude
#'
#' @importFrom graphics points
#'
#'
#' @return This function returns map plot of the state for the year specified with the dots for
#'    each fatal injury incident with the map at their corresponding latitude and longitude of
#'    occurence
#'
#'
#' @examples
#' fars_map_state(1,2013)
#' fars_map_state(4,2013)
#'
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}
#' @export
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
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

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


