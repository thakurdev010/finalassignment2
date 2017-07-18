#' Function
#' Load and print data set
#' function that loads and print data set and check if it exist in R directory.
#'
#' @param filename name of the file
#'
#' @return return the structure of file loaded as data frame
#' @export
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @examples
#' \dontrun{
#' fars_read("C:\\data\\accident_2013.csv")
#' }


fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Function
#' Accident Data set with year
#' This function gives the name of an accident dataset with "accident_year.csv.bz2" structure.
#'
#' @param year year of choice
#'
#' @return return to the chosen year with the dataset
#' @export
#'
#' @examples 
#' \dontrun{
#' make_filename(2014)
#' make_filename("2018")
#' }

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Function
#' list of accident dataset for selected years
#' This function gives list of accident datasets with year.csv.bz2 structure
#'
#' @param years vector or list of selected dataset in month-year format
#'
#' @return returns to names accident datasets with the selected year in month-year format
#' @export
#' @importFrom  dplyr mutate
#' @importFrom  dplyr select
#'
#' @examples 
#' \dontrun{
#' fars_read_years(2013, 2014, 2015)
#' fars_read_years("2013")
#' }

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

#' Function
#' Summary statistics of accident each month of selected year
#' Gives summary statistics by year and month
#'
#' @param years selected years
#'
#' @return returns to a table with summary of accidents in each month of selected year
#' @export
#' @importFrom tidyr spread
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @import magrittr
#' 
#' @examples 
#' \dontrun{
#' fars_summarize_years(c(2013, 2014, 2015))
#' fars_summarize_years("2013")
#' }

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Function
#' Map of accident.
#' This function indicates which location is prone to which accident
#'
#' @param state.num selected state
#' @param year selected year
#'
#' @return returns to map which shows the places where accident happned
#' @export
#' @importFrom graphics points
#' @importFrom maps map
#' @importFrom dplyr filter
#'
#' @examples 
#' \dontrun{
#' fars_map_state(13, 2013)
#' fars_map_state("4", "2015")
#' }

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

