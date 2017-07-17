#' Function
#' Load and print data set
#' function that loads and print data set and check if it exist in R directory.
#'
#' @param filename name of the file
#'
#' @return return the structure of file loaded as data frame
#' @export
#' @importFrom reader read_csv
#' @importFrom dplyr tbl_df
#' @examples fars_read("accident_2014.csv.bz2")

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
#' @examples makefile_name(2013)
#'
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
#' @examples fars_read_year(2013)
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
#' @importFrom dplyr
#' @importFrom magrittr
#'
#' @examples fars_summarize_years(2013)
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
#' @importFrom maps maps
#' @importFrom dplyr filter
#'
#' @examples fars_map_state(1,2014)
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