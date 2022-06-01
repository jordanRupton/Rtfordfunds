#' Create records for every individual, date & period
#'
#' Given data frames for individuals, dates and periods,
#' create a data frame with a record for every combination.
#'
#' @param individuals Data frame with a single id column for each "individual" (person or pe)
#' @param dates Data frame with a single as_of column for when we want to see the data "as of"
#' @param periods Data frame of day windows relative to the as_of date
#'
#' @return A data frame with every combination of individual, date & period
#' @export
#'

create_indiv_date_period <- function(
  #individuals data frame captures the "who".
  #Records can be people, PEs, whatever, it's just a primary key.
  individuals = dplyr::tibble(id = letters[1:10]),

  #dates captures the "as of" date, that is... we want to see data
  #on the individuals as of when?
  dates = dplyr::tibble(as_of = seq.Date(Sys.Date() - 361,
                                  Sys.Date() - 1,
                                  30)
  ),

  #periods captures the date windows relative to the as_of dates.
  #For example, the window from 0 to 30 will be the
  #30 days leading up to the as of date.
  periods = dplyr::tibble(from = c(seq(0, 180, 30), 365),
                   to = c(seq(30, 180, 30), 365, 730),
                   period = paste("period", from, to, sep = "_"))
){
  individuals %>%
    tidyr::expand_grid(dates) %>%
    tidyr::expand_grid(periods) %>%
    dplyr::mutate(start_date = as_of - to,
           end_date = as_of - from) %>%
    dplyr::select(-from, -to)
}
