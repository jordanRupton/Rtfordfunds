#' Get the number of days since last activity
#'
#' Given a data frame of individuals and as-of dates and another
#' data frame with a history of activities, find the number of days
#' since the last occurance of each activity for each inidividual and as-of date.
#'
#' @param indiv_date Data frame with a column for individual id's and a column of as-of dates
#' @param history Data frame generated using the create_history function
#' @import data.table
#'
#' @return A data frame with columns for each activity type noting the days since last occurance.  If there was no previous occurance, the value is NA.
#' @export
#'
#' @examples
#' \dontrun{
#' days_since_last(indiv_date, history)
#' }
days_since_last <- function(indiv_date,
                            history){

  #Get all activities prior to the as of date
  data.table::as.data.table(history)[data.table::as.data.table(indiv_date),
                         .(id, as_of, metric, hist_date = x.date),
                         on = .(id, date < as_of),
                         allow.cartesian = TRUE] %>%
    tibble::as_tibble() %>%
    #For each id/date/metric, find the most recent occurance and take the differnce in days
    dtplyr::lazy_dt() %>%
    dplyr::group_by(id, as_of, metric) %>%
    dplyr::summarise(most_recent = max(hist_date)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(days_since_last = as.numeric(as_of - most_recent),
           metric = paste0("days_since_last_", metric)) %>%
    dplyr::select(-most_recent) %>%
    tibble::as_tibble() %>%
    #pivot from long to wide
    tidyr::spread(key = metric, value = days_since_last) %>%
    dplyr::select(-contains("_NA"))

}
