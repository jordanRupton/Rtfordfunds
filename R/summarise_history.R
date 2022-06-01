#' Get summary metrics for individuals across various dates and periods
#'
#' Given the "history" data frame (which is a newsfeed of past activities and transactions),
#' get a sum, distinct count or whatever of each type of activity which
#' occurred in the period window for that individual and as-of date.
#'
#' @param indiv_date_period Data frame of individuals, as of dates and periods from create_indiv_date_period
#' @param history Data frame of past activities and transactions from create_history
#' @param summary_func The function we want to use to summarise
#' @param value_fill How should any missing values be filled?
#' @param force_all_columns Do we want to ensure a column is created for every metric/period, even if no one has any values?
#' @import data.table
#'
#' @return A data frame with a row for every individual and as-of date, and a column for every metric/period
#' @export
#'

summarise_history <- function(indiv_date_period,
                              history,
                              summary_func = sum,
                              value_fill = 0,
                              force_all_columns = FALSE){

  result <- data.table::as.data.table(history)[data.table::as.data.table(indiv_date_period),
                                   on = .(id, date >= start_date, date < end_date),
                                   allow.cartesian = TRUE] %>%
    #annoyingly, in order to use dtplyr, we first have to convert back to a data frame
    dplyr::as_tibble() %>%
    #and then signify that we want to start using dtplyr
    dtplyr::lazy_dt() %>%
    dplyr::mutate(metric_period = paste0(metric, "_", period)) %>%
    dplyr::select(id, as_of, metric_period, value) %>%
    dplyr::group_by(id, as_of, metric_period) %>%
    dplyr::summarise(value = summary_func(value)) %>%
    dplyr::ungroup() %>%
    #converting back to a tibble tells dtplyr that we're done, so it translates everything
    #above to data.table and executes it all at once
    dplyr::as_tibble() %>%
    #Pivot to have a column for each metric_period
    #pivot_wider wasn't working with dtplyr and spread is faster in dplyr
    tidyr::spread(key = metric_period, value = value, fill = value_fill) %>%
    #Advisors that had no activites in any of the periods will have Metric_Period containing "NA_".
    #We don't want to filter them out, so instead we'll just unselect the column
    #after the spread.
    dplyr::select(-contains("NA_"))


  #If no individual has any instance of a metric_period, then the column
  #won't be created.  If we want to force all columns to be created,
  #append on the missing ones
  if(force_all_columns){
    missing_columns <- history %>%
      dplyr::distinct(metric) %>%
      tidyr::expand_grid({
        indiv_date_period %>%
          dplyr::distinct(period)
      }) %>%
      tidyr::unite(metric_period, metric, period) %>%
      dplyr::mutate(value = 0) %>%
      #remove any columns that are already in the result tibble
      dplyr::anti_join({
        result %>%
          colnames() %>%
          tibble::enframe(name = NULL, value = "metric_period")
      }, by = "metric_period") %>%
      tidyr::pivot_wider(names_from = metric_period,
                  values_from = value)
    #create all of the rows with 0's
    missing_columns[1:nrow(result), ] <- 0

    #bind the missing columns to result
    result <- result %>%
      dplyr::bind_cols(missing_columns)
  }

  return(result)
}
