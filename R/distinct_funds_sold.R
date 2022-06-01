#' Get the number of distinct funds sold by an individual during some period
#'
#' Given an individual, as-of date and period of days, determine the number
#' of distinct funds that were sold by that individual during the peroid
#'
#' @param indiv_date_period Data frame of individuals, as of dates and periods from create_indiv_date_period
#' @param transactions Data frame of transactions, from a data warehouse query
#'
#' @return A data frame with individual id, date, period and #of distinct funds sold
#' @export
#'
#' @examples
#' \dontrun{
#' distinct_funds_sold(indiv_date_period, transactions)
#' }
distinct_funds_sold <- function(indiv_date_period,
                                transactions){
  indiv_date_period %>%
    #use summarise_history to count distinct funds per period
    summarise_history(history = {
      #Need to get transaction history at the portfolio, date & at_level level
      transactions %>%
        #need to modify transactions to work with summarise_activity
        dplyr::filter(grosssales > 0) %>%
        dplyr::rename(value = grosssales,
               metric = port_short_nm) %>%
        summarise_activity(id_map = id_map,
                           at_level = at_level,
                           using_level = transaction_asset_using_level) %>%
        dplyr::rename(grossales = value,
               value = metric) %>%
        dplyr::mutate(metric = "distinct_funds_sold") %>%
        dplyr::select(id, date, metric, value)
    }, summary_func = n_distinct)
}
