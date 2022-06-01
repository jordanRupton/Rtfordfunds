#' Combine activity & transactions into a single history table
#'
#' This creates a "newsfeed" like table at either the person or PE level
#'
#' @param activity Data frame of activities
#' @param transactions Data frame of transactions
#' @param at_level What level do we want results at (person or pe)?
#' @param activity_using_level What level do we want use to group activities?
#' @param transaction_using_level What level do we want use to group transactions?
#'
#' @return The history data frame
#' @export
#'
#' @examples
#' \dontrun{
#' history <- activity %>%
#'     create_history(transactions,
#'                    at_level = "person",
#'                    activity_using_level = "person",
#'                    transaction_using_level = "pe")
#'}
create_history <- function(activity,
                           transactions,
                           at_level = c("person", "pe"),
                           activity_using_level = c("person", "pe"),
                           transaction_using_level = c("person", "pe")){
  activity %>%
    #need some minor tweaks before feeding into the function
    dplyr::rename(metric = type) %>%
    dplyr::mutate(value = 1) %>%
    summarise_activity(id_map = id_map,
                       at_level = at_level,
                       using_level = activity_using_level) %>%
    dplyr::bind_rows({
      transactions %>%
        #pivot before feeding into function
        tidyr::pivot_longer(cols = c(grosssales, redemptions),
                            names_to = "metric",
                            values_to = "value") %>%
        dplyr::filter(value > 0) %>%
        summarise_activity(id_map = id_map,
                           at_level = at_level,
                           using_level = transaction_using_level)
    })
}
