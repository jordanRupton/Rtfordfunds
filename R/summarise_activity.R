#' Summarise tables at various possible levels
#'
#' Given a data frame such as activity/transaction/asset with person and/or pe ids,
#' summarise values to various possible levels.
#'
#' @param df The data frame with some value to be summarised
#' @param id_map A two-column data frame mapping dim_pers_id to dim_prod_entty_id
#' @param at_level What level do we want the final results at?
#' @param using_level What level do we want to use to summarise?
#' @param group_vars Other than person/pe, what other grouping variables do we need?
#' @param count_distinct Do we also want to count the distinct values of something?
#'
#' @return A data frame at the at-level & group_vars level, with the sum of a value (and maybe distinct count of some attribute)
#' @export
#'
#' @examples
#' \dontrun{
#' #Create the id_map
#' id_map <- person %>%
#'   filter(!is.na(dim_prod_entty_id)) %>%
#'   select(dim_pers_id, dim_prod_entty_id)
#'
#' #Count of activities at the person-date-type level
#' activity %>%
#'     rename(metric = type) %>%
#'     mutate(value = 1) %>%
#'     summarise_activity(id_map = id_map,
#'                        at_level = "person",
#'                        using_level = "person",
#'                        group_vars = c("date", "metric"))
#'
#' #Sum transactions at the PE level, then redistribute back to the person level
#' transactions %>%
#' #pivot before feeding into function
#' pivot_longer(cols = c(grosssales, redemptions),
#'              names_to = "metric",
#'              values_to = "value") %>%
#'   filter(value > 0) %>%
#'   summarise_activity(id_map = id_map,
#'                      at_level = "person",
#'                      using_level = "pe")
#'}

summarise_activity <- function(df,
                               id_map = id_map,
                               #What level do we want the final results at?
                               at_level = c('person', 'pe'),
                               #What level do we want to use to summarise?
                               using_level = c('person', 'pe'),
                               #Other than person/pe, what other grouping variables do we need?
                               group_vars = c("date", "metric"),
                               #do we want to count the distinct values of something?
                               count_distinct = NULL){
  if(at_level == "pe" & using_level == "person"){
    stop("Can't have person-level metrics at the pe level!")
  }


  #if we're doing anything at the pe level, then append dim_prod_entty_id
  if(at_level == "pe" | using_level == "pe"){
    df <- df %>%
      dplyr::left_join(id_map, by = "dim_pers_id")
  }

  #group according to the using_level
  if(using_level == "person"){
    df <- df %>%
      dplyr::group_by_at(c("dim_pers_id", group_vars))
  }else{
    df <- df %>%
      dplyr::group_by_at(c("dim_prod_entty_id", group_vars))
  }

  #Summarise; count distinct values of a variable if provided
  if(is.null(count_distinct)){
    df <- df %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup()
  }else{
    df <- df %>%
      dplyr::summarise(value = sum(value),
                count_distinct = dplyr::n_distinct(!!rlang::sym(count_distinct))) %>%
      dplyr::ungroup()
  }


  #if we're using the pe level but want results at the person level,
  #append person id back on.  This is giving every person in the pe
  #"credit" for every activity/transaction in the pe.
  if(at_level == "person" & using_level == "pe"){
    df <- df %>%
      dplyr::inner_join(id_map, by = "dim_prod_entty_id") %>%
      dplyr::select(-dim_prod_entty_id)
  }

  #Rename the primary key depending on the at_level
  if(at_level == "person"){
    df <- df %>%
      dplyr::rename(id = dim_pers_id)
  }else{
    df <- df %>%
      dplyr::rename(id = dim_prod_entty_id)
  }

  return(df)
}
