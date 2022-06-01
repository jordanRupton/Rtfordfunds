#' Easily connect to a SQL server by name
#'
#' @param server_name Name of the server to connect to (ods, mfdw, Xpressfeed, Factset...)
#' See Rtfordfunds::sql_servers for a complete list.
#'
#' @return An odbc connection
#' @export
#'
#' @examples
#' con <- easy_dbConnect("ods")
easy_dbConnect <- function(server_name){
  odbc::dbConnect(
    odbc::odbc(),
    Driver = "ODBC Driver 17 for SQL Server",
    Server = Rtfordfunds::sql_servers$server[Rtfordfunds::sql_servers$name == server_name],
    Database = Rtfordfunds::sql_servers$database[Rtfordfunds::sql_servers$name == server_name],
    trusted_connection = "yes"
  )
}
