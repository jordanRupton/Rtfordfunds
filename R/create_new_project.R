#' Create a new RStudio project with sub-directories
#'
#' @param proj The name of the new project
#' @param path The path of the directory where the project folder will be created.
#' Defaults to the Analytics directory.
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#'   create_new_project("my_test_project")
#' }
create_new_project <- function(proj,
                               path = "//AD1WAYFPS001/Analytics") {

  #create the directory
  dir.create(paste0(path, "/", proj))

  #create all of the sub-folders
  folders <- c("code", "csv", "data", "documentation", "plots", "queries", "slides")
  for(i in folders){
    dir.create(paste0(path, "/", proj, "/", i))
  }

  #create the R project file
  x <- c("Version: 1.0", "", "RestoreWorkspace: Default", "SaveWorkspace: Default",
         "AlwaysSaveHistory: Default", "", "EnableCodeIndexing: Yes",
         "UseSpacesForTab: Yes", "NumSpacesForTab: 4", "Encoding: UTF-8",
         "", "RnwWeave: knitr", "LaTeX: pdfLaTeX")

  cat(paste(x, collapse="\n"),
      file=paste0(path, "/", proj, "/", proj, ".Rproj"))

  message(paste(proj, "has been created!"))
}
