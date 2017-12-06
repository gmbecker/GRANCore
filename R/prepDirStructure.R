#' prepDirStructure
#'
#' Initialize the directory structure for the GRAN repo
#'
#' @param basedir The base directory. By default the temporary repository,
#' temporary install library, and package staging area will be located in
#' <basedir>/<subrepoName>/, while the  temporary source checkout will be in t
#' he basedir itself.
#' @param repo_name The name of the repository, e.g. stable or devel
#' @param temp_repo Location to create the temporary repository
#' @param temp_checkout Location to create temporary checkouts/copies of package
#'   source code
#' @param tempLibLoc Location to create the temporary installed package library
#'   for use during the testing process
#' @param destination Base location (not including repository name) of the
#'   final repository to be built
#' @export
prepDirStructure <- function(basedir, repo_name, temp_repo, temp_checkout,
                             tempLibLoc, destination) {
    if (!file.exists(file.path(basedir, repo_name, "src", "contrib", "Archive")))
        dir.create(file.path(basedir, repo_name, "src", "contrib", "Archive"),
                   recursive=TRUE)
    if (!file.exists(file.path(temp_repo, "src", "contrib", "Archive")))
        dir.create(file.path(temp_repo, "src", "contrib", "Archive"),
                   recursive = TRUE)
    if (!file.exists(temp_checkout))
        dir.create(temp_checkout, recursive = TRUE)
    if (!file.exists(tempLibLoc))
        dir.create(tempLibLoc, recursive = TRUE)
    if (!file.exists(file.path(basedir, repo_name, "staging")))
        dir.create(file.path(basedir, repo_name, "staging"),
                   recursive = TRUE)
    if (!file.exists(file.path(destination, repo_name, "src", "contrib")))
        dir.create(file.path(destination, repo_name, "src", "contrib"),
                   recursive = TRUE)
    if (!file.exists(file.path(destination, repo_name, "CheckResults")))
        dir.create(file.path(destination, repo_name, "CheckResults"),
                   recursive = TRUE)
    chklogs_as_html <- file.path(destination, repo_name, "CheckResults", ".htaccess")
    if (!file.exists(chklogs_as_html)) {
      cat("AddType text/html .log", file = chklogs_as_html)
    }
    if (!file.exists(file.path(destination, repo_name, "SinglePkgLogs")))
        dir.create(file.path(destination, repo_name, "SinglePkgLogs"),
                   recursive = TRUE)
    # If you're running an Apache HTTP server for your repo, this is handy
    pkglogs_as_html <- file.path(destination, repo_name, "SinglePkgLogs", ".htaccess")
    if (!file.exists(pkglogs_as_html)) {
      cat("AddType text/html .log", file = pkglogs_as_html)
    }
    if (!file.exists(file.path(destination, repo_name, "CovrReports")))
        dir.create(file.path(destination, repo_name, "CovrReports"),
                   recursive = TRUE)
    if (!file.exists(file.path(destination, repo_name, "InstallResults")))
        dir.create(file.path(destination, repo_name, "InstallResults"),
                   recursive = TRUE)
    instlogs_as_html <- file.path(destination, repo_name, "InstallResults", ".htaccess")
    if (!file.exists(instlogs_as_html)) {
      cat("AddType text/html .log", file = instlogs_as_html)
    }
    if (!file.exists(file.path(destination, repo_name, "PkgDocumentation")))
        dir.create(file.path(destination, repo_name, "PkgDocumentation"),
                   recursive = TRUE)
}
