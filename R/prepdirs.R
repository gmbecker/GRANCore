#' prepDirStructure
#'
#' Initialize the directory structure for the GRAN repo
#'
#' @param basedir The base directory. By default the temporary repository,
#' temporary install library, and package staging area will be located in
#' <basedir>/<subrepoName>/, while the  temporary source checkout will be in
#' the basedir itself.
#' @param repo_name The name of the repository, e.g. stable or devel
#' @param temp_repo Location to create the temporary repository
#' @param temp_checkout Location to create temporary checkouts/copies of package
#'   source code
#' @param tempLibLoc Location to create the temporary installed package library
#'   for use during the testing process
#' @param destination Base location (not including repository name) of the
#'   final repository to be built
#' @param logfile Comprehensive GRAN log file
#' @param errlogfile Error log file
#' @note This function is not intended for use by the end user.
#' @export
prepDirStructure <- function(basedir, repo_name, temp_repo, temp_checkout,
                             tempLibLoc, destination, logfile, errlogfile) {
    # Vector containing dir paths
    dir_list <- c(
        file.path(basedir, repo_name, "src", "contrib", "Archive"),
        file.path(basedir, repo_name, "staging"),
        file.path(temp_repo, "src", "contrib", "Archive"),
        temp_checkout,
        tempLibLoc,
        file.path(destination, repo_name, "src", "contrib", "Archive"),
        file.path(destination, repo_name, "src", "contrib", "Meta"),
        file.path(destination, repo_name, "CheckResults"),
        file.path(destination, repo_name, "SinglePkgLogs"),
        file.path(destination, repo_name, "CovrReports"),
        file.path(destination, repo_name, "InstallResults"),
        file.path(destination, repo_name, "PkgDocumentation"),
        dirname(logfile),
        dirname(errlogfile)
    )

    # Create the directories
    for (dir in dir_list) {
        .createDir(dir)
    }

    # If Windows machine, create directory where binaries will be installed
    if (.Platform$OS.type == "windows") {
        .createDir(file.path(destination, repo_name, "bin", "windows"))
    }

    .touchFile(file.path(temp_repo, "src", "contrib", "PACKAGES"))
    .touchFile(file.path(destination, repo_name, "src", "contrib", "PACKAGES"))
    .touchFile(logfile)
    .touchFile(errlogfile)
}

.createDir <- function(dir, makehtaccess = TRUE) {
    if (!file.exists(dir))
        dir.create(dir, recursive = TRUE)
    if (makehtaccess) {
        # If you're running an Apache HTTP server for your repo, this will
        # enable logs to be viewed as HTML
        if (!file.exists(file.path(dir, ".htaccess"))) {
            cat("AddType text/html .log .md", file = file.path(dir, ".htaccess"))
        }
    }
}

.touchFile <- function(fil) {
    if (!file.exists(fil))
        file.create(fil)
}
