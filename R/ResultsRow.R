#' ResultsRow
#'
#' A row from the 'results' slot in the GRANRepository object
#'
#' @param name Name of the package
#' @param building Logical indicating whether the package is building
#' @param status Status of the package build
#' @param version Package version
#' @param lastAttempt Last attempt time of package build
#' @param lastAttemptVersion Package version when build was last attempted
#' @param lastAttemptStatus Package build status of last attempt
#' @param lastbuilt Last built time of package, if successful
#' @param lastbuiltversion Last built version of package, if successful
#' @param lastbuiltstatus Last built status of package, if successful
#' @param buildReason The reason why the package build was attempted
#' @param maintainer Package maintainer
#' @param suspended Is the package suspended?
#' @return data.frame
#' @rdname repobuildparam
#' @export
ResultsRow <- function(name = NA_character_,
    building = TRUE,
    status = "ok",
    version = "0.0-0",
    lastAttempt = NA_character_,
    lastAttemptVersion = NA_character_,
    lastAttemptStatus = NA_character_,
    lastbuilt = NA_character_,
    lastbuiltversion = NA_character_,
    lastbuiltstatus = NA_character_,
    buildReason = NA_character_,
    maintainer  = NA_character_,
    suspended  = FALSE) {
    data.frame(name = name, status = status,
               version = version,
               lastAttempt = lastAttempt,
               lastAttemptStatus = lastAttemptStatus,
               lastAttemptVersion = lastAttemptVersion,
               lastbuilt = lastbuilt,
               lastbuiltversion = lastbuiltversion,
               lastbuiltstatus = lastbuiltstatus,
               maintainer = maintainer,
               suspended = suspended, building = building,
               stringsAsFactors = FALSE)
}
