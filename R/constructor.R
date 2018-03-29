#' GRANRepository
#'
#' A constructor for the \code{GRANRepository} class of S4 objects representing
#' individual repositories
#'
#' @param manifest A PkgManifest object
#' @param results A data.frame containing previous build results
#' @param param A RepoBuildParam object controlling the location and behavior of
#' the repository being built
#' @param ... Passed through to the default value of \code{param}
#' @rdname GRANRepository
#' @export
GRANRepository <- function(manifest, results, param = RepoBuildParam(...), ...) {

    if(missing(results))
        results = ResultsRow(name = manifest_df(manifest)$name)
    if(is(manifest, "PkgManifest"))
        manifest = SessionManifest(manifest = manifest,
                    versions = data.frame(name = manifest_df(manifest)$name,
                    version = NA_character_,
                    stringsAsFactors = FALSE))

    new("GRANRepository", manifest = manifest, results = results, param = param)
}
