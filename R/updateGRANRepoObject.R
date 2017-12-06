#' updateGRANRepoObject
#'
#' Update the GRAN repo object with newer information
#'
#' @param object The GRAN repo object
#' @param ... Other parameters from RepoBuildParam
#' @export
updateGRANRepoObject <- function(object, ...) {
    param = RepoBuildParam(basedir = object@baseDir,
                           temp_repo = object@tempRepo,
                           repo_name = object@subrepoName,
                           errlog = object@errlog,
                           logfile = object@logfile,
                           temp_checkout = object@tempCheckout,
                           check_note_ok = object@checkNoteOk,
                           check_warn_ok = object@checkWarnOk,
                           tempLibLoc = object@tempLibLoc,
                           extra_fun = object@extraFun,
                           destination = object@dest_base,
                           dest_url = object@dest_url,
                           shell_init = object@shell_init,
                           auth = object@auth,
                           email_notifications = object@email_notifications,
                           email_opts = object@email_opts,
                           ...)

    man = PkgManifest(manifest = object@manifest[,names(ManifestRow())])
    results = data.frame(name = manifest_df(man)$name,
            object@manifest[,!names(object@manifest) %in% names(ManifestRow())],
            stringsAsFactors = FALSE)
    vers = data.frame(name = manifest_df(man)$name, version = NA_character_,
                      stringsAsFactors = FALSE)
    GRANRepository(manifest = SessionManifest(manifest =man, versions = vers),
                   results = results,
                   param = param)
}
