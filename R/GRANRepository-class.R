#' @import switchr
#' @import methods
NULL


#' @rdname repobuildparam
#' @export
setClass("RepoBuildParam",
         representation(repo_name = "character",
                        temp_repo = "character",
                        base_dir = "character",
                        temp_checkout = "character",
                        errlog = "character",
                        logfile = "character",
                        tempLibLoc = "character",
                        check_warn_ok = "logical",
                        check_note_ok = "logical",
                        extra_fun = "function",
                        auth = "character",
                        dest_base = "character",
                        dest_url = "character",
                        install_test = "logical",
                        check_test = "logical",
                        suspended = "character",
                        use_cran_granbase = "logical",
                        build_timeout = "numeric",
                        check_timeout = "numeric",
                        email_notifications = "logical",
                        email_opts = "list",
                        repo_archive = "character",
                        repo_metadata_dir = "character",
                        make_windows_bins = "logical",
			platform = "character",
			r_version = "character",
			bioc_version = "character"),
         prototype = prototype(use_cran_granbase = TRUE,
                               build_timeout = 10*60,
                               check_timeout = 15*60,
                               email_notifications = FALSE,
                               email_opts = list(smtp_server = "localhost",
                                                 smtp_port = 25,
                                                 sender_email = "gran@localhost",
                                                 unsubscribers = NULL),
                               make_windows_bins = TRUE,
			       platform = "",
			       r_version = "",
			       bioc_version = ""),
         contains = "SwitchrParam")


#' @rdname GRANRepository
#' @export
setClass("GRANRepository", representation(
    results = "data.frame",
    manifest = "SessionManifest",
    param = "RepoBuildParam"
))
