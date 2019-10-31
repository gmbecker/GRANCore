
#' Accessors
#'
#' Set or retrieve the relevant values from a GRANRepository object
#'
#' @docType methods
#' @aliases logfile-method,GRANRepository
#' @rdname GRANRepository-accessors
#' @title Log file location of a GRAN (sub) repository
#' @param repo a GRANRepository object
#' @return file location of the full logfile
#' @examples
#' repo = GRANRepository(GithubManifest("gmbecker/fastdigest"), basedir = tempdir())
#' # parameter object
#' param(repo)
#' ##fundamental sub-objects
#' manifest(repo)
#' repo_results(repo)
#' ##important directories
#' repobase(repo)
#' staging(repo)
#' temp_lib(repo)
#' notrack(repo)
#' destination(repo)
#' dest_base(repo)
#' windowsbindir(repo)
#' archivedir(repo)
#' metadatadir(repo)
#' check_result_dir(repo)
#' backup_archive(repo)
#' coverage_report_dir(repo)
#' pkg_doc_dir(repo)
#' install_result_dir(repo)
#' repo_url(repo)
#' checkout_dir(repo)
#' ## logging
#' logfile(repo)
#' errlogfile(repo)
#' staging_logs(repo)
#' pkg_log_dir(repo)
#' pkg_log_file("switchr", repo)
#' ## email and other behavior
#' email_options(repo)
#' email_notify(repo)
#' ## miscellaneous
#' make_windows_bins(repo)
#' use_cran_granbase(repo)
#' check_timeout(repo)
#' build_timeout(repo)
#' platform(repo)
#' r_version(repo)
#' bioc_version(repo)
#' 
#' @export
setGeneric("logfile", function(repo) standardGeneric("logfile"))
#' @rdname GRANRepository-accessors
#' @aliases logfile,GRANRepository-method
setMethod("logfile", "GRANRepository", function(repo) {
    ret = param(repo)@logfile
    .createDir(dirname(ret))
    ret})

#' @rdname GRANRepository-accessors
#' @aliases logfile,RepoBuildParam-method
setMethod("logfile", "RepoBuildParam", function(repo) repo@logfile)

#' errlogfile
#' Retrieve the path to the errors-only logfile for a GRAN repository
#'
#' @docType methods
#' @rdname errlogfile-methods
#' @title Log file location of a GRAN (sub) repository
#' @param repo a GRANRepository object
#' @return file location of the errors-only logfile
#' @export
setGeneric("errlogfile", function(repo) standardGeneric("errlogfile"))
#' @rdname errlogfile-methods
#' @aliases errlogfile,GRANRepository-method
setMethod("errlogfile", "GRANRepository", function(repo) {
        ret = param(repo)@errlog
        .createDir(dirname(ret))
        ret})

#' @rdname errlogfile-methods
#' @aliases errlogfile,RepoBuildParam-method
setMethod("errlogfile", "RepoBuildParam", function(repo) repo@errlog)

#' email_options
#' Email options for sending build failure notifications
#'
#' @rdname email_options-methods
#' @param repo a GRANRepository object
#' @return A list containing the email options
#' @docType methods
#' @export
setGeneric("email_options", function(repo) standardGeneric("email_options"))
#' @rdname email_options-methods
#' @aliases email_options,GRANRepository-method
#' @export
setMethod("email_options", "GRANRepository",
          function(repo) param(repo)@email_opts)


#' email_notify
#' Should emails be sent for build failure notifications?
#'
#' @rdname email_notify-methods
#' @param repo a GRANRepository object
#' @return logical
#' @docType methods
#' @export
setGeneric("email_notify", function(repo) standardGeneric("email_notify"))
#' @rdname email_notify-methods
#' @aliases email_notify,GRANRepository-method
#' @export
setMethod("email_notify", "GRANRepository",
          function(repo) param(repo)@email_notifications)


#'repobase
#' Generic accessor function to retreive the repo specific subdirectory within the base directory
#'
#' @rdname repobase-methods
#' @param repo a GRANRepository object
#' @return The path to the repository specific directory
#' @export
setGeneric("repobase", function(repo) standardGeneric("repobase"))
#' @rdname repobase-methods
#' @aliases repobase,GRANRepository-method
#' @export
setMethod("repobase", "GRANRepository", function(repo) {
    ret <- file.path(param(repo)@base_dir, param(repo)@repo_name)
    .createDir(ret)
    normalizePath2(ret)
})


#' staging
#' Return the staging directory or the staging_logs to be used when building
#' the repository. If the directory does not exist it will be created.
#'
#' @rdname staging-methods
#' @param repo a GRANRepository object
#' @return The path to the repository specific directory
#' @export
setGeneric("staging", function(repo) standardGeneric("staging"))
#' @rdname staging-methods
#' @aliases staging,GRANRepository-method
#' @export
setMethod("staging", "GRANRepository", function(repo) {
    ret <- file.path(repobase(repo), "staging")
    .createDir(ret)
    normalizePath2(ret)
})

#' @rdname staging-methods
#' @return The path to the repository specific directory
#' @export
setGeneric("staging_logs", function(repo) standardGeneric("staging_logs"))
#' @rdname staging-methods
#' @aliases staging_logs,GRANRepository-method
#' @export
setMethod("staging_logs", "GRANRepository", function(repo) {
    ret <- file.path(staging(repo), "logs")
    .createDir(ret)
    normalizePath2(ret)
})


#' temporary library
#'
#' @param repo A GRANRepository object
#' @docType methods
#' @rdname templib
#' @export
setGeneric("temp_lib", function(repo) standardGeneric("temp_lib"))
#' @rdname templib
#' @aliases temp_lib,GRANRepository
setMethod("temp_lib", "GRANRepository",
          function(repo) normalizePath2(param(repo)@tempLibLoc))

#' notrack
#' Return the directory which stores retreived versions of non-GRAN packages
#' for use in virtual repositories
#'
#' @param repo a GRANRepository object
#' @return The path to the notrack directory
#' @rdname notrack-methods
#' @aliases notrack,GRANRepository-method
#' @docType methods
setMethod("notrack", "GRANRepository",
          function(repo) file.path(repobase(repo), "notrack"))

#' destination
#' Return the full path to the contrib directory for the final repository
#' deployment.
#'
#' @rdname destination-methods
#' @param repo a GRANRepository object
#' @return For destination, the full path to the contrib directory the packages
#' will be deployed to
#' @docType methods
#' @export
setGeneric("destination", function(repo) standardGeneric("destination"))
#' @rdname destination-methods
#' @aliases destination,GRANRepository-method
#' @export
setMethod("destination", "GRANRepository",
          function(repo) {
              ret <- file.path(normalizePath2(param(repo)@dest_base),
                                   param(repo)@repo_name,  "src", "contrib")
              .createDir(ret)
              ret
})


#' make_windows_bins
#' Return the logical that determines whether to build Windows binaries
#'
#' @rdname make_windows_bins-methods
#' @param repo a GRANRepository object
#' @return Logical indicating whether Windows binaries will be built
#' @docType methods
#' @export
setGeneric("make_windows_bins", function(repo) standardGeneric("make_windows_bins"))
#' @rdname make_windows_bins-methods
#' @aliases make_windows_bins,GRANRepository-method
#' @export
setMethod("make_windows_bins", "GRANRepository",
          function(repo) param(repo)@make_windows_bins )

#' windowsbindir
#' Return the full path to the location of the windows binary builds
#'
#' @rdname windowsbindir-methods
#' @param repo a GRANRepository object
#' @return Full path to the location of the windows binary builds
#' @docType methods
#' @export
setGeneric("windowsbindir", function(repo) standardGeneric("windowsbindir"))
#' @rdname windowsbindir-methods
#' @aliases windowsbindir,GRANRepository-method
#' @export
setMethod("windowsbindir", "GRANRepository",
          function(repo) {
              ret <- file.path(normalizePath2(param(repo)@dest_base),
                                       param(repo)@repo_name,  "bin", "windows")
              .createDir(ret)
              ret
          })

#' archivedir
#' Return the full path to the archive directory for the final repository
#' deployment.
#'
#' @rdname archivedir-methods
#' @param repo a GRANRepository object
#' @return The full path to the archive directory where the archived packages
#' will be deployed to
#' @docType methods
#' @export
setGeneric("archivedir", function(repo) standardGeneric("archivedir"))
#' @rdname archivedir-methods
#' @aliases archivedir,GRANRepository-method
#' @export
setMethod("archivedir", "GRANRepository",
          function(repo) {
            archive_dir <- param(repo)@repo_archive
            if (is.null(archive_dir) || identical(archive_dir, character(0))) {
              archive_dir <- file.path(destination(repo), "Archive")
            }
            .createDir(archive_dir)
            return(archive_dir)
})

#' metadatadir
#' Return the full path to the pkg metadata directory for the final repository
#' deployment.
#'
#' @rdname metadatadir-methods
#' @param repo a GRANRepository object
#' @return The full path to the metadata directory
#' @docType methods
#' @export
setGeneric("metadatadir", function(repo) standardGeneric("metadatadir"))
#' @rdname metadatadir-methods
#' @aliases metadatadir,GRANRepository-method
#' @export
setMethod("metadatadir", "GRANRepository",
          function(repo) {
            metadata_dir <- param(repo)@repo_metadata_dir
            if (is.null(metadata_dir) || identical(metadata_dir, character(0))) {
              metadata_dir <- file.path(destination(repo), "Meta")
            }
            .createDir(metadata_dir)
            return(metadata_dir)
})

#' dest_base
#' Return the full path to the contrib directory for the final repository
#' deployment.
#'
#' @rdname dest_base-methods
#' @param repo a GRANRepository object
#' @return For dest_base, the base directory the repository will reside in
#' @docType methods
#' @export
setGeneric("dest_base", function(repo) standardGeneric("dest_base"))
#' @rdname dest_base-methods
#' @aliases dest_base,GRANRepository-method
#' @export
setMethod("dest_base","GRANRepository",
          function(repo) {
              ret <- file.path(normalizePath2(param(repo)@dest_base))
              .createDir(ret)
              ret
})


#' check_result_dir
#' Return the path where check results for packages will be deployed for use in
#' the build report.
#'
#' @rdname check_result_dir-methods
#' @param repo a GRANRepository object
#' @return The directory where check results should be deployed for use in the
#' build report
#' @docType methods
#' @export
setGeneric("check_result_dir", function(repo) standardGeneric("check_result_dir"))
#' @rdname check_result_dir-methods
#' @aliases check_result_dir,GRANRepository-method
#' @export
setMethod("check_result_dir","GRANRepository",
          function(repo) {
              ret <- file.path(normalizePath2(param(repo)@dest_base),
                                   param(repo)@repo_name, "CheckResults" )
              .createDir(ret)
              ret
})


#' backup_archive
#' Return path where packages are backed up by default when clearing the repo
#'
#' @rdname backup_archive-methods
#' @param repo a GRANRepository object
#' @return Directory where packages are backed up by default when clearing repo
#' @docType methods
#' @export
setGeneric("backup_archive", function(repo) standardGeneric("backup_archive"))
#' @rdname backup_archive-methods
#' @aliases backup_archive, GRANRepository-method
#' @export
setMethod("backup_archive", "GRANRepository",
          function(repo) {
              ret <- file.path(normalizePath2(param(repo)@dest_base),
                                   param(repo)@repo_name, "Archive" )
              .createDir(ret)
              ret
})


#' coverage_report_dir
#' Return the path where test coverage reports for packages will be deployed
#' for use in the build report.
#'
#' @rdname coverage_report_dir-methods
#' @param repo a GRANRepository object
#' @return The directory where test coverage results should be deployed for use
#' in the build report
#' @docType methods
#' @export
setGeneric("coverage_report_dir", function(repo) standardGeneric("coverage_report_dir"))
#' @rdname coverage_report_dir-methods
#' @aliases coverage_report_dir,GRANRepository-method
#' @export
setMethod("coverage_report_dir","GRANRepository",
          function(repo) {
              ret <- file.path(normalizePath2(param(repo)@dest_base),
                                   param(repo)@repo_name, "CovrReports" )
              .createDir(ret)
              ret
})

#' pkg_doc_dir
#' Return the path where test coverage reports for packages will be deployed
#' for use in the build report.
#'
#' @rdname pkg_doc_dir-methods
#' @param repo a GRANRepository object
#' @return The directory where test coverage results should be deployed for use
#' in the build report
#' @docType methods
#' @export
setGeneric("pkg_doc_dir", function(repo) standardGeneric("pkg_doc_dir"))
#' @rdname pkg_doc_dir-methods
#' @aliases pkg_doc_dir,GRANRepository-method
#' @export
setMethod("pkg_doc_dir","GRANRepository",
          function(repo) {
              ret <- file.path(normalizePath2(param(repo)@dest_base),
                                   param(repo)@repo_name, "PkgDocumentation" )
              .createDir(ret)
              ret
})


#' install_result_dir
#' Return the path where instal results for packages will be deployed for use in
#' the build report.
#'
#' @rdname install_result_dir-methods
#' @param repo a GRANRepository object
#' @return The directory where install results should be deployed for use in the
#' build report
#' @docType methods
#' @export
setGeneric("install_result_dir", function(repo) standardGeneric("install_result_dir"))
#' @rdname install_result_dir-methods
#' @aliases install_result_dir,GRANRepository-method
#' @export
setMethod("install_result_dir","GRANRepository",
          function(repo) {
              ret <- file.path(normalizePath2(param(repo)@dest_base),
                                   param(repo)@repo_name, "InstallResults" )
              .createDir(ret)
              ret
})


#' repo_url
#' Return the url that the repository will be served at. This is the web
#' address corresponding to repository, suitable for calling contrib.url.  e.g
#' http://www.website.com/gran/current/
#'
#' @rdname repo_url-methods
#' @param repo a GRANRepository object
#' @return For destination, the full path to the contrib directory the packages
#' will be deployed to
#' @docType methods
#' @export
setGeneric("repo_url", function(repo) standardGeneric("repo_url"))
#' @rdname repo_url-methods
#' @aliases repo_url,GRANRepository-method
#' @export
setMethod("repo_url","GRANRepository",
          function(repo) paste(param(repo)@dest_url,
                               param(repo)@repo_name,
                               sep="/"))
#' @rdname repo_url-methods
#' @aliases repo_url,NULL
#' @export
setMethod("repo_url","NULL", function(repo) NULL)


#' checkout_dir
#' Return the directory that package sources will be checked-out into
#' for use in the build process
#'
#' @rdname checkout_dir-methods
#' @param repo a GRANRepository object
#' @return For destination, the full path to the contrib directory the packages
#' will be deployed to
#' @docType methods
#' @export
setGeneric("checkout_dir", function(repo) standardGeneric("checkout_dir"))
#' @rdname checkout_dir-methods
#' @aliases checkout_dir,GRANRepository-method
#' @export
setMethod("checkout_dir","GRANRepository",
          function(repo) param(repo)@temp_checkout)

#' @rdname checkout_dir-methods
#' @aliases checkout_dir,NULL
#' @export
setMethod("checkout_dir","NULL", function(repo) NULL)


setMethod("manifest<-", "GRANRepository",
          function(x, value ) {
              x@manifest = value
              x
          })


setMethod("manifest", "GRANRepository", function(x) x@manifest)


#' @rdname GRANRepository-accessors
#' @aliases dep_repos,RepoBuildParam
#' @export
setMethod("dep_repos", "GRANRepository", function(x) {
    dep_repos(manifest(x)@pkg_manifest)
})

## only get manifest rows for pkgs in the 'session' by default
## override with session_only=FALSE if desired
setMethod("manifest_df", "GRANRepository", function(x, ...) manifest_df(manifest(x), ...))

setMethod("manifest_df<-", "GRANRepository", function(x, value) {
    manifest_df(manifest(x)) = value
    x
    })
setMethod("versions_df", "GRANRepository", function(x) versions_df(manifest(x)))
setMethod("versions_df<-", "GRANRepository", function(x, value) {
    versions_df(manifest(x)) = value
    x
    })

#' Repository build results
#'
#' @param x A GRANRepository object
#' @return A data.frame of build results
#' @docType methods
#' @rdname reporesults
#' @export
setGeneric("repo_results", function(x) standardGeneric("repo_results"))
#' @rdname reporesults
#' @aliases repo_results,GRANRepository
setMethod("repo_results", "GRANRepository", function(x) x@results)

#' @rdname reporesults
#' @param value The new results data.frame
#' @export
setGeneric("repo_results<-", function(x, value) standardGeneric("repo_results<-"))
#' @rdname reporesults
#' @aliases repo_results<-,GRANRepository
#' @export
setMethod("repo_results<-", "GRANRepository", function(x, value) {
    x@results = value
    x
    })

#' Extract parameter object
#' @param x An object with an associated paramater
#' @docType methods
#' @export
setGeneric("param", function(x) standardGeneric("param"))
#' @rdname param
#' @aliases param,GRANRepository
setMethod("param", "GRANRepository",
          function(x) x@param)

#' @param value A new parameter object
#' @rdname param
#' @export
setGeneric("param<-", function(x, value) standardGeneric("param<-"))
#' @rdname param
#' @aliases param<-,GRANRepository
setMethod("param<-", "GRANRepository",
          function(x, value){
              x@param = value
              x
          })

#' Get or set individual parameters on a GRANRepository object
#' @param x A GRANRepository object
#' @details These functions get or set individual repository build parameters
#' on a GRANRepository object.
#' @seealso \code{\link{RepoBuildParam}}
#' @docType methods
#' @rdname GRANRepository-accessors
#' @export
setGeneric("repo_name", function(x) standardGeneric("repo_name"))
#' @rdname GRANRepository-accessors
#' @aliases repo_name,GRANRepository
setMethod("repo_name", "GRANRepository",
          function(x) param(x)@repo_name)


#' @rdname GRANRepository-accessors
#' @export
setGeneric("temp_repo", function(x) standardGeneric("temp_repo"))
#' @rdname GRANRepository-accessors
#' @aliases temp_repo,GRANRepository
setMethod("temp_repo", "GRANRepository",
          function(x) param(x)@temp_repo)



#' @rdname GRANRepository-accessors
#' @export
setGeneric("check_warn_ok", function(x) standardGeneric("check_warn_ok"))
#' @rdname GRANRepository-accessors
#' @aliases check_warn_ok,GRANRepository
setMethod("check_warn_ok", "GRANRepository",
          function(x)  param(x)@check_warn_ok)

#'@rdname GRANRepository-accessors
#' @export
setGeneric("check_note_ok", function(x) standardGeneric("check_note_ok"))
#' @rdname GRANRepository-accessors
#' @aliases check_note_ok,GRANRepository
setMethod("check_note_ok", "GRANRepository",
          function(x)  param(x)@check_note_ok)

#' @rdname GRANRepository-accessors
#' @export
setGeneric("suspended_pkgs", function(x) standardGeneric("suspended_pkgs"))
#' @rdname GRANRepository-accessors
#' @aliases suspended_pkgs,GRANRepository
setMethod("suspended_pkgs", "GRANRepository",
          function(x) param(x)@suspended)

#' @rdname GRANRepository-accessors
#' @param value The new parameter value
#' @export
setGeneric("suspended_pkgs<-", function(x, value) standardGeneric("suspended_pkgs<-"))
#' @rdname GRANRepository-accessors
#' @aliases suspended_pkgs<-,GRANRepository
setMethod("suspended_pkgs<-", "GRANRepository",
          function(x, value) {
              param(x)@suspended = value
              x
              })


#'@rdname GRANRepository-accessors
#' @aliases sh_init_script,GRANRepository
#' @export
setMethod("sh_init_script", "GRANRepository",
          function(x) param(x)@shell_init)
#'@rdname GRANRepository-accessors
#' @aliases sh_init_script<-,GRANRepository
#'@export

setMethod("sh_init_script<-", "GRANRepository",
          function(x, value) {
              param(x)@shell_init= value
              x
              })


#' @rdname GRANRepository-accessors
#' @export
setGeneric("extra_fun", function(x) standardGeneric("extra_fun"))
#' @rdname GRANRepository-accessors
#' @export
setMethod("extra_fun", "GRANRepository",
          function(x)  param(x)@extra_fun)

#' @rdname GRANRepository-accessors
#' @export
setGeneric("check_test_on", function(x) standardGeneric("check_test_on"))
#' @rdname GRANRepository-accessors
#' @aliases check_test_on,RepoBuildParam
setMethod("check_test_on", "RepoBuildParam", function(x) x@check_test)
#' @rdname GRANRepository-accessors
#' @aliases check_test_on,GRANRepository
setMethod("check_test_on", "GRANRepository", function(x) check_test_on(param(x)))

#' @rdname GRANRepository-accessors
#' @export
setGeneric("install_test_on", function(x) standardGeneric("install_test_on"))
#' @rdname GRANRepository-accessors
#' @aliases install_test_on,RepoBuildParam

setMethod("install_test_on", "RepoBuildParam", function(x) x@install_test)
#' @rdname GRANRepository-accessors
#' @aliases install_test_on,GRANRepository
setMethod("install_test_on", "GRANRepository", function(x) install_test_on(param(x)))


#' @rdname GRANRepository-accessors
#' @aliases logfun,GRANRepository
#' @export
setMethod("logfun", "GRANRepository",
          function(x) logfun(param(x)))


#' @rdname GRANRepository-accessors
#' @aliases logfun<-,GRANRepository
#' @export
setMethod("logfun<-", "GRANRepository",
          function(x, value) {
              p = param(x)
              logfun(p) = value
              param(x) = p
              x
              })




#' @rdname GRANRepository-accessors
#' @docType methods
#' @export
setGeneric("use_cran_granbase", function(x) stop("This object doesn't contain repository build parameters"))

#' @rdname GRANRepository-accessors
#' @docType methods
#' @export
setGeneric("use_cran_granbase<-", function(x, value) stop("This object doesn't contain repository build parameters"))


#'@rdname GRANRepository-accessors
#' @aliases use_cran_granbase,GRANRepository
#' @export
setMethod("use_cran_granbase", "GRANRepository",
          function(x) param(x)@use_cran_granbase)
#'@rdname GRANRepository-accessors
#' @aliases use_cran_granbase<-,GRANRepository
#'@export

setMethod("use_cran_granbase<-", "GRANRepository",
          function(x, value) {
              param(x)@use_cran_granbase= value
              x
              })

#' @rdname GRANRepository-accessors
#' @aliases use_cran_granbase,RepoBuildParam
#' @export
setMethod("use_cran_granbase", "RepoBuildParam",
          function(x) x@use_cran_granbase)
#'@rdname GRANRepository-accessors
#' @aliases use_cran_granbase<-,RepoBuildParam
#'@export

setMethod("use_cran_granbase<-", "RepoBuildParam",
          function(x, value) {
              x@use_cran_granbase= value
              x
              })


#' @rdname GRANRepository-accessors
#' @export
setGeneric("check_timeout", function(x) stop("This object doesn't contain repository build parameters"))

#' @rdname GRANRepository-accessors
#' @docType methods
#' @export
setGeneric("check_timeout<-", function(x, value) stop("This object doesn't contain repository build parameters"))

#'@rdname GRANRepository-accessors
#' @aliases check_timeout,GRANRepository
#' @export
setMethod("check_timeout", "GRANRepository",
          function(x) param(x)@check_timeout)
#'@rdname GRANRepository-accessors
#' @aliases check_timeout<-,GRANRepository
#'@export

setMethod("check_timeout<-", "GRANRepository",
          function(x, value) {
              param(x)@check_timeout= value
              x
              })


#'@rdname GRANRepository-accessors
#' @aliases check_timeout,RepoBuildParam
#' @export
setMethod("check_timeout", "RepoBuildParam",
          function(x) x@check_timeout)
#'@rdname GRANRepository-accessors
#' @aliases check_timeout<-,RepoBuildParam
#'@export

setMethod("check_timeout<-", "RepoBuildParam",
          function(x, value) {
              x@check_timeout= value
              x
              })


#' @rdname GRANRepository-accessors
#' @export
setGeneric("build_timeout", function(x) stop("This object doesn't contain repository build parameters"))

#' @rdname GRANRepository-accessors
#' @docType methods
#' @export
setGeneric("build_timeout<-", function(x, value) stop("This object doesn't contain repository build parameters"))


#'@rdname GRANRepository-accessors
#' @aliases build_timeout,GRANRepository
#' @export
setMethod("build_timeout", "GRANRepository",
          function(x) param(x)@build_timeout)
#'@rdname GRANRepository-accessors
#' @aliases build_timeout<-,GRANRepository
#'@export

setMethod("build_timeout<-", "GRANRepository",
          function(x, value) {
              param(x)@build_timeout= value
              x
              })


#'@rdname GRANRepository-accessors
#' @aliases build_timeout,RepoBuildParam
#' @export
setMethod("build_timeout", "RepoBuildParam",
          function(x) x@build_timeout)
#'@rdname GRANRepository-accessors
#' @aliases build_timeout<-,RepoBuildParam
#'@export

setMethod("build_timeout<-", "RepoBuildParam",
          function(x, value) {
              x@build_timeout= value
              x
              })

#' @rdname GRANRepository-accessors
#' @docType methods
#' @export
setGeneric("pkg_log_dir", function(x) standardGeneric("pkg_log_dir"))

#' @rdname GRANRepository-accessors
#' @aliases pkg_log_dir,RepoBuildParam
#' @export
setMethod("pkg_log_dir", "RepoBuildParam", function(x) {
    ret <- file.path(normalizePath2(x@dest_base),
                    x@repo_name,
                    "SinglePkgLogs")
    .createDir(ret)
    ret
})

#' @rdname GRANRepository-accessors
#' @aliases pkg_log_dir,GRANRepository
#' @export
setMethod("pkg_log_dir", "GRANRepository", function(x) pkg_log_dir(param(x)))


#' @rdname GRANRepository-accessors
#' @param pkg The package name, accepted by pkg_log_file.
#' @docType methods
#' @export
setGeneric("pkg_log_file", function(pkg, x) standardGeneric("pkg_log_file"))

#' @rdname GRANRepository-accessors
#' @aliases pkg_log_file,RepoBuildParam
#' @export
setMethod("pkg_log_file", c(x = "RepoBuildParam"), function(pkg, x) file.path(pkg_log_dir(x), paste0(pkg, ".log")))

#' @rdname GRANRepository-accessors
#' @aliases pkg_log_file,GRANRepository
#' @export
setMethod("pkg_log_file", c(x = "GRANRepository"), function(pkg,x ) pkg_log_file(pkg, param(x)))

#' contrib.url
#' A generic for contrib.url so that available.packages et al can
#' interact with GRANRepository objects.
#' @param repos A repository to extract the contrib url from
#' @param type The type of package repository it is
#' @docType methods
#' @examples
#' repo = GRANRepository(GithubManifest("gmbecker/fastdigest"), basedir = tempdir())
#' contrib.url(repo)
#' @export
#' @rdname contriburl
setGeneric("contrib.url", contrib.url)
#'@rdname contriburl
#' @aliases contrib.url,GRANRepository
setMethod("contrib.url", "GRANRepository", function(repos, type) {
    contrib.url(repos = repo_url(repos), type = type)
})

#' available.packages
#' A generic for available.packages and a method for GRANRepository objects
#' @param contriburl The repository or contrib url
#' @param method See base documentation
#' @param fields See base documentation
#' @param type The type of packages to query
#' @param filters See base documetnation
#' @param repos Character string for the repository to query. GRANRepository
#' objects should be passed to the contriburl argument.
#' @rdname availpkgs
#' @docType methods
#' @examples
#' repo = GRANRepository(GithubManifest("gmbecker/fastdigest"), basedir = tempdir())
#' ## none because the repository hasn't been built...
#' available.packages(repo)
#' @export
setGeneric("available.packages", function(contriburl, method, fields = NULL,
                                          type = getOption("pkgType"),
                                          filters = NULL, repos = NULL)
                                          standardGeneric("available.packages"))

#' @rdname availpkgs
#' @aliases available.packages,ANY
setMethod("available.packages", "ANY",
          function (contriburl,
                    method, fields = NULL, type = getOption("pkgType"),
                    filters = NULL,
                    repos = NULL)
          {
              args = list(contriburl = contriburl, fields = fields,
                          type = type, filters = filters)
              if("repos" %in% names(formals(utils::available.packages)))
                  args$repos = repos
              else if(!is.null(repos))
                  args$contriburl = contrib.url(repos, type= type)
              if(!missing(method))
                  args$method = method

              do.call(utils::available.packages, args)
          })

#' @rdname availpkgs
#' @aliases available.packages,GRANRepository
setMethod("available.packages", "GRANRepository",
          function (contriburl,
                    method, fields = NULL, type = getOption("pkgType"),
                    filters = NULL,
                    repos = NULL) {
          available.packages(contrib.url(contriburl, type), method = method,
                             fields = fields, type = type,
                             filters = filters,
                             repos = NULL)
})

#' @rdname GRANRepository-accessors
#' @export
setGeneric("platform", function(x) standardGeneric("platform"))

#' @rdname GRANRepository-accessors
#' @docType methods
#' @export
setGeneric("platform<-", function(x, value) standardGeneric("platform<-"))

#'@rdname GRANRepository-accessors
#' @aliases platform,GRANRepository
#' @export
setMethod("platform", "GRANRepository",
          function(x) param(x)@platform)

#'@rdname GRANRepository-accessors
#' @aliases platform<-,GRANRepository
#'@export
setMethod("platform<-", "GRANRepository",
          function(x, value) {
              param(x)@platform= value
              x
              })

#' @rdname GRANRepository-accessors
#' @export
setGeneric("r_version", function(x) standardGeneric("r_version"))

#' @rdname GRANRepository-accessors
#' @docType methods
#' @export
setGeneric("r_version<-", function(x, value) standardGeneric("r_version<-"))

#'@rdname GRANRepository-accessors
#' @aliases r_version,GRANRepository
#' @export
setMethod("r_version", "GRANRepository",
          function(x) param(x)@r_version)

#'@rdname GRANRepository-accessors
#' @aliases r_version<-,GRANRepository
#'@export
setMethod("r_version<-", "GRANRepository",
          function(x, value) {
              param(x)@r_version= value
              x
              })

#' @rdname GRANRepository-accessors
#' @export
setGeneric("bioc_version", function(x) standardGeneric("bioc_version"))

#' @rdname GRANRepository-accessors
#' @docType methods
#' @export
setGeneric("bioc_version<-", function(x, value) standardGeneric("bioc_version<-"))

#'@rdname GRANRepository-accessors
#' @aliases bioc_version,GRANRepository
#' @export
setMethod("bioc_version", "GRANRepository",
          function(x) param(x)@bioc_version)

#'@rdname GRANRepository-accessors
#' @aliases bioc_version<-,GRANRepository
#'@export
setMethod("bioc_version<-", "GRANRepository",
          function(x, value) {
              param(x)@bioc_version= value
              x
              })
