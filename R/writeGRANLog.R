#' writeGRANLog
#'
#' Utility function which writes gran logs
#' @param pkg The name of the package the log is about
#' @param msg The log message, collapsed if length>1
#' @param type "full", "error", "warn", or "both" indicating which
#' log(s) the message should be written to
#' @param logfile The location of the full log file to write/append to
#' @param errfile the location of the error log file to write/append to
#' @param pkglog character. The package-specific log file to write to if
#' applicable.
#' @note This function is not intended for direct use by the end user.
#' @export
writeGRANLog = function(pkg, msg, type = "full", logfile,
                        errfile, pkglog = NULL)
{
    if(type == "error") {
        targ = errfile
        err = " ERROR "
    } else if (type == "both") {
        targ = c(logfile, errfile)
        err = " ERROR "
    } else if (type == "warn") {
        targ = c(logfile, errfile)
        err = " WARNING "
    } else {
        targ = logfile
        err = character()
    }
    if(!is.null(pkg) && !is.na(pkg))
        targ = c(targ, pkglog)

    fullmsg <- paste("\n", err, "pkg:", pkg, "(", date(), ") - ",
        paste(paste0("\t",msg), collapse="\n\t"))
    sapply(targ, function(x) {
      if (!file.exists(x))
          file.create(x)
      cat(fullmsg, append = TRUE, file = x)
    })
}
