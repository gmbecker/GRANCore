#'Transform a GRANRepository object into a list
#'
#' Utility to transform a GRANRepository object into a list
#' so that repos saved using GRANBase can be loaded by GRAN
#' without requiring GRANBase
#'
#' @param repo repository
#' @return a list suitable for use with RepoFromList
#' @examples
#' repo = GRANRepository(GithubManifest("gmbecker/switchr"), basedir = tempdir())
#' lst = RepoToList(repo)
#' @export
#' @rdname tofromlist
RepoToList = function(repo) {
  sl = names(getSlots(class(repo)))
  l = lapply(sl, function(x)
    slot(repo, x))
  names(l) = sl
  l
}


#'Create a GRANRepository object from a list
#'
#' @param rlist A list with entries that are slot name-value
#' pairs for a GRANRepository object
#' @return a GRANRepository object
#' @export
#' @examples
#' repo2 = RepoFromList(lst)
#' @rdname tofromlist
RepoFromList = function(rlist) {
  do.call(new, c("GRANRepository", rlist))
}


#'Backwards compatible load utility
#'
#' Load a repository serialized to an R code file
#'
#' @param filename The file to load
#' @export
#' @rdname saveload
loadRepo <- function(filename) {
  res = tryCatch(
    dget(filename),
    error = function(e)
      e)
  if (is(res, "error")) {
    txt = readLines(filename)
    txt2 = gsub("GRANRepository", "GRANRepositoryv0.9", txt)
    res = dget(textConnection(txt2))
    res = updateGRANRepoObject(res)
  }

  ##refresh closure for log function
  logfun(res) <- function(pkg, msg, type = "full")
    writeGRANLog(
      pkg,
      msg,
      type,
      logfile = logfile(res),
      errfile = errlogfile(res),
      pkglog = pkg_log_file(pkg, res)
    )
  res
}


#'Backwards compatible save utility
#'
#' serialize a repository to a file so that it does not require GRANBase
#' to load
#'
#' @param repo The GRANRepository object to save
#' @return NULL
#' @export
#' @examples
#' repo = GRANRepository(GithubManifest("gmbecker/rpath"), basedir = tempdir())
#' fil = file.path(tempdir(), "repo.R")
#' saveRepo(repo, fil)
#' repo2 = loadRepo(fil)
#' @rdname saveload
saveRepo <- function(repo, filename) {
  dput(repo, filename)
}
