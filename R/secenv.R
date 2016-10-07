#' Parse Renviron file
#'
#' @param fname character, file to parse
#'
#' @return
#' Named list of environment variables
#'
#' @export
read_Renviron <- function(fname) {
  fname <- "~/.Renviron"
  stopifnot(file.exists(fname))
  l <- strsplit(readLines(fname), "=")
  lens <- sapply(l, length)
  empty <- lens == 0
  ok <- lens == 2
  structure(
    as.list(sapply(l[ok], "[", 2)),
    names = sapply(l[ok], "[", 1)
  )
}


#' Write environment variables to file
#'
#' @param l named list of environment variables
#' @param output connection or character string where to write to, passed to \code{\link{writeLines}}
#' @param ... other arguments passed to \code{\link{writeLines}}
#'
#' @return
#' Nothing
#'
#' @export
write_Renviron <- function(l, output, ...) {
  ch <- sapply(names(l), function(x) paste(x, l[[x]], collapse="=") )
  writeLines(ch, con=output, ...)
}




#' Load GPG-protected environment file
#'
#' @param fname file name to read
#'
#' @return Nothing
#'
#' @export
load_secret_Renviron <- function(fname="~/.Renviron-secret", read_only=FALSE) {
  ofile <- tempfile(pattern="Renviron")
  rcrypt::decrypt(fname, output=ofile)
  if(read_only) {
    read_Renviron(ofile) )
  }
  readRenviron(ofile)
  unlink(ofile)
}
