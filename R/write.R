#' Write a Perana Sports volleyball data file
#'
#' This is somewhat experimental. It may be useful if one wants to read an existing file, modify the content, and re-write it back to a PSVB file.
#' 
#' @param x character: data to write. See e.g the \code{raw} component of the object returned by \code{\link{pv_read}}
#' @param filename string: path to file
#'
#' @seealso \code{\link{pv_read}}
#'
#' @examples
#'
#' \dontrun{
#'  x <- pv_read(pv_example_file())
#'  new_file_name <- tempfile(fileext = ".psvb")
#'  pv_write(x$raw, filename = new_file_name)
#' }
#'
#' @export
pv_write <- function(x, filename) {
    assert_that(is.character(x))
    x <- unname(x)
    tf <- tempfile()
    on.exit(unlink(tf))
    ## write to gzipped file
    gzcon <- gzfile(tf)
    writeLines(text = x, con = gzcon)
    close(gzcon)
    ## now read back in bytes
    gb <- readBin(tf, what = "raw", n = file.info(tf)$size*2)
    ## first four bytes are to be the buffer size
    sz <- packBits(intToBits(sum(vapply(x, nchar, FUN.VALUE = 1))), "raw")
    if (length(sz) > 4) {
        ## should not happen?
        sz <- sz[1:4]
    } else if (length(sz) < 4) {
        sz <- c(as.raw(rep(0, 4-length(sz))), sz)
    }
    gb <- c(sz, gb)
    ## now base64-encode
    gb <- base64enc::base64encode(gb)
    ## and finally write to (UTF-8) file
    writeLines(gb, filename)
}


