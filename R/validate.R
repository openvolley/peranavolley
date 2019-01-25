#' Validate parsed Perana Volleyball scouting data
#'
#' @param x peranavolley: peranavolley object as returned by \code{pv_read}
#' @param validation_level numeric: how strictly to check? If 0, perform no checking; if 1, only identify major errors; if 2, also return any issues that are likely to lead to misinterpretation of data; if 3, return all issues (including minor issues)
#'
#' @return data.frame with columns message (the validation message), file_line_number (the corresponding line number in the file), video_time, and file_line (the actual line from the file).
#'
#' @seealso \code{\link{pv_read}}
#'
#' @examples
#' filename <- pv_example_file()
#' x <- pv_read(filename)
#' pv_validate(x)
#'
#' @export
pv_validate <- function(x, validation_level = 2) {
    class(x) <- c("datavolley", class(x))
    out <- datavolley::validate_dv(x, file_type = "indoor")
    ## filter out some messages that don't apply to perana files
    idx <- grepl("have no position (opposite/outside/etc) assigned in the players list", out$message, fixed = TRUE)
    out <- out[!idx, ]
    out
}

