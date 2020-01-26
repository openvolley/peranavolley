## internal utility functions

`%eq%` <- function(x,y) x==y & !is.na(x) & !is.na(y)

fixnames <- function(obj) {
    nms <- gsub("[\\.[:space:]]+", "_", tolower(names(obj)))
    setNames(obj, nms)
}

rot_forward <- function(z, by = 1L) do_rot(z, abs(by))
rot_backward <- function(z, by = 1L) do_rot(z, -abs(by))
do_rot <- function(z, by) (((z+by)-1L) %% 6)+1L

rot_one <- function(z) z[c(2:6, 1)]

rot_p1 <- function(lineup, p1) {
    ## p1 is the id of the player in pos 1
    idx <- which(lineup %eq% p1)
    if (length(idx) != 1) stop("cannot align rotation: player not present")
    if (idx == 1) lineup else lineup[c(seq(from = idx, to = 6, by = 1), seq(from = 1, to = idx-1, by = 1))]
}


## helper function to make sure things don't go wrong when we join tables together
## evaluate expression expr and check that the number of rows of the object in obj doesn't change unexpectedly
check_rows <- function(expr, obj, expect = "==") {
    chk1 <- nrow(get(obj))
    parenv <- parent.frame()
    eval(expr, parenv)
    chk2 <- nrow(get(obj))
    stopifnot(get(expect)(chk1, chk2))
}

dmapvalues <- function(x, from, to, ...) {
    ## equivalent to plyr::mapvalues but using dplyr::recode
    arglist <- as.list(to)
    names(arglist) <- from
    arglist <- c(list(x), arglist, ...)
    do.call(dplyr::recode, arglist)
}

## Accumulate messages for later display
## Internal function, not exported
## severity: 1=critical, 2=informative, may lead to misinterpretation of data, 3=minor, esp. those that might have resulted from selective post-processing of combo codes
collect_messages <- function(msgs, msg_text, line_nums, raw_lines, severity, fatal = FALSE) {
    if (missing(line_nums)) line_nums <- NA
    if (missing(raw_lines)) raw_lines <- "[unknown]"
    if (missing(severity)) severity <- NA
    vt <- rep(NA_real_, length(line_nums))
    ##if (!missing(raw_lines)) vt <- video_time_from_raw(raw_lines)
    if (fatal) {
        lnt <- as.character(line_nums)
        lnt[is.na(lnt)] <- "[unknown]"
        txt <- paste0("line ", lnt,": ", msg_text, " (line in file is: \"", raw_lines, "\")")
        if (fatal) stop(paste(txt, collapse = " / "))
    } else {
        msgs[[length(msgs)+1]] <- list(file_line_number = line_nums, video_time = vt, message = msg_text, file_line = unname(raw_lines), severity = severity)
    }
    msgs
}

##video_time_from_raw <- function(raw_lines) {
##    tryCatch(vapply(raw_lines, function(z) tryCatch(if (!is.null(z) && is.character(z) && nzchar(z) && !is.na(z)) as.numeric(read.csv(text = z, sep = ";", header = FALSE, stringsAsFactors = FALSE)[1, 13]) else NA_integer_, error = function(e) NA_integer_), FUN.VALUE = 1, USE.NAMES = FALSE), error = function(e) rep(NA_integer_, length(raw_lines)))
##}

join_messages <- function(msgs1, msgs2) {
    if (length(msgs2) > 0) {
        msgs1 <- c(msgs1, msgs2)
    }
    msgs1
}


##str2im <- function(str) {
##    jpeg::readJPEG(base64enc::base64decode(str))
##}
##plotim <- function(im) {
##    plot(1:2, type = "n")
##    rasterImage(im, 1.2, 1.27, 1.8, 1.73)
##}

b64gunz <- function(x) {
    x <- base64enc::base64decode(x)
    if (length(x) < 6 || !identical(x[5:6], as.raw(c(31, 8*16+11))))
        stop("cannot read text")
    ## first four bytes are the buffer size
    rc <- rawConnection(x[seq_along(x)[-1:-4]])
    z <- gzcon(rc)
    readLines(z, warn = FALSE)
}

## same thing but writing to temporary file, so slower
b64gunzf <- function(x) {
    x <- base64enc::base64decode(x)
    if (length(x) < 6 || !identical(x[5:6], as.raw(c(31, 8*16+11))))
        stop("cannot read text")
    ## first four bytes are the buffer size
    tf <- tempfile()
    on.exit(unlink(tf))
    writeBin(x[5:length(x)], con = tf)
    mygzcon <- gzfile(tf)
    x <- readLines(mygzcon, warn = FALSE)
    close(mygzcon)
    x
}
