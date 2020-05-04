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


#' Experimental: write VBStats file to DVW format
#'
#' @param x peranavolley: a peranavolley object as returned by \code{\link{pv_read}}
#' @param file string: the filename to write to. If not supplied, no file will be written but the dvw content will be returned
#' @param attack_codes data.frame: the attack combination codes table to embed in the file. If \code{NULL}, the internal one will be used, which corresponds to the setting zones 1-5 used in VBStats. See also \code{\link[datavolley]{dv_default_attack_codes}}
#' @param text_encoding string: the text encoding to use
#'
#' @return The dvw file contents as a character vector (invisibly)
#'
#' @seealso \code{\link[datavolley]{dv_write}}
#'
#' @export
pv_write_dvw <- function(x, filename, dv_style_ids = TRUE, attack_codes = NULL, text_encoding = "UTF-8") {
    if (!inherits(x, "peranavolley")) stop("x must be a peranavolley object")
    assert_that(is.flag(dv_style_ids), !is.na(dv_style_ids))
    ## but now change that to datavolley
    class(x) <- sub("peranavolley", "datavolley", class(x))
    ## fill in a bunch of metadata slots that are probably not populated in the peranavolley object
    if (is.null(x$meta$winning_symbols)) {
        ## add default winning_symbols
        x$meta$winning_symbols <- "=~~~#~~~=~~~~~~~=/~~#~~~=/~~#~~~=~~~~~~~=~~~~~~~=~~~~~~~"
    }
    if (is.null(x$meta$comments)) {
        x$meta$comments <- data.frame(comment_1 = "no comments", stringsAsFactors = FALSE)
    }
    x$file_meta$preferred_date_format <- "ymd"
    if (!"generator_day" %in% names(x$file_meta)) x$file_meta$generator_day <- ""
    if (!"generator_idp" %in% names(x$file_meta)) x$file_meta$generator_idp <- ""
    if (!"generator_prg" %in% names(x$file_meta)) x$file_meta$generator_prg <- "VBStats"
    if (!"generator_rel" %in% names(x$file_meta)) x$file_meta$generator_rel <- ""
    if (!"generator_ver" %in% names(x$file_meta)) x$file_meta$generator_ver <- ""
    if (!"generator_nam" %in% names(x$file_meta)) x$file_meta$generator_nam <- ""
    x$file_meta$fileformat <- "2.0" ## dvw v2 format

    xref <- datavolley::dv_read(datavolley::dv_example_file())
    if (is.null(x$meta$sets)) x$meta$sets <- xref$meta$sets
    x$plays$point_phase <- NA_character_
    x$plays$attack_phase <- NA_character_

    if (dv_style_ids) {
        pids <- bind_rows(mutate(x$meta$players_h[, c("lastname", "firstname", "player_id")], team = "home"),
                          mutate(x$meta$players_v[, c("lastname", "firstname", "player_id")], team = "visiting"))
        if (any(duplicated(pids$player_id))) stop("duplicate player_ids not handled yet")
        pids$player_id_new <- toupper(paste0(substr(gsub("[[:space:][:punct:]]", "", pids$lastname), 1, 3), "-", substr(gsub("[[:space:][:punct:]]", "", pids$firstname), 1, 3)))
        if (any(duplicated(pids$player_id_new))) stop("duplicate player_ids not handled yet")
        for (idx in seq_len(nrow(pids))) {
            pid_from <- pids$player_id[idx]
            pid_to <- pids$player_id_new[idx]
            x$meta$players_h$player_id[x$meta$players_h$player_id %eq% pid_from] <- pid_to
            x$meta$players_v$player_id[x$meta$players_v$player_id %eq% pid_from] <- pid_to
            for (cl in c("player_id", paste0("home_player_id", 1:6), paste0("visiting_player_id", 1:6)))
                x$plays[[cl]][x$plays[[cl]] %eq% pid_from] <- pid_to
        }

        ## team codes
        if ((!"team_code" %in% names(x$meta$teams)) || is.na(x$meta$teams$team_code[1]) || is.na(x$meta$teams$team_code[2]) || x$meta$teams$team_code[1] %eq% x$meta$teams$team_code[2]) {
            warning("cannot use team_code as team_id")
        } else {
            from_tid <- x$meta$teams$team_id[1]
            to_tid <- x$meta$teams$team_code[1]
            for (cl in c("team_id", "home_team_id")) {
                x$plays[[cl]][x$plays[[cl]] %eq% from_tid] <- to_tid
            }
            from_tid <- x$meta$teams$team_id[2]
            to_tid <- x$meta$teams$team_code[2]
            for (cl in c("team_id", "visiting_team_id")) {
                x$plays[[cl]][x$plays[[cl]] %eq% from_tid] <- to_tid
            }
            x$meta$teams$team_id <- x$meta$teams$team_code
        }
    }

    if (!is.null(attack_codes)) x$meta$attacks <- attack_codes
    datavolley::dv_write(x, file = filename, text_encoding = text_encoding)
}
