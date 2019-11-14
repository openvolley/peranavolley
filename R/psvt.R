#' Read a Perana Sports tagger (psvt) data file
#'
#' @references \url{http://peranasports.com/}
#' @param filename string: path to file
#' @param raw_only logical: if \code{TRUE}, just decompress the file, don't parse it
#'
#' @return A list with elements \code{meta} (metadata including the project and template information, and information about the skills and grades used in the tags) and \code{tags} the tag data.
#'
#' @export
pt_read <- function(filename, raw_only = FALSE) {
    assert_that(is.string(filename))
    assert_that(is.flag(raw_only), !is.na(raw_only))
    x <- b64gunzf(readLines(filename, warn = FALSE)) ## switch over to b64gunz when better tested
    names(x) <- vapply(x, function(z) sub("~.*", "", z), FUN.VALUE = "", USE.NAMES = FALSE)
    if (raw_only) {
        list(raw = unname(x))
    } else {
        pt_parse(x)
    }
}

pt_parse <- function(x) {
    pparse <- function(z, df = TRUE) {
        temp <- sub("^[A-Z]+~", "", z)
        if (grepl("^\\(?null\\)?", temp, ignore.case = TRUE)) {
            if (df) tibble() else NULL
        } else {
            jsonlite::fromJSON(temp)
        }
    }
    pparse_df <- function(z) fixnames(bind_rows(lapply(z, pparse)))

    out <- list(raw = x)
    ## project metadata
    meta <- list(project = pparse_df(x[names(x) == "PRJ"]))
    ##PRJ~{\"ProjectDateString\":\"14 Nov 2019\",\"TemplateName\":\"Volleyball\",\"VideoFile\":\"xyz\",\"Guid\":\"9386897B-F55A-4544-B067-F220D266D0C4-2201-0000035138F58B2D\",\"Name\":\"Test\",\"ProjectDate\":\"2019-11-14T15:06:57.839Z\"}"

    ## V is video info
    ## "V~{\"projectOrder\":1,\"fullPath\":\"ipod-library://item/item.mp4?id=4271945980203926134\",\"assetType\":0,\"duration\":5585.003,\"startTime\":0,\"name\":\"xyz\"}"
    meta$video <- pparse_df(x[names(x) == "V"])
    ## "TMPL~{\"Name\":\"Volleyball\",\"useCustomLayout\":0,\"Guid\":\"50150D15-FE66-43AB-9A6E-F598E4E0C1A5-1774-000007420FD2F1D1\"}"
    meta$template <- pparse_df(x[names(x) == "TMPL"])

    ## skills and their grades are in order
    skx <- x[names(x) %in% c("SK", "G")]
    skills <- tibble()
    this_skill <- tibble()
    this_grades <- tibble()
    grades <- tibble()
    for (xi in seq_along(skx)) {
        if (names(skx)[xi] %eq% "SK") {
            if (nrow(this_skill) > 0) {
                skills <- bind_rows(skills, this_skill)
                if (nrow(this_grades) > 0) {
                    this_grades$skill_guid <- this_skill$guid
                    grades <- bind_rows(grades, this_grades)
                }
            }
            this_skill <- pparse_df(skx[[xi]])
            this_grades <- tibble()
        } else {
            ## a grade to go with the last skill
            this_grades <- bind_rows(this_grades, pparse_df(skx[[xi]]))
        }
    }
    meta$skills <- skills
    meta$grades <- grades
    out$meta <- meta
    ## and finally the tags themselves
    ## "TAG~{\"tagDescription\":\"Pass - Error\",\"playerGuid\":\"\",\"skillGuid\":\"69F4E45B-3843-44FB-A48B-ABDD228131C5-2201-00000350FEA496F0\",\"subSkillGuid\":\"\",\"subSkill2Guid\":\"\",\"gradeGuid\":\"9EA8F3A4-94BA-4FE4-BFEE-A8DC269FAD3E-2201-00000350FEA71615\",\"videoName\":\"xyz\",\"playListOrder\":0,\"Guid\":\"000001\",\"VideoPosition\":1278.94,\"VideoThumbnailTime\":0,\"Duration\":5,\"selected\":1}"
    tx <- pparse_df(x[names(x) %in% "TAG"])
    tx <- left_join(tx, meta$skills %>% dplyr::select(skill = "name", guid), by = c(skillguid = "guid"))
    tx <- left_join(tx, meta$grades %>% dplyr::select(grade = "name", guid), by = c(gradeguid = "guid"))
    out$tags <- tx
    out
}


#' Write a Perana Sports tagger (psvt) data file
#'
#' This is somewhat experimental. It may be useful if one wants to read an existing file, modify the content, and re-write it back to a psvt file.
#'
#' @param x character: data to write. See e.g the \code{raw} component of the object returned by \code{\link{pt_read}}
#' @param filename string: path to file
#'
#' @seealso \code{\link{pt_read}}
#'
#' @export
pt_write <- function(x, filename) {
    pv_write(x, filename)
}

#' Example psvt data files provided as part of the peranavolley package
#'
#' @param choice numeric: which data file to return?
#' \itemize{
#'   \item{1 - trivial volleyball example file}
#' }
#' @return path to the file
#'
#' @seealso \code{\link{pt_read}}
#'
#' @examples
#' myfile <- pt_example_file()
#' x <- pt_read(myfile)
#'
#' @export
pt_example_file <- function(choice = 1) {
    assert_that(is.numeric(choice))
    switch(as.character(choice),
           "1" = system.file("extdata/volley_example.psvt", package = "peranavolley"),
           stop("unrecognized 'choice' value (", choice, ")")
           )
}
