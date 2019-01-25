#' Example data files provided as part of the peranavolley package
#'
#' These example files were kindly provided by Chau Le from Perana Sports.
#'
#' @param choice numeric: which data file to return?
#' \itemize{
#'   \item{1 - Men's Australian Volleyball League 2017: Canberra Heat vs UTSSU}
#'   \item{2 - Women's Australian Volleyball League 2017: Canberra Heat vs UTSSU}
#' }
#' @return path to the file
#'
#' @seealso \code{\link{pv_read}}
#'
#' @examples
#' myfile <- pv_example_file()
#' x <- pv_read(myfile)
#' summary(x)
#'
#' @export
pv_example_file <- function(choice = 1) {
    assert_that(is.numeric(choice))
    switch(as.character(choice),
           "1" = system.file("extdata/20170923_2017_AVL_MENS_CHM_17_vs_UTSSUM_17.psvb", package = "peranavolley"),
           "2" = system.file("extdata/20170923_2017_AVL_WOMENS_CHW_17_vs_UTSSUW_17.psvb", package = "peranavolley"),
           stop("unrecognized 'choice' value (", choice, ")")
           )
}
