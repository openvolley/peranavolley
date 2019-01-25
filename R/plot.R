## funcs from datavolley
dv_xy_xbins <- function() 0.5+(seq_len(100) - 11) / 80 * 3.0
dv_xy_ybins <- function() 0.5+(seq_len(101) - 11) / 81 * 6.0

dv_xy2index <- function(x, y) {
    if (missing(y) && ncol(x) > 1) {
        y <- x[, 2]
        x <- x[, 1]
    }
    assert_that(is.numeric(x))
    assert_that(is.numeric(y))
    ## define cells (these are LEFT edges) in plot x, y space
    ##binx <- seq(from=3*(1-10.5)/79+0.5, to=3*(100-10.5)/79+0.5, length.out=100)
    binx <- dv_xy_xbins()
    binx[1] <- -Inf ## so that anything to the left of the first cell is put in the first cell
    binx <- c(binx, Inf) ## and anything beyond the last cell is put into the last cell
    ##biny <- seq(from=3*(1-10.5)/40.5+0.5, to=3*(101-10.5)/40.5+0.5, length.out=101)
    biny <- dv_xy_ybins()
    biny[1] <- -Inf
    biny <- c(biny, Inf)
    xi <- .bincode(x, binx, right = FALSE)
    yi <- .bincode(y, biny, right = FALSE)
    as.integer(xi + (yi - 1) * (length(binx) - 1))
}

xy2zone <- function(x, y, as_for_serve = FALSE) {
    assert_that(is.flag(as_for_serve), !is.na(as_for_serve))
    if (!as_for_serve) {
        zm <- matrix(c(5L, 6L, 1L, 7L, 8L, 9L, 4L, 3L, 2L), nrow = 3)
        zm <- cbind(zm, zm[3:1, 3:1])
        x <- pmax(1, pmin(3, round(x)))
        y <- pmax(1, pmin(6, round(y)))
        zm[(y - 1)*3 + x]
    } else {
        x <- pmax(2, pmin(6, round(x*2))) - 1 # round to 0.5m bins
        ## positions 7 and 9 lie in between 5-6 and 6-1
        zm <- c(5L, 7L, 6L, 9L, 1L)
        out <- zm[x]
        out[y > 3.5] <- rev(zm)[x[y > 3.5]]
        out
    }
}

xy2subzone <- function(x, y) {
    szm <- matrix(c("D", "A", "C", "B"), nrow = 2)
    szm <- rbind(szm, szm, szm)
    szm <- cbind(szm, szm, szm, szm[6:1, 2:1], szm[6:1, 2:1], szm[6:1, 2:1])
    x <- pmax(1, pmin(6, ceiling((x-0.5)/0.5)))
    y <- pmax(1, pmin(12, ceiling((y-0.5)/0.5)))
    szm[(y - 1)*6 + x]
}
