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
