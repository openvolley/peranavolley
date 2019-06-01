context("recode")
test_that("pv_recode function works ok", {
    aaa <- data.frame(a=1:3, b=c(1, 2, NA), c = letters[1:3], stringsAsFactors = FALSE)
    rmp <- list(conditions = data.frame(a=1, b=1), values = data.frame(c="new", d=0, stringsAsFactors = FALSE))
    aaar <- pv_recode(aaa, remap = rmp)
    expect_identical(aaar$c, c("new", "b", "c"))
    expect_identical(aaar$d, c(0, NA, NA))

    rmp <- list(conditions = data.frame(a=c(1, NA), b=c(1, NA)), values = data.frame(c=c("new", "foo"), d = c(0, 1), stringsAsFactors = FALSE))
    aaar <- pv_recode(aaa, remap = rmp)
    expect_identical(aaar$c, c("new", "foo", "foo"))
    expect_identical(aaar$d, c(0, 1, 1))
})

test_that("pv_tas_recode function works ok", {
    xp <- pv_read(pv_example_file())
    xp2 <- pv_tas_recode(xp, log_changes = TRUE)

    expect_false(is.null(attr(xp2, "changes")))
    ch <- attr(xp2, "changes")
    expect_is(ch, "data.frame")
    expect_equal(nrow(ch), 11)
    expect_equal(ch$n[ch$change == "Setter dumps to PP attacks"], 1)
    expect_equal(ch$n[ch$change == "Setting zone 1 attacks to X1"], 73)
    expect_equal(ch$n[ch$change == "Setting zone 2 attacks to X2"], 19)
    expect_equal(ch$n[ch$change == "Setting zone 3 attacks to X7"], 29)
    expect_equal(ch$n[ch$change == "Setting zone 4 attacks to CF"], 7)
})
