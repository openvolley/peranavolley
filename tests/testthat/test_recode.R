context("recode")
test_that("pv_tas_recode function works ok", {
    fn <- pv_example_file()
    xp <- pv_read(fn)
    xp2 <- pv_tas_recode(xp, log_changes = TRUE)
    expect_is(attr(xp2, "changes"), "data.frame")
})
