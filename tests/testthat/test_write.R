context("write")
test_that("pv_write function works ok", {
    x <- pv_read(pv_example_file())
    new_file_name <- tempfile(fileext = ".psvb")
    pv_write(x$raw, filename = new_file_name)
    x2 <- pv_read(new_file_name)
    ## the $meta$filename components will differ
    x2$meta$filename <- x$meta$filename
    ## but otherwise x and x2 should be identical
    expect_identical(x, x2)
})
