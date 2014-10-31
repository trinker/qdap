context("Checking blank2NA")


test_that("blank2NA gives the desired output and replaces empty cells",{

    set.seed(15)
    dat <- data.frame(matrix(sample(c(month.abb[1:4], ""), 50, TRUE),
        10, byrow = TRUE), stringsAsFactors = FALSE)

    expect_true(any(sapply(dat, function(x) any(na.omit(x) == ""))))
    expect_true(is.data.frame(blank2NA(dat)))
    expect_false(any(sapply(blank2NA(dat), function(x) any(na.omit(x) == ""))))

})
