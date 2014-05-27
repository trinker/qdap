context("Checking clean")

test_that("clean gives the desired output",{

    x <- "I go \r\n    to the \tnext line"
    expect_true(clean(x) == "I go to the next line")

})