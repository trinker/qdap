context("Checking unique_by")

test_that("unique_by gives the desired output",{

    x <- with(DATA, unique_by(state, person))
    expect_true(is.list(x))
    expect_false(is.data.frame(x))

})