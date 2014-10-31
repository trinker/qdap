context("Checking end_mark")

test_that("end_mark gives the desired output",{
    
    ends <- end_mark(mraja1spl$dialogue)
    cum_end <- cumulative(ends)

    expect_true(is.character(ends))
    expect_true(length(ends) == 508)
    
})

test_that("end_mark_by gives the desired output",{

    x_by <- end_mark_by(DATA.SPLIT$state, DATA.SPLIT$person)
    expect_true(is.list(x_by))
    expect_true(inherits(x_by, "end_mark_by"))

    FUNS <- list(scores, counts, proportions, preprocessed)
    outs <- lapply(FUNS, function(x) x(x_by))
    expect_true(all(sapply(outs, is.data.frame)))

    dims <- structure(c(5L, 4L, 5L, 4L, 5L, 4L, 15L, 4L), .Dim = c(2L, 4L))
    expect_equivalent(sapply(outs, dim), dims)
    
})
