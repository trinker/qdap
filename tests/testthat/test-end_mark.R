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
    expect_true(identical(sapply(outs, dim), dims))
    
})

test_that("end_mark and friends gives correct output",{

    expect_true(all(table(end_mark(mraja1spl$dialogue)) == c(84, 343, 80)))
    
    x_by <- end_mark_by(DATA.SPLIT$state, DATA.SPLIT$person)
    expect_true(all(class(x_by) == c("end_mark_by", "list")))
    
    x_by_c <- structure(list(person = c("greg", "researcher", "sally", "sam", 
        "teacher"), `!` = c(1L, 0L, 0L, 1L, 0L), . = c(4L, 1L, 0L, 3L, 
        0L), `?` = c(1L, 1L, 2L, 0L, 1L)), class = c("end_mark_by_count", 
        "data.frame"), type = "end_mark_by_counts", .Names = c("person", 
        "!", ".", "?"), row.names = c("1", "2", "3", "4", "5"))
    
    expect_identical(counts(x_by), x_by_c)

})
