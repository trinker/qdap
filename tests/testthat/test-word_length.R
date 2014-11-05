context("Checking word_length")

test_that("word_length gives the desired output",{
    
    wls <- with(DATA, word_length(state, person))

    expect_true(class(wls) == "word_length")
    m <- counts(wls)
    expect_true(is.data.frame(m))
    expect_true(all(dim(m) == c(5, 10)))
    
})

