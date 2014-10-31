context("Checking common")

test_that("common gives the desired output",{
    
    a <- c("a", "cat", "dog", "the", "the")
    b <- c("corn", "a", "chicken", "the")
    d <- c("house", "feed", "a", "the", "chicken")
    x <- common(a, b, d, overlap=2)
    y <- common(a, b, d, overlap=3)
    expect_true(all(sapply(list(x, y), is.data.frame)))
    expect_true(all(unique(x[, 2]) == 3:2))
    expect_true(all(unique(y[, 2]) == 3))
    
    r <- list(a, b, d)
    expect_equivalent(common(r), y)
    
    expect_true(is.data.frame(common(word_list(DATA$state, DATA$person)$cwl, 
        overlap = 2)))

})