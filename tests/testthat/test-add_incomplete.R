context("Checking add_incomplete")

test_that("add_incomplete adds `|` to no punctuation sentences.",{

    expected <- c("This in a|", "I am funny!", "An ending of sorts%|", "What do you want?")
    
    x <- add_incomplete(
        c(
            "This in a",
            "I am funny!",
            "An ending of sorts%",
            "What do you want?"
        ), 
        silent = TRUE
    )
    
    expect_equal(x, expected)
    
})

