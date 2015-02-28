context("Checking strip")

test_that("strip gives the desired output for character",{
    
    v <- "id~~want of the order"
    w <- "id~~want % of the order !"
    x <- "id~~want 45 of the order"
    y <- "i'd~~want of the order"
    z <- "Id~~want of the order"
    
    a <- " I'd~~want 45% of the    order ! "
    
    expect_equal(strip(a), v)
    expect_equal(strip(a, char.keep = c("%", "!", "~~")), w)
    expect_equal(strip(a, digit.remove = FALSE), x)
    expect_equal(strip(a, apostrophe.remove = FALSE), y)
    expect_equal(strip(a, lower.case = FALSE), z)  
    
})


test_that("strip gives the desired output for factor",{
    
    v <- "id~~want of the order"
    w <- "id~~want % of the order !"
    x <- "id~~want 45 of the order"
    y <- "i'd~~want of the order"
    z <- "Id~~want of the order"
    
    a <- factor(" I'd~~want 45% of the    order ! ")
    
    expect_equal(strip(a), v)
    expect_equal(strip(a, char.keep = c("%", "!", "~~")), w)
    expect_equal(strip(a, digit.remove = FALSE), x)
    expect_equal(strip(a, apostrophe.remove = FALSE), y)
    expect_equal(strip(a, lower.case = FALSE), z)    
})

