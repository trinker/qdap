context("Checking colcomb2class")


test_that("colcomb2class gives the correct output including number of columns",{

    dat4 <- data.frame(X=LETTERS[1:5], matrix(sample(0:5, 20, TRUE), ncol = 4))
    out <- colcomb2class(dat4, list(new = c("X1", "X4")))
    
    expect_true(length(dat4) == 5)
    expect_true(class(out) == "list")
    
    classes <- structure(c("data.frame", "data.frame", "data.frame", "numeric", 
        "logical"), .Names = c("raw", "prop", "rnp", "digits", "percent"
        ))
    
    expect_equivalent(sapply(out, class), classes)
    expect_true(all(unlist(sapply(out, ncol)) == rep(4, 3)))

})

