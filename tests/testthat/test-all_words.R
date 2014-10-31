context("Checking all_words")


test_that("all_words begins with or contains given chars, are data.frames and are sorted appropriately",{

    x1 <- all_words(raj$dialogue, begins.with="re")
    x2 <- all_words(raj$dialogue, "q")
    x3 <- all_words(raj$dialogue)
    x3b <- all_words(raj$dialogue, alphabetical = FALSE)
    x4 <- all_words(raj$dialogue, contains="the")
    x5 <- all_words(raj$dialogue, contains="read")
    
    L1 <- list(x1, x2, x3, x4, x5)
    
    expect_true(all(sapply(L1, is.data.frame)))
    expect_true(all(sapply(L1, function(x) is.character(x[, 1]))))
    expect_true(all(sapply(L1, function(x) is.numeric(x[, 2]))))
    
    expect_true(all(substring(x1[, 1], 1, 2) == "re"))
    expect_true(all(substring(x2[, 1], 1, 1) == "q"))
    expect_false(all(substring(x4[, 1], 1, 3) == "the"))
    expect_true(any(substring(x4[, 1], 1, 3) == "the"))
    expect_false(all(substring(x5[, 1], 1, 4) == "read"))
    expect_true(any(substring(x5[, 1], 1, 4) == "read"))
    
    expect_true(all(diff(rev(x3b[, 2])) >= 0))
    expect_false(all(diff(rev(x3[, 2])) >= 0))

})
