context("Checking beg2char")


test_that("beg2char gives the desired output",{

    x <- c("a_b_c_d", "1_2_3_4", "<_?_._:")
    x2 <- gsub("_", " ", x)
    x3 <- gsub("_", "\\^", x)
    
    expect_true(all(beg2char(x, "_") == c("a", "1", "<")))
    expect_true(all(beg2char(x, "_", 2) == c("a_b", "1_2", "<_?")))
    expect_true(all(beg2char(x, "_", 3) == c("a_b_c", "1_2_3", "<_?_.")))
    expect_true(all(beg2char(x, "_", 4) == x))
    expect_true(all(beg2char(x, "_", 3, include=TRUE) == c("a_b_c_", "1_2_3_", "<_?_._")))
    
    expect_true(all(beg2char(x2, " ", 2) == c("a b", "1 2", "< ?")))
    expect_true(all(beg2char(x3, "^", 2) == c("a^b", "1^2", "<^?")))
    
})

test_that("char2end gives the desired output",{
    
    x <- c("a_b_c_d", "1_2_3_4", "<_?_._:")
    x2 <- gsub("_", " ", x)
    x3 <- gsub("_", "\\^", x)
    
    expect_true(all(char2end(x, "_") == c("b_c_d", "2_3_4", "?_._:")))
    expect_true(all(char2end(x, "_", 2) == c("c_d", "3_4", "._:")))
    expect_true(all(char2end(x, "_", 3) == c("d", "4", ":")))
    expect_true(all(char2end(x, "_", 4) == x))
    expect_true(all(char2end(x, "_", 3, include=TRUE) == c()))
    
    expect_true(all(char2end(x2, " ", 2) == c("c d", "3 4", ". :")))
    expect_true(all(char2end(x3, "^", 2) == c("c^d", "3^4", ".^:")))

})
