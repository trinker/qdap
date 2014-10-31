context("Checking capitalizer")

test_that("capitalizer gives the desired output",{

    expected <- list(c("I", "Like", "it", "but", "I'm", "not", "certain"), c("i", 
        "Like", "it", "but", "i'm", "not", "certain"))


     expect_equivalent(capitalizer(bag_o_words("i like it but i'm not certain"), 
         "like"), expected[[1]])
     expect_equivalent(a2 <- capitalizer(bag_o_words("i like it but i'm not certain"), 
         "like", FALSE), expected[[2]])
    
})