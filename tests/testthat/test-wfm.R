context("Checking wfm")
# expect_true <- isTRUE; expect_equivalent <- all.equal

test_that("wfm out puts a matrix with appropriate attributes",{

    wfm1 <- with(DATA, wfm(state, list(sex, adult)))[1:15, ]
    wfm2 <- wfm(DATA[["state"]], DATA[["person"]])[1:15, ]
    
    wfm1_c <- structure(c(1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 
        1, 0, 1, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 
        0), .Dim = c(15L, 4L), .Dimnames = list(c("about", "already", 
        "am", "are", "be", "can", "certain", "computer", "distrust", 
        "do", "dumb", "eat", "fun", "good", "how"), c("f.0", "f.1", "m.0", 
        "m.1")))
    
    wfm2_c <- structure(c(0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 
        0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 2, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0), .Dim = c(15L, 
        5L), .Dimnames = list(c("about", "already", "am", "are", "be", 
        "can", "certain", "computer", "distrust", "do", "dumb", "eat", 
        "fun", "good", "how"), c("greg", "researcher", "sally", "sam", 
        "teacher")))
    
    expect_equivalent(wfm1,wfm1_c)
    expect_equivalent(wfm2,wfm2_c)

})


test_that("wfdf out puts a matrix with appropriate attributes",{

    wfdf1 <- with(DATA, wfdf(state, list(sex, adult)))[1:15, ]
    wfdf2 <- with(DATA, wfdf(state, person))[1:15, ]
    
    wfdf1_c <- structure(list(Words = c("about", "already", "am", "are", "be", 
        "can", "certain", "computer", "distrust", "do", "dumb", "eat", 
        "fun", "good", "how"), f.0 = c(1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 
        0, 0, 0, 0, 1), f.1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        1, 0), m.0 = c(0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 2, 0, 0), 
            m.1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0)), .Names = c("Words", 
        "f.0", "f.1", "m.0", "m.1"), row.names = c(NA, 15L), class = c("wfdf", 
        "t.df", "data.frame"))
    
    wfdf2_c <- structure(list(Words = c("about", "already", "am", "are", "be", 
        "can", "certain", "computer", "distrust", "do", "dumb", "eat", 
        "fun", "good", "how"), greg = c(0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 
        1, 1, 0, 0, 0), researcher = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 1, 0), sally = c(1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 
        0, 0, 1), sam = c(0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 2, 0, 0
        ), teacher = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0)), .Names = c("Words", 
        "greg", "researcher", "sally", "sam", "teacher"), row.names = c(NA, 
        15L), class = c("wfdf", "t.df", "data.frame"))

    expect_equivalent(wfdf1, wfdf1_c)
    expect_equivalent(wfdf2, wfdf2_c)

})


test_that("wfm_expanded expands",{

    expect_true(2 == sum(rownames(wfm_expanded(wfm(DATA$state, 
        DATA$person))) == "fun"))
    
})

test_that("wf_combine combines",{

    ## wf_combine examples:
    #===================
    ## raw no margins (will work)
    x <- wfm(DATA$state, DATA$person)
    
    ## raw with margin (will work)
    y <- wfdf(DATA$state, DATA$person, margins = TRUE)
    
    ## Proportion matrix
    z2 <- wfm(DATA$state, DATA$person, output="proportion")
    
    WL1 <- c(y[, 1])
    WL2 <- list(c("read", "the", "a"), c("you", "your", "you're"))
    WL3 <- list(bob = c("read", "the", "a"), yous = c("you", "your", "you're"))
    WL4 <- list(bob = c("read", "the", "a"), yous = c("a", "you", "your", "your're"))
    WL5 <- list(yous = c("you", "your", "your're"))
    WL6 <- list(c("you", "your", "your're"))  #no name so will be called words 1
    WL7 <- c("you", "your", "your're")
    
    expect_error(wfm_combine(z2, WL2)) #Won't work not a raw frequency matrix
    expect_error(wfm_combine(y, WL4)) #Error
    
    wc1 <- wfm_combine(x, WL2) #Works (raw and no margins)
    wc2 <- wfm_combine(y, WL2) #Works (raw with margins)
    wc3 <- wfm_combine(y, c("you", "your", "your're"))
    wc4 <- wfm_combine(y, WL1)
    wc5 <- wfm_combine(y, WL3)
    wc6 <- wfm_combine(y, WL5)
    wc7 <- wfm_combine(y, WL6)
    wc8 <- wfm_combine(y, WL7)
    
    wc1_c <- structure(c(1, 1, 19, 0, 0, 7, 0, 1, 10, 0, 2, 12, 0, 0, 5), .Dim = c(3L, 
        5L), .Dimnames = list(c("words 1", "words 2", "else.words"), 
            c("greg", "researcher", "sally", "sam", "teacher")), class = c("wfm", 
        "true.matrix", "matrix"))
    
    wc2_c <- structure(c(1, 1, 18, 0, 0, 6, 0, 1, 9, 0, 2, 11, 0, 0, 4), .Dim = c(3L, 
        5L), .Dimnames = list(c("words 1", "words 2", "else.words"), 
            c("greg", "researcher", "sally", "sam", "teacher")), class = c("wfm", 
        "true.matrix", "matrix"))
    
    wc3_c <- structure(c(1, 19, 0, 6, 1, 9, 2, 11, 0, 4), .Dim = c(2L, 5L), .Dimnames = list(
            c("words", "else.words"), c("greg", "researcher", "sally", 
            "sam", "teacher")), class = c("wfm", "true.matrix", "matrix"
        ))
    
    wc4_c <- structure(c(20, 6, 10, 13, 4), .Dim = c(1L, 5L), .Dimnames = list(
            "WL1", c("greg", "researcher", "sally", "sam", "teacher")), class = c("wfm", 
        "true.matrix", "matrix"))
    
    wc5_c <- structure(c(1, 1, 18, 0, 0, 6, 0, 1, 9, 0, 2, 11, 0, 0, 4), .Dim = c(3L, 
        5L), .Dimnames = list(c("bob", "yous", "else.words"), c("greg", 
        "researcher", "sally", "sam", "teacher")), class = c("wfm", "true.matrix", 
        "matrix"))
    
    wc6_c <- structure(c(1, 19, 0, 6, 1, 9, 2, 11, 0, 4), .Dim = c(2L, 5L), .Dimnames = list(
            c("yous", "else.words"), c("greg", "researcher", "sally", 
            "sam", "teacher")), class = c("wfm", "true.matrix", "matrix"
        ))
    
    wc7_c <- structure(c(1, 19, 0, 6, 1, 9, 2, 11, 0, 4), .Dim = c(2L, 5L), .Dimnames = list(
            c("words 1", "else.words"), c("greg", "researcher", "sally", 
            "sam", "teacher")), class = c("wfm", "true.matrix", "matrix"
        ))
            
    wc8_c <- structure(c(1, 19, 0, 6, 1, 9, 2, 11, 0, 4), .Dim = c(2L, 5L), .Dimnames = list(
            c("WL7", "else.words"), c("greg", "researcher", "sally", 
            "sam", "teacher")), class = c("wfm", "true.matrix", "matrix"
        ))
    
    
    expect_equivalent(wc2, wc2_c)
    expect_equivalent(wc3, wc3_c)
    expect_equivalent(wc4, wc4_c)
    expect_equivalent(wc5, wc5_c)
    expect_equivalent(wc6, wc6_c)
    expect_equivalent(wc7, wc7_c)
    expect_equivalent(wc8, wc8_c)

})


# test_that("weight produces matrices of the right class with right attributes",{
#     
#     WFM <- with(DATA, wfm(state, list(sex, adult)))
#     w_wfm1 <- weight(WFM, "prop")
#     w_wfm2 <- weight(WFM, "max")
#     w_wfm3 <- weight(WFM, "scaled")
#     
#     w_wfm1_c <- structure(list(dim = c(43L, 4L), dimnames = list(c("about", "already", 
#         "am", "are", "be", "can", "certain", "computer", "distrust", 
#         "do", "dumb", "eat", "f", "fun", "good", "how", "hungry", "i", 
#         "i'm", "is", "it", "it's", "let's", "liar", "m", "move", "no", 
#         "not", "on", "shall", "should", "stinks", "talking", "telling", 
#         "the", "then", "there", "too", "truth", "way", "we", "what", 
#         "you"), c("f.0", "f.1", "m.0", "m.1")), class = c("weighted_wfm", 
#         "matrix"), weighting = "prop"), .Names = c("dim", "dimnames", 
#         "class", "weighting"))
#     
#     w_wfm2_c <- structure(list(dim = c(43L, 4L), dimnames = list(c("about", "already", 
#         "am", "are", "be", "can", "certain", "computer", "distrust", 
#         "do", "dumb", "eat", "f", "fun", "good", "how", "hungry", "i", 
#         "i'm", "is", "it", "it's", "let's", "liar", "m", "move", "no", 
#         "not", "on", "shall", "should", "stinks", "talking", "telling", 
#         "the", "then", "there", "too", "truth", "way", "we", "what", 
#         "you"), c("f.0", "f.1", "m.0", "m.1")), class = c("weighted_wfm", 
#         "matrix"), weighting = "max"), .Names = c("dim", "dimnames", 
#         "class", "weighting"))
#     
#     w_wfm3_c <- structure(list(dim = c(43L, 4L), dimnames = list(c("about", "already", 
#         "am", "are", "be", "can", "certain", "computer", "distrust", 
#         "do", "dumb", "eat", "f", "fun", "good", "how", "hungry", "i", 
#         "i'm", "is", "it", "it's", "let's", "liar", "m", "move", "no", 
#         "not", "on", "shall", "should", "stinks", "talking", "telling", 
#         "the", "then", "there", "too", "truth", "way", "we", "what", 
#         "you"), c("f.0", "f.1", "m.0", "m.1")), class = c("weighted_wfm", 
#         "matrix"), weighting = "scaled"), .Names = c("dim", "dimnames", 
#         "class", "weighting"))
#     
#     expect_equivalent(attributes(w_wfm1), w_wfm1_c)
#     expect_equivalent(attributes(w_wfm2), w_wfm2_c)
#     expect_equivalent(attributes(w_wfm3), w_wfm3_c)
# 
# })