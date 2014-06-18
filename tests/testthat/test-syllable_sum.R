context("Checking syllable_sum")

test_that("syllable_count, gives the desired output",{
    
    x1 <- syllable_count("Robots like Dason lie.")
    x2 <- syllable_count("Robots like Dason lie.", algorithm.report = TRUE)

    x1_c <- structure(list(words = structure(c(4L, 3L, 1L, 2L), .Label = c("dason", 
        "lie", "like", "robots"), class = "factor"), syllables = c(2, 
        1, 2, 1), in.dictionary = structure(c(1L, 1L, 2L, 1L), .Label = c("-", 
        "NF"), class = "factor")), .Names = c("words", "syllables", "in.dictionary"
        ), row.names = c(NA, -4L), class = "data.frame")

    x2_c <- structure(list(`ALGORITHM REPORT` = structure(list(words = structure(1L, .Label = c("dason", 
        "lie", "like", "robots"), class = "factor"), syllables = 2, in.dictionary = structure(2L, .Label = c("-", 
        "NF"), class = "factor")), .Names = c("words", "syllables", "in.dictionary"
        ), row.names = 3L, class = "data.frame"), `SYLLABLE DATAFRAME` = structure(list(
            words = structure(c(4L, 3L, 1L, 2L), .Label = c("dason", 
            "lie", "like", "robots"), class = "factor"), syllables = c(2, 
            1, 2, 1), in.dictionary = structure(c(1L, 1L, 2L, 1L), .Label = c("-", 
            "NF"), class = "factor")), .Names = c("words", "syllables", 
        "in.dictionary"), row.names = c(NA, -4L), class = "data.frame")), .Names = c("ALGORITHM REPORT", 
        "SYLLABLE DATAFRAME"))
   
    expect_true(identical(x1, x1_c))
    expect_true(identical(x2, x2_c))
    
})

test_that("syllable_sum, gives the desired output",{
    
    x3 <- syllable_sum(DATA$state)
   
    x3_c <- c(8, 5, 4, 5, 6, 6, 4, 4, 7, 6, 9)

    expect_true(all(x3 == x3_c))
    
})

test_that("polysyllable_sum, gives the desired output",{
    
    x4 <- polysyllable_sum(DATA$state)
    
    x4_c <- c(1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L)    

    expect_true(all(x4 ==  x4_c))
    
})

test_that("combo_syllable_sum, gives the desired output",{
    
    x5 <- combo_syllable_sum(DATA$state)

    x5_c <- structure(list(syllable.count = c(8, 5, 4, 5, 6, 6, 4, 4, 7, 
        6, 9), polysyllable.count = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
        )), .Names = c("syllable.count", "polysyllable.count"), row.names = c(NA, 
        -11L), class = c("combo_syllable_sum", "data.frame"))

    expect_true(identical(x5, x5_c))
    
})
