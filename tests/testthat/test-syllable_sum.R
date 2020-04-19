context("Checking syllable_sum")

test_that("syllable_count, gives the desired output",{
    
    x1 <- syllable_count("Robots like Dason lie.")
    x2 <- syllable_count("Robots like Dason lie.", algorithm.report = TRUE)

    x1_c <- structure(list(words = c("robots", "like", "dason", "lie"), syllables = c(2, 
        1, 2, 1), in.dictionary = c("-", "-", "NF", "-")), class = "data.frame", row.names = c(NA, 
        -4L))
    
    x2_c <- list(`ALGORITHM REPORT` = structure(list(words = "dason", syllables = 2, 
        in.dictionary = "NF"), row.names = 3L, class = "data.frame"), 
        `SYLLABLE DATAFRAME` = structure(list(words = c("robots", 
        "like", "dason", "lie"), syllables = c(2, 1, 2, 1), in.dictionary = c("-", 
        "-", "NF", "-")), class = "data.frame", row.names = c(NA, 
        -4L)))
   
    expect_equivalent(x1, x1_c)
    expect_equivalent(x2, x2_c)
    
})

test_that("syllable_sum, gives the desired output",{
    
    x3 <- syllable_sum(DATA$state)
   
    x3_c <- structure(c(8, 5, 4, 5, 6, 6, 4, 4, 7, 6, 9), class = c("syllable_sum", 
        "syllable_freq", "numeric"), wc = c(6L, 5L, 4L, 4L, 5L, 5L, 4L, 
        3L, 5L, 6L, 6L), type = "Syllable")

    expect_true(all(x3 == x3_c))
    
})

test_that("polysyllable_sum, gives the desired output",{
    
    x4 <- polysyllable_sum(DATA$state)
    
    x4_c <- structure(c(1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L), 
        class = c("polysyllable_sum", "syllable_freq", "integer"), 
        wc = c(6L, 5L, 4L, 4L, 5L, 5L, 4L, 3L, 5L, 6L, 6L), 
        type = "Pollysyllable")

    expect_true(all(x4 ==  x4_c))
    
})

test_that("combo_syllable_sum, gives the desired output",{
    
    x5 <- combo_syllable_sum(DATA$state)

    x5_c <- structure(list(syllable.count = c(8, 5, 4, 5, 6, 6, 4, 4, 7, 
        6, 9), polysyllable.count = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
        )), .Names = c("syllable.count", "polysyllable.count"), row.names = c(NA, 
        11L), class = c("combo_syllable_sum", "data.frame"), text.var = c("Computer is fun. Not too fun.", 
        "No it's not, it's dumb.", "What should we do?", "You liar, it stinks!", 
        "I am telling the truth!", "How can we be certain?", "There is no way.", 
        "I distrust you.", "What are you talking about?", "Shall we move on?  Good then.", 
        "I'm hungry.  Let's eat.  You already?"), wc = c(6L, 5L, 4L, 
        4L, 5L, 5L, 4L, 3L, 5L, 6L, 6L))

    expect_equivalent(x5, x5_c)
    
})

test_that("cumulative methods for syllable_freq, gives the desired output",{
 
    x3_cum <- cumulative(syllable_sum(DATA$state))
    x4_cum <- cumulative(polysyllable_sum(DATA$state)  )
    
    expect_true(is.data.frame(x3_cum))
    expect_true(all(colnames(x3_cum) == c("cumave", "Time")))
    expect_true(is.data.frame(x4_cum))
    expect_true(all(colnames(x4_cum) == c("cumave", "Time")))
})
