context("Checking polarity")

test_that("polarity gives the desired output for default polarity frame",{
    
    poldat <- structure(list(person = structure(c(4L, 4L, 1L, 5L, 4L, 1L), .Label = c("greg", 
        "researcher", "sally", "sam", "teacher"), class = "factor"), 
            wc = c(3L, 3L, 5L, 4L, 4L, 5L), polarity = c(0.577350269189626, 
            -0.577350269189626, -0.447213595499958, 0, -1, 0), pos.words = list(
                "fun", "fun", "-", "-", "-", "-"), neg.words = list("-", 
                "-", "dumb", "-", c("liar", "stinks"), "-"), text.var = c("Computer is fun.", 
            "Not too fun.", "No it's not, it's dumb.", "What should we do?", 
            "You liar, it stinks!", "I am telling the truth!")), class = c("polarity_count", 
        "data.frame"), type = "polarity_count", .Names = c("person", 
        "wc", "polarity", "pos.words", "neg.words", "text.var"), row.names = c("1", 
        "2", "3", "4", "5", "6"), digits = 3)
    
    
    
    expect_equivalent(
        poldat, 
        counts(with(sentSplit(DATA[1:5, ], 4), polarity(state, person)))
    )
    
})


test_that("polarity gives the desired output for phrases used in polarity.frame",{
    
    poldat2 <- structure(list(all = "all", wc = 5L, polarity = 0.357770876399966, 
            pos.words = list("simply the best"), neg.words = list("-"), 
            text.var = "This is simply the best"), class = c("polarity_count", 
        "data.frame"), type = "polarity_count", .Names = c("all", "wc", 
        "polarity", "pos.words", "neg.words", "text.var"), row.names = "1", digits = 3)
    
    
    phrase <- "This is simply the best"
    key <- sentiment_frame(c("simply", "best", "simply the best"), "", c(0.1,0.3,0.8))
    
    expect_equivalent(
        poldat2, 
        counts(polarity(phrase, polarity.frame=key))
    )
    

})

