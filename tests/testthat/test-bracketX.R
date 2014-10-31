context("Checking bracketX")

test_that("bracketX gives the desired output",{

    examp <- structure(list(person = structure(c(1L, 2L, 1L, 3L),
        .Label = c("bob", "greg", "sue"), class = "factor"), text =
        c("I love chicken [unintelligible]!",
        "Me too! (laughter) It's so good.[interrupting]",
        "Yep it's awesome {reading}.", "Agreed. {is so much fun}")), .Names =
        c("person", "text"), row.names = c(NA, -4L), class = "data.frame")
    
    expected1 <- list(c("I love chicken !", "Me too! (laughter) It's so good.", 
    "Yep it's awesome {reading} .", "Agreed. {is so much fun}"), 
        c("I love chicken [unintelligible] !", "Me too! (laughter) It's so good. [interrupting]", 
        "Yep it's awesome .", "Agreed."), c("I love chicken !", "Me too! It's so good.", 
        "Yep it's awesome {reading} .", "Agreed. {is so much fun}"
        ), c("I love chicken !", "Me too! It's so good.", "Yep it's awesome .", 
        "Agreed."))
    
    expect_equivalent(bracketX(examp$text, "square"), expected1 [[1]])
    expect_equivalent(bracketX(examp$text, "curly"), expected1 [[2]])
    expect_equivalent(bracketX(examp$text, c("square", "round")), expected1 [[3]])
    expect_equivalent(bracketX(examp$text), expected1 [[4]])
})


test_that("bracketXtract gives the desired output",{
    
    examp <- structure(list(person = structure(c(1L, 2L, 1L, 3L),
        .Label = c("bob", "greg", "sue"), class = "factor"), text =
        c("I love chicken [unintelligible]!",
        "Me too! (laughter) It's so good.[interrupting]",
        "Yep it's awesome {reading}.", "Agreed. {is so much fun}")), .Names =
        c("person", "text"), row.names = c(NA, -4L), class = "data.frame")    

    expected2 <- list(structure(list(square1 = "unintelligible", square2 = "interrupting", 
            square3 = character(0), square4 = character(0)), .Names = c("square1", 
        "square2", "square3", "square4")), structure(list(curly1 = character(0), 
            curly2 = character(0), curly3 = "reading", curly4 = "is so much fun"), .Names = c("curly1", 
        "curly2", "curly3", "curly4")), list("unintelligible", c("interrupting", 
        "laughter"), character(0), character(0)), structure(list(square = list(
            "unintelligible", "interrupting", character(0), character(0)), 
            round = list(character(0), "laughter", character(0), character(0))), .Names = c("square", 
        "round")), structure(list(all1 = "unintelligible", all2 = c("laughter", 
        "interrupting"), all3 = "reading", all4 = "is so much fun"), .Names = c("all1", 
        "all2", "all3", "all4")), structure(list(all1 = "[unintelligible]", 
            all2 = c("(laughter)", "[interrupting]"), all3 = "{reading}", 
            all4 = "{is so much fun}"), .Names = c("all1", "all2", "all3", 
        "all4")), "reading is so much fun")
    
    expect_equivalent(bracketXtract(examp$text, "square"), expected2 [[1]])
    expect_equivalent(bracketXtract(examp$text, "curly"), expected2 [[2]])
    expect_equivalent(bracketXtract(examp$text, c("square", "round")), expected2 [[3]])
    expect_equivalent(bracketXtract(examp$text, c("square", "round"), merge = FALSE), expected2 [[4]])
    expect_equivalent(bracketXtract(examp$text), expected2 [[5]])
    expect_equivalent(bracketXtract(examp$text, with = TRUE), expected2 [[6]])
    expect_equivalent(paste2(bracketXtract(examp$text, "curly"), " "), expected2 [[7]])
    
})

test_that("genX gives the desired output",{

    expected3 <- c("Computer fun.", "No it's not, it's dumb.", "What should we do?", 
        "You liar, it stinks!", "I am telling the truth!", "How can we be certain?", 
        "There is no way.", "I distrust you.", "What are you talking about?", 
        "Shall? Good then.", "I'm hungry. Let's eat. You already?")
    
    expect_equivalent(genX(DATA$state, c("is", "we"), c("too", "on")), expected3)

    x <- c("Where is the /big dog#?",
        "I think he's @arunning@b with /little cat#.")
    expected4 <- c("Where is the?", "I think he's with.")
    
    expect_equivalent(genX(x, c("/", "@a"), c("#", "@b")), expected4)
    
})

test_that("genXtract gives the desired output",{

    expected5 <- list("big dog", c("little cat", "running"))
    
    x2 <- c("Where is the /big dog#?",
        "I think he's @arunning@b with /little cat#.")
    expect_equivalent(genXtract(x2, c("/", "@a"), c("#", "@b")), expected5)
    
    x3 <- c("Where is the L1big dogL2?",
        "I think he's 98running99 with L1little catL2.")
    expect_equivalent(genXtract(x3, c("L1", 98), c("L2", 99)), expected5)

})