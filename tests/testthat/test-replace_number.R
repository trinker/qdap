context("Checking replace_number")

test_that("replace_number is replacing numbers as expected 0 - 12 (single and 2 digit)",{

    x <- c("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", 
        "nine", "ten", "eleven", "twelve")

    expect_equivalent(x, replace_number(0:12))
	
})

test_that("replace_number is repalcing numbers (w/ & w/o comma as well as w/ & w/o spaces) as expected in text",{

    output1 <- c("I like three hundred forty six thousand four hundred fifty seven ice cream cones.", 
        "They are ninety nine percent good")
    
    output2 <- c("I like threehundredfortysixthousandfourhundredfiftyseven ice cream cones.", 
        "They are ninetynine percent good")
    
    
    x <- c("I like 346,457 ice cream cones.", "They are 99 percent good")
    y <- c("I like 346457 ice cream cones.", "They are 99 percent good")
    expect_equivalent(replace_number(x), output1)
    expect_equivalent(replace_number(y), output1)
    expect_equivalent(replace_number(x, FALSE), output2)
})