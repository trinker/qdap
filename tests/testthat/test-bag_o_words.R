context("Checking bag_o_words")


test_that("bag_o_words, breaker, and word_split gives list of data.frames with correct dimensions",{

    expect_true(all(bag_o_words("I'm going home!") %in% c("i'm", "going", "home")))
    expect_true(all(bag_o_words("I'm going home!", apostrophe.remove = TRUE) %in% c("im", "going", "home")))
    
    x <- bag_o_words(DATA$state)
    expect_true(is.vector(x))
    expect_false(is.list(x))
    expect_true(length(x) == 53)
    
    y <- breaker(DATA$state)
    expect_true(is.vector(y))
    expect_false(is.list(y))
    expect_true(length(y) == 68)
    
    z <- word_split(c(NA, DATA$state))
    
    expect_true(is.vector(z))
    expect_true(is.list(z))
    expect_true(length(z) == nrow(DATA) + 1)
    expect_equivalent(sort(unique(unlist(z))), sort(unique(y)))

})

test_that("unbag gives a pasted string",{

    expect_true(unbag(bag_o_words("I'm going home!")) == "i'm going home")
    expect_true(unbag(breaker(DATA$state)) == unbag(word_split(c(NA, DATA$state))))


})