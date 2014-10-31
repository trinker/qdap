context("Checking chunker")


test_that("chunker produces lists of the correct dimensions",{

    x1 <- with(DATA, chunker(state, n.chunks = 10))
    x2 <- with(DATA, chunker(state, n.words = 10))
    x3 <- with(DATA, chunker(state, n.chunks = 10, as.string=FALSE))
    x4 <- with(DATA, chunker(state, n.chunks = 10, rm.unequal=TRUE))
    x5 <- with(DATA, chunker(state, person, n.chunks = 10))
    x6 <- with(DATA, chunker(state, list(sex, adult), n.words = 10))
    x7 <- with(DATA, chunker(state, person, n.words = 10, rm.unequal=TRUE))

    expect <- list(structure(11L, .Names = "all"), structure(6L, .Names = "all"), 
        structure(11L, .Names = "all"), structure(10L, .Names = "all"), 
        structure(c(10L, 1L, 10L, 11L, 1L), .Names = c("greg", "researcher", 
        "sally", "sam", "teacher")), structure(c(1L, 1L, 4L, 1L), .Names = c("f.0", 
        "f.1", "m.0", "m.1")), structure(c(2L, 0L, 0L, 1L, 0L), .Names = c("greg", 
        "researcher", "sally", "sam", "teacher")))

    expect_equivalent(lapply(list(x1, x2, x3, x4, x5, x6, x7), sapply, length), expect)
    
})

test_that("chunker unbags correctly",{

    x1 <- with(DATA, chunker(state, n.chunks = 10))
    x2 <- with(DATA, chunker(state, n.chunks = 10, as.string=FALSE))

    lens <- lapply(list(x1, x2), sapply, sapply, length)
    
    expect_true(all(head(lens[[2]], -1) > 1))
})
