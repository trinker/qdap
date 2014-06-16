context("Checking automated_readability_index")


test_that("automated_readability_index gives list of data.frames with correct dimensions",{

    AR1 <- with(rajSPLIT, automated_readability_index(dialogue, list(person, act)))
    
    expect_true(is.list(AR1))
    expect_true(all(names(AR1) == c("Counts", "Readability")))
    expect_true(is.data.frame(scores(AR1)))
    expect_true(all(dim(scores(AR1)) == c(69, 5)))
    expect_true(is.data.frame(counts(AR1)))
    expect_true(all(dim(counts(AR1)) == c(2145, 5)))

})

test_that("coleman_liau gives list of data.frames with correct dimensions",{

    CL1 <- with(rajSPLIT, coleman_liau(dialogue, list(person, act)))
    
    expect_true(is.list(CL1))
    expect_true(all(names(CL1) == c("Counts", "Readability")))
    expect_true(is.data.frame(scores(CL1)))
    expect_true(all(dim(scores(CL1)) == c(69, 5)))
    expect_true(is.data.frame(counts(CL1)))
    expect_true(all(dim(counts(CL1)) == c(2145, 5)))

})


test_that("SMOG gives list of data.frames with correct dimensions",{

    SM2 <- with(rajSPLIT[1:100, ], SMOG(dialogue, list(sex, fam.aff)))
    
    expect_true(is.list(SM2))
    expect_true(all(names(SM2) == c("Counts", "Readability")))
    expect_true(is.data.frame(scores(SM2)))
    expect_true(all(dim(scores(SM2)) == c(1, 5)))
    expect_true(is.data.frame(counts(SM2)))
    expect_true(all(dim(counts(SM2)) == c(53, 5)))

})


test_that("flesch_kincaid gives list of data.frames with correct dimensions",{

    FL2 <-  with(rajSPLIT[1:50, ], flesch_kincaid(dialogue, list(sex, fam.aff)))
    
    expect_true(is.list(FL2))
    expect_true(all(names(FL2) == c("Counts", "Readability")))
    expect_true(is.data.frame(scores(FL2)))
    expect_true(all(dim(scores(FL2)) == c(2, 6)))
    expect_true(is.data.frame(counts(FL2)))
    expect_true(all(dim(counts(FL2)) == c(50, 5)))

})

test_that("fry gives list of data.frames with correct dimensions",{

    FR1 <- with(rajSPLIT, fry(dialogue, list(sex, fam.aff)))

    expect_true(is.list(FR1))
    expect_true(all(names(FR1) == c("Counts", "Readability")))
    expect_true(is.data.frame(scores(FR1)))
    expect_true(all(dim(scores(FR1)) == c(5, 3)))
    expect_true(is.data.frame(counts(FR1)))
    expect_true(all(dim(counts(FR1)) == c(15, 4)))

})

test_that("linsear_write gives list of data.frames with correct dimensions",{

    LW1 <- with(rajSPLIT, linsear_write(dialogue, list(person, act)))
    
    expect_true(is.list(LW1))
    expect_true(all(names(LW1) == c("Counts", "Readability")))
    expect_true(is.data.frame(scores(LW1)))
    expect_true(all(dim(scores(LW1)) == c(43, 4)))
    expect_true(is.data.frame(counts(LW1)))
    expect_true(all(dim(counts(LW1)) == c(2145, 5)))


})