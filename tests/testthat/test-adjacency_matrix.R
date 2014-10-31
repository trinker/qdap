context("Checking adjacency_matrix")

test_that("adjacency_matrix gives desired output",{

    words <- c(" you", " the", "it", "oo")
    Terms <- with(DATA, termco(state, list(sex, adult), words))
    Terms
    adj <- adjacency_matrix(Terms)
    nms <- c("boolean", "adjacency", "shared", "sum")
    
    expect_equivalent(nms, names(adj))
    expect_true(is.matrix(adj$boolean))
    expect_true(all(dim(adj$boolean) == c(4, 4)))
    expect_false(isSymmetric(adj$boolean))

    expect_true(is.matrix(adj$adjacency))
    expect_true(all(dim(adj$adjacency) == c(4, 4)))
    expect_true(isSymmetric(adj$adjacency))
    expect_equivalent(diag(adj$adjacency), adj$sum)
    
    expect_true(is.matrix(adj$shared))
    expect_true(all(dim(adj$shared) == c(3, 3)))
    expect_false(isSymmetric(adj$shared))
    expect_true(all(sapply(adj$shared[upper.tri(adj$shared)], is.na)))
	
})
