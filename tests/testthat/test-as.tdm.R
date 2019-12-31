context("Checking as.tdm")
# expect_true <- isTRUE; expect_equivalent <- all.equal

test_that("as.tdm, as.dtm, as.DocumentTermMatrix, and as.TermDocumentMatrix 
    convert wfm, character and existing TermDocumentMatrix/DocumentTermMatrix",{

    expect_true(inherits(as.dtm(DATA$state, DATA$person), "DocumentTermMatrix"))
    expect_true(inherits(as.tdm(DATA$state, DATA$person), "TermDocumentMatrix"))

    x <- wfm(DATA$state, DATA$person)
    expect_true(inherits(as.dtm(x), "DocumentTermMatrix"))
    expect_true(inherits(as.tdm(x), "TermDocumentMatrix"))
    expect_true(inherits(as.dtm(as.tdm(x)), "DocumentTermMatrix"))
    expect_true(inherits(as.tdm(as.dtm(x)), "TermDocumentMatrix"))

    expect_true(inherits(as.DocumentTermMatrix(x), "DocumentTermMatrix"))
    expect_true(inherits(as.TermDocumentMatrix(x), "TermDocumentMatrix"))
    expect_true(inherits(as.DocumentTermMatrix(as.TermDocumentMatrix(x)), "DocumentTermMatrix"))
    expect_true(inherits(as.TermDocumentMatrix(as.DocumentTermMatrix(x)), "TermDocumentMatrix"))

})

test_that("apply_as_tm applies tm functions and then optionally converts back",{

    a <- with(DATA, wfm(state, list(sex, adult)))
    
    x1 <- apply_as_tm(a, tm:::removeSparseTerms, sparse=0.6)
    x2 <- apply_as_tm(a, tm:::findAssocs, "computer", .8)
    x3 <- apply_as_tm(a, tm:::findFreqTerms, 2, 3)
    x4 <- apply_as_tm(a, tm::weightBin)
    x5 <- apply_as_tm(a, tm::weightBin, to.qdap = FALSE)
    x6 <- apply_as_tm(a, tm::weightSMART)
    x7 <- apply_as_tm(a, tm::weightTfIdf)
    
    types <- list(c("wfm", "true.matrix", "matrix", "array"), "list", "character", 
        c("weighted_wfm", "matrix", "array"), c("TermDocumentMatrix", 
        "simple_triplet_matrix"), c("weighted_wfm", "matrix", "array"
    ), c("weighted_wfm", "matrix", "array"))
    
    actual <- lapply(list(x1, x2, x3, x4, x5, x6, x7), class)
    
    ## Can't use because R 3.x and 4.x have different ways of class(matrix(M)) where the later says 'array' too
    ## Eventually we can drop this portion until the excpect_equivalent
    conds <- Map(function(a, e){
        
        list(actual = a[!a %in% 'array'], expected = e[!e %in% 'array'])
        
        }, actual, types)
    
    types <- conds$expected
    actual <- conds$actual
    
    expect_equivalent(actual, types)
    

})

test_that("as.Corpus and as.data.frame methods work to convert between qadp/tm",{
    
    library(tm)
    reut21578 <- system.file("texts", "crude", package = "tm")
    reuters <- Corpus(DirSource(reut21578),
        readerControl = list(reader = readReut21578XML))
    
    corp_df <- as.data.frame(reuters)
    expect_true(is.data.frame(corp_df))
    expect_true(all(dim(corp_df) == c(20, 2)))
    
    z <- as.Corpus(DATA$state, DATA$person,
           demographic=DATA[, qcv(sex, adult, code)])
    expect_true(inherits(z, "Corpus"))
    expect_true(is.data.frame(as.data.frame(z)))
    expect_true(all(dim(as.data.frame(z)) == c(5, 7)))

})

test_that("apply_as_df allows users to apply data.frame based functions to a Corpus",{
    
    library(tm)
    reut21578 <- system.file("texts", "crude", package = "tm")
    reuters <- Corpus(DirSource(reut21578),
        readerControl = list(reader = readReut21578XML))
    
    matches <- list(
        oil = qcv(oil, crude),
        money = c("economic", "money")
    )
    
    
    x1 <- apply_as_df(reuters, word_list)
    x2 <- apply_as_df(reuters, Dissimilarity)
    x3 <- apply_as_df(reuters, diversity)
    x4 <- apply_as_df(reuters, character_table)
    x5 <- apply_as_df(reuters, termco, match.list = matches)
    x6 <- suppressWarnings(apply_as_df(reuters, word_cor, word = unlist(matches)))
    x7 <- apply_as_df(reuters, freq_terms, at.least = 3)
    
    outs <- c("word_list", "Dissimilarity", "diversity", "character_table", 
        "termco", "word_cor", "freq_terms")
    
    actual <- lapply(list(x1, x2, x3, x4, x5, x6, x7), is)
    
    ## Can't use because R 3.x and 4.x have different ways of class(matrix(M)) where the later says 'array' too
    ## Eventually we can drop this portion until the excpect_equivalent
    conds <- Map(function(a, e){
        
        list(actual = a[!a %in% 'array'], expected = e[!e %in% 'array'])
        
        }, actual, outs)
    
    outs <- conds$expected
    actual <- conds$actual
    
    expect_equivalent(actual, outs)

})