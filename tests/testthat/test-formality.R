context("Checking formality")

test_that("formality gives the desired output",{
    
    DATA[1:2, "state"] <- c("", NA)
    x <- with(DATA, formality(state, list(sex, adult)))

    ## expected <- lapply(c(scores, counts, proportions, preprocessed), function(F) F(x))
    ## dput(expected)

    expected <- list(structure(list(`sex&adult` = c("f.1", "m.0", "f.0", "m.1"
        ), word.count = c(6L, 24L, 10L, 4L), formality = c(33.3333333333333, 
        31.25, 20, 0)), class = c("formality_scores", "data.frame"), type = "formality_scores", row.names = c("1", 
        "2", "3", "4")), structure(list(`sex&adult` = c("f.0", "f.1", 
        "m.0", "m.1"), word.count = c(10L, 6L, 24L, 4L), noun = c(0, 
        0, 4, 0), adj = c(1, 1, 2, 0), prep = c(1, 1, 0, 0), articles = c(0L, 
        0L, 1L, 0L), pronoun = c(3, 1, 7, 2), verb = c(4, 2, 8, 2), adverb = c(1, 
        1, 1, 0), interj = c(0, 0, 0, 0), other = c(0, 0, 1, 0)), class = c("table_count", 
        "data.frame"), type = "formality_counts", row.names = c("1", 
        "2", "3", "4")), structure(list(`sex&adult` = c("f.0", "f.1", 
        "m.0", "m.1"), word.count = c(10L, 6L, 24L, 4L), noun = c(0, 
        0, 0.166666666666667, 0), adj = c(0.1, 0.166666666666667, 0.0833333333333333, 
        0), prep = c(0.1, 0.166666666666667, 0, 0), articles = c(0, 0, 
        0.0416666666666667, 0), pronoun = c(0.3, 0.166666666666667, 0.291666666666667, 
        0.5), verb = c(0.4, 0.333333333333333, 0.333333333333333, 0.5
        ), adverb = c(0.1, 0.166666666666667, 0.0416666666666667, 0), 
            interj = c(0, 0, 0, 0), other = c(0, 0, 0.0416666666666667, 
            0)), class = c("table_proportion", "data.frame"), type = "formality_proportions", row.names = c("1", 
        "2", "3", "4")), structure(list(POStagged = c(NA, NA, "what/WP should/MD we/PRP do/VB", 
        "you/PRP liar/VBP it/PRP stinks/VB", "i/PRP am/VBP telling/VBG the/DT truth/NN", 
        "how/WRB can/MD we/PRP be/VB certain/JJ", "there/EX is/VBZ no/DT way/NN", 
        "i/FW distrust/NN you/PRP", "what/WP are/VBP you/PRP talking/VBG about/IN", 
        "shall/MD we/PRP move/VB on/IN good/JJ then/RB", "i/NN 'm/VBP hungry/JJ let/VBD 's/PRP eat/VB you/PRP already/RB"
        ), POStags = list(NA, NA, c("WP", "MD", "PRP", "VB"), c("PRP", 
        "VBP", "PRP", "VB"), c("PRP", "VBP", "VBG", "DT", "NN"), c("WRB", 
        "MD", "PRP", "VB", "JJ"), c("EX", "VBZ", "DT", "NN"), c("FW", 
        "NN", "PRP"), c("WP", "VBP", "PRP", "VBG", "IN"), c("MD", "PRP", 
        "VB", "IN", "JJ", "RB"), c("NN", "VBP", "JJ", "VBD", "PRP", "VB", 
        "PRP", "RB")), word.count = c(NA, NA, 4L, 4L, 5L, 5L, 4L, 3L, 
        5L, 6L, 8L)), class = c("pos_preprocessed", "data.frame"), type = "formality_preprocessed", row.names = c("1", 
        "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")))
    
    expect_equal(scores(x), expected[[1]])
    expect_equal(counts(x), expected[[2]])
    expect_equal(proportions(x), expected[[3]])
    expect_equal(preprocessed(x), expected[[4]])

    DATA <- qdap::DATA
    
})

