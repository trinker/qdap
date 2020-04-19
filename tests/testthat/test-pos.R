context("Checking pos")

test_that("pos gives the desired output",{
    
     DATA[1:2, "state"] <- c("", NA)
    posdat <- pos(DATA$state)

##     expected <- list(
##         posdat[["POSrnp"]],
##         preprocessed(posdat),
##         counts(posdat),
##         proportions(posdat)
##     )
##     dput(expected)
    
     expected <- list(structure(list(wrd.cnt = c(NA, NA, 4L, 4L, 5L, 5L, 4L, 3L, 
        5L, 6L, 8L), DT = c("NA(NA%)", "NA(NA%)", "0", "0", "1(20.0%)", 
        "0", "1(25.0%)", "0", "0", "0", "0"), EX = c("NA(NA%)", "NA(NA%)", 
        "0", "0", "0", "0", "1(25.0%)", "0", "0", "0", "0"), FW = c("NA(NA%)", 
        "NA(NA%)", "0", "0", "0", "0", "0", "1(33.3%)", "0", "0", "0"
        ), IN = c("NA(NA%)", "NA(NA%)", "0", "0", "0", "0", "0", "0", 
        "1(20.0%)", "1(16.7%)", "0"), JJ = c("NA(NA%)", "NA(NA%)", "0", 
        "0", "0", "1(20.0%)", "0", "0", "0", "1(16.7%)", "1(12.5%)"), 
            MD = c("NA(NA%)", "NA(NA%)", "1(25.0%)", "0", "0", "1(20.0%)", 
            "0", "0", "0", "1(16.7%)", "0"), NN = c("NA(NA%)", "NA(NA%)", 
            "0", "0", "1(20.0%)", "0", "1(25.0%)", "1(33.3%)", "0", "0", 
            "1(12.5%)"), PRP = c("NA(NA%)", "NA(NA%)", "1(25.0%)", "2(50.0%)", 
            "1(20.0%)", "1(20.0%)", "0", "1(33.3%)", "1(20.0%)", "1(16.7%)", 
            "2(25.0%)"), RB = c("NA(NA%)", "NA(NA%)", "0", "0", "0", 
            "0", "0", "0", "0", "1(16.7%)", "1(12.5%)"), VB = c("NA(NA%)", 
            "NA(NA%)", "1(25.0%)", "1(25.0%)", "0", "1(20.0%)", "0", 
            "0", "0", "1(16.7%)", "1(12.5%)"), VBD = c("NA(NA%)", "NA(NA%)", 
            "0", "0", "0", "0", "0", "0", "0", "0", "1(12.5%)"), VBG = c("NA(NA%)", 
            "NA(NA%)", "0", "0", "1(20.0%)", "0", "0", "0", "1(20.0%)", 
            "0", "0"), VBP = c("NA(NA%)", "NA(NA%)", "0", "1(25.0%)", 
            "1(20.0%)", "0", "0", "0", "1(20.0%)", "0", "1(12.5%)"), 
            VBZ = c("NA(NA%)", "NA(NA%)", "0", "0", "0", "0", "1(25.0%)", 
            "0", "0", "0", "0"), WP = c("NA(NA%)", "NA(NA%)", "1(25.0%)", 
            "0", "0", "0", "0", "0", "1(20.0%)", "0", "0"), WRB = c("NA(NA%)", 
            "NA(NA%)", "0", "0", "0", "1(20.0%)", "0", "0", "0", "0", 
            "0")), class = "data.frame", row.names = c(NA, -11L)), structure(list(
            POStagged = c(NA, NA, "what/WP should/MD we/PRP do/VB", "you/PRP liar/VBP it/PRP stinks/VB", 
            "i/PRP am/VBP telling/VBG the/DT truth/NN", "how/WRB can/MD we/PRP be/VB certain/JJ", 
            "there/EX is/VBZ no/DT way/NN", "i/FW distrust/NN you/PRP", 
            "what/WP are/VBP you/PRP talking/VBG about/IN", "shall/MD we/PRP move/VB on/IN good/JJ then/RB", 
            "i/NN 'm/VBP hungry/JJ let/VBD 's/PRP eat/VB you/PRP already/RB"
            ), POStags = list(NA, NA, c("WP", "MD", "PRP", "VB"), c("PRP", 
            "VBP", "PRP", "VB"), c("PRP", "VBP", "VBG", "DT", "NN"), 
                c("WRB", "MD", "PRP", "VB", "JJ"), c("EX", "VBZ", "DT", 
                "NN"), c("FW", "NN", "PRP"), c("WP", "VBP", "PRP", "VBG", 
                "IN"), c("MD", "PRP", "VB", "IN", "JJ", "RB"), c("NN", 
                "VBP", "JJ", "VBD", "PRP", "VB", "PRP", "RB")), word.count = c(NA, 
            NA, 4L, 4L, 5L, 5L, 4L, 3L, 5L, 6L, 8L)), class = c("pos_preprocessed", 
        "data.frame"), type = "pos_preprocessed", row.names = c("1", 
        "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")), structure(list(
            wrd.cnt = c(NA, NA, 4L, 4L, 5L, 5L, 4L, 3L, 5L, 6L, 8L), 
            DT = c(NA, NA, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 0L, 0L), EX = c(NA, 
            NA, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L), FW = c(NA, NA, 0L, 
            0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L), IN = c(NA, NA, 0L, 0L, 0L, 
            0L, 0L, 0L, 1L, 1L, 0L), JJ = c(NA, NA, 0L, 0L, 0L, 1L, 0L, 
            0L, 0L, 1L, 1L), MD = c(NA, NA, 1L, 0L, 0L, 1L, 0L, 0L, 0L, 
            1L, 0L), NN = c(NA, NA, 0L, 0L, 1L, 0L, 1L, 1L, 0L, 0L, 1L
            ), PRP = c(NA, NA, 1L, 2L, 1L, 1L, 0L, 1L, 1L, 1L, 2L), RB = c(NA, 
            NA, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L), VB = c(NA, NA, 1L, 
            1L, 0L, 1L, 0L, 0L, 0L, 1L, 1L), VBD = c(NA, NA, 0L, 0L, 
            0L, 0L, 0L, 0L, 0L, 0L, 1L), VBG = c(NA, NA, 0L, 0L, 1L, 
            0L, 0L, 0L, 1L, 0L, 0L), VBP = c(NA, NA, 0L, 1L, 1L, 0L, 
            0L, 0L, 1L, 0L, 1L), VBZ = c(NA, NA, 0L, 0L, 0L, 0L, 1L, 
            0L, 0L, 0L, 0L), WP = c(NA, NA, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 
            0L, 0L), WRB = c(NA, NA, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 
            0L)), class = c("table_count", "data.frame"), type = "pos_counts", row.names = c("1", 
        "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")), structure(list(
            wrd.cnt = c(NA, NA, 4L, 4L, 5L, 5L, 4L, 3L, 5L, 6L, 8L), 
            DT = c(NA, NA, 0, 0, 0.2, 0, 0.25, 0, 0, 0, 0), EX = c(NA, 
            NA, 0, 0, 0, 0, 0.25, 0, 0, 0, 0), FW = c(NA, NA, 0, 0, 0, 
            0, 0, 0.333333333333333, 0, 0, 0), IN = c(NA, NA, 0, 0, 0, 
            0, 0, 0, 0.2, 0.166666666666667, 0), JJ = c(NA, NA, 0, 0, 
            0, 0.2, 0, 0, 0, 0.166666666666667, 0.125), MD = c(NA, NA, 
            0.25, 0, 0, 0.2, 0, 0, 0, 0.166666666666667, 0), NN = c(NA, 
            NA, 0, 0, 0.2, 0, 0.25, 0.333333333333333, 0, 0, 0.125), 
            PRP = c(NA, NA, 0.25, 0.5, 0.2, 0.2, 0, 0.333333333333333, 
            0.2, 0.166666666666667, 0.25), RB = c(NA, NA, 0, 0, 0, 0, 
            0, 0, 0, 0.166666666666667, 0.125), VB = c(NA, NA, 0.25, 
            0.25, 0, 0.2, 0, 0, 0, 0.166666666666667, 0.125), VBD = c(NA, 
            NA, 0, 0, 0, 0, 0, 0, 0, 0, 0.125), VBG = c(NA, NA, 0, 0, 
            0.2, 0, 0, 0, 0.2, 0, 0), VBP = c(NA, NA, 0, 0.25, 0.2, 0, 
            0, 0, 0.2, 0, 0.125), VBZ = c(NA, NA, 0, 0, 0, 0, 0.25, 0, 
            0, 0, 0), WP = c(NA, NA, 0.25, 0, 0, 0, 0, 0, 0.2, 0, 0), 
            WRB = c(NA, NA, 0, 0, 0, 0.2, 0, 0, 0, 0, 0)), class = c("table_proportion", 
        "data.frame"), type = "pos_proportions", row.names = c("1", "2", 
        "3", "4", "5", "6", "7", "8", "9", "10", "11")))

    expect_equal(posdat[["POSrnp"]], expected[[1]])
    expect_equal(preprocessed(posdat), expected[[2]]) 
    expect_equal(counts(posdat), expected[[3]])       
    expect_equal(proportions(posdat), expected[[4]])   
    
    DATA <- qdap::DATA

    
})

