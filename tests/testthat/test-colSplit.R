context("Checking colSplit")


test_that("colSplit splits columns and column names",{

    foo1 <- data.frame(`A&B&C` = paste2(CO2[, 1:3]), check.names=F)
    
    foo1_c <- structure(list(A = structure(c(10L, 10L, 10L, 10L, 10L, 10L), .Label = c("Mc1", 
        "Mc2", "Mc3", "Mn1", "Mn2", "Mn3", "Qc1", "Qc2", "Qc3", "Qn1", 
        "Qn2", "Qn3"), class = "factor"), B = structure(c(2L, 2L, 2L, 
        2L, 2L, 2L), .Label = c("Mississippi", "Quebec"), class = "factor"), 
            C = structure(c(2L, 2L, 2L, 2L, 2L, 2L), .Label = c("chilled", 
            "nonchilled"), class = "factor")), .Names = c("A", "B", "C"
        ), row.names = c(NA, 6L), class = "data.frame")
    
    
    expect_identical(head(colSplit(foo1)), foo1_c)
    
    foo2  <- paste2(mtcars[, 1:3], sep="|")
    
    foo2_c <- structure(list(X1 = structure(c(16L, 16L, 19L, 17L, 13L, 12L), .Label = c("10.4", 
        "13.3", "14.3", "14.7", "15", "15.2", "15.5", "15.8", "16.4", 
        "17.3", "17.8", "18.1", "18.7", "19.2", "19.7", "21", "21.4", 
        "21.5", "22.8", "24.4", "26", "27.3", "30.4", "32.4", "33.9"), class = "factor"), 
            X2 = structure(c(2L, 2L, 1L, 2L, 3L, 2L), .Label = c("4", 
            "6", "8"), class = "factor"), X3 = structure(c(8L, 8L, 1L, 
            11L, 18L, 10L), .Label = c("108", "120.1", "120.3", "121", 
            "140.8", "145", "146.7", "160", "167.6", "225", "258", "275.8", 
            "301", "304", "318", "350", "351", "360", "400", "440", "460", 
            "472", "71.1", "75.7", "78.7", "79", "95.1"), class = "factor")), .Names = c("X1", 
        "X2", "X3"), row.names = c(NA, 6L), class = "data.frame")
    
    expect_identical(head(colSplit(foo2, col.sep = "|")), foo2_c)


})


