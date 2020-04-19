context("Checking colSplit")


test_that("colSplit splits columns and column names",{

    foo1 <- data.frame(`A&B&C` = paste2(CO2[, 1:3]), check.names=F)
    
    foo1_c <- structure(list(A = c("Qn1", "Qn1", "Qn1", "Qn1", "Qn1", "Qn1"
        ), B = c("Quebec", "Quebec", "Quebec", "Quebec", "Quebec", "Quebec"
        ), C = c("nonchilled", "nonchilled", "nonchilled", "nonchilled", 
        "nonchilled", "nonchilled")), row.names = c(NA, 6L), class = "data.frame")
    
    
    expect_equivalent(head(colSplit(foo1)), foo1_c)
    
    foo2  <- paste2(mtcars[, 1:3], sep="|")
    
    foo2_c <- structure(list(X1 = c("21", "21", "22.8", "21.4", "18.7", "18.1"
        ), X2 = c("6", "6", "4", "6", "8", "6"), X3 = c("160", "160", 
        "108", "258", "360", "225")), row.names = c(NA, 6L), class = "data.frame")
    
    expect_equivalent(head(colSplit(foo2, col.sep = "|")), foo2_c)


})

