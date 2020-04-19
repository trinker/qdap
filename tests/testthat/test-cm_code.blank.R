context("Checking cm_code.blank")

test_that("cm_code.blank, gives the desired output",{
    
    foo <- list(
        AA = qcv(terms="1:10"),
        BB = qcv(terms="1:2, 3:10, 19"),
        CC = qcv(terms="1:3, 5:6")
    )
    
    foo2  <- list(
        AA = qcv(terms="4:8"),
        BB = qcv(terms="1:4, 10:12"),
        CC = qcv(terms="1, 11, 15:20"),
        DD = qcv(terms="")
    )
    
    ## Single occurrence version
    x <- cm_range2long(object = list(foo=foo))
    
    x1_c <- structure(list(code = c("AA", "BB", "BB", "CC", "CC", "ABC", 
        "ABC"), start = c(0L, 0L, 18L, 0L, 4L, 0L, 19L), end = c(10L, 
        10L, 19L, 3L, 6L, 18L, 20L)), row.names = c(NA, -7L), class = c("cmspans", 
        "cmrange", "data.frame"))
    
    
    x1 <- cm_code.blank(x, combine.code.list = list(ABC=qcv(AA, BB, CC)),
        overlap = "!=1")
    
    expect_equivalent(x1, x1_c)
    
    ## Repeated measures version
    z <- cm_range2long(v.name="time", object=list(foo = foo, foo2 = foo2))
    
    x2_c <- structure(list(code = c("AA", "BB", "BB", "CC", "CC", "ABC", 
        "ABC", "AA", "BB", "BB", "CC", "CC", "CC", "ABC", "ABC", "ABC", 
        "ABC", "ABC", "ABC"), start = c(0L, 0L, 18L, 0L, 4L, 0L, 19L, 
        3L, 0L, 9L, 0L, 10L, 14L, 0L, 3L, 8L, 10L, 12L, 20L), end = c(10L, 
        10L, 19L, 3L, 6L, 18L, 20L, 8L, 4L, 12L, 1L, 11L, 20L, 1L, 4L, 
        9L, 11L, 14L, 21L), time = c("foo", "foo", "foo", "foo", "foo", 
        "foo", "foo", "foo2", "foo2", "foo2", "foo2", "foo2", "foo2", 
        "foo2", "foo2", "foo2", "foo2", "foo2", "foo2")), class = c("cmspans", 
        "vname_time", "data.frame"), row.names = c(NA, -19L))
    
    x2 <- cm_code.blank(z, combine.code.list = list(ABC=qcv(AA, BB, CC)),
        rm.var = "time", overlap = "!=1")
    
    expect_equivalent(x2, x2_c)
    
    x3_c <- structure(list(code = c("AA", "BB", "BB", "CC", "CC", "AA_and_BB", 
        "AA", "BB", "BB", "CC", "CC", "CC", "AA_and_BB"), start = c(0L, 
        0L, 18L, 0L, 4L, 0L, 3L, 0L, 9L, 0L, 10L, 14L, 3L), end = c(10L, 
        10L, 19L, 3L, 6L, 10L, 8L, 4L, 12L, 1L, 11L, 20L, 4L), time = c("foo", 
        "foo", "foo", "foo", "foo", "foo", "foo2", "foo2", "foo2", "foo2", 
        "foo2", "foo2", "foo2")), class = c("cmspans", "vname_time", 
        "data.frame"), row.names = c(NA, -13L))
    
    
    x3 <- cm_code.blank(z, combine.code.list = list(AA_and_BB=qcv(AA, BB)),
        rm.var = "time", overlap = TRUE)
    
    expect_equivalent(x3, x3_c)
    
    x4_c <- structure(list(code = c("AA", "BB", "BB", "CC", "CC", "AA_or_BB", 
        "AA_or_BB", "AA", "BB", "BB", "CC", "CC", "CC", "AA_or_BB", "AA_or_BB"
        ), start = c(0L, 0L, 18L, 0L, 4L, 0L, 18L, 3L, 0L, 9L, 0L, 10L, 
        14L, 0L, 9L), end = c(10L, 10L, 19L, 3L, 6L, 10L, 19L, 8L, 4L, 
        12L, 1L, 11L, 20L, 8L, 12L), time = c("foo", "foo", "foo", "foo", 
        "foo", "foo", "foo", "foo2", "foo2", "foo2", "foo2", "foo2", 
        "foo2", "foo2", "foo2")), class = c("cmspans", "vname_time", 
        "data.frame"), row.names = c(NA, -15L))
    
    
    x4 <- cm_code.blank(z, combine.code.list = list(AA_or_BB=qcv(AA, BB)),
        rm.var = "time", overlap = FALSE)
    
    expect_equivalent(x4, x4_c)
    
    
    x <- list(
        transcript_time_span = qcv(00:00 - 1:12:00),
        A = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00"),
        B = qcv(terms = "2.40, 3.01:3.02, 5.01, 6.02:7.00, 9.00,
            1.12.00:1.19.01"),
        C = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00, 17.01")
    )
    
    y <- list(
        transcript_time_span = qcv(00:00 - 1:12:00),
        A = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00"),
        B = qcv(terms = "2.40, 3.01:3.02, 5.01, 6.02:7.00, 9.00,
            1.12.00:1.19.01"),
        C = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00, 17.01")
    )
    
    dat <- cm_time2long(v.name="time", object=list(x = x, y = y))
    
    
    out <- cm_code.blank(dat, combine.code.list = list(ABC=qcv(A, B, C)),
        rm.var = "time", overlap = "!=1")
    
    
    expect_true(all(dim(out) ==c(38, 6)))
    
})