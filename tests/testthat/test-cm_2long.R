context("cm_2long clean")

test_that("cm_2long gives the desired output for code spans",{

    foo <- list(
        person_greg = qcv(terms='7:11, 20:24, 30:33, 49:56'),
        person_researcher = qcv(terms='42:48'),
        person_sally = qcv(terms='25:29, 37:41'),
        person_sam = qcv(terms='1:6, 16:19, 34:36'),
        person_teacher = qcv(terms='12:15'),
        adult_0 = qcv(terms='1:11, 16:41, 49:56'),
        adult_1 = qcv(terms='12:15, 42:48'),
        AA = qcv(terms="1"),
        BB = qcv(terms="1:2, 3:10, 19"),
        CC = qcv(terms="1:9, 100:150")
    )
    
    foo2  <- list(
        person_greg = qcv(terms='7:11, 20:24, 30:33, 49:56'),
        person_researcher = qcv(terms='42:48'),
        person_sally = qcv(terms='25:29, 37:41'),
        person_sam = qcv(terms='1:6, 16:19, 34:36'),
        person_teacher = qcv(terms='12:15'),
        adult_0 = qcv(terms='1:11, 16:41, 49:56'),
        adult_1 = qcv(terms='12:15, 42:48'),
        AA = qcv(terms="40"),
        BB = qcv(terms="50:90"),
        CC = qcv(terms="60:90, 100:120, 150"),
        DD = qcv(terms="")
    )
    
    expected1 <- structure(list(code = structure(c(1L, 1L, 1L, 1L, 2L, 3L, 3L, 
        4L, 4L, 4L, 5L, 6L, 6L, 6L, 7L, 7L, 8L, 9L, 9L, 9L, 10L, 10L, 
        1L, 1L, 1L, 1L, 2L, 3L, 3L, 4L, 4L, 4L, 5L, 6L, 6L, 6L, 7L, 7L, 
        8L, 9L, 10L, 10L, 10L), .Label = c("person_greg", "person_researcher", 
        "person_sally", "person_sam", "person_teacher", "adult_0", "adult_1", 
        "AA", "BB", "CC"), class = "factor"), start = c(6, 19, 29, 48, 
        41, 24, 36, 0, 15, 33, 11, 0, 15, 48, 11, 41, 0, 0, 2, 18, 0, 
        99, 6, 19, 29, 48, 41, 24, 36, 0, 15, 33, 11, 0, 15, 48, 11, 
        41, 39, 49, 59, 99, 149), end = c(11, 24, 33, 56, 48, 29, 41, 
        6, 19, 36, 15, 11, 41, 56, 15, 48, 1, 2, 10, 19, 9, 150, 11, 
        24, 33, 56, 48, 29, 41, 6, 19, 36, 15, 11, 41, 56, 15, 48, 40, 
        90, 90, 120, 150), time = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
        2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
        2L, 2L, 2L, 2L, 2L), .Label = c("foo", "foo2"), class = "factor")), .Names = c("code", 
        "start", "end", "time"), row.names = c(NA, -43L), class = c("cmspans", 
        "cmrange", "cmrange2long", "vname_time", "data.frame"))
    
    expect_true(identical(cm_2long(foo, foo2, v.name = "time"), expected1))
    
})    
    
test_that("cm_2long gives the desired output for time spans",{
    
    x <- list(
        transcript_time_span = qcv(00:00 - 1:12:00),
        A = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00"),
        B = qcv(terms = "2.40, 3.01:3.02, 5.01, 6.02:7.00,
            9.00, 1.12.00:1.19.01"),
        C = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00, 17.01")
    )
    
    expected2 <- structure(list(code = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 
        2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L), .Label = c("A", "B", "C"), class = "factor"), 
            start = c(159, 300, 361, 539, 159, 180, 300, 361, 539, 4319, 
            159, 300, 361, 539, 1020), end = c(180, 301, 420, 540, 160, 
            182, 301, 420, 540, 4741, 180, 301, 420, 540, 1021), Start = structure(c(0.00184027777777778, 
            0.00347222222222222, 0.00417824074074074, 0.00623842592592593, 
            0.00184027777777778, 0.00208333333333333, 0.00347222222222222, 
            0.00417824074074074, 0.00623842592592593, 0.0499884259259259, 
            0.00184027777777778, 0.00347222222222222, 0.00417824074074074, 
            0.00623842592592593, 0.0118055555555556), format = "h:m:s", class = "times"), 
            End = structure(c(0.00208333333333333, 0.0034837962962963, 
            0.00486111111111111, 0.00625, 0.00185185185185185, 0.00210648148148148, 
            0.0034837962962963, 0.00486111111111111, 0.00625, 0.0548726851851852, 
            0.00208333333333333, 0.0034837962962963, 0.00486111111111111, 
            0.00625, 0.0118171296296296), format = "h:m:s", class = "times"), 
            variable = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
            1L, 1L, 1L, 1L, 1L, 1L), .Label = "x", class = "factor")), .Names = c("code", 
        "start", "end", "Start", "End", "variable"), row.names = c(NA, 
        -15L), class = c("cmspans", "cmtime", "cmtime2long", "vname_variable", 
        "data.frame", "spans_4320"))
    
    expect_true(all.equal(cm_2long(x), expected2))
 
})
    
