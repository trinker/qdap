context("Checking cm_2long")

test_that("cm_2long gives the desired output for ranges",{
    
    foo <- structure(list(AA = "1:10", BB = c("1:2,", "3:10,", "19"), CC = c("1:3,", 
        "5:6")), .Names = c("AA", "BB", "CC"))

    foo2 <- structure(list(AA = "4:8", BB = c("1:4,", "10:12"), CC = c("1,", 
        "11,", "15:20"), DD = ""), .Names = c("AA", "BB", "CC", "DD"))

    
    x2 <- structure(list(code = structure(c(1L, 2L, 2L, 2L, 3L, 3L), .Label = c("AA", 
        "BB", "CC"), class = "factor"), start = c(0, 0, 2, 18, 0, 4), 
            end = c(10, 2, 10, 19, 3, 6), variable = structure(c(1L, 
            1L, 1L, 1L, 1L, 1L), .Label = "foo", class = "factor")), .Names = c("code", 
        "start", "end", "variable"), row.names = c(NA, -6L), class = c("cmspans", 
        "cmrange", "cmrange2long", "vname_variable", "data.frame"))
    
    y2 <- structure(list(code = structure(c(1L, 2L, 2L, 2L, 3L, 3L, 1L, 
        2L, 2L, 3L, 3L, 3L), .Label = c("AA", "BB", "CC"), class = "factor"), 
            start = c(0, 0, 2, 18, 0, 4, 3, 0, 9, 0, 10, 14), end = c(10, 
            2, 10, 19, 3, 6, 8, 4, 12, 1, 11, 20), time = structure(c(1L, 
            1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c("foo", 
            "foo2"), class = "factor")), .Names = c("code", "start", 
        "end", "time"), row.names = c(NA, -12L), class = c("cmspans", 
        "cmrange", "cmrange2long", "vname_time", "data.frame"))
    
    x <- cm_range2long(object=list(foo=foo))
    y <- cm_range2long(object=list(foo=foo, foo2=foo2), v.name="time")
    
    expect_true(all.equal(x, x2))
    expect_true(all.equal(y, y2))

})

test_that("cm_2long gives the desired output for time spans",{
    
    bar1 <- structure(list(transcript_time_span = c("-", "0:0", "1:12:0"), 
            A = c("2.40:3.00,", "5.01,", "6.02:7.00,", "9.00"), B = c("2.40,", 
            "3.01:3.02,", "5.01,", "6.02:7.00,", "9.00,", "1.12.00:1.19.01"
            ), C = c("2.40:3.00,", "5.01,", "6.02:7.00,", "9.00,", "16.25:17.01"
            )), .Names = c("transcript_time_span", "A", "B", "C"))
    
    
    bar2 <- structure(list(transcript_time_span = c("-", "0:0", "1:12:0"), 
            A = c("2.40:3.00,", "5.01,", "6.02:7.00,", "9.00"), B = c("2.40,", 
            "3.01:3.02,", "5.01,", "6.02:7.00,", "9.00,", "1.12.00:1.19.01"
            ), C = c("2.40:3.00,", "5.01,", "6.02:7.00,", "9.00,", "17.01"
            )), .Names = c("transcript_time_span", "A", "B", "C"))

    x2 <- structure(list(code = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 
        2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L), .Label = c("A", "B", "C"), class = "factor"), 
            start = c(159, 300, 361, 539, 159, 180, 300, 361, 539, 4319, 
            159, 300, 361, 539, 984), end = c(180, 301, 420, 540, 160, 
            182, 301, 420, 540, 4741, 180, 301, 420, 540, 1021), Start = structure(c(0.00184027777777778, 
            0.00347222222222222, 0.00417824074074074, 0.00623842592592593, 
            0.00184027777777778, 0.00208333333333333, 0.00347222222222222, 
            0.00417824074074074, 0.00623842592592593, 0.0499884259259259, 
            0.00184027777777778, 0.00347222222222222, 0.00417824074074074, 
            0.00623842592592593, 0.0113888888888889), format = "h:m:s", class = "times"), 
            End = structure(c(0.00208333333333333, 0.0034837962962963, 
            0.00486111111111111, 0.00625, 0.00185185185185185, 0.00210648148148148, 
            0.0034837962962963, 0.00486111111111111, 0.00625, 0.0548726851851852, 
            0.00208333333333333, 0.0034837962962963, 0.00486111111111111, 
            0.00625, 0.0118171296296296), format = "h:m:s", class = "times"), 
            variable = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
            1L, 1L, 1L, 1L, 1L, 1L), .Label = "bar1", class = "factor")), .Names = c("code", 
        "start", "end", "Start", "End", "variable"), row.names = c(NA, 
        -15L), class = c("cmspans", "cmtime", "cmtime2long", "vname_variable", 
        "data.frame", "spans_4320"))
    
    y2 <- structure(list(code = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 
        2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 
        2L, 2L, 3L, 3L, 3L, 3L, 3L), .Label = c("A", "B", "C"), class = "factor"), 
            start = c(159, 300, 361, 539, 159, 180, 300, 361, 539, 4319, 
            159, 300, 361, 539, 984, 159, 300, 361, 539, 159, 180, 300, 
            361, 539, 4319, 159, 300, 361, 539, 1020), end = c(180, 301, 
            420, 540, 160, 182, 301, 420, 540, 4741, 180, 301, 420, 540, 
            1021, 180, 301, 420, 540, 160, 182, 301, 420, 540, 4741, 
            180, 301, 420, 540, 1021), Start = structure(c(0.00184027777777778, 
            0.00347222222222222, 0.00417824074074074, 0.00623842592592593, 
            0.00184027777777778, 0.00208333333333333, 0.00347222222222222, 
            0.00417824074074074, 0.00623842592592593, 0.0499884259259259, 
            0.00184027777777778, 0.00347222222222222, 0.00417824074074074, 
            0.00623842592592593, 0.0113888888888889, 0.00184027777777778, 
            0.00347222222222222, 0.00417824074074074, 0.00623842592592593, 
            0.00184027777777778, 0.00208333333333333, 0.00347222222222222, 
            0.00417824074074074, 0.00623842592592593, 0.0499884259259259, 
            0.00184027777777778, 0.00347222222222222, 0.00417824074074074, 
            0.00623842592592593, 0.0118055555555556), format = "h:m:s", class = "times"), 
            End = structure(c(0.00208333333333333, 0.0034837962962963, 
            0.00486111111111111, 0.00625, 0.00185185185185185, 0.00210648148148148, 
            0.0034837962962963, 0.00486111111111111, 0.00625, 0.0548726851851852, 
            0.00208333333333333, 0.0034837962962963, 0.00486111111111111, 
            0.00625, 0.0118171296296296, 0.00208333333333333, 0.0034837962962963, 
            0.00486111111111111, 0.00625, 0.00185185185185185, 0.00210648148148148, 
            0.0034837962962963, 0.00486111111111111, 0.00625, 0.0548726851851852, 
            0.00208333333333333, 0.0034837962962963, 0.00486111111111111, 
            0.00625, 0.0118171296296296), format = "h:m:s", class = "times"), 
            time = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
            1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
            2L, 2L, 2L, 2L, 2L), .Label = c("bar1", "bar2"), class = "factor")), .Names = c("code", 
        "start", "end", "Start", "End", "time"), row.names = c(NA, -30L
        ), class = c("cmspans", "cmtime", "cmtime2long", "vname_time", 
        "data.frame", "spans_4320||4320"))
    
    x <- cm_time2long(object=list(bar1=bar1))
    y <- cm_time2long(object=list(bar1=bar1, bar2=bar2), v.name="time")
    
    expect_true(all.equal(x, x2))
    expect_true(all.equal(y, y2))    
    
})
    