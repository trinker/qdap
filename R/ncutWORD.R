ncutWORD <-
function(freqWORDS.DF, cut.n = 15) {
    if (nrow(freqWORDS.DF) > cut.n) {
        subset(freqWORDS.DF, FREQ >= freqWORDS.DF[cut.n, "FREQ"])
    } else {
        freqWORDS.DF
    }
}
