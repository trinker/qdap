#helper function for word_list (not exported)
ncutWORD <-
function(freqWORDS.DF, cut.n = 15) {
    FREQ <- NULL
    if (nrow(freqWORDS.DF) > cut.n) {
        subset(freqWORDS.DF, FREQ >= freqWORDS.DF[cut.n, "FREQ"])
    } else {
        freqWORDS.DF
    }
}
