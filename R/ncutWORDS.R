#helper function for word_list (not exported)
ncutWORDS <- 
function(freqWORDSobj, cut.n = 15) {
    FREQ <- NULL
    ncutWORDSa <- function(freqWORDS.DF, cut.ns) {
        if (nrow(freqWORDS.DF) > cut.ns) {
            subset(freqWORDS.DF, FREQ >= freqWORDS.DF[cut.ns, "FREQ"])
        } else {
            freqWORDS.DF
        }
    }
    if (is.data.frame(freqWORDSobj) & is.data.frame(freqWORDSobj)) {
        ncutWORDSa(freqWORDS.DF = freqWORDSobj, cut.ns = cut.n)
    } else {
        lapply(freqWORDSobj, function(x) ncutWORDSa(x, cut.ns = cut.n))
    }
} 
