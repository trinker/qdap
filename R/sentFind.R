sentFind <-
function(dataframe, n, margin = 2, text.col = "all") {
    surround <- function(N, mar, DF) {
        x <- c((N - mar:1), N, (N + 1:mar))
        x[x > 0 & x < nrow(DF)]
    }
    z <- if (margin == 0) {
        n
    } else {
        surround(N = n, mar = margin, DF = dataframe)
    }
    if (text.col == "all" | is.null(text.col)) {
        dataframe[z, ]
    } else {
        as.character(dataframe[z, text.col])
    }
}
