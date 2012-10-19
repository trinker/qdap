url_dl <-
function(..., url = "http://dl.dropbox.com/u/61803503/") {
    mf <- match.call(expand.dots = FALSE)
    payload <- as.character(mf[[2]])
    FUN <- function(x, url) {
        bin <- getBinaryURL(paste0(url, x), ssl.verifypeer=FALSE)  
        con <- file(x, open = "wb")
        writeBin(bin, con)
        close(con)
        print(noquote(paste(x, "read into", getwd())))
    }
    lapply(payload, function(z) FUN(x = z, url = url))
}