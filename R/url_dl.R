url_dl <-
function(x, url = "http://dl.dropbox.com/u/61803503/") {
    bin <- getBinaryURL(paste0(url, x), ssl.verifypeer=FALSE)  
    con <- file(x, open = "wb")
    writeBin(bin, con)
    close(con)
    print(noquote(paste(x, "read into", getwd())))
}
