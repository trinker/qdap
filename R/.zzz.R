.onLoad <- function(libname = find.package("qdap"), 
    pkgname = "qdap") {
    if (!"pacman" %in% .packages(all.available = TRUE)) {
        install.packages("devtools")
        library(devtools)
        install_github("pacman", "trinker")
    }
    if (!"openNLP" %in% .packages(all.available = TRUE)) {
        install.packages("openNLP", type = "source")
    }    
    require(pacman)
    if (!"XML" %in% .packages(all.available = TRUE)) {
        p_getRipley("XML")
    }
    if (!"XML" %in% .packages(all.available = TRUE)) {
        p_getRipley("RCurl")
    }    
    require(openNLP)
    require(XML)
    require(RCurl)
}