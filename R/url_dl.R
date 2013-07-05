#' Download Instructional Documents
#' 
#' This function enables downloading documents for future instructional training. 
#' 
#' @param \ldots Document names to download. Quoted strings (complete urls) can
#' also be supplied (if so no url argument is supplied).
#' @param url The download url or dropbox key. 
#' @return Places a copy of the downloaded document in the users working 
#' directory.
#' @note Not intended for general use.
#' @export
#' @examples
#' \dontrun{
#' ## Example 1 (download from dropbox)
#' # download transcript of the debate to working directory
#' url_dl(pres.deb1.docx, pres.deb2.docx, pres.deb3.docx)   
#' 
#' # load multiple files with read transcript and assign to working directory
#' dat1 <- read.transcript("pres.deb1.docx", c("person", "dialogue"))
#' dat2 <- read.transcript("pres.deb2.docx", c("person", "dialogue"))
#' dat3 <- read.transcript("pres.deb3.docx", c("person", "dialogue"))
#' 
#' docs <- qcv(pres.deb1.docx, pres.deb2.docx, pres.deb3.docx)
#' dir() %in% docs
#' delete(docs)    #remove the documents
#' dir() %in% docs
#' 
#' ## Example 2 (quoted string urls)
#' url_dl("https://dl.dropboxusercontent.com/u/61803503/qdap.pdf", 
#'    "http://www.cran.r-project.org/doc/manuals/R-intro.pdf")
#'
#' delete(qcv(qdap.pdf, R-intro.pdf))
#' }
url_dl <-
function(..., url = 61803503) {
    if (!grepl("http|www\\.", url)) {
        url <- tail(unlist(strsplit(as.character(url), "/", fixed = TRUE)), 
            1)
        url <- paste0("http://dl.dropboxusercontent.com/u/", 
            url, "/")
    }
    mf <- match.call(expand.dots = FALSE)
    payload <- as.character(mf[[2]])
    FUN <- function(x, url) {
        bin <- getBinaryURL(paste0(url, x), ssl.verifypeer = FALSE)
        con <- file(x, open = "wb")
        writeBin(bin, con)
        close(con)
        message(noquote(paste(x, "read into", getwd())))
    }
    if (all(grepl("http|www\\.", payload))) {
        File <- basename(payload)
        Root <- dirname(payload)
        for (i in seq_len(length(payload))) {
            FUN(x = File[i], url = Root[i])
        }
    } else {
        invisible(lapply(payload, function(z) FUN(x = z, url = url)))
    }
}