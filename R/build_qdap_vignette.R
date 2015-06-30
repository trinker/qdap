#' Replace Temporary Introduction to qdap Vignette
#' 
#' Replaces the temporary (place holder) \emph{Introduction to qdap Vignette} 
#' with the actual vignette.  
#' 
#' @param download.html logical.  If \code{TRUE} the file will be downloaded 
#' from: \url{http://trinker.github.io/qdap/vignettes/qdap_vignette.html}.  This 
#' @return Places the (1) HTML, (2) source, & (3) R code for the 
#' \emph{Introduction to qdap Vignette} in the user's 
#' \file{R-VERSION/library/qdap/doc}.  
#' @note The \pkg{knitr} built HTML approach above takes about 4 minutes.  The 
#' user may choose the faster approach (< 30 seconds) that downloads the HTML 
#' file directly from the Internet (this is for the latest CRAN release of 
#' \pkg{qdap}).  This choice is controlled via the \code{download.html} 
#' argument.  The function will ask for the user's permission before writing the 
#' documents. Once the user has run this function 
#' \code{browseVignettes(package = 'qdap')} will allow access to the new
#' vignette files.
#' @keywords vignette
#' @importFrom tools buildVignettes
#' @export
build_qdap_vignette <- function(download.html = FALSE) {

    path <- system.file("Rmd_vignette", package = "qdap")
    path2 <- system.file("doc", package = "qdap")

    message(paste0("`qdap` wants to rebuild the Intro Vignette in:\n\n    ", 
        path2, "\n\nDo you want to continue?\n"))
    ans <- utils::menu(c("Yes", "No"))
    if (ans == "2") {
        stop("`build_qdap_vignette` build aborted")
    }  
    
    fls <- paste0("qdap_vignette", c(".Rmd", ".html", ".R", "imgs"))
    new <- file.path(path, fls)
    old <- file.path(path2, fls)
       
    ## Here we build the R code by untangling the Rmd
    knitr::knit(new[1], output = old[3], tangle=TRUE)
    file.copy(new[1], old[1], overwrite = TRUE)

    ## =========================================================    
    ## Here we choose between building the HTML file from Rmd
    ## or downloading from the Internet
    ## =========================================================
    if (!download.html){   
        ## BUILD FROM Rmd
        if (file.info(old[2])[["size"]] > 2000000) {
            message(paste0("It appears the qdap Intro Vignette in:\n\n    ", 
                path2, "\nwas already built.\n\nDo you want to still render?\n"))
            ans <- utils::menu(c("Yes", "No"))
            if (ans == "2") {
                stop("`build_qdap_vignette` build aborted")
            }  

        }     

        ## set working directory to qdpa_Rmd directory temporarily
        WD <- getwd()
        on.exit(setwd(WD))
        setwd(path)
        suppressWarnings(knitr::knit2html(new[1], output = old[2], 
            stylesheet=file.path(path, 'css/style.css'),
            options=c("use_xhtml", "smartypants", "mathjax", "highlight_code", "base64_images")))
    } else {     
        ## IMPORT FROM INTERNET
        url <- paste0("https://raw.githubusercontent.com/trinker/qdap", 
            "/master/inst/Rmd_vignette/qdap_vignette.html")
        
        bin <- RCurl::getBinaryURL(url, ssl.verifypeer = FALSE)
        temp <- tempdir()
        con <- file(old[2], open = "wb")
        writeBin(bin, con)
        close(con)
    }
}
