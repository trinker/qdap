#' Wrap Character Strings to Format Paragraphs
#' 
#' A wrapper for \code{\link[base]{as.character}} that writes to the Mac/Windows 
#' clipboard.
#' 
#' @param text  character vector, or an object which can be converted to a 
#' character vector by \code{\link[base]{as.character}}.
#' @param width A positive integer giving the target column for wrapping lines 
#' in the output.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard.
#' @return Prints a wrapped text vector to the console and copies the wrapped 
#' text to the clipboard on a Mac or Windows machine.
#' @seealso \code{\link[base]{strwrap}}
#' @keywords string-wrap
#' @export
#' @examples
#' \dontrun{
#' x <- paste2(DATA$state, sep = " " )
#' strWrap(x)
#' strWrap(x, 10)
#' #should be copied to the clipboard on a Mac or Windows machine.
#' }
strWrap <-
function(text = "clipboard", width = 70, copy2clip = interactive()) {
    if (Sys.info()["sysname"] != "Windows") {
        writeClipboard <- NULL
    }  
    if (text == "clipboard") {
        if (Sys.info()["sysname"] == "Darwin") {        
            pcon <- pipe("pbpaste")
            text <- paste(scan(pcon, what="character", 
                quiet=TRUE), collapse=" ")
            close(pcon)
        }                                             
        if (Sys.info()["sysname"] == "Windows") {
            text <- paste(utils::readClipboard(), collapse=" ")
        }
        if(!Sys.info()["sysname"] %in% c("Darwin", "Windows")) {
          warning("not Windows or Darwin:
                \b\b\b\b\b\b\b\bmay not be able to read from the clipboard")
        }
    } 
    x <- gsub("\\s+", " ", gsub("\n|\t", " ", text))
    x <- strwrap(x, width = width)
    if(copy2clip){
        if (Sys.info()["sysname"] == "Windows") {
            writeClipboard(x, format = 1)
        }
        if (Sys.info()["sysname"] == "Darwin") {           
            j <- pipe("pbcopy", "w")                       
            writeLines(x, con = j)                               
            close(j)                                    
        }             
    }
    writeLines(x)
}
