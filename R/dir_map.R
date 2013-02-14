#' Map Transcript Files from a Directory to a Script
#' 
#' Generate script text (and optionally output it to the clipboard and/or an 
#' external file) that can be used to individually read in every file in a 
#' directory and assign it to an object.
#' 
#' @param loc The path/location of the transcript data files.
#' @param obj.prefix A character string that will be used as the prefix (followed 
#' by a unique digit) as the assignment object.
#' @param col.names Supplies a vector of column names to the transcript columns.
#' @param file A connection, or a character string naming the file to print to.
#' @param copy2clip logical.  If TRUE attempts to copy the output to the 
#' clipboard.
#' @details Generally, the researcher will want to read in and parse every 
#' transcript document separately.  The task of writing the script for multiple 
#' transcript documents can be tedious.  This function is designed to make the 
#' process more efficient and less prone to errors.  
#' @note \code{skip} is set to 0, however, it is likely that this value will 
#' need to be changed for each transcript.
#' @return Prints a read in script text to the console, optionally copies the 
#' wrapped text to the clipboard on a Mac or Windows machine and optionally 
#' prints to an outside file.
#' @seealso \code{\link[qdap]{read.transcript}}
#' @export
#' @examples
#' \dontrun{
#' (DIR <- system.file("extdata", package = "qdap"))
#' dir_map(DIR)
#' }
dir_map <- 
function(loc, obj.prefix = "dat", col.names = c("person", "dialogue"), 
    file = NULL, copy2clip = TRUE) {
    if (Sys.info()["sysname"] != "Windows") {
        writeClipboard <- NULL
    }    
    WD <- getwd()
    setwd(loc)
    fls <- dir()
    file_ext <- function(x){
        pos <- regexpr("\\.([[:alnum:]]+)$", x)
        ifelse(pos > -1L, substring(x, pos + 1L), "")
    }
    exts <- sapply(fls, file_ext)
    if (any(!exts %in% c("docx", "csv", "xlsx"))) {
        warning("read.transcript only works with .docx, .csv or .xlsx")
    }
    lead <- function(string, digits) {
        sprintf(paste0("%0", digits, "d"), string)
    }
    len <- length(fls)
    digs <- nchar(as.character(max(len))) 
    vals <- lead(1:length(fls), digs)
    x <- paste0(obj.prefix, vals, " <- ", "read.transcript('", loc, "/", fls, 
        "', col.names = c('", paste(col.names, collapse = "', '"), 
        "'), skip = 0", ")")
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
    setwd(WD)
    writeLines(x)
    if (!is.null(file)) {
        cat(paste(x, collapse = "\n"), file = file)
    }
}


