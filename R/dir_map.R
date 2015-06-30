#' Map Transcript Files from a Directory to a Script
#' 
#' Generate script text (and optionally output it to the clipboard and/or an 
#' external file) that can be used to individually read in every file in a 
#' directory and assign it to an object.
#' 
#' @param loc The path/location of the transcript data files.
#' @param obj.prefix A character string that will be used as the prefix (followed 
#' by a unique digit) as the assignment object.
#' @param use.path logical.  If \code{TRUE} use the actual path to the 
#' \code{loc} argument.  If \code{FALSE}, the  code may be more portable in that 
#' the actual input to \code{loc} is supplied to the 
#' \code{\link[qdap]{read.transcript}}.
#' @param col.names Supplies a vector of column names to the transcript columns.
#' @param file A connection, or a character string naming the file to print to.
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
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
#' (DIR <- system.file("extdata/transcripts", package = "qdap"))
#' dir_map(DIR)
#' }
dir_map <- 
function(loc = "DATA/TRANSCRIPTS/CLEANED_TRANSCRIPTS", obj.prefix = "dat", 
    use.path = TRUE, col.names = c("person", "dialogue"), file = NULL, 
    copy2clip = interactive()) {
  
    if (Sys.info()["sysname"] != "Windows") {
        writeClipboard <- NULL
    }    
    fls <- dir(loc)
    
    if (identical(character(0), fls)) {
        stop(sprintf("No files located in:\n%s", loc))  
    }
    
    file_ext <- function(x){
        pos <- regexpr("\\.([[:alnum:]]+)$", x)
        ifelse(pos > -1L, substring(x, pos + 1L), "")
    }
    exts <- sapply(fls, file_ext)
    if (any(!exts %in% c("docx", "csv", "xlsx", "txt"))) {
        warning("read.transcript only works with .docx, .csv, .xlsx or .txt")
    }
    lead <- function(string, digits) {
        sprintf(paste0("%0", digits, "d"), string)
    }
    len <- length(fls)
    digs <- nchar(as.character(max(len))) 
    vals <- lead(1:length(fls), digs)
    if (use.path) {
        string <- paste0("\"", file.path(loc, fls), "\"")
    } else {
        basic <- utils::capture.output(substitute(loc))
        string <- paste0("file.path(", basic, ", \"", fls, "\")")
    }
    x <- paste0(obj.prefix, vals, " <- ", "read.transcript(",  
        string, ", col.names = c(\"", paste(col.names, collapse = "\", \""), 
        "\"), skip = 0", ")")
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
    if (!is.null(file)) {
        cat(paste(x, collapse = "\n"), file = file)
    }
}


