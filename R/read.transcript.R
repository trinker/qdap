#' Read Transcripts Into R
#' 
#' Read a .docx, .csv or .xlsx files into R.
#' 
#' @aliases read.transcript
#' @param file the name of the file which the data are to be read from. Each row of the table appears as one line of the file. If it does not contain an absolute path, the file name is relative to the current working directory, getwd().
#' @param col.names supplies a vector of column names to the transcript columns.
#' @param text.var specifying the name of the text variable will ensure that variable is classed as character.  If NULL read.transcript attempts to guess the text.variable (dialogue).
#' @param merge.broke.tot if the file being read in is .docx and the transcript if formated to have broken space between a single turn of talk read.transcript will attempt to merge these into a single turn of talk.
#' @param header a logical value indicating whether the file contains the names of the variables as its first line.
#' @param dash character to replace the en and em dashes special characters (default is to remove).
#' @param ellipsis character to replace the ellipsis special characters (default is text ...).
#' @param quote2bracket a logical value indicating wheter to replace curly quotes with curly braces (default is FALSE).  If FALSE curly quotes are removed.
#' @param rm.empty.rows a logical value.  If TURE read.transcript attempts to remove empty rows.
#' @param na.strings a character vector of strings which are to be interpreted as NA values.
#' @param sep the field separator character. Values on each line of the file are separated by this character.  The default of NULL instructs read.transcript to use a separator suitable for the file type being read in.
#' @param skip integer: the number of lines of the data file to skip before beginning to read data.
#' @param \ldots Further arguments to be passed to read.table.
#' @return Retruns a dataframe of dialogue and people
#' @note If a transcript is a .docx file read transcript expects two columns (generally person and dialogue) with some sort of separator (default is colon separator).  .doc fils must be converted to .docx before reding in.
#' @seealso \url{https://github.com/trinker/qdap/wiki/Reading-Transcripts-into-R}
#' @keywords transcript
#' @examples
#' require(RCurl);library(qdap)
#' bin <- getBinaryURL("https://dl.dropbox.com/u/61803503/Test2.docx",
#'                     ssl.verifypeer=FALSE)  
#' con <- file("TEST5.docx", open = "wb")
#' writeBin(bin, con)
#' close(con)
#' dat <- read.transcript("TEST5.docx") 
#' dat    
#' delete("TEST5")  #clean up and delete the file
read.transcript <-
function(file, col.names = NULL, text.var = NULL, merge.broke.tot = TRUE, 
    header = FALSE, dash = "", ellipsis = "...", quote2bracket = FALSE, 
    rm.empty.rows = TRUE, na.strings = c("999", "NA", "", " "), 
    sep = NULL, skip = 0, ...) {
    y <- unlist(strsplit(file, "\\."))
    y <- y[[length(y)]]
    if (is.null(sep)) {
        if (y == "docx") {
            sep <- ":"
        } else {
            sep <- ","
        }
    }
    switch(y, 
        xlsx = {require(gdata) 
            x <-gdata::read.xls(file,  header = header, 
                sep = sep, as.is=FALSE, na.strings= na.strings, 
                strip.white = TRUE, stringsAsFactors = FALSE, 
                blank.lines.skip = rm.empty.rows, ...)
            },
        docx = {
            x <- read.docx(file, skip = skip, sep = sep)
            },
        csv = {
            x <- read.csv(file,  header = header, 
                sep = sep, as.is=FALSE, na.strings= na.strings, 
                strip.white = TRUE, stringsAsFactors = FALSE, 
                blank.lines.skip = rm.empty.rows, ...)
            },
        doc = stop("convert file to docx"),
        stop("invalid file extension:\n \bfile must be a .docx .csv .xls or .xlsx" )
    )
    if (!is.null(text.var) & !is.numeric(text.var)) {
        text.var <- which(colnames(x) == text.var)
    } else {
        text.col <- function(dataframe) {
            dial <- function(x) {
                if(is.factor(x) | is.character(x)) {
                    n <- max(nchar(as.character(x)))
                } else {
                    n <- NA
                }
            }
            which.max(unlist(lapply(dataframe, dial)))
        }
        text.var <- text.col(x)
    }
    x[, text.var] <- as.character(x[, text.var])
    x[, text.var] <- Trim(iconv(x[, text.var], "", "ASCII", "byte"))
    if (is.logical(quote2bracket)) {
        if (quote2bracket) {
            rbrac <- "}"
            lbrac <- "{"
        } else {
            lbrac <- rbrac <- ""
        }
    } else {
            rbrac <- quote2bracket[2]
            lbrac <- quote2bracket[1]
    }
    ser <- c("<e2><80><9c>", "<e2><80><9d>", "<e2><80><98>", "<e2><80><99>", 
        "<e2><80><9b>", "<ef><bc><87>", "<e2><80><a6>", "<e2><80><93>", 
        "<e2><80><94>", "<c3><a1>", "<c3><a9>")
    reps <- c(lbrac, rbrac, "'", "'", "'", "'", ellipsis, dash, dash, "a", "e")
    Encoding(x[, text.var]) <-"latin1"
    x[, text.var] <- clean(mgsub(ser, reps, x[, text.var]))
    if(rm.empty.rows) {
        x <- rm_empty_row(x) 
    }
    if (!is.null(col.names)) {
        colnames(x) <- col.names
    }
    if (merge.broke.tot) {
        x <- combine_tot(x)
    }
    return(x)
}