#' Read Transcripts Into R
#' 
#' Read .docx, .csv or .xlsx files into R.
#' 
#' @param file The name of the file which the data are to be read from. Each row 
#' of the table appears as one line of the file. If it does not contain an 
#' absolute path, the file name is relative to the current working directory, 
#' \code{getwd()}.
#' @param col.names  A character vector specifying the column names of the 
#' transcript columns.
#' @param text.var A character string specifying the name of the text variable 
#' will ensure that variable is classed as character.  If NULL 
#' \code{\link[qdap]{read.transcript}} attempts to guess the text.variable 
#' (dialogue).
#' @param merge.broke.tot logical.  If TRUE and if the file being read in is 
#' .docx with broken space between a single turn of talk read.transcript 
#' will attempt to merge these into a single turn of talk.
#' @param header logical.  If TRUE the file contains the names of the variables 
#' as its first line.
#' @param dash A character string to replace the en and em dashes special 
#' characters (default is to remove).
#' @param ellipsis A character string to replace the ellipsis special characters 
#' (default is text ...).
#' @param quote2bracket logical. If TRUE replaces curly quotes with curly braces 
#' (default is FALSE).  If FALSE curly quotes are removed.
#' @param rm.empty.rows logical.  If TURE \code{\link[qdap]{read.transcript}}  
#' attempts to remove empty rows.
#' @param na.strings A vector of character strings which are to be interpreted 
#' as NA values.
#' @param sep The field separator character. Values on each line of the file are 
#' separated by this character.  The default of NULL instructs 
#' \code{\link[qdap]{read.transcript}} to use a separator suitable for the file 
#' type being read in.
#' @param skip Integer; the number of lines of the data file to skip before 
#' beginning to read data.
#' @param nontext2factor logical.  If TRUE attempts to convert any non text to a 
#' factor.
#' @param \ldots Further arguments to be passed to \code{\link[utils]{read.table}}.
#' @return Returns a dataframe of dialogue and people.
#' @note If a transcript is a .docx file read transcript expects two columns 
#' (generally person and dialogue) with some sort of separator (default is colon 
#' separator).  .doc files must be converted to .docx before reading in.
#' @section Warning: \code{\link[qdap]{read.transcript}} may contain errors if the 
#' file being read in is .docx.  The researcher should carefully investigate 
#' each transcript for errors before further parsing the data.
#' @author Bryan Goodrich and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @references \url{https://github.com/trinker/qdap/wiki/Reading-.docx-\%5BMS-Word\%5D-Transcripts-into-R}
#' @keywords transcript
#' @export
#' @import XML gdata RCurl
#' @examples
#' \dontrun{
#' #Note: to view the document below use the path:
#' gsub("trans1.docx", "", system.file("extdata/trans1.docx", package = "qdap"))
#' (doc1 <- system.file("extdata/trans1.docx", package = "qdap"))
#' (doc2 <- system.file("extdata/trans2.docx", package = "qdap"))
#' (doc3 <- system.file("extdata/trans3.docx", package = "qdap"))
#' (doc4 <- system.file("extdata/trans4.xlsx", package = "qdap"))
#' 
#' dat1 <- read.transcript(doc1)
#' truncdf(dat1, 40)
#' dat2 <- read.transcript(doc1, col.names = c("person", "dialogue"))
#' truncdf(dat2, 40)
#' dat2b <- rm_row(dat2, "person", "[C") #remove bracket row
#' truncdf(dat2b, 40)
#' 
#' ## read.transcript(doc2) #throws an error (need skip)
#' dat3 <- read.transcript(doc2, skip = 1); truncdf(dat3, 40)
#' 
#' ## read.transcript(doc3, skip = 1) #throws an error; wrong sep
#' dat4 <- read.transcript(doc3, sep = "-", skip = 1); truncdf(dat4, 40)
#' 
#' dat5 <- read.transcript(doc4); truncdf(dat5, 40) #an .xlsx file
#' }
read.transcript <-
function(file, col.names = NULL, text.var = NULL, merge.broke.tot = TRUE, 
    header = FALSE, dash = "", ellipsis = "...", quote2bracket = FALSE, 
    rm.empty.rows = TRUE, na.strings = c("999", "NA", "", " "), 
    sep = NULL, skip = 0, nontext2factor = TRUE, ...) {
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
        xlsx = {
            x <- read.xls(file,  header = header, 
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
    if (nontext2factor) {
        x <- data.frame(sapply(x,  as.factor))
    }
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
        "<e2><80><94>", "<c3><a1>", "<c3><a9>", "<c2><bd>")
    reps <- c(lbrac, rbrac, "'", "'", "'", "'", ellipsis, dash, dash, "a", "e", "half")
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