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
#' will ensure that variable is classed as character.  If \code{NULL} 
#' \code{\link[qdap]{read.transcript}} attempts to guess the text.variable 
#' (dialogue).
#' @param merge.broke.tot logical.  If \code{TRUE} and if the file being read in 
#' is .docx with broken space between a single turn of talk read.transcript 
#' will attempt to merge these into a single turn of talk.
#' @param header logical.  If \code{TRUE} the file contains the names of the 
#' variables as its first line.
#' @param dash A character string to replace the en and em dashes special 
#' characters (default is to remove).
#' @param ellipsis A character string to replace the ellipsis special characters 
#' (default is text ...).
#' @param quote2bracket logical. If \code{TRUE} replaces curly quotes with curly 
#' braces (default is \code{FALSE}).  If \code{FALSE} curly quotes are removed.
#' @param rm.empty.rows logical.  If \code{TRUE} 
#' \code{\link[qdap]{read.transcript}}  attempts to remove empty rows.
#' @param na.strings A vector of character strings which are to be interpreted 
#' as \code{NA} values.
#' @param sep The field separator character. Values on each line of the file are 
#' separated by this character.  The default of \code{NULL} instructs 
#' \code{\link[qdap]{read.transcript}} to use a separator suitable for the file 
#' type being read in.
#' @param skip Integer; the number of lines of the data file to skip before 
#' beginning to read data.
#' @param nontext2factor logical.  If \code{TRUE} attempts to convert any 
#' non-text to a factor.
#' @param text Character string: if file is not supplied and this is, then data 
#' are read from the value of text. Notice that a literal string can be used to 
#' include (small) data sets within R code.
#' @param comment.char A character vector of length one containing a single 
#' character or an empty string. Use \code{""} to turn off the interpretation of 
#' comments altogether.
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
#' @seealso \code{\link[qdap]{dir_map}}
#' @export
#' @import XML RCurl
#' @importFrom openxlsx read.xlsx
#' @importFrom tools file_ext
#' @examples
#' \dontrun{
#' #Note: to view the document below use the path:
#' system.file("extdata/transcripts/", package = "qdap")
#' (doc1 <- system.file("extdata/transcripts/trans1.docx", package = "qdap"))
#' (doc2 <- system.file("extdata/transcripts/trans2.docx", package = "qdap"))
#' (doc3 <- system.file("extdata/transcripts/trans3.docx", package = "qdap"))
#' (doc4 <- system.file("extdata/transcripts/trans4.xlsx", package = "qdap"))
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
#' ## read.transcript(doc3, skip = 1) #incorrect read; wrong sep
#' dat4 <- read.transcript(doc3, sep = "-", skip = 1); truncdf(dat4, 40)
#' 
#' dat5 <- read.transcript(doc4); truncdf(dat5, 40) #an .xlsx file
#' trans <- "sam: Computer is fun. Not too fun.
#' greg: No it's not, it's dumb.
#' teacher: What should we do?
#' sam: You liar, it stinks!"
#' 
#' read.transcript(text=trans)
#' 
#' ## Read in text specify spaces as sep
#' ## EXAMPLE 1
#' 
#' read.transcript(text="34    The New York Times reports a lot of words here.
#' 12    Greenwire reports a lot of words.
#' 31    Only three words.
#'  2    The Financial Times reports a lot of words.
#'  9    Greenwire short.
#' 13    The New York Times reports a lot of words again.", 
#'     col.names=qcv(NO,    ARTICLE), sep="   ")
#' 
#' ## EXAMPLE 2
#' 
#' read.transcript(text="34..    The New York Times reports a lot of words here.
#' 12..    Greenwire reports a lot of words.
#' 31..    Only three words.
#'  2..    The Financial Times reports a lot of words.
#'  9..    Greenwire short.
#' 13..    The New York Times reports a lot of words again.", 
#'     col.names=qcv(NO,    ARTICLE), sep="\\.\\.")
#' }
read.transcript <-
function(file, col.names = NULL, text.var = NULL, merge.broke.tot = TRUE, 
    header = FALSE, dash = "", ellipsis = "...", quote2bracket = FALSE, 
    rm.empty.rows = TRUE, na.strings = c("999", "NA", "", " "), 
    sep = NULL, skip = 0, nontext2factor = TRUE, text, comment.char = "", 
    ...) {
    if (missing(file) && !missing(text)) {
        file <- textConnection(text)
        on.exit(close(file))
        y <- "text"
    } else {
        y <- file_ext(file)
    }

    ## Handling for text= && multi-char sep
    revert <- FALSE
    if (!is.null(sep) && !missing(text) && nchar(sep) > 1) {
    
        text <- gsub(sep, "QDAP_SEP_HOLDER", text)                
        text <- gsub(":", "QDAP_PLACE_HOLDER", text)
        text <- gsub("QDAP_SEP_HOLDER", ":", text)
        sep <- ":"
        revert <- TRUE
    
    }

    if (is.null(sep)) {
        if (y %in% c("docx", "txt", "text")) {
            sep <- ":"
        } else {
            sep <- ","
        }
    }
    switch(y, 
        xlsx = {
            x <- read.xlsx(file, colNames = header, 
                sep.names = sep, na.strings= na.strings, 
                skipEmptyRows = rm.empty.rows, ...)
            },
        xls = {
            x <- read.xlsx(file, colNames = header, 
                sep.names = sep, na.strings= na.strings, 
                skipEmptyRows = rm.empty.rows, ...)
            },
        docx = {
            x <- read.docx(file, skip = skip, sep = sep)
            sep_hits <- grepl(sep, x[, 2])
            if(any(sep_hits)) {
                warning(sprintf("The following text contains the \"%s\" separator and may not have split correctly:\n", sep), 
                    paste(which(sep_hits), collapse=", "))
                }
            },
        csv = {
            x <- utils::read.csv(file,  header = header, 
                sep = sep, as.is=FALSE, na.strings= na.strings, 
                strip.white = TRUE, stringsAsFactors = FALSE, 
                blank.lines.skip = rm.empty.rows, ...)
            },
        doc = stop("convert file to docx"),
        txt = {
            x <- utils::read.table(file=file, header = header, sep = sep, skip=skip)
        },
        text = {
            x <- utils::read.table(text=text, header = header, sep = sep, skip=skip)
            if(revert) {
                x[, 2] <- gsub("QDAP_PLACE_HOLDER", ":", x[, 2])
                x[, 1] <- gsub("QDAP_PLACE_HOLDER", ":", x[, 1])
            }
        },
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
