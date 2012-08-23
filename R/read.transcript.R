read.transcript <-
function(file, col.names = NULL, text.var = NULL, header = FALSE, dash = "",
    ellipsis = "...", quote2bracket = FALSE, rm.empty.rows = TRUE, 
    na.strings = c("999", "NA", "", " "), sep = ",", skip = 0, ...) {

    y <- unlist(strsplit(file, "\\.")); y[[length(y)]]
    switch(y, 
        xlsx = {require(gdata) 
            x <-gdata::read.xls(file,  header = header, 
                sep = sep, as.is=FALSE, na.strings= na.strings, 
                strip.white = TRUE, stringsAsFactors = FALSE, 
                blank.lines.skip = rm.empty.rows, ...)
            },
        docx = {
            x <- read.docx(file, skip = skip)
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
    if (quote2bracket == TRUE) {
        rbrac <- "}"
        lbrac <- "{"
    } else {
        if (length(quote2bracket) == 2) {
            rbrac <- quote2bracket[2]
            lbrac <- quote2bracket[1]
        } else {
            lbrac <- rbrac <- ""
        }
    }
    ser <- c("<e2><80><9c>", "<e2><80><9d>", "<e2><80><98>", "<e2><80><99>", 
        "<e2><80><9b>", "<ef><bc><87>", "<e2><80><a6>", "<e2><80><93>", 
        "<e2><80><94>")
    reps <- c(lbrac, rbrac, "'", "'", "'", "'", ellipsis, dash, dash)
    Encoding(x[, text.var]) <-"latin1"
    x[, text.var] <- clean(mgsub(ser, reps, x[, text.var]))
    if(rm.empty.rows) {
        x <- rm_empty_row(x) 
    }
    if (!is.null(col.names)) {
        colnames(x) <- col.names
    }
    return(x)
}