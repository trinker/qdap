#' Break Transcript Dialogue into Blank Code Matrix
#' 
#' Breaks transcript dialogue into words while retaining the demographic factors 
#' associate with each word.  The codes argument provides a matrix of zeros that 
#' can serve as a dummy coded matrix of codes per word.
#' 
#' @param dataframe A dataframe containing a text variable.
#' @param text.var The name of the text variable.
#' @param codes Optional list of codes.
#' @param csv logical.  If TRUE creates a csv in the working directory.
#' @param file.name The name of the csv file.  If NULL defaults to the dataframe 
#' name.
#' @param transpose logical.  If TRUE transposes the dataframe so that the text 
#' is across the top. 
#' @param strip logical.  If TRUE all punctuation is removed.
#' @param \ldots Other arguments passed to strip.
#' @return Generates a dataframe, and optional csv file, of individual words 
#' while maintaining demographic information.  If a vector of codes is provided the 
#' outcome is a matrix of words used by codes filled with zeros.  This dataframe 
#' is useful for dummy coded (1-yes code exists; 2-no it does not) 
#' representation of data and can be used for visualizations and statistical 
#' analysis.
#' @seealso 
#' \code{\link{cm_range2long}},
#' \code{\link{cm_df.fill}}
#' @references Miles, M. B. & Huberman, A. M. (1994). An expanded sourcebook: 
#' Qualitative   data analysis. 2nd ed. Thousand Oaks, CA: SAGE Publications.
#' @keywords coding
#' @export
#' @examples
#' \dontrun{
#' codes <- qcv(dc, sf, wes, pol, rejk, lk, azx, mmm)
#' cm_df.temp(DATA, "state", codes)
#' cm_df.temp(DATA, "state", codes, transpose = TRUE)
#' head(cm_df.temp(raj.act.1, "dialogue", codes))
#' cm_df.temp(raj.act.1, "dialogue", codes, transpose = TRUE)[, 1:9]
#' }
cm_df.temp <- function(dataframe, text.var, codes = NULL, csv = TRUE, 
    file.name = NULL, transpose = FALSE, strip =FALSE, ...){
    tv <- as.character(dataframe[, text.var])
    if (strip) {
        tv <- strip(tv, ...)
    }
    wrds <- lapply(tv, function(x) Trim(unlist(strsplit(x, " "))))
    lens <- sapply(wrds, length) 
    leftover <- dataframe[, !colnames(dataframe) %in% text.var, drop =FALSE]
    if (!is.null(codes)) {
        lcodes <- length(codes)
        lwrds <- length(unlist(wrds))
        MAT <- matrix(rep(0, lcodes*lwrds), lwrds, lcodes)
        colnames(MAT) <- codes      
        DF <- data.frame(leftover[rep(1:nrow(leftover), lens), , 
            drop = FALSE], text=unlist(wrds), word.num = 1:lwrds, MAT)  
    } else {
        DF <- data.frame(leftover[rep(1:nrow(leftover), lens), , 
            drop = FALSE], text=unlist(wrds)) 
    }
    DF[, "word.num"] <- 1:nrow(DF)
    if (transpose) {
        DF <- t(DF)
        DF <- data.frame(vars = rownames(DF), DF, check.names = FALSE)
    }
    rownames(DF) <- NULL
    if(csv) {
        if (is.null(file.name)) {
            file.name <- as.character(substitute(dataframe))
        } 
        write.table(DF, file = paste0(file.name, ".csv"),  sep = ",", 
            col.names = T, row.names=F, qmethod = "double") 
    }
    return(DF)
} 
