#' Range Coding of a Code Matrix
#' 
#' Allows range coding of words for efficient coding.
#' 
#' @param dataframe A dataframe containing a text variable.
#' @param ranges A named list of ranges to recode.  Names correspond to code 
#' names in dataframe.
#' @param value The recode value.  Takes a vector of length one or a vector of 
#' length equal to the number of code columns.
#' @param text.var The name of the text variable.
#' @param code.vars Optional vector of codes.
#' @param transform logical.  If TRUE the words are located across the top of 
#' dataframe.
#' @return Generates a dummy coded dataframe.
#' @seealso 
#' \code{\link{cm_df.temp}},
#' \code{\link{cm_df2long}}
#' @references Miles, M. B. & Huberman, A. M. (1994). An expanded sourcebook: 
#' Qualitative   data analysis. 2nd ed. Thousand Oaks, CA: SAGE Publications.
#' @keywords coding
#' @export
#' @examples
#' \dontrun{
#' codes <- qcv(dc, sf, wes, pol, rejk, lk, azx, mmm)
#' X <- cm_df.temp(DATA, "state", codes)
#' 
#' #recommended structure
#' cds1 <- list(
#'      dc=c(1:3, 5), 
#'      sf=c(4, 6:9, 11), 
#'      wes=0, 
#'      pol=0, 
#'      rejk=0,
#'      lk=0, 
#'      azx=1:30, 
#'      mmm=5
#' )
#' cm_df.fill(X, cds1)
#' 
#' #recommended structure
#' cds2 <- list(
#'     sf=c(4, 6:9, 11), 
#'     dc=c(1:3, 5), 
#'     azx=1:30, 
#'     mmm=5
#' )
#' cm_df.fill(X, cds2)
#' }
cm_df.fill <-
function(dataframe, ranges, value = 1, text.var = NULL, code.vars = NULL, 
    transform = FALSE) {
    if (transform) {
        dataframe <- cm_df.transform(dataframe = dataframe, 
            text.var = text.var, code.vars = code.vars)
    }
    if (!is.null(text.var) && !is.numeric(text.var)) {
        text.var <- which(colnames(DF) == text.var)
    }
    if (!is.null(code.vars) && !is.numeric(code.vars)) {
        code.vars <- which(colnames(DF) %in% c(code.vars))
    }
    if (is.null(text.var)) {
        text.var <- which(colnames(dataframe) == "text")
    }
    if (is.null(code.vars)) {
        code.vars <- (text.var + 2):ncol(dataframe)
    }
    left.overs <- which(!1:ncol(dataframe) %in% c(code.vars, text.var))
    dataframe[, text.var] <- as.character(dataframe[, text.var])
    if (length(value) == 1) {
        value <- rep(value, length(code.vars))
    }
    CV <- dataframe[, code.vars]
    inds <- which(colnames(CV) %in% names(ranges))
    lapply(inds, function(i) {
            CV[ ranges[[colnames(CV)[i]]], colnames(CV)[i]] <<- value[i]
            return(CV)
        }
    )
    DF <- data.frame(dataframe[, 1:(text.var + 1)], CV, stringsAsFactors = FALSE)
    return(DF)
}
