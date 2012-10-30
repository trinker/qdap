#' Transform Codes to Start-End Durations
#' 
#' Transforms the range coding structure(s) from cm_df.temp (in list format) into a data frame of start and end durations in long format.
#' 
#' @param df.temp.obj a character vector of names of object(s) created by cm_df.temp
#' @param v.name sn optional name for the column created for the list.var argument
#' @param list.var logical.  If TRUE creates a column for the data frame created by each time.list passed to cm_t2l
#' @param code.vars a character vector of code variables.  If NULL uses all variables from the first column after the column named word.num.
#' @param no.code the value to assign to no code; default is NA
#' @param add.start.end logical.  If TURE adds a column for start and end times
#' @param repeat.vars a character vector of repeated/stacked variables.  If NULL uses all non code.vars variables.
#' @param rev.code logical.  If TRUE reverses the order of code.vars and no.code varaibles.
#' @return Generates a data frame of start and end times for each code.
#' @seealso 
#' \code{\link{cm_time2long}},
#' \code{\link{cm_range2long}},
#' \code{\link{cm_df.temp}}
#' @references Miles, M. B. & Huberman, A. M. (1994). An expanded sourcebook: Qualitative   data analysis. 2nd ed. Thousand Oaks, CA: SAGE Publications.
#' @keywords coding, time span
#' @examples
cm_df2long <-
function(df.temp.obj, v.name = "variable", list.var = TRUE, code.vars = NULL, 
    no.code = NA, add.start.end = TRUE, repeat.vars = NULL, 
    rev.code = FALSE){
    objs <- df.temp.obj
    L1 <- lapply(objs, get)
    names(L1) <- objs
    if (is.null(code.vars)) {
        code.vars <- which(colnames(L1[[1]]) == "word.num"):ncol(L1[[1]])
    }
    L2 <- lapply(L1, function(x) { 
            cm_d2l(x, code.vars = code.vars, no.code=no.code, 
                add.start.end = add.start.end, repeat.vars = repeat.vars, 
                rev.code = rev.code)
        }
    )
    if (list.var) {
        L2 <- lapply(seq_along(L2), function(i) data.frame(L2[[i]], 
            VAR = objs[i]))
    }
    DF <- data.frame(do.call(rbind, L2), row.names = NULL)
    if (list.var) {
        colnames(DF)[ncol(DF)] <- v.name
    }
    comment(DF) <- "cmrange"
    DF
}
