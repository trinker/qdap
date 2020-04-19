#' Transform Codes to Start-End Durations
#' 
#' Transforms the range coding structure(s) from \code{\link[qdap]{cm_df.temp}}
#' (in list format) into a data frame of start and end durations in long format.
#' 
#' @param df.temp.obj A character vector of names of object(s) created by 
#' \code{\link{cm_df.temp}}, a list of \code{\link{cm_df.temp}} created objects 
#' or a data frame created by \code{\link{cm_df.temp}}.
#' @param v.name An optional name for the column created for the list.var 
#' argument.
#' @param list.var logical.  If \code{TRUE} creates a column for the data frame 
#' created by each time.list.
#' @param code.vars A character vector of code variables.  If \code{NULL} uses 
#' all variables from the first column after the column named word.num.
#' @param no.code The value to assign to no code; default is \code{NA}.
#' @param add.start.end logical.  If \code{TRUE} adds a column for start and end 
#' times.
#' @param repeat.vars A character vector of repeated/stacked variables.  If 
#' \code{NULL} uses all non code.vars variables.
#' @param rev.code logical.  If \code{TRUE} reverses the order of 
#' \code{code.vars} and \code{no.code} variables.
#' @return Generates a data frame of start and end times for each code.
#' @seealso 
#' \code{\link{cm_time2long}},
#' \code{\link{cm_range2long}},
#' \code{\link{cm_df.temp}}
#' @references Miles, M. B. & Huberman, A. M. (1994). An expanded sourcebook: 
#' Qualitative   data analysis. 2nd ed. Thousand Oaks, CA: SAGE Publications.
#' @export
#' @examples
#' \dontrun{
#' codes <- qcv(dc, sf, wes, pol, rejk, lk, azx, mmm)
#' x1 <- cm_df.temp(DATA, "state", codes)
#' head(x1)
#' 
#' #empty code matrix
#' out1 <- cm_df2long(x1,  code.vars = codes)
#' head(out1, 15)
#' 
#' #fill it randomly
#' x1[, 7:14] <- lapply(7:14,  function(i) sample(0:1, nrow(x1), TRUE))
#' out2 <- cm_df2long(x1,  code.vars = codes)
#' head(out2, 15)
#' plot(out2)
#' }
cm_df2long <-
function(df.temp.obj, v.name = "variable", list.var = TRUE, code.vars = NULL, 
    no.code = NA, add.start.end = TRUE, repeat.vars = NULL, 
    rev.code = FALSE){ 
    if (is.list(df.temp.obj)) { 
        if (is.data.frame(df.temp.obj)){
            objs <- as.character(substitute(df.temp.obj))
            L1 <- list(df.temp.obj)
            names(L1) <- objs    
        } else {
            objs <- names(df.temp.obj)
            L1 <- df.temp.obj
        }
    } else {
        objs <- df.temp.obj
        L1 <- lapply(objs, get)
        names(L1) <- objs
    }
    if (is.null(code.vars)) {
        code.vars <- (1 + which(colnames(L1[[1]]) == "word.num")):ncol(L1[[1]])
    }
    L2 <- lapply(L1, function(x) { 
            cm_d2l(x, code.vars = code.vars, no.code=no.code, 
                add.start.end = add.start.end, repeat.vars = repeat.vars, 
                rev.code = rev.code)
        }
    )
    if (list.var) {
        L2 <- lapply(seq_along(L2), function(i) data.frame(L2[[i]], 
            VAR = objs[i], stringsAsFactors = FALSE))
    }
    DF <- data.frame(do.call(rbind, L2), row.names = NULL, stringsAsFactors = FALSE)
    if (list.var) {
        colnames(DF)[ncol(DF)] <- v.name
    }
    if (is.null(v.name)) {
        v.name <- "*NULL*"
    }
    class(DF) <- c("cmspans", "cmrange", "cmdf2long", paste0("vname_", v.name), 
        class(DF))
    DF
}






