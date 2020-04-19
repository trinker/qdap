#' Range Code Sheet
#'
#' Generates a range coding sheet for coding words.
#' 
#' @param codes Character vector of codes.
#' @param text.var The text variable.     
#' @param grouping.var The grouping variables.  Also takes a single grouping 
#' variable or a list of 1 or more grouping variables. 
#' @param file A connection, or a character string naming the file to print to 
#' (.txt or .doc is recommended). 
#' @references Miles, M. B. & Huberman, A. M. (1994). An expanded sourcebook: 
#' Qualitative   data analysis. 2nd ed. Thousand Oaks, CA: SAGE Publications.
#' @keywords coding
#' @seealso 
#' \code{\link[qdap]{cm_time.temp}}
#' @export
#' @examples
#' \dontrun{
#' cm_range.temp(qcv(AA, BB, CC))
#' with(DATA, cm_range.temp(qcv(AA, BB, CC), state, list(person, adult)))
#' ## cm_range.temp(qcv(AA, BB, CC), file = "foo.txt")
#' ## delete("foo.txt")
#' }
cm_range.temp <-
function(codes, text.var = NULL, grouping.var = NULL, file=NULL) {
    if (Sys.info()["sysname"] != "Windows") {
        writeClipboard <- NULL
    }  
    if (!is.null(text.var) | !is.null(grouping.var)) {
        if (c(!is.null(text.var) & !is.null(grouping.var))) {
            key <- TRUE    
            if (!is.list(grouping.var)) {
                G <- as.character(substitute(grouping.var))            
                G <- G[length(G)]   
                group <- list(grouping.var) 
            } else {
                m <- unlist(as.character(substitute(grouping.var))[-1])
                G <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                        x[length(x)]                                   
                    }                                                  
                )  
                group <- grouping.var
            }
        } else {
            key <- FALSE
            ins <- ifelse(is.null(text.var), "grouping.var", "text.var")
            warning(paste0(
                "Both text.var and a list of grouping.var must be supplied:\n", 
                "  -", ins, " was ignored-"))
        }
    } else {
        key <- FALSE
    }
    x1 <- matrix(c("list(", 
        paste0("    ", codes[1:(length(codes)-1)], " = qcv(terms=''),"),
        paste0("    ", codes[length(codes)], " = qcv(terms='')"),
        ")"), ncol = 1)
    if (key) {
        DF <- data.frame(text.var, group)
        names(DF)[-1] <- G
        DF2 <- cm_df.temp(DF, "text.var")
        colcodes <- lapply(G, function(x) {
            y <- DF2[, c(x, "word.num")]
            z <- split(y[, 2], y[, 1])
            w <- lapply(z, function(x) {
                diffs <- c(1, diff(x))
                start_indexes <- c(1, which(diffs > 1))
                end_indexes <- c(start_indexes - 1, length(x))
                sings <- (start_indexes - end_indexes[-1]) == 0
                coloned <- paste(x[start_indexes], x[end_indexes], sep=":")
                coloned[sings] <- sapply(strsplit(coloned[sings], "\\:"), "[", 1)
                paste0(coloned, collapse=", ")        
            })
            names(w) <- paste(x, names(w), sep = "_")
            w
        })
        colcodes <- unlist(colcodes)
        x1 <- matrix(c(x1[1, ], 
            paste0("    ", names(colcodes), " = qcv(terms='", colcodes, "'),"), 
            x1[-1, ]), ncol = 1)
    }
    message(paste(x1, collapse="\n"))
    dimnames(x1) <- list(c(rep("", nrow(x1))), c(""))
    if (Sys.info()["sysname"] == "Windows") {
        writeClipboard(noquote(x1), format = 1)                        
    }                                                        
    if (Sys.info()["sysname"] == "Darwin") {                 
        j <- pipe("pbcopy", "w")                             
        writeLines(noquote(x1), con = j)                               
        close(j)  
    }
    if (!is.null(file)) {                                   
        cat(paste(x1, collapse="\n"), file = file) 
    }   
}
