#' Range Code Sheet
#'
#' Generates a range coding sheet for coding words.
#' 
#' @param codes List of codes.
#' @param file A connection, or a character string naming the file to print to (.txt is recommended).
#' @references Miles, M. B. & Huberman, A. M. (1994). An expanded sourcebook: Qualitative   data analysis. 2nd ed. Thousand Oaks, CA: SAGE Publications.
#' @keywords coding
#' @seealso 
#' \code{\link[qdap]{cm_time.temp}}
#' @examples
#' \dontrun{
#' cm_range.temp(qcv(AA, BB, CC), file = "foo.txt")
#' # delete("foo.txt")
#' }
cm_range.temp <-
function(codes, file=NULL) {
    x1 <- matrix(c("list(", 
        paste0("    ", codes[1:(length(codes)-1)], " = qcv(terms=''),"),
        paste0("    ", codes[length(codes)], " = qcv(terms='')"),
        ")"), ncol = 1)
    cat(paste0("list(\n",
        paste0("    ", paste0(paste(codes, 
        collapse = " = qcv(terms=''),\n    "), " = qcv(terms='')")), "\n)\n"))
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
        cat(paste0("list(\n",
            paste0("    ", paste0(paste(codes, 
            collapse = " = qcv(terms=''),\n    "), " = qcv(terms='')")), "\n)\n"),
            file = file) 
    }   
}