#' Range Code Sheet
#'
#' Generates a range coding sheet for coding words.
#' 
#' @param codes List of codes.
#' @param file A connection, or a character string naming the file to print to (.txt is recommended).
#' @references Miles, M. B. & Huberman, A. M. (1994). An expanded sourcebook: Qualitative   data analysis. 2nd ed. Thousand Oaks, CA: SAGE Publications.
#' @keywords coding
#' @seealso 
#' \code{\link[qdap]{cm_time.temp}},
#' @examples
#' \dontrun{
#' cm_range.temp(qcv(AA, BB, CC), file = "foo.txt")
#' # delete("foo.txt")
#' }
cm_range.temp <- 
function(codes, file=NULL) {
    cat(paste("list(\n",
        paste0("    ", paste0("\b", paste(codes, 
            collapse = " = c(),\n    "), " = c()")), "\n)\n")
    )
    x <- matrix(c("list(", 
        paste0("    ", codes[1:(length(codes)-1)], " = c(),"),
        paste0("    ", codes[length(codes)], " = c()"),
        ")"), ncol = 1)
    if (Sys.info()["sysname"] == "Windows") {                
        writeClipboard(x, format = 1)                        
    }                                                        
    if (Sys.info()["sysname"] == "Darwin") {                 
        j <- pipe("pbcopy", "w")                             
        writeLines(x, con = j)                               
        close(j)  
    }
    if (!is.null(file)) {                                           
        cat(paste0("list(\n",
            paste0("    ", paste0(paste(codes, 
                collapse = " = c(),\n    "), " = c()")), "\n)\n"),
        file=file)  
    }         
}