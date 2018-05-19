#currently not being exported
# Debug Time Span Coding
# 
# Debug a list of time span codes for cm_time2long
# 
# @param cm.list.obj an object being prepared for the cm_time2long function
# @return Returns possible bugs if any.
# @note Not intended for general use.  Used within cm_time2long.
# @seealso \code{\link[qdap]{cm_time2long}}
# @keywords debug
# @examples
# video3 <- list(
#     transcript_time_span = qcv(terms='3:30 - 18:30'),
#     A2 = qcv(terms=''),
#     B1 = qcv(terms='3:33, 3.35, 3.39, 3.43:3.44, 3.50, 3.52:3.53, 
#         3.56, 4.12, 4.20, 4.30, 4.32, 4.35, 4.41, 4.52:4.53, 5.06'),
#     B2 = qcv(terms='3.46:3:47, 430, 8.36, 11.59:11.59, 14.22:14.27'),
#     B3 = qcv(terms='10.36, 9.12, 16.29')
# )
# 
# cm_debug(video3)
cm_debug <- 
function(cm.list.obj) {
    debug <- function(string){
        if (length(Trim(string)) == 1 && Trim(string)== "") {
            return(NULL)
        }
        tester <- function(x, y){
            p <- suppressWarnings(unlist(gregexpr(x, y, fixed = TRUE)))
                j <- suppressWarnings(if(length(p) == 1 && (is.na(p) |  p < 1)) { 
                    0 
                } else {
                    length(p)
                }
            )
            return(j)
        }
        cols <- sapply(string, tester, x=":") > 1
        pers <- sapply(string, tester, x=".") < 1
        v <- unlist(strsplit(gsub(",", "", string), ":"))
        w <- strsplit(v, "\\.")
        lens <- unlist(lapply(w, length))
        m2 <- lapply(seq_along(w), function(i){
            if(lens[i] == 1) {
                m <- c("00", "00", w[[i]])
            } 
            if(lens[i] == 2) {
                m <- c("00", w[[i]])
            } else {
                m <- w[[i]]
            }  
            return(m)      
        })
        d1 <- do.call(rbind, lapply(m2, as.numeric))
        inc <- c(FALSE, diff(d1[, 1]*60*60 + d1[, 2]*60 + d1[, 3]) < 0 )
        names(inc) <- paste2(do.call(rbind, m2))
        if (any(pers)) {inc <- NULL}
        list(COLON=cols, PERIOD=pers, INCREASE=inc)
    }
    cm.list.obj <- cm.list.obj[names(cm.list.obj) != "transcript_time_span"]
    LIST <- unlist(lapply(cm.list.obj, debug), recursive=FALSE)
    LIST <- LIST[!sapply(LIST, is.null)]
    L2 <- LIST[sapply(LIST, any)]
    L3 <- lapply(L2, function(x) names(x[x]))  
    if (identical(as.character(L3), character(0))) {
        message("Obvious errors not found.")
    } else {
        message("Possible errors found:")
        L3
    }
}
