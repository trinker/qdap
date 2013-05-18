##currently not being exported
# Debug Range Coding
# 
# Debug a list of range codes for cm_range2long
# 
# @param cm.list.obj an object being prepared for the cm_time2long function
# @return Returns possible bugs if any. 
# @note Not intended for general use.  Used within cm_range2long.
# @seealso \code{\link[qdap]{cm_range2long}}
# @keywords debug
# @examples
# z <- list(
#     A = qcv(terms='0:12, 15:22, 199:22'),
#     B = qcv(terms='12:18, 9:0:99'),
#     C = qcv(terms='12'),
#     D = qcv(terms='12, 14:19'),
#     E = qcv(terms='0, 1O:19'),
#     F = qcv(terms='')
# )
# 
# cm_debug2(z)
cm_debug2 <- function(cm.list.obj){
    debug2 <-
    function(cm.list.obj) {
        FUN <- function(x) nchar(gsub("\\d|\\:|\\,", "", x))
        FUN2 <- function(x) {
            nchar(gsub("\\d|\\,", "", x)) -
            nchar(gsub("\\d|\\:|\\,", "", x))
        }
        L1 <- lapply(cm.list.obj, FUN)
        L2 <- lapply(seq_along(L1), function(i) {
            if (sum(L1[[i]]) == 0) {
                 NULL
            } else {
                 cm.list.obj[[i]][L1[[i]] > 0]
            }
        })
        names(L2) <- names(L1)
        L3 <- lapply(cm.list.obj, FUN2)
        L4 <- lapply(seq_along(L3), function(i) {
            if (!any(L3[[i]] > 1)) {
                 NULL
            } else {
                 cm.list.obj[[i]][L3[[i]] > 1]
            }
        })
        names(L4) <- names(L1)        
        L5 <- lapply(cm.list.obj, function(x) {
            if (all(x %in% "")) {
                return()
            }         
            a <- gsub(",", "", x)
            b <- suppressWarnings(as.numeric(unlist(strsplit(a, "\\:"))))
            if (any(is.na(b))) {
                return()
            }
            if (!any(diff(b) < 0)) {
                 NULL
            } else {
                 b[diff(b) < 0]
            }
        })
        list(ODD.CHARACTER = L2, EXTRA.COLON = L4, DECREASING = L5)
    }
    O <- debug2(cm.list.obj)
    OP <- unlist(O, recursive=TRUE)
    if (identical(as.character(OP), character(0))) {
        message("Obvious errors not found.")
    } else {
        message("Possible errors found:")
        OP2 <- lapply(seq_along(OP), function(i) unname(OP[i]))
        names(OP2) <- names(OP)
        OP2
    }
}
