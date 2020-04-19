#' Find Co-occurrence Between Dummy Codes
#'
#' Combine code columns where they co-occur.
#'
#' @param cm.l2d.obj An object from \code{\link[qdap]{cm_long2dummy}}.
#' @param combine.code A list of named character vectors of at least two code 
#' column names to combine
#' @param rm.var Name of the repeated measures column.  Default is "time".
#' @param overlap logical, integer or character of binary operator + integer.  
#' If \code{TRUE} finds the overlap.  If \code{FALSE} finds anywhere any of the 
#' codes occur.  If integer finds that exact combination of overlaps.  If 
#' character must be a logical vector 
#' c(\code{>}, \code{<}, \code{=<}, \code{=>}, \code{==}, \code{!=}) followed by 
#' an integer and wrapped with quotes.
#' @return Returns a dataframe with co-occurrences of provided code columns.
#' @seealso \code{\link[qdap]{cm_long2dummy}}
#' @keywords co-occurrence
#' @export
#' @examples
#' \dontrun{
#' foo <- list(
#'     AA = qcv(terms="1:10"),
#'     BB = qcv(terms="1:2, 3:10, 19"),
#'     CC = qcv(terms="1:3, 5:6")
#' )
#' 
#' foo2  <- list(
#'     AA = qcv(terms="4:8"),
#'     BB = qcv(terms="1:4, 10:12"),
#'     CC = qcv(terms="1, 11, 15:20"),
#'     DD = qcv(terms="")
#' )
#' 
#' 
#' (x <- cm_range2long(foo))
#' (D1 <- cm_long2dummy(x))
#' 
#' (z <- cm_range2long(foo, foo2, v.name="time"))
#' (D2 <- cm_long2dummy(z, "time"))
#' cm_combine.dummy(D1, combine.code = list(AB=qcv(AA, BB)))
#' cm_combine.dummy(D1, combine.code = list(AB=qcv(AA, BB)), overlap="==1")
#' cm_combine.dummy(D1, combine.code = list(AB=qcv(AA, BB)), overlap="!=1")
#' D1 <- cm_combine.dummy(D1, combine.code = list(AB=qcv(AA, BB)), overlap=0)
#' D1 <- cm_combine.dummy(D1, combine.code = list(CAB=qcv(AB, CC)), overlap=FALSE)
#' 
#' combines <- list(AB=qcv(AA, BB), ABC=qcv(AA, BB, CC))
#' cm_combine.dummy(D1, combine.code = combines)
#' cm_combine.dummy(D2, combine.code = combines)
#' }
cm_combine.dummy <- 
function(cm.l2d.obj, combine.code, rm.var = "time", 
    overlap = TRUE) {

    ## Grab the cmclass from cm.l2d.obj
    com <- which.lcm(cm.l2d.obj)

    if (is.data.frame(cm.l2d.obj) | is.matrix(cm.l2d.obj)) {
        NMS <- as.character(substitute(cm.l2d.obj))
        if(is.matrix(cm.l2d.obj)) {
            cm.l2d.obj <- data.frame(cm.l2d.obj, stringsAsFactors = FALSE)
        }
        cm.l2d.obj <- list(cm.l2d.obj)
        names(cm.l2d.obj) <- NMS
    }

    if (is.null(rm.var)) {
        rm.var <- "time"
    }
    for(i in seq_along(cm.l2d.obj)) {
        cm.l2d.obj[[i]] <- data.frame(cm.l2d.obj[[i]], stringsAsFactors = FALSE)
        cm.l2d.obj[[i]][, rm.var] <- rep(names(cm.l2d.obj)[i], nrow(cm.l2d.obj[[i]]))
    }

    DF <- data.frame(do.call(rbind, cm.l2d.obj), row.names=NULL, stringsAsFactors = FALSE)
    if (!is.list(combine.code)) {
        combine.code <- list(combine.code)
    }
    
    NEW <- lapply(seq_along(combine.code), function(i) {
   
    	## Added 11-7-13 to deal with single length column combine (a rename)
        if (length(combine.code[[i]]) == 1) {
        	return(DF[, c(combine.code[[i]])])
        }
    	## Added 11-7-13    	
    	
        if (is.logical(overlap)) {
            if (overlap) {
                as.numeric(rowSums(DF[, 
                    c(combine.code[[i]])]) == length(combine.code[[i]]))
            } else {
                as.numeric(rowSums(DF[, c(combine.code[[i]])]) > 0)  
            }
        } else {
            if (is.numeric(overlap)) {
                as.numeric(rowSums(DF[, c(combine.code[[i]])]) == overlap) 
            } else {
                if (is.character(overlap)) {
                    Olap <- unblanker(Trim(unlist(strsplit( overlap, NULL))))
                    sel <- Olap %in% 0:9
                    as.numeric(paste(Olap[sel], collapse=""))
                    Thresh <- as.numeric(paste(Olap[sel], collapse=""))
                    Comp <- match.fun(paste(Olap[!sel], collapse=""))
                    as.numeric(Comp(rowSums(DF[, c(combine.code[[i]])]), Thresh))
                } else {
                  stop("incorect output supplied to overlap")
                }    
            }               
            
        }
    })
    names(NEW) <- names(combine.code)
    tv <- ncol(DF)
    DF <- data.frame(DF[, -tv, drop=FALSE], do.call(cbind, NEW), 
        DF[, tv, drop=FALSE], stringsAsFactors = FALSE)

    if (length(unique(DF[, rm.var])) > 1) {
        DF <- split(DF, DF[, rm.var])
    }
    class(DF) <- c(com, class(DF))
    DF
}
