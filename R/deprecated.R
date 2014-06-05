#' tm Package Compatibility Tools: Apply to or Convert to/from Term Document 
#' Matrix or Document Term Matrix
#' 
#' \code{df2tm_corpus} - Convert a qdap dataframe to a tm package 
#' \code{\link[tm]{Corpus}}.
#' 
#' @param text.var The text variable or a \code{\link[qdap]{wfm}} object.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param demographic.vars Additional demographic information about the grouping 
#' variables.  This is a data.frame, list of equal length vectors, or a single 
#' vector corresponding to the grouping variable/text variable.  This 
#' information will be mapped to the DMetaData in the \code{\link[tm]{Corpus}}.
#' @param \ldots Other arguments passed to \code{\link[qdap]{sentSplit}}.  
#' @rdname deprecated
#' @section Warning: The \code{df2tm_corpus} function is deprecated.  It will be 
#' removed in a subsequent version of qdap.  Use \code{as.Corpus} instead.
#' @return \code{df2tm_corpus} - Converts a qdap oriented dataframe and returns 
#' a \code{\link[tm]{Corpus}}.
#' @export
#' @importFrom qdapTools list_df2df
df2tm_corpus <- function(text.var, grouping.var = NULL, demographic.vars, ...){

    .Deprecated(msg = paste("df2tm_corpus is deprecated and will be removed in", 
        "a subsequent version of qdap.  Please use `as.Corpus` instead."), 
        old = as.character(sys.call(sys.parent()))[1L])    
    
    if(is.null(grouping.var)) {
        G <- "all"
    } else {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            m <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                    x[length(x)]
                }
            )
            G <- paste(m, collapse="&")
        } else {
            G <- as.character(substitute(grouping.var))
            G <- G[length(G)]
        }
    }
    if(is.null(grouping.var)){
        grouping <- rep("all", length(text.var))
    } else {
        if (is.list(grouping.var) & length(grouping.var)>1) {
            grouping <- paste2(grouping.var)
        } else {
            grouping <- unlist(grouping.var)
        } 
    } 
    DF <- data.frame(grouping, text.var, check.names = FALSE, 
        stringsAsFactors = FALSE)

    ## convert text.var to character and grouping.var to factor
    DF[, "grouping"] <- factor(DF[, "grouping"])
    DF[, "text.var"] <- as.character(DF[, "text.var"])

    ## Split apart by grouping variables and collapse text
    LST <- sapply(split(DF[, "text.var"], DF[, "grouping"]), 
        paste, collapse = " ")

    ## Use the tm package to convert to a Corpus
    mycorpus <- Corpus(VectorSource(LST), ...)
    
    ## Add metadata info
    attributes(mycorpus)[["DMetaData"]][,1] <- names(LST)
    pers <- unname(Sys.info()["user"])
    if (!is.null(pers)) {
        attributes(mycorpus)[["CMetaData"]][["MetaData"]][["creator"]] <- pers
    }

    ## Add other demographic variables to "DMetaData"
    if(!missing(demographic.vars)) {
        if (is.data.frame(demographic.vars)) {
    
        } else {
            if (is.list(demographic.vars)) {
                nms <- colnames(demographic.vars)
                demographic.vars <- do.call(cbind.data.frame, demographic.vars)
                if (is.null(nms)) {

                    colnames(demographic.vars) <- paste0("X", 1:ncol(demographic.vars))
                } else {
                    colnames(demographic.vars) <- nms
                }
            } else {
                if (is.vector(demographic.vars) | is.factor(demographic.vars)) {
                    demographic.vars <- data.frame(dems=demographic.vars)
                } else {
                    warning("Please supply a data.frame, list of equal length vectors,\n",  
                        "   or a single vector to `demographic.vars`")
                }
        
            }
        }
        metadat <- split(demographic.vars, grouping)
        checks <- colSums(do.call(rbind, lapply(metadat, function(x) {
            unlist(lapply(x, function(y) !compare(y)))
        }))) == 0
        if (sum(checks) != 0){
            metadat <- list_df2df(lapply(metadat, 
                function(x) x[1, checks, drop = FALSE]), "MetaID")
            attributes(mycorpus)[["DMetaData"]] <- 
                key_merge(attributes(mycorpus)[["DMetaData"]], metadat)
        }
    }

    mycorpus
}