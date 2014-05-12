#' Generate Unit Spans for Repeated Measures
#' 
#' Produces start and end times for occurrences for each repeated measure 
#' condition.
#' 
#' @param rm.var An optional single vector or list of 1 or 2 of repeated 
#' measures to facet by.   
#' @param text.var The text variable.    
#' @param grouping.var The grouping variables. Also takes a single grouping 
#' variable or a list of 1 or more grouping variables.
#' @param units The unit of measurement to analyze.  One of the strings 
#' \code{"character"}, \code{"syllable"}, \code{"word"}, or \code{"sentence"}.
#' @param col.sep The character string to use to separate pasted variables in 
#' the pasted columns.
#' @param name.sep The character string to use to separate column names of the 
#' pasted columns.
#' @return Returns a data frame of start and end times by repeated measure and 
#' grouping variable(s)
#' @note For non-repeated measures data use \code{\link[qdap]{gantt}}.  For
#' more flexible plotting needs use \code{\link[qdap]{gantt_wrap}} over the 
#' generic plotting method.
#' @seealso \code{\link[qdap]{gantt}},
#' \code{\link[qdap]{gantt_wrap}},
#' \code{\link[qdap]{gantt_plot}} 
#' @references Clark, W. & Gantt, H. (1922) The Gantt chart, a working 
#' tool of management. New York, Ronald Press.
#' @keywords Gantt
#' @export
#' @examples
#' \dontrun{
#' dat <- with(rajSPLIT, gantt_rep(act, dialogue, list(fam.aff, sex),
#'     units = "words", col.sep = "_"))
#' head(dat, 20)
#' plot(dat)
#' 
#' gantt_wrap(dat,  "fam.aff_sex",  facet.vars = "act",
#'     title = "Repeated Measures Gantt Plot",
#'     minor.line.freq = 25, major.line.freq = 100)
#' 
#' ## Two facets variables
#' dat2 <- with(DATA2, gantt_rep(list(day, class), state, person,
#'     units = "words", col.sep = "_"))
#' head(dat2, 20)
#' plot(dat2)
#' }
gantt_rep <-
function(rm.var, text.var, grouping.var = NULL, units = "words", col.sep = "_", 
    name.sep = "_"){
    g <- grouping.var
    r <- rm.var

    if(is.null(grouping.var)) {
        NAME <- "all"
    } else {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            m <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                    x[length(x)]
                }
            )
            NAME <- paste(m, collapse=name.sep)
        } else {
            NAME <- as.character(substitute(grouping.var))
            NAME <- NAME[length(NAME)]
        }
    }

    if(is.null(grouping.var)){
        grouping.var <- rep("all", length(text.var))
    } else {
        if (is.list(grouping.var) & length(grouping.var)>1) {
            grouping.var <- paste2(grouping.var, sep = col.sep)
        } else {
            grouping.var <- unlist(grouping.var)
        } 
    } 

    if (is.list(rm.var)) {
        m2 <- unlist(as.character(substitute(rm.var))[-1])
        m2 <- sapply(strsplit(m2, "$", fixed=TRUE), 
            function(x) x[length(x)])
        #m2 <- gsub(".", "", m2, fixed = TRUE)
        NAME2 <- paste(m2, collapse=name.sep)
    } else {
        G2 <- as.character(substitute(rm.var))
        NAME2 <- G2[length(G2)]
    } 
    
    if (is.list(rm.var) & length(rm.var)>1) {
        rm.var <- paste2(rm.var, sep = col.sep)
    } else {
        rm.var <- unlist(rm.var)
    }     
    
    if (is.list(grouping.var) & length(grouping.var)>1) {
        grouping.var <- paste2(grouping.var, sep = name.sep)
    } else {
        grouping.var <- unlist(grouping.var)
    } 
    
    DAT <- data.frame(rm.var, grouping.var, text.var)
    DAT2 <- split(DAT, rm.var)

    DAT3 <- lapply(seq_along(DAT2), function(i) {
            rm1 <- DAT2[[i]][, 1]
            gn <- DAT2[[i]][, -1]
            gn2 <- gantt(gn[, "text.var"], gn[, "grouping.var"], units = units)
            gn3 <- data.frame(rm.var = rm1[nrow(gn2)], gn2)
            return(gn3)
        }
    )
    DAT3 <- lapply(DAT3, function(x) {
        colnames(x)[ncol(x) - 3] <- NAME
        x
    })
    DAT3 <- do.call("rbind", DAT3)
    names(DAT3)[1:2] <- c(NAME2, NAME)
#    if (col.sep != "&") {
#        colnames(DAT3) <- gsub("&", col.sep, colnames(DAT3), fixed = TRUE)
#    }
    row.names(DAT3) <- 1:nrow(DAT3)
    nrf2 <- sum(gregexpr("col.sep", names(DAT3[, 1, drop = FALSE]))[[1]] < 0)
    if (nrf2==0) {
        RMV <- colSplit(DAT3[, 1, drop = FALSE])
    }
    nrf <- sum(gregexpr("col.sep", names(DAT3[, 2, drop = FALSE]))[[1]] < 0)
    if (nrf==0) {
        GV <- colSplit(DAT3[, 2, drop = FALSE])
    }
    if (nrf==0){
        DAT4 <- data.frame(DAT3[, 2, drop =FALSE], GV, DAT3[, -c(1:2)], 
            check.names = FALSE)
    } else {
        DAT4 <- data.frame(DAT3[, 2, drop =FALSE], DAT3[, -c(1:2)], 
            check.names = FALSE)
    }
    if (nrf2==0){
        DAT3 <- data.frame(DAT3[, 1, drop =FALSE], RMV, DAT4, 
            check.names = FALSE)
    } else {
        DAT3 <- data.frame(DAT3[, 1, drop =FALSE], DAT4, check.names = FALSE)
    }
    rstatus <- ifelse(is.null(r), 0, ifelse(is.list(r), 2, 1))
    class(DAT3) <- c("rmgantt", paste0("csep_", col.sep), 
        paste0("name.sep_", name.sep), paste0("unit_", units), 
        paste0("rmv_", rstatus), paste0("group_", NAME), class(DAT3))
    DAT3
}

#' Plots a rmgantt object
#' 
#' Plots a rmgantt object.
#' 
#' @param x The sums_rmgantt object
#' @param title An optional title.
#' @param transform logical.  If \code{TRUE} and there are two repeated measures 
#' the faceting is reversed.
#' @param \ldots Other arguments passed to \code{gantt_wrap} 
#' @method plot rmgantt
#' @export
plot.rmgantt <- function(x, title, transform = FALSE, ...) {
    class(x) <- cls <- class(x)[!class(x) %in% "rmgantt"]

    nf <- as.numeric(gsub("rmv_", "", class(x)[grepl("rmv_", class(x))]))
    group <- gsub("group_", "", class(x)[grepl("group_", class(x))])
    if (nf == 2) {
        csep <- gsub("csep_", "", class(x)[grepl("csep_", class(x))])
        nsep <- gsub("name.sep_", "", class(x)[grepl("name.sep_", class(x))])
        x <- colsplit2df(x, sep = csep, name.sep = nsep)
        if (transform) {
            fv <- colnames(x)[1:2]
        } else {
            fv <- colnames(x)[2:1]          
        }
    } else {
        fv <- colnames(x)[1]
    }
    if (missing(title)) {
        title <- "Repeated Measures Gantt Plot"
    }
    class(x) <- cls
    gantt_wrap(x, group, facet.vars = fv, title = title, ...)   
}
