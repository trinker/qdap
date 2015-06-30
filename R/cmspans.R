#' Summarize a cmspans object
#' 
#' Summarize a cmspans object
#' 
#' @param object The cmspans object 
#' @param grouping.var The grouping variables. Also takes a single grouping 
#' variable or a list of 1 or more grouping variables.
#' @param rm.var An optional single vector or list of 1 or 2 of repeated 
#' measures to aggregate by.
#' @param total.span logical or an option list of vectors (length 1 or 2) of the 
#' total duration of the event.  If \code{FALSE} the "total" column is divided 
#' by the sum of the total duration for all codes in that rm.var to arrive at 
#' "total_percent".  If \code{TRUE} and object is from 
#' \code{cm_time2long} the difference for the time span from the 
#' \strong{transcript_time_span} of the list used in \code{cm_time2long} are 
#' utilized to divide the "total" column. The user may also provide a list of 
#' vectors with each vector representing a single total time duration or 
#' provide the start and end time of the event.  The user may give input in 
#' numeric seconds or in character "hh:mm:ss" form.
#' @param aggregate logical.  If \code{TRUE} the output will be aggregated 
#' (i.e., the output will collapse the \code{rm.var}).
#' @param percent logical.  If \code{TRUE} output given as percent.  If 
#' \code{FALSE} the output is proportion.
#' @param digits Integer; number of decimal places to round when printing.  
#' @param \ldots Other argument passed to \code{qheat} in plot (ignored in 
#' summary).
#' @method summary cmspans
#' @seealso \code{\link[qdap]{plot.sum_cmspans}}
#' @export
#' @importFrom qdapTools sec2hms hms2sec
#' @examples
#' \dontrun{
#' ## Example 1
#' foo <- list(
#'     person_greg = qcv(terms='7:11, 20:24, 30:33, 49:56'),
#'     person_researcher = qcv(terms='42:48'),
#'     person_sally = qcv(terms='25:29, 37:41'),
#'     person_sam = qcv(terms='1:6, 16:19, 34:36'),
#'     person_teacher = qcv(terms='12:15'),
#'     adult_0 = qcv(terms='1:11, 16:41, 49:56'),
#'     adult_1 = qcv(terms='12:15, 42:48'),
#'     AA = qcv(terms="1"),
#'     BB = qcv(terms="1:2, 3:10, 19"),
#'     CC = qcv(terms="1:9, 100:150")
#' )
#' 
#' foo2  <- list(
#'     person_greg = qcv(terms='7:11, 20:24, 30:33, 49:56'),
#'     person_researcher = qcv(terms='42:48'),
#'     person_sally = qcv(terms='25:29, 37:41'),
#'     person_sam = qcv(terms='1:6, 16:19, 34:36'),
#'     person_teacher = qcv(terms='12:15'),
#'     adult_0 = qcv(terms='1:11, 16:41, 49:56'),
#'     adult_1 = qcv(terms='12:15, 42:48'),
#'     AA = qcv(terms="40"),
#'     BB = qcv(terms="50:90"),
#'     CC = qcv(terms="60:90, 100:120, 150"),
#'     DD = qcv(terms="")
#' )
#' 
#' v <- cm_2long(foo, foo2, v.name = "time")
#' plot(v)
#' summary(v)
#' plot(summary(v))
#' 
#' ## Example 2
#' x <- list(
#'     transcript_time_span = qcv(00:00 - 1:12:00),
#'     A = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00"),
#'     B = qcv(terms = "2.40, 3.01:3.02, 5.01, 6.02:7.00,
#'         9.00, 1.12.00:1.19.01"),
#'     C = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00, 17.01")
#' )
#' z <-cm_2long(x)
#' 
#' summary(z)
#' summary(z, total.span = FALSE)
#' summary(z, total.span = c(0, 3333))
#' summary(z, total.span = c("00:01:00", "03:02:00"))
#' plot(summary(z))
#' 
#' ## suppress printing measurement units
#' suppressMessages(print(summary(z)))
#' 
#' ## remove print method
#' as.data.frame(summary(z))
#' }
summary.cmspans <-
function(object, grouping.var = NULL, rm.var = NULL, total.span = TRUE,
    aggregate = FALSE, percent = TRUE, digits = 2,  ...) {

    class(object) <- class(object)[!class(object) %in% "cmspans"]

    ## denote the grouping variables
    if(is.null(rm.var)) {
        rm.var <- R <- which.cmrm(object)
        if (is.null(R) || length(unique(object[, rm.var])) < 2) {
            R <- rm.var <- "time"
            object[, "time"] <- rep("all", nrow(object))
        }
    } else {
        if (length(rm.var) > 1) {
            R <- paste(rm.var, collapse="&")
        } else {
            R <- rm.var
        }
    }

    if (aggregate) {
        sp.loc <- grepl("spans_", class(object))
        new.spans <- class(object)[sp.loc]
    	new.spans <- strsplit(gsub("spans_", "", new.spans), "||", fixed = TRUE)
    	class(object)[sp.loc] <- paste0("spans_", 
    		sum(as.numeric(unlist(new.spans))))
    	object[, rm.var] <- "t1"
    }
	
    ## denote the grouping measures variable
    if(is.null(grouping.var)) {
        G <- group <- colnames(object)[1]
    } else {
        group <- grouping.var
        if (length(grouping.var) > 1) {
            G <- paste(grouping.var, collapse="&")
        } else {
            G <- grouping.var
        }
    }

    ## get column names for percent/proportion
    per <- ifelse(percent, "percent", "proportion")
    pt <- sprintf("%s_total", per)
    pn <- sprintf("%s_n", per)

    L1 <- split(object, object[, rm.var])

### Added 10-12-13
    ## check the total.span input
 
    if (is.logical(total.span) & !isTRUE(total.span)){
        total.span <- NULL
    } else {
        
        if (is.logical(total.span)){
            
            sp.loc <- grepl("spans_", class(object))
            if (sum(sp.loc) == 0) {
                total.span <- NULL        
            } else {
                spns <- gsub("spans_", "", class(object)[sp.loc])
                total.span <- lapply(as.list(unlist(strsplit(spns, "\\|\\|"))), as.numeric)
                total.span <- sapply(total.span, total2tot) 
            }
        
        } else {
            if (!is.list(total.span)) {
                total.span <- list(total.span)
            }
            total.span <- sapply(total.span, total2tot)
        }
    }
###

    output <- lapply(seq_along(L1), function(i) {
        L2 <- split(droplevels(L1[[i]]), droplevels(L1[[i]][, group]))
        output2 <- lapply(L2, function(b) {   
            diffs <- b[, "end"] - b[, "start"] 
            data.frame(total = sum(diffs), n = length(diffs), ave = mean(diffs), 
                sd = ifelse(is.na(stats::sd(diffs)), 0, stats::sd(diffs)), min = min(diffs),
                max = max(diffs))
        }) 
        out2 <- do.call(rbind, output2)
        out2 <- data.frame(code = names(L2), out2, row.names = NULL)

#####  Added 10-12-13
        ## divisor (total time span (external) or total time of codes (summed)
        if (is.null(total.span)) {
            tot <- sum(out2[, "total"])
        } else {
            tot <- total.span[i]          
        }
#####
        out2[, pt] <- (out2[, "total"]/tot) * ifelse(percent, 100, 1)
        out2[, pn] <- (out2[, "n"]/sum(out2[, "n"])) * ifelse(percent, 100, 1)
        out2[, c("code", "total", pt, "n", pn, "ave", "sd", "min", "max")]
    })

    out1 <- data.frame(do.call(rbind, output), row.names = NULL)
   
    if(which.cm(object) == "cmtime") {
        out1[, "total"] <- sec2hms(out1[, "total"])
    }

    units <- ifelse(which.cm(object) == "cmrange", "words", "time")

    ## If a repeated measures add this column
    if(length(unique(object[, rm.var])) > 1) {
        out1 <- data.frame(time = rep(names(L1), sapply(output, nrow)),
            out1, row.names = NULL, check.names = FALSE) 
    }
    class(out1) <- c("sum_cmspans", paste0("units_", units), 
        paste0("digits_", digits), which.cm(object), 
        paste0("percent_", per), class(out1))
    out1
}

#' Prints a sum_cmspans object
#' 
#' Prints a sum_cmspans object.
#' 
#' @param x The sum_cmspans object
#' @param digits Integer; number of decimal places to round in the display of 
#' the output. 
#' @param \ldots ignored
#' @method print sum_cmspans
#' @export
print.sum_cmspans <- function(x, digits = NULL, ...) {

    x.nms <- c("code", "total", "percent_total", "n", "percent_n", 
        "ave", "sd", "min", "max")

    if (!all(x.nms %in% colnames(x))) {
        class(x) <- "data.frame"
        print(x)
        return(invisible(NULL))
    }

    if (!is.null(digits)) {
        digs <- sprintf("digits_%s", digits)
    } else {
        digs <- class(x)[grepl("digits_", class(x))]
    }

    class(x) <- c(class(x)[(!class(x) %in% "sum_cmspans") & !grepl("digits_", 
        class(x))], digs) 

    wdt <- options()[["width"]]
    options(width = 10000)
    on.exit(options(width = wdt))

    nums <- function(x) is.numeric(x) && !methods::is(x, "times")
    if (is.null(digits)) {
        digits <- as.numeric(gsub("digits_", "", class(x)[grepl("digits_", 
            class(x))]))
    }
    locs <- sapply(x, nums)

    locs[names(locs) %in% c("min", "max", "total", "n")] <- FALSE
    x[, locs] <- lapply(x[, locs], round, digits = digits)
    x[, locs] <- lapply(x[, locs], function(a) {
        numbformat(a, digits =digits)
    })

    x[, "mean(sd)"] <- sprintf("%s(%s)", x[, "ave"], x[, "sd"])
    x[, "sd"] <- NULL
    x[, "ave"] <- NULL
    
    if(which.class(x, "percent_") == "percent") {
        cn <- colnames(x)
        tot <- grepl("_total", cn) 
        n <- grepl("_n", cn) 
        x[, n] <- paste0(x[, n], "%")
        x[, tot] <- paste0(x[, tot], "%")
    }

    if(which.cm(x) == "cmtime") {   
        locs2 <- sapply(colnames(x), function(z) z %in% c("total"))
        x[, locs2] <- lapply(x[, locs2, drop = FALSE], tred)
    }


    grabs <- c("code", "total", "percent_total", "n", "percent_n", "mean(sd)", 
        "min", "max")
    if (colnames(x)[1] == "time") {
        grabs <- c("time", grabs)
    }
    x <- x[, grabs]     
    
    WD <- options()[["width"]]
    options(width=3000)
    print(x)
    
    message(paste(rep("====", 7), collapse = ""))
    message(sprintf("Unit of measure: %s", ifelse(which.cm(x)== "cmtime", 
        "time", "words")))
    if (which.cm(x)== "cmtime") {
          message("Columns measured in seconds unless in the form hh:mm:ss")
    }
    options(width=WD)
    
    invisible(x)
}

## helper 
tred <- function(tmv) {
    tdat <- do.call(rbind, strsplit(as.character(tmv), "\\:"))
    remv <- !apply(tdat, 2, function(z) all(z == "00"))
    if (sum(remv) == 1) {
        paste0(":", tdat[, 3])
    } else {
        paste2(tdat[, remv, drop = FALSE], sep = ":")
    }
}

## helper to format numbers
numbformat <- function(b, digits) {
        numformat <- function(val) { 
            sub("^(-?)0.", "\\1.", sprintf(paste0("%.", digits, "f"), val)) 
        }
    b2 <- sprintf(paste0("%.", digits, "f"), b)
    b3 <- numformat(as.numeric(b2))
    ifelse(as.numeric(b3) == 0, "0", as.character(b3))
}

## helper to convert time spans to diff
total2tot <- function(x) {
    if (is.character(x)) {
 
        ## stop if not properly formatted
        if(!all(sapply(x, timecheck))) stop("format `total.time` in the form \"hh:mm:ss\"")

        if (length(x) == 1) {
            tot <- hms2sec(x)
        } else {
            if (length(x) > 2){
                warning(paste("`total.time` must be of length 1 or 2:",
                    "Additional total.time ignored."))
            }
            tot <- diff(hms2sec(x[1:2])) 
        }
    } else {
        if (length(x) == 1) {
            tot <- x
        } else {
            if (length(x) > 2){
                warning(paste("`total.span` must be of length 1 or 2:",
                    "Additional total.span ignored."))
            }
            tot <- diff(x[1:2]) 
        }              
    }
    if (tot < 1) {
        stop("`total.span` can not be negative")
    }
    tot 
}

## time check helper
timecheck <- function(val) {
    valp <- unlist(strsplit(val, NULL))
    t1 <- sum(valp %in% ":") == 2
    t2 <- length(valp) == 8
    t3 <- sum(valp %in% 0:9) == 6
    sum(t1, t2, t3) == 3
}




#' Plot Summary Stats for a Summary of a cmspans Object
#' 
#' Plots a heat map of summary statistics for 
#' sum_cmspans objects (the object produced by calling \code{summary} on a 
#' cmspans object).
#' 
#' @param x The sum_cmspans object (the object produced by calling 
#' \code{summary} on a cmspans object)
#' @param digits The number of digits displayed if \code{values} is \code{TRUE}.
#' @param sep The character that was used in \code{paste2} to paste the columns.
#' @param name.sep The character that was used to paste the column names.
#' @param values logical.  If \code{TRUE} the cell values will be included on 
#' the heatmap.
#' @param high The color to be used for higher values.
#' @param transpose logical.  If \code{TRUE} the dataframe is rotated 90 degrees.
#' @param plot logical.  If \code{TRUE} the plot will automatically plot.  
#' The user may wish to set to \code{FALSE} for use in knitr, sweave, etc.
#' to add additional plot layers.
#' @param facet.vars A character vector of names to facet by.  Default is 
#' \code{"time"}.
#' @param rev.codes logical If \code{TRUE} the plotting order of the code 
#' groups is reversed.
#' @param rev.stats logical If \code{TRUE} the plotting order of the code 
#' descriptive statistics is reversed.
#' @param \dots Other arguments passed to qheat.
#' @export
#' @seealso \code{\link[qdap]{summary.cmspans}}
#' @importFrom ggplot2 coord_flip
#' @method plot sum_cmspans
#' @export
plot.sum_cmspans <- function(x, digits = 3, sep = ".", 
    name.sep = "&", values = TRUE, high = "red", transpose = TRUE, 
    plot =  TRUE, facet.vars = "time", rev.codes = !transpose, 
    rev.stats = !transpose, ...) {

    if (!"time" %in% colnames(x)) {
        facet.vars  <- NULL
    }

    class(x) <- c(class(x)[!class(x) %in% "sum_cmspans"]) 
    nvars <- sapply(x, is.numeric)
    if (is.null(facet.vars) && sum(!nvars) > 1) {
        chars <- paste2(x[, !nvars], sep = sep)
        G <- paste(colnames(x)[!nvars], collapse = name.sep)
        facet.vars <- NULL
    } else {
        chars <- data.frame(x)[, !nvars, drop = FALSE]
        if (!is.null(facet.vars) && sum(!nvars) > 1) {
            if (is.numeric(facet.vars)) {
                facet.vars <- colnames(x)[facet.vars]
            } 
            G <- colnames(x)[!nvars]
            G <- G[!G %in% facet.vars]

            if (length(G) > 1) {
                chars <- data.frame(x[, facet.vars], x = paste2(x[, G], sep = sep))
                G <- paste(G,  collapse = name.sep)
            } else {
                chars <- chars[, c(facet.vars, G)]
            }

        } else {
            G <- colnames(x)[!nvars]
            facet.vars <- NULL
        }
    }
    x2 <- data.frame(chars, x[, nvars, drop = FALSE])
    nms <- c(facet.vars, G)
    colnames(x2)[1:length(nms)] <- nms
    if(is.null(digits)) {
        digits <-  as.numeric(which.class(x, "digits_"))
    }   

    ## reverse the codes and stat vars
    if (!rev.stats) {
        num.cols <- sapply(x2, is.numeric)
        x2 <- x2[, c(which(!num.cols), rev(which(num.cols)))]
    }
    
    if (!rev.codes) {
        x2[, G] <- factor(x2[, G], levels =rev(levels(x2[, G])))
    }

    if (transpose) {
       # x2 <- data.frame(x2[, 1, drop = FALSE], x2[, ncol(x2):2, drop = FALSE])
        out <- qheat(x2, digits = digits, high = high, values = values, 
            plot = FALSE, facet.vars = facet.vars,  ...) +
            coord_flip()
    } else {
        out <- qheat(x2, digits = digits, high = high, values = values, 
            plot = FALSE, facet.vars = facet.vars,  ...)
    }
    if (plot) {
        print(out)
    }
    invisible(out)
}
