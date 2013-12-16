#' Distance Matrix Between Codes 
#' 
#' Generate distance measures to ascertain a mean distance measure between codes.
#' 
#' @param dataframe A data frame from the cm_x2long family 
#' (\code{cm_range2long}; \code{cm_df2long}; \code{cm_time2long}).
#' @param pvals A logical vector of length 1 or 2.  If element 2 is blank 
#' element 1 will be recycled.  If the first element is \code{TRUE} pvalues 
#' will be calculated for the combined (main) output for all repeated measures 
#' from simulated resampling of the data.  If the second element is \code{TRUE} 
#' pvalues will be calculated for the individual (extended) repeated measures 
#' output from simulated resampling of the data.  Default is to calculate 
#' pvalues for the main output but not for the extended output.  This process 
#' involves multiple resampling of the data and is a time consuming process.  It 
#' may take from a few minutes to days to calculate the pvalues depending on the
#' number of all codes use, number of different codes and number of 
#' \code{replications}.
#' @param replications An integer value for the number of replications used in 
#' resampling the data if any \code{pvals} is \code{TRUE}.  It is recommended 
#' that this value be no lower than 1000. Failure to use enough replications 
#' may result in unreliable pvalues.
#' @param parallel logical.  If \code{TRUE} runs the \code{cm_distance} on 
#' multiple cores (if available).  This will generally be effective with most 
#' data sets, given there are repeated measures, because of the large number of 
#' simulations.  Default uses 1/2 of the available cores.
#' @param extended.output logical.  If \code{TRUE} the information on individual 
#' repeated measures is calculated in addition to the aggregated repeated 
#' measures results for the main output.
#' @param time.var An optional variable to split the dataframe by (if you have 
#' data that is by various times this must be supplied).
#' @param code.var The name of the code variable column.  Defaults to "codes" as 
#' out putted by x2long family.
#' @param causal logical.  If \code{TRUE} measures the distance between x and y 
#' given that x must precede y.  That is, only those \eqn{y_i} that begin after 
#' the \eqn{x_i} has begun will be considered, as it is assumed that x precedes 
#' y.  If \code{FALSE} x is not assumed to precede y.  The closest \eqn{y_i} 
#' (either its begining or end) is is calculated to \eqn{x_i} (either it's 
#' begining or end).
#' @param start.var The name of the start variable column.  Defaults to "start" 
#' as out putted by x2long family.
#' @param end.var The name of the end variable column.  Defaults to "end" as out 
#' putted by x2long family.
#' @param cores An integer value describing the number of cores to use if 
#' \code{parallel = TRUE}.  Default is to use half of the available cores.
#' @return An object of the class \code{"cm_distance"}.  This is a list with the 
#' following components: 
#'
#' \item{pvals}{A logical indication of whether pvalues were calculated}
#' \item{replications}{Integer value of number of replications used}
#' \item{extended.output}{An optional list of individual repeated measures 
#' information}
#' \item{main.output}{A list of aggregated repeated measures information}
#' \item{adj.alpha}{An adjusted alpha level (based on \eqn{\alpha = .05}) for 
#' the estimated p-values using the upper end of the confidence interval around 
#' the p-values}
#'
#' Within the lists of extended.output and list of the main.output are the 
#' following items: 
#'
#' \item{mean}{A distance matrix of average distances between codes}
#' \item{sd}{A matrix of standard deviations of distances between codes}
#' \item{n}{A matrix of counts of distances between codes}
#' \item{stan.mean}{A matrix of standardized values of distances between 
#' codes.  The closer a value is to zero the closer two codes relate.}
#' \item{pvalue}{A n optional matrix of simulated pvalues associated with 
#' the mean distances}
#' @section Warning: p-values are estimated and thus subject to error.  More 
#' replications decreases the error.  Use:
#' 
#' \deqn{p \pm \left (  1.96 \cdot \sqrt{\frac{\alpha(1-\alpha)}{n}}\right )}
#' 
#' to adjust the confidence in the 
#' estimated p-values based on the number of replications.
#'
#' @details Note that row names are the first code and column names are the 
#' second comparison code. The values for Code A compared to Code B will not be 
#' the same as Code B compared to Code A. This is because, unlike a true 
#' distance measure, cm_distance's matrix is asymmetrical. \code{cm_distance} 
#' computes the distance by taking each span (start and end) for Code A and 
#' comparing it to the nearest start or end for Code B.
#' @references \url{http://stats.stackexchange.com/a/22333/7482}
#' @keywords distance codes association
#' @seealso \code{\link[qdap]{print.cm_distance}}
#' @export
#' @importFrom parallel parLapply makeCluster detectCores stopCluster clusterExport 
#' @examples
#' \dontrun{
#' foo <- list(
#'     AA = qcv(terms="02:03, 05"),
#'     BB = qcv(terms="1:2, 3:10"),
#'     CC = qcv(terms="1:9, 100:150")
#' )
#' 
#' foo2  <- list(
#'     AA = qcv(terms="40"),
#'     BB = qcv(terms="50:90"),
#'     CC = qcv(terms="60:90, 100:120, 150"),
#'     DD = qcv(terms="")
#' )
#' 
#' (dat <- cm_2long(foo, foo2, v.name = "time"))
#' plot(dat)
#' (out <- cm_distance(dat, replications=100))
#' names(out)
#' names(out$main.output)
#' out$main.output
#' out$extended.output
#' print(out, new.order = c(3, 2, 1))
#' print(out, new.order = 3:2)
#' #========================================
#' x <- list(
#'     transcript_time_span = qcv(00:00 - 1:12:00),
#'     A = qcv(terms = "2.40:3.00, 6.32:7.00, 9.00,
#'         10.00:11.00, 59.56"),
#'     B = qcv(terms = "3.01:3.02, 5.01,  19.00, 1.12.00:1.19.01"),
#'     C = qcv(terms = "2.40:3.00, 5.01, 6.32:7.00, 9.00, 17.01")
#' )
#' (dat <- cm_2long(x))
#' plot(dat)
#' (a <- cm_distance(dat, causal=TRUE, replications=100))
#' }
cm_distance <- 
function(dataframe, pvals = c(TRUE, FALSE), replications = 1000,  
    parallel = TRUE, extended.output = TRUE, time.var = TRUE, 
    code.var = "code", causal = FALSE, start.var = "start", 
    end.var = "end", cores = detectCores()/2) {

    ## Attempt to determine time var from class
    if (isTRUE(time.var)) {
        cls <- class(dataframe)
        wch.tm <- grepl("vname_", cls)
        if (sum(wch.tm) == 0) {
            time.var <- NULL
        } else {
            time.var <- gsub("vname_", "", cls[wch.tm ])
        }
    }

    ## Warning adj alpha for estmated pvalue
    if (any(pvals)) {

        if (confup(replications) <= 0) {stop("Number of replications too small")}

        mess <- sprintf(paste0("Based on %s replications, estimated p-values must be <= %s",
            "\n    to have 95%% confidence that the 'true' p-value is < .05"), 
            replications, numbformat(confup(replications), digits = 6))

        warning(mess, call. = FALSE, immediate. = TRUE)
        flush.console()
    }

    o <- list(pvals = pvals, replications = replications, extended.output = NULL)       

    ## determine extended output (for each time)
    if (extended.output && !is.null(time.var)) {

        if (!is.null(time.var)) {
            L1 <- split(dataframe, dataframe[, time.var])
        } else {
            L1 <- list(dataframe)
            names(L1) <- as.character(substitute(dataframe))
        }  

        if (parallel && cores > 1){
            cl <- makeCluster(mc <- getOption("cl.cores", cores))

            vars <- c("L1", "DIST", "code.var",
                "causal", "replications", "v_outer", "mgsub", "vdists1",
                "vdists2", "vs2df", "Nget", "remix", "pvals.n", "pvals.c",
                "pvals", "FUN_apply", "scale.all", "paste2")

            clusterExport(cl=cl, varlist=vars, envir = environment())
            output <- parLapply(cl, L1, DIST, cvar = code.var, cause = causal, 
                reps = replications, pvals = tail(pvals, 1))

            stopCluster(cl)
        } else { 
            output <- lapply(L1, DIST, cvar = code.var, cause = causal, 
                reps = replications, pvals = tail(pvals, 1))
        }
        o[["extended.output"]] <- output
    }

    ## determine main output (combined for each code)
    o[["main.output"]] <- DIST2(dataframe, cvar = code.var, cause = causal, 
        reps = replications, pvals = head(pvals, 1), time.var = time.var)
  
    o[["adj.alpha"]] <- numbformat(confup (replications), digits = 12)

    class(o) <- "cm_distance"
    return(o)
}


#' Prints a cm_distance Object
#' 
#' Prints a cm_distance object.
#' 
#' @param x The cm_distance object.
#' @param mean.digits The number of digits to print for the mean code distances.
#' @param sd.digits The number of digits to print for the standard deviations of 
#' the code distances.
#' @param sd.mean.digits The number of digits to print for the standardized mean 
#' distances.
#' @param pval.digits The number of digits to print for the p-values.
#' @param new.order An integer vector reordering the columns and rows of the 
#' output.  Omission of a column number will result in omission from the output.
#' @param na.replace A character to replace \code{NA} values with.
#' @param diag.replace A character to replace the diagonal of the mean distance 
#' matrix.
#' @param print logical.  If \code{TRUE} prints to the console.  \code{FALSE} 
#' may be used to extract the invisibly returned output without printing to the 
#' console.
#' @param \ldots ignored
#' @method print cm_distance
#' @S3method print cm_distance
print.cm_distance <- function(x, mean.digits = 0, sd.digits = 2, 
    sd.mean.digits = 3, pval.digits = 3, new.order = NULL, na.replace = "-", 
    diag.replace = na.replace, print = TRUE, ...){

    ## Change width
    WD <- options()[["width"]]
    options(width=3000)

    ## remove class
    x <- lview(x, print = FALSE)

    ## Determine if pvals were generated for The main display
    pvals <- head(x[["pvals"]], 1)

    ## function to reclass v_outer to matrix
    vrc <- function(x, digs) {
        class(x) <- "matrix"
        dfnumfor(data.frame(x), digits = digs)
    }

    ## Grab and format the means, sd, stan.mean, pvals, n
    M <- vrc(x[[c("main.output", "mean")]], mean.digits)
    SD <- vrc(x[[c("main.output", "sd")]], sd.digits)
    SM <- as.matrix(vrc(x[[c("main.output", "stan.mean")]], sd.mean.digits))
    N <- x[[c("main.output", "n")]]
 
    ## format the p values for the mean distances
    PV <- NULL
    if (pvals) {
        PV <- as.matrix(vrc(x[[c("main.output", "pvalue")]], pval.digits))
        PV[is.na(x[[c("main.output", "mean")]])] <- na.replace
        if (!is.null(new.order)) {
            PV <-  data.frame(n = N[new.order], PV[new.order, new.order])  
        } else {
            PV <-  data.frame(n = N, PV)  
        }
    }

    ## Format the mean distances and their sd
    MSD <- mapply(paste, M, "(", SD, ")", MoreArgs = list(sep =""))
    MSD[MSD == "NA(NA)"] <- na.replace
    diag(MSD) <- diag.replace
    if (!is.null(new.order)) {
        MSD <-  data.frame(n = N[new.order], MSD[new.order, new.order])  
    } else {
        MSD <-  data.frame(n = N, MSD)  
    }

    ## format the standardized mean distances
    SM[SM == "NA"] <- na.replace
    if (!is.null(new.order)) {
        SM <-  data.frame(n = N[new.order], SM[new.order, new.order])  
    } else {
        SM <-  data.frame(n = N, SM)  
    }

    ## can supress print and use just for the formatting of the invisible return
    if (print) {
        cat("\nMean Distances and Standard Deviations:\n\n")
        print(MSD)
        if (!is.null(PV)) {
            cat("\n===========")
            cat("\nEstimated P-Values for Mean Distances:\n\n")
            print(PV)
            cat("\n")
            cat(sprintf("*Number of replications: %s\n", x[["replications"]]))
            mess <- sprintf(paste0("*Based on %s replications, estimated p-values must be",
                "\n  <= %s to have 95%% confidence that the 'true'\n  p-value is < .05"), 
                x[["replications"]], numbformat(confup (x[["replications"]]), digits = 6))
            cat(mess)
            cat("\n")
        }
        cat("\n===========")
        cat("\nStandardized Mean Distances:\n\n")
        print(SM)
        cat(paste0("\n*Note: The closer a value is to zero, the more closely", 
            "\n  associated the codes are\n"))

    }
    
    ## Fix width and return the print invisibly
    options(width=WD)
    return(invisible(list(`mean(sd)` = MSD, pvalues = PV, stand_means = SM)))
}


## HELPER FUNCTIONS for cm_distance
## Not causal Pass an x and y dataframe
vdists1 <- function(x, y2){
    apply(x, 1, function(d, y = y2) {
      overlap <- y[, "end"] >= d[1] & y[, "start"] <= d[2]
      if (any(overlap)) 0
      else min(abs(c(d[1] - y[!overlap, "end"], y[!overlap, "start"] - d[2])))
    })
}

##Causal
vdists2 <- function(x, y2) {
    Dc <- sapply(1:nrow(x), function(i) {
        FUN <- function(xstart, xend, ystart){
             max(0, min(ystart[ystart > xstart] - xend))
        }
        suppressWarnings(FUN(x[i, "start"], x[i, "end"], y2[, "start"]))
    })
    Dc[is.infinite(Dc)] <- NA
    Dc
}

## reforms pasted vectors to dataframes
vs2df <- function(col) {
   x <- apply(do.call(rbind, strsplit(col, "\\|")), 2, as.numeric)
   if (length(col) == 1) {
       x <- data.frame(t(x))
   }
   colnames(x) <- c("start", "end")
   x
}

## reforms pasted vectors with time var to dataframes
vs2dfb <- function(col) {
   if (length(col) > 1) {
       x <- do.call(rbind, strsplit(col, "\\|"))
          x <- data.frame(apply(x[, 1:2], 2, as.numeric), x[, 3])
   } else {
       x <- unlist(strsplit(col, "\\|"))
          x <- data.frame(t(as.numeric(x[1:2])), x[3])
   }
   colnames(x) <- c("start", "end", "time")
   x
}

## get number of rows
Nget  <- function(a, b) {
   nrow(vs2df(a))
}

## used to resample the second code dataframe for replication/simulated pvalue
remix <- function(b2 = b2, m = m) {
    lens <- b2[, 2] - b2[, 1]
    starts <- sample(1:m, nrow(b2))
    data.frame(start = starts, end = starts + sample(lens))
}

## noncausal pvals function
pvals.n <- function(a, b, trials = 10000, means) {
    a2 <- vs2df(a)
    b2 <- vs2df(b)
    m <- max(c(a2[, "end"], b2[, "end"]))
    Mean <- mean(vdists1(a2, b2), na.rm = TRUE)
    simmeans <- unlist(lapply(1:trials, function(i) mean(vdists1(a2, remix(b2, m)))))
    sum(simmeans <= Mean)/trials
}

## noncausal pvals function for repeated measures
pvals.nb <- function(a, b, trials = 10000, means) {

    splits <- lapply(list(vs2dfb(a), vs2dfb(b)), 
        function(x) split(x[, 1:2], x[, "time"]))

    out <- lapply(unique(c(names(splits[[1]]), names(splits[[2]]))), function(x) {
        v <- splits[[1]][[x]]
        w <- splits[[2]][[x]]
        if (any(sapply(list(v, w), is.null))) return(NULL)
        m <- max(c(v[, "end"], w[, "end"]))
        Mean <- mean(vdists1(v, w), na.rm = TRUE)
        vals <- unlist(lapply(1:trials, function(i) mean(vdists1(v, remix(w, m)))))
        list(vals = vals, means = Mean)
    })

    sum(sapply(out, function(x) sum(x[["vals"]] <= x[["means"]])), 
        na.rm = TRUE)/(trials*length(out))
}

## causal pvals function
pvals.c <- function(a, b, trials = 10000, means) {
    a2 <- vs2df(a)
    b2 <- vs2df(b)
    m <- max(c(a2[, "end"], b2[, "end"]))
    Mean <- mean(vdists2(a2, b2), na.rm = TRUE)

    simmeans <- unlist(lapply(1:trials, function(i) mean(vdists2(a2, remix(b2, m)))))
    sum(simmeans <= Mean)/trials
}

## causal pvals function for repeated measures
pvals.cb <- function(a, b, trials = 10000, means) {
    splits <- lapply(list(vs2dfb(a), vs2dfb(b)), 
        function(x) split(x[, 1:2], x[, "time"]))

    out <- lapply(unique(c(names(splits[[1]]), names(splits[[2]]))), function(x) {
        v <- splits[[1]][[x]]
        w <- splits[[2]][[x]]
        if (any(sapply(list(v, w), is.null))) return(NULL)
        m <- max(c(v[, "end"], w[, "end"]))
        Mean <- mean(vdists1(v, w), na.rm = TRUE)
        vals <- unlist(lapply(1:trials, function(i) mean(vdists2(v, remix(w, m)))))
        list(vals = vals, means = Mean)
    })

    sum(sapply(out, function(x) sum(x[["vals"]] <= x[["means"]])), 
        na.rm = TRUE)/(trials*length(out))
}

## apply functions to start end codes a and b
FUN_apply <- function(a, b, FUN, ..., cause = FALSE) {
    if (cause) {
        out <- vdists2(vs2df(a), vs2df(b))
    } else {
        out <- vdists1(vs2df(a), vs2df(b))
    }
    FUN(out, ...)
}

## apply functions to start end codes a and b for main aggregated output
FUN_apply2 <- function(a, b, FUN, ..., cause = FALSE) {

    splits <- lapply(list(vs2dfb(a), vs2dfb(b)), 
        function(x) split(x[, 1:2], x[, "time"]))

    if (cause) {
        out <- unlist(lapply(unique(c(names(splits[[1]]), names(splits[[2]]))), function(x) {
            v <- splits[[1]][[x]]
            w <- splits[[2]][[x]]
            if (any(sapply(list(v, w), is.null))) return(NULL)
            vdists2(v, w)
        }))
    } else {
        out <- unlist(lapply(unique(c(names(splits[[1]]), names(splits[[2]]))), function(x) {
            v <- splits[[1]][[x]]
            w <- splits[[2]][[x]]
            if (any(sapply(list(v, w), is.null))) return(NA)
            vdists1(v, w)
        }))
    }
    FUN(out, ...)
}

## scale each column
scale.all <- function(x) {
    dims <- dim(x)
    dnms <- dimnames(x)
    x <- matrix(scale(c(x), center = FALSE), dims)
    dimnames(x) <- dnms
    x
}

##  Calculates distance measures for individual times
DIST <- function(dataframe, cvar, cause, reps, pvals) {

    ## Drop unused levels
    df <- droplevels(dataframe[, c("start", "end", cvar)])

    ## get unique codes and then all possible combinations
    cds <- sort(levels(df[, cvar]))
    xy <- expand.grid(cds, cds)
    xy <- xy[xy[, 1] != xy[, 2], 2:1]

    ## split apart dataframe bycodes
    codesplits <- lapply(lapply(split(df, df[, cvar]), "[", -3), 
        paste2, sep = "|")

    Means <- v_outer(codesplits, FUN_apply, mean, na.rm = TRUE)
    Means[is.nan(Means)] <- NA
    Sds <- v_outer(codesplits, FUN_apply, sd, na.rm = TRUE)
    Sds[is.na(Sds) & !is.na(Means)] <- 0
    N <- sapply(codesplits, Nget)

    o <- list(mean = Means, sd = Sds, n = N, 
        stan.mean = scale.all(Means)*scale.all(Sds))

    Pvals <- NULL
    ## the simulations to derive p values for the means.
    if (pvals) {
        if (cause) {
            Pvals <- v_outer(codesplits, pvals.c, trials = reps)
        } else {
            Pvals <- v_outer(codesplits, pvals.n, trials = reps)
        }
        o[["pvalue"]] <- Pvals
    } 
    o
}

##  Calculates distance measures for aggregated times
DIST2 <- function(dataframe, cvar, cause, reps, pvals, time.var) {

    df <- dataframe[, c("start", "end", cvar, time.var)]

    ## get unique codes and then all possible combinations
    cds <- sort(levels(df[, cvar]))
    xy <- expand.grid(cds, cds)
    xy <- xy[xy[, 1] != xy[, 2], 2:1]

    ## split apart dataframe bycodes
    codesplits <- lapply(lapply(split(df, df[, cvar]), "[", -3), 
        paste2, sep = "|")

    Means <- v_outer(codesplits, FUN_apply2, mean, na.rm = TRUE)
    Means[is.nan(Means)] <- NA
    Sds <- v_outer(codesplits, FUN_apply2, sd, na.rm = TRUE)
    Sds[is.na(Sds) & !is.na(Means)] <- 0
    N <- sapply(codesplits, length)

    o <- list(mean = Means, sd = Sds, n = N, 
        stan.mean = scale.all(Means)*scale.all(Sds))

    Pvals <- NULL
    ## the simulations to derive p values for the means.
    if (pvals) {
        if (cause) {
            Pvals <- v_outer(codesplits, pvals.cb, trials = reps)
        } else {
            Pvals <- v_outer(codesplits, pvals.nb, trials = reps)
        }
        o[["pvalue"]] <- Pvals
    } 
    o
}



confup <- function(reps, alpha = .05) {
   alpha - sqrt(1.96*((alpha*(1-alpha))/reps))
}



