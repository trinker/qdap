#' Sentence End Marks
#' 
#' 
#' \code{end_mark} - Grab the sentence end marks for a transcript.  This can be 
#' useful to categorize based on sentence type.
#' 
#' @param text.var The text variable.        
#' @param missing.end.mark A value to use for sentences with missing endmarks.
#' @param missing.text A value to use for sentences with missing (\code{NA}) 
#' text.
#' @param other.endmarks Other 1-2 character endmarks to search for.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param percent logical.  If \code{TRUE} output given as percent.  If 
#' \code{FALSE} the output is proportion.
#' @param zero.replace Value to replace 0 values with.
#' @param digits Integer; number of decimal places to round when printing.  
#' @param \ldots Other arguments passed to \code{end_mark}.
#' @rdname end_mark
#' @return Returns a character vector of qdap end marks for each sentence.  
#' End marks include:
#' \item{"."}{Declarative sentence.} 
#' \item{"?"}{Question sentence.} 
#' \item{"!"}{Exclamatory sentence.} 
#' \item{"|"}{Incomplete sentence.} 
#' \item{"*."}{Imperative-declarative sentence.} 
#' \item{"*?"}{Imperative-question sentence (unlikely to occur)} 
#' \item{"*!"}{Imperative-exclamatory sentence.} 
#' \item{"*|"}{Imperative-incomplete sentence.} 
#' \item{"no.em"}{No end mark.}
#' \item{"blank"}{Empty cell/NA.} 
#' @keywords end-mark
#' @export
#' @importFrom qdapTools matrix2df  
#' @examples
#' \dontrun{
#' end_mark(DATA.SPLIT$state)
#' end_mark(mraja1spl$dialogue)
#' table(end_mark(mraja1spl$dialogue))
#' plot(end_mark(mraja1spl$dialogue))
#' ques <- mraja1spl[end_mark(mraja1spl$dialogue) == "?", ] #grab questions
#' htruncdf(ques)
#' non.ques <- mraja1spl[end_mark(mraja1spl$dialogue) != "?", ] #non questions
#' htruncdf(non.ques, 20)
#' ques.per <- mraja1spl[end_mark(mraja1spl$dialogue) %in% c(".", "?"), ] #grab ? and .
#' htruncdf(ques.per, 20)
#' 
#' (x_by <- end_mark_by(DATA.SPLIT$state, DATA.SPLIT$person))
#' scores(x_by)
#' counts(x_by)
#' proportions(x_by)
#' preprocessed(x_by)
#' plot(scores(x_by))
#' plot(counts(x_by))
#' plot(proportions(x_by))
#' plot(preprocessed(x_by))
#' 
#' #================================#
#' ## End Marks Over Time Examples ##
#' #================================#
#' ##EXAMPLE 1
#' sentpres <- lapply(with(pres_debates2012, split(dialogue, time)), function(x) {
#'     end_mark(x)
#' })
#' 
#' sentplots <- lapply(seq_along(sentpres), function(i) {
#'     m <- plot(cumulative(sentpres[[i]]))
#'     if (i != 2) m <- m + ylab("")
#'     if (i != 3) m <- m + xlab(NULL)
#'     m + ggtitle(paste("Debate", i))
#' })
#' 
#' library(grid)
#' library(gridExtra)
#' do.call(grid.arrange, sentplots)
#' 
#' ##EXAMPLE 2
#' sentraj <- lapply(with(rajSPLIT, split(dialogue, act)), function(x) {
#'     end_mark(x)
#' })
#'  
#' sentplots2 <- lapply(seq_along(sentraj), function(i) {
#'     m <- plot(cumulative(sentraj[[i]]))
#'     if (i != 2) m <- m + ylab("")
#'     if (i != 3) m <- m + xlab(NULL)
#'     act <- qcv(I, II, III, IV, V)
#'     m + ggtitle(paste("Act", act[i]))
#' })
#' 
#' ## ggplot2 function to extract legend
#' g_legend <- function(a.gplot){ 
#'     tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
#'     leg <- which(sapply(tmp[["grobs"]], function(x) x[["name"]]) == "guide-box") 
#'     legend <- tmp[["grobs"]][[leg]] 
#'     legend
#' } 
#' 
#' ## remove legends from plots
#' sentplots3 <- lapply(sentplots2, function(x){
#'     x + theme(legend.position="none") + xlab(NULL) + ylab(NULL)
#' })
#' 
#' sentplots3[[6]] <- g_legend(sentplots2[[1]]) 
#' 
#' do.call(grid.arrange, sentplots3)
#' }
end_mark <- function(text.var, missing.end.mark = "_", missing.text = NA, 
    other.endmarks = NULL) {
    text.var <-  as.character(text.var)
    if (is.dp(text.var=text.var)){
        warning(paste0("\n  Some rows contain double punctuation.",
          "  Suggested use of sentSplit function."))
    }
    y <- nchar(text.var)
    last1 <- substring(text.var, y)
    last2 <- substring(text.var, y-1)
    vals <- c("*.", "*?", "*!", "*|", other.endmarks)
    for (i in seq_along(vals)) {
        last1[last2 == vals[i]] <- vals[i]
    }
    last1[!last1 %in% c(vals, ".", "?", "!", "|")] <- missing.end.mark
    last1[is.na(text.var)] <- missing.text
    class(last1) <- c("end_mark", class(last1))
    last1
}

#' Prints an end_mark object
#' 
#' Prints an end_mark object
#' 
#' @param x The end_mark object
#' @param \ldots ignored
#' @export
#' @method print end_mark
print.end_mark <-
function(x, ...) {
    class(x) <- "character"
    print(x)
}

#' Sentence End Marks
#' 
#' 
#' \code{end_mark_by} - Grab the sentence end marks for a transcript by grouping 
#' variable(s).
#' 
#' @rdname end_mark
#' @importFrom qdapTools mtabulate
#' @export
end_mark_by <- function(text.var, grouping.var, digits = 3, percent = FALSE, 
    zero.replace = 0, ...) {

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

    DF <- data.frame(grouping, wc = wc(text.var), text.var, 
        check.names = FALSE, stringsAsFactors = FALSE)
    DF[, "grouping"] <- factor(DF[, "grouping"])
    DF[, "end.mark"] <- end_mark(DF[, "text.var"], ...)
  
    DF2 <- mtabulate(split(DF[, "end.mark"], DF[, "grouping"]))
    props <- prop(DF2, round=FALSE)
    comb <- stats::setNames(data.frame(rownames(DF2), 
       raw_pro_comb(DF2, digits = digits, percent = percent, 
       zero.replace = zero.replace), check.names=FALSE),  
       c(G, colnames(props)))
    out <- list(raw= stats::setNames(DF, c(G, colnames(DF[-1]))), count = matrix2df(DF2, G), 
        prop = matrix2df(props, G), rnp = comb)
    class(out) <- c("end_mark_by", class(out))
    attributes(out)[["digits"]] <- digits
    attributes(out)[["percent"]] <- percent
    attributes(out)[["zero.replace"]] <- zero.replace
    out
}

#' Prints an end_mark_by object
#' 
#' Prints an end_mark_by object
#' 
#' @param x The end_mark_by object
#' @param \ldots ignored
#' @export
#' @method print end_mark_by
print.end_mark_by <-
function(x, ...) {
    WD <- options()[["width"]]
    options(width=3000)
    print(x$rnp)
    options(width=WD)
}

#' Question Counts
#' 
#' View end_mark_by scores.
#' 
#' end_mark_by Method for scores
#' @param x The \code{\link[qdap]{end_mark_by}} object.
#' @param \ldots ignored
#' @export
#' @method scores end_mark_by
scores.end_mark_by <- function(x, ...) {

    out <- x[["rnp"]]
    attributes(out) <- list(
            class = c("end_mark_by_score", class(out)),
            type = "end_mark_by_scores",
            names = colnames(out),
            row.names = rownames(out),
            count = (x[["count"]])
    )
    out
}



#' Question Counts
#' 
#' View end_mark_by counts.
#' 
#' end_mark_by Method for counts
#' @param x The \code{\link[qdap]{end_mark_by}} object.
#' @param \ldots ignored
#' @export
#' @method counts end_mark_by
counts.end_mark_by <- function(x, ...) {

    out <- x[["count"]]
    attributes(out) <- list(
            class = c("end_mark_by_count", class(out)),
            type = "end_mark_by_counts",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}

#' Question Counts
#' 
#' View \code{\link[qdap]{end_mark_by}} proportions.
#' 
#' end_mark_by Method for proportions
#' @param x The end_mark_by object.
#' @param \ldots ignored
#' @export
#' @method proportions end_mark_by
proportions.end_mark_by <- function(x, ...) {

    out <- x[["prop"]]
    attributes(out) <- list(
            class = c("end_mark_by_proportion", class(out)),
            type = "end_mark_by_proportions",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}


#' Question Counts
#' 
#' View \code{\link[qdap]{end_mark_by}} preprocessed.
#' 
#' end_mark_by Method for preprocessed
#' @param x The end_mark_by object.
#' @param \ldots ignored
#' @export
#' @method preprocessed end_mark_by
preprocessed.end_mark_by <- function(x, ...) {

    out <- x[["raw"]]
    attributes(out) <- list(
            class = c("end_mark_by_preprocessed", class(out)),
            type = "end_mark_by_preprocessed",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}

#' Prints a end_mark_by_preprocessed object
#' 
#' Prints a end_mark_by_preprocessed object
#' 
#' @param x The end_mark_by_preprocessed object
#' @param \ldots ignored
#' @export
print.end_mark_by_preprocessed <-
function(x, ...) {
    WD <- options()[["width"]]
    options(width=3000)
    class(x) <- "data.frame"
    print(x)
    options(width=WD)
}

#' Plots a end_mark_by_score Object
#' 
#' Plots a end_mark_by_score object.
#' 
#' @param x The end_mark_by_score object.
#' @param values logical.  If \code{TRUE} the cell values will be included on 
#' the heatmap.
#' @param \ldots Arguments passed to \code{\link[qdap]{qheat}}.
#' @method plot end_mark_by_score
#' @export
plot.end_mark_by_score <- function(x, values = TRUE, ...){ 

    if (values) {
        qheat(attributes(x)[["count"]], mat2 = x, ...)
    } else {
        qheat(attributes(x)[["count"]], ...) 
    }
  
}


#' Plots a end_mark_by_count Object
#' 
#' Plots a end_mark_by_count object.
#' 
#' @param x The end_mark_by_count object.
#' @param values logical.  If \code{TRUE} the cell values will be included on 
#' the heatmap.
#' @param \ldots Arguments passed to \code{\link[qdap]{qheat}}.
#' @method plot end_mark_by_count
#' @export
plot.end_mark_by_count <- function(x, values = TRUE, ...){ 

    qheat(x, values = values, ...)

}

#' Plots a end_mark_by_proportion Object
#' 
#' Plots a end_mark_by_proportion object.
#' 
#' @param x The end_mark_by_proportion object.
#' @param values logical.  If \code{TRUE} the cell values will be included on 
#' the heatmap.
#' @param \ldots Arguments passed to \code{\link[qdap]{qheat}}.
#' @method plot end_mark_by_proportion
#' @export
plot.end_mark_by_proportion <- function(x, values = TRUE, ...){ 

    qheat(x, values = values, ...)

}



#' Plots a end_mark_by_preprocessed Object
#' 
#' Plots a end_mark_by_preprocessed object.
#' 
#' @param x The end_mark_by_preprocessed object.
#' @param ncol The number of columns to use for \code{\link[ggplot2]{facet_wrap}}.
#' @param \ldots ignored
#' @importFrom ggplot2 xlab ggplot geom_bar facet_wrap coord_flip aes_string
#' @method plot end_mark_by_preprocessed
#' @export
plot.end_mark_by_preprocessed <- function(x, ncol = 1, ...){ 

    group <- colnames(x)[1]    
    ord1 <- sort(table(x[, group] ))
    x[, group] <- factor(x[, group], levels = names(ord1))
    ord2 <- sort(table(x[, "end.mark"]), decreasing = TRUE)
    x[, "end.mark"] <- factor(x[, "end.mark"], levels = names(ord2))

    colnames(x)[1] <- "QDAP_GROUP"
    ggplot(x, aes_string(x="QDAP_GROUP", fill="QDAP_GROUP")) + 
        geom_bar(show_guide = FALSE) + 
        coord_flip() +
        facet_wrap(stats::reformulate("end.mark"), ncol = ncol) +
        xlab(paste(unlist(sapply(unlist(strsplit(group, "&")), 
            Caps)), collapse =" & ")) 
}

#' Plots a end_mark_by Object
#' 
#' Plots a end_mark_by object.
#' 
#' @param x The end_mark_by object.
#' @param values logical.  If \code{TRUE} the cell values will be included on 
#' the heatmap.
#' @param \ldots Other arguments passed to \code{\link[qdap]{qheat}}.
#' @method plot end_mark_by
#' @export 
plot.end_mark_by <- function(x, values = FALSE, ...) {

    graphics::plot(scores(x), values = values, ...)

}



#' \code{cumulative.end_mark} - Generate end_mark over time (duration in 
#' sentences).
#' @rdname cumulative
#' @export
#' @importFrom qdapTools %lc%
#' @method cumulative end_mark
cumulative.end_mark <- function(x, ...){

    types_key <- structure(list(Symbol = c(".", "?", "!", "|", "*.", "*?", "*!", 
        "*|", "no.em", "blank"), Type = structure(c(1L, 10L, 2L, 7L, 
        3L, 6L, 4L, 5L, 9L, 8L), .Label = c("Declarative", "Exclamatory", 
        "Imperative-declarative", "Imperative-exclamatory", "Imperative-incomplete", 
        "Imperative-question", "Incomplete", "Missing", "No end mark", 
        "Question"), class = "factor")), .Names = c("Symbol", "Type"), row.names = c(NA, 
        -10L), class = "data.frame")

    `%lcqdap%` <- qdapTools::`%lc%`    
    x <- x %lcqdap% types_key
    x[is.na(x)] <- "Missing"
    
    all <- names(table(x))
    
    out <- lapply(1:length(x), function(i){
    
        y <- c(table(x[1:i]))
        missing <- all[!all %in% names(y)]
        
        if(!identical(missing, character(0))){
            y <- c(y, stats::setNames(rep(0, length(missing)), missing))
        }
        data.frame(t(y[as.character(types_key[["Type"]][types_key[["Type"]] 
            %in% names(y)])]), stringsAsFactors = FALSE)
    })
    
    plottingdf <- data.frame(Sentence = 1:length(out), do.call(rbind, out))
    class(plottingdf) <- c("cumulative_end_mark", class(plottingdf))
    plottingdf
}

#' Plots a cumulative_end_mark Object
#' 
#' Plots a cumulative_end_mark object.
#' 
#' @param x The cumulative_end_mark object.
#' @param \ldots ignored
#' @method plot cumulative_end_mark 
#' @export
plot.cumulative_end_mark <- function(x, ...){
    
    ord <- names(sort(colSums(x[nrow(x), -1, drop=FALSE]), decreasing=FALSE))

    mplottingdf <- reshape2::melt(x, id="Sentence", variable="Type")
    mplottingdf[, "Type"] <- factor(mplottingdf[, "Type"], levels=colnames(x)[-1])
    
    ggplot2::ggplot(data=mplottingdf, ggplot2::aes_string(x="Sentence", 
        y="value", group="Type", color="Type")) + ggplot2::theme_bw() +
        ggplot2::geom_line(size=1) + 
        ggplot2::ylab("Number") + 
        ggplot2::xlab("Duration") +
        ggplot2::scale_x_continuous(expand = c(0, 0)) +
        ggplot2::scale_colour_discrete(name = "Sentence Type")
}

#' Prints a cumulative_end_mark Object
#' 
#' Prints a cumulative_end_mark  object.
#' 
#' @param x The cumulative_end_mark object.
#' @param \ldots ignored
#' @method print cumulative_end_mark
#' @export
print.cumulative_end_mark <- function(x, ...) {
    print(plot.cumulative_end_mark(x, ...))
}

#' Plots an end_mark Object
#' 
#' Plots an end_mark object.
#' 
#' @param x The end_mark object.
#' @param \ldots ignored
#' @importFrom qdapTools %lc%
#' @method plot end_mark 
#' @export
plot.end_mark <- function(x, ...) {

    types_key <- structure(list(Symbol = c(".", "?", "!", "|", "*.", "*?", "*!", 
        "*|", "no.em", "blank"), Type = structure(c(1L, 10L, 2L, 7L, 
        3L, 6L, 4L, 5L, 9L, 8L), .Label = c("Declarative", "Exclamatory", 
        "Imperative-declarative", "Imperative-exclamatory", "Imperative-incomplete", 
        "Imperative-question", "Incomplete", "Missing", "No end mark", 
        "Question"), class = "factor")), .Names = c("Symbol", "Type"), row.names = c(NA, 
        -10L), class = "data.frame")

    `%lcqdap%` <- qdapTools::`%lc%`        
    x <- x %lcqdap% types_key
    x[is.na(x)] <- "Missing"
    y <- table(x)
    dat <- data.frame(y)
    dat[, "x"] <- factor(dat[, "x"], levels=names(sort(y)))

    ggplot2::ggplot(data=dat, aes_string(x="x", weight="Freq")) + 
        ggplot2::geom_bar() +
        ggplot2::coord_flip() + ggplot2::ylab("Count") +
        ggplot2::xlab("Sentence Type")

}

