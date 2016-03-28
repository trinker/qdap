#' Count Object Pronouns Per Grouping Variable
#' 
#' Count the number of object pronouns per grouping variables.
#' 
#' @param text.var The text variable
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param object.pronoun.list A named list of object pronouns.  See 
#' \strong{Details} for more.
#' @param \ldots Other arguments passed to \code{\link[qdap]{termco}}
#' @details The following object pronoun categories are the default searched terms:
#' \itemize{
#'     \item me = c(" me ", " my ", " mine ")
#'     \item us = c(" us ", " our ", " ours ")
#'     \item you = c(" you'd ",  " you'll ", " you're ", " you've ", " you ", " your ")
#'     \item him = c(" him ", " his ")
#'     \item her = c(" her ", " hers ")
#'     \item them = c(" them ")
#'     \item their = c(" their ", "theirs ")
#'     \item it = c(" it'd ", " it'll ", " it's ", " it ")
#' }
#' @return Returns a list, of class "object_pronoun_type", of data frames 
#' regarding object pronoun word counts:
#' \item{preprocessed}{List of uncollapsed dataframes (raw, prop, rnp) of the class "termco" that contain all searchable object pronouns.} 
#' \item{raw}{raw word counts by grouping variable} 
#' \item{prop}{proportional word counts by grouping variable; proportional to 
#' each individual's object pronoun use} 
#' \item{rnp}{a character combination data frame of raw and proportional object pronoun use}     
#' @keywords pronouns
#' @seealso \code{\link[qdap]{subject_pronoun_type}},
#' \code{\link[qdap]{pronoun_type}}
#' @export
#' @examples
#' \dontrun{
#' dat <- pres_debates2012
#' dat <- dat[dat$person %in% qcv(ROMNEY, OBAMA), ]
#' (out <- object_pronoun_type(dat$dialogue, dat$person))
#' plot(out)
#' plot(out, 2)
#' plot(out, 3)
#' plot(out, 3, ncol=2)
#' 
#' scores(out)
#' counts(out)
#' proportions(out)
#' preprocessed(out)
#' 
#' plot(scores(out))
#' plot(counts(out))
#' plot(proportions(out))
#' }
object_pronoun_type <- function(text.var, grouping.var = NULL, 
    object.pronoun.list = NULL, ...) {
    
    if (is.null(object.pronoun.list)) object.pronoun.list <- .objectpronouns

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
    
    out <- termco(text.var = text.var, grouping.var = grouping.var, 
        match.list = object.pronoun.list, elim.old = FALSE, ...)

    nms <- c("raw", "prop", "rnp")
    output <- stats::setNames(lapply(nms, function(x){
        data.frame(stats::setNames(out[[x]][, c(1:2)], c(G, "word.count")),
            out[[x]][, names(.objectpronouns), drop = TRUE])
    }), nms)
    
    out3 <- stats::setNames(lapply(nms, function(x){
            out[[x]][, !colnames(out[[x]]) %in% names(.objectpronouns), drop = TRUE]
    }), nms)

    output[["preprocessed"]] <- out3
    output <- output[c("preprocessed", nms)]

    class(output) <- "object_pronoun_type"
    attributes(output)[c("zero.replace", "percent", "digits")] <-
        out[c("zero.replace", "percent", "digits")]
    attributes(output)[["text.var"]] <- text.var
    attributes(output)[["grouping.var"]] <- grouping.var
    attributes(output)[["object.pronoun.list"]] <- object.pronoun.list
    attributes(output)[["grouping.var.name"]] <- G
    
    output
}

#' Plots an object_pronoun_type Object
#' 
#' Plots an object_pronoun_type object.
#' 
#' @param x The object_pronoun_type object.
#' @param type An integer of \code{1}, \code{2}, \code{3}) corresponding to 
#' 1 - heat map; 2 - lexical dispersion plot; 3 - facetted bar graph.
#' @param \ldots Other arguments passed to \code{\link[qdap]{qheat}}, 
#' \code{\link[qdap]{dispersion_plot}}, or \code{\link[ggplot2]{facet_wrap}}.
#' @export
#' @method plot object_pronoun_type
plot.object_pronoun_type <- function(x, type = 1, ...) {

    switch(type,
        `1` = graphics::plot(scores(x), ...),
        `2` = plot_object_pronoun_type_helper1(x, ...),
        `3` = plot_object_pronoun_type_helper2(x, ...),
        stop("`type` must be 1, 2, or 3:\n1 - heat map\n2 - lexical dispersion plot\n3 - facetted pie graph")
    )

}

plot_object_pronoun_type_helper1 <- function(x, ...){
    nms <- paste(sapply(unlist(strsplit(attributes(x)[["grouping.var.name"]], 
         "\\&")), Caps), collapse = " & ")

    dat <- data.frame(text= attributes(x)[["text.var"]],  
        group = attributes(x)[["grouping.var"]], stringsAsFactors = FALSE)
    
    dat <- stats::na.omit(dat)
    dispersion_plot(strip(dat[["text"]]), attributes(x)[["object.pronoun.list"]], 
        dat[["group"]], plot=FALSE, ...) + 
        ggplot2::ylab(nms)
}


plot_object_pronoun_type_helper2 <- function(x, ...){

    nms <- paste(sapply(unlist(strsplit(attributes(x)[["grouping.var.name"]], 
         "\\&")), Caps), collapse = " & ")

    dat <- counts(x)[, -2]
    colnames(dat)[1] <- "group.var"
    dat[, -1] <- dat[, -1, drop=FALSE]/rowSums(dat[, -1, drop=FALSE])
    nms2 <- names(sort(colSums(dat[, -1, drop=FALSE]), TRUE))
    mdat <- reshape2::melt(dat, variable = "Pronoun", id="group.var")
    mdat[["Pronoun"]] <- factor(mdat[["Pronoun"]], levels=nms2)

    ggplot2::ggplot(mdat, ggplot2::aes_string(fill = "group.var", weight = "value", x = "group.var")) +
        ggplot2::geom_bar() +
        ggplot2::coord_flip() + 
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::ylab("Percentage of Pronoun Use") +
        ggplot2::xlab(nms) + 
        ggplot2::theme_bw() + 
        ggplot2::facet_wrap(~Pronoun, ...) +
        ggplot2::theme(
            legend.position="none"
        )         
}

#' Question Counts
#' 
#' View object_pronoun_type scores.
#' 
#' object_pronoun_type Method for scores
#' @param x The \code{\link[qdap]{object_pronoun_type}} object.
#' @param \ldots ignored
#' @export
#' @method scores object_pronoun_type
scores.object_pronoun_type <- function(x, ...) {

    out <- x[["rnp"]]
    attributes(out) <- list(
            class = c("table_score", class(out)),
            type = "object_pronoun_type_scores",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}


#' Question Counts
#' 
#' View object_pronoun_type counts.
#' 
#' object_pronoun_type Method for counts
#' @param x The \code{\link[qdap]{object_pronoun_type}} object.
#' @param \ldots ignored
#' @export
#' @method counts object_pronoun_type
counts.object_pronoun_type <- function(x, ...) {

    out <- x[["raw"]]
    attributes(out) <- list(
            class = c("table_count", class(out)),
            type = "object_pronoun_type_counts",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}

#' Question Counts
#' 
#' View \code{\link[qdap]{object_pronoun_type}} proportions.
#' 
#' object_pronoun_type Method for proportions
#' @param x The object_pronoun_type object.
#' @param \ldots ignored
#' @export
#' @method proportions object_pronoun_type
proportions.object_pronoun_type <- function(x, ...) {

    out <- x[["prop"]]
    attributes(out) <- list(
            class = c("table_proportion", class(out)),
            type = "object_pronoun_type_proportions",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}


#' Question Counts
#' 
#' View \code{\link[qdap]{object_pronoun_type}} preprocessed.
#' 
#' object_pronoun_type Method for preprocessed
#' @param x The object_pronoun_type object.
#' @param \ldots ignored
#' @export
#' @method preprocessed object_pronoun_type
preprocessed.object_pronoun_type <- function(x, ...) {

    nms <- attributes(x)[["grouping.var.name"]]
    out <- lapply(x[["preprocessed"]], function(x) {
        colnames(x)[1] <- nms
        x
    })
    out[["zero.replace"]] <- attributes(x)[["zero.replace"]]
    out[["percent"]] <- attributes(x)[["percent"]]
    out[["digits"]] <- attributes(x)[["digits"]]
    class(out) <- "termco"
    out
}

#' Prints a object_pronoun_type object
#' 
#' Prints a object_pronoun_type object
#' 
#' @param x The object_pronoun_type object
#' @param \ldots ignored
#' @export
#' @method print object_pronoun_type
print.object_pronoun_type <-
function(x, ...) {
    print(scores(x))
}


.objectpronouns <- list(
    me = c(" me ", " my ", " mine "),
    us = c(" us ", " our ", " ours "),
    you = c(" you'd ",  " you'll ", " you're ", " you've ", " you ", " your "),
    him = c(" him ", " his "),
    her = c(" her ", " hers "),
    them = c(" them "),
    their = c(" their ", "theirs "),
    it = c(" it'd ", " it'll ", " it's ", " it ")
)
