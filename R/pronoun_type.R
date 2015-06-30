#' Count Object/Subject Pronouns Per Grouping Variable
#' 
#' Count the number of subject/object pronouns per grouping variables.
#' 
#' @param text.var The text variable
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param pronoun.list A named list of subject/object pronouns.  See 
#' \strong{Details} for more.
#' @param \ldots Other arguments passed to \code{\link[qdap]{termco}}
#' @details The following subject/object pronoun categories are the default searched terms:
#' \itemize{
#'     \item I = c(" i'd ", " i'll ", " i'm ", " i've ", " i ")
#'     \item we = c(" we'd ", " we'll ", " we're ", " we've ", " we ")
#'     \item you = c(" you'd ",  " you'll ", " you're ", " you've ", " you ", " your ")
#'     \item he = c(" he'd ", " he'll ", " he's ", " he ")
#'     \item she = c(" she'd ", " she'll ", " she's ", " she ")
#'     \item they = c(" they'd ", " they'll ", " they're ", "they've ", " they ")
#'     \item it = c(" it'd ", " it'll ", " it's ", " it ")
#'     \item me = c(" me ", " my ", " mine ")
#'     \item us = c(" us ", " our ", " ours ")
#'     \item him = c(" him ", " his ")
#'     \item her = c(" her ", " hers ")
#'     \item them = c(" them ")
#'     \item their = c(" their ", "theirs ")
#' }
#' @return Returns a list, of class "pronoun_type", of data frames 
#' regarding subject/object pronoun word counts:
#' \item{preprocessed}{List of uncollapsed dataframes (raw, prop, rnp) of the class "termco" that contain all searchable subject/object pronouns.} 
#' \item{raw}{raw word counts by grouping variable} 
#' \item{prop}{proportional word counts by grouping variable; proportional to 
#' each individual's subject/object pronoun use} 
#' \item{rnp}{a character combination data frame of raw and proportional subject/object pronoun use}     
#' @keywords pronouns
#' @seealso \code{\link[qdap]{object_pronoun_type}},
#' \code{\link[qdap]{subject_pronoun_type}}
#' @export
#' @references
#' Fairclough, N. (1989). Language and power. London: Longman. \cr
#' 
#' Fairclough, N. (2003). Analysing discourse: Textual analysis for social 
#' research. Oxford and New York: Routledge.\cr 
#' 
#' Okamura, A. (2009). Use of personal pronouns in two types of monologic 
#' academic speech.  The Economic Journal of Takasaki City University of 
#' Economics, 52(1). 17-26. \cr
#' 
#' Us and them: Social categorization and the process of intergroup bias. 
#' Perdue, C. W., Dovidio, J. F., Gurtman, M. B., & Tyler, R. B. (1990). Journal 
#' of Personality and Social Psychology, 59(3), 475-486. 
#' doi: 10.1037/0022-3514.59.3.475 
#' @examples
#' \dontrun{
#' dat <- pres_debates2012
#' dat <- dat[dat$person %in% qcv(ROMNEY, OBAMA), ]
#' (out <- pronoun_type(dat$dialogue, dat$person))
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
#' 
#' (out2 <- pronoun_type(hamlet$dialogue, hamlet$person))
#' plot(out2, 3, ncol=7)
#' }
pronoun_type <- function(text.var, grouping.var = NULL, 
    pronoun.list = NULL, ...) {
    
    if (is.null(pronoun.list)) pronoun.list <- .pronouns

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
        match.list = pronoun.list, elim.old = FALSE, ...)

    nms <- c("raw", "prop", "rnp")
    output <- stats::setNames(lapply(nms, function(x){
        data.frame(stats::setNames(out[[x]][, c(1:2)], c(G, "word.count")),
            out[[x]][, names(.pronouns), drop = TRUE])
    }), nms)
    
    out3 <- stats::setNames(lapply(nms, function(x){
            out[[x]][, !colnames(out[[x]]) %in% names(.pronouns), drop = TRUE]
    }), nms)

    output[["preprocessed"]] <- out3
    output <- output[c("preprocessed", nms)]

    class(output) <- "pronoun_type"
    attributes(output)[c("zero.replace", "percent", "digits")] <-
        out[c("zero.replace", "percent", "digits")]
    attributes(output)[["text.var"]] <- text.var
    attributes(output)[["grouping.var"]] <- grouping.var
    attributes(output)[["pronoun.list"]] <- pronoun.list
    attributes(output)[["grouping.var.name"]] <- G
    
    output
}

#' Plots an pronoun_type Object
#' 
#' Plots an pronoun_type object.
#' 
#' @param x The pronoun_type object.
#' @param type An integer of \code{1}, \code{2}, \code{3}) corresponding to 
#' 1 - heat map; 2 - lexical dispersion plot; 3 - facetted bar graph.
#' @param \ldots Other arguments passed to \code{\link[qdap]{qheat}}, 
#' \code{\link[qdap]{dispersion_plot}}, or \code{\link[ggplot2]{facet_wrap}}.
#' @export
#' @method plot pronoun_type
plot.pronoun_type <- function(x, type = 1, ...) {

    switch(type,
        `1` = graphics::plot(scores(x), ...),
        `2` = plot_pronoun_type_helper1(x, ...),
        `3` = plot_pronoun_type_helper2(x, ...),
        stop("`type` must be 1, 2, or 3:\n1 - heat map\n2 - lexical dispersion plot\n3 - facetted pie graph")
    )

}

plot_pronoun_type_helper1 <- function(x, ...){
    nms <- paste(sapply(unlist(strsplit(attributes(x)[["grouping.var.name"]], 
         "\\&")), Caps), collapse = " & ")

    dat <- data.frame(text= attributes(x)[["text.var"]],  
        group = attributes(x)[["grouping.var"]], stringsAsFactors = FALSE)
    
    dat <- stats::na.omit(dat)
    dispersion_plot(strip(dat[["text"]]), attributes(x)[["pronoun.list"]], 
        dat[["group"]], plot=FALSE, ...) + 
        ggplot2::ylab(nms)
}


plot_pronoun_type_helper2 <- function(x, ...){

    nms <- paste(sapply(unlist(strsplit(attributes(x)[["grouping.var.name"]], 
         "\\&")), Caps), collapse = " & ")
    dat <- counts(x)[, -2]
    colnames(dat)[1] <- "group.var"
    dat[, -1] <- dat[, -1, drop=FALSE]/rowSums(dat[, -1, drop=FALSE])
    dat2 <- as.matrix(dat[, -1])
    dat2[is.nan(dat2)] <- 0
    dat <- data.frame(dat[, 1, drop=FALSE], dat2, stringsAsFactors = FALSE)
    nms2 <- names(sort(colSums(dat[, -1, drop=FALSE]), TRUE))
    mdat <- reshape2::melt(dat, variable = "Pronoun", id="group.var")
    mdat[["Pronoun"]] <- factor(mdat[["Pronoun"]], levels=nms2)

    ggplot2::ggplot(mdat, ggplot2::aes_string(fill = "group.var", weight = "value", x = "group.var")) +
        ggplot2::geom_bar() +
        ggplot2::coord_flip() + 
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::ylab("Percentage of Prounoun Use") +
        ggplot2::xlab(nms) + 
        ggplot2::theme_bw() + 
        ggplot2::facet_wrap(~Pronoun, ...) +
        ggplot2::theme(
            legend.position="none"
        )         
}

#' Question Counts
#' 
#' View pronoun_type scores.
#' 
#' pronoun_type Method for scores
#' @param x The \code{\link[qdap]{pronoun_type}} object.
#' @param \ldots ignored
#' @export
#' @method scores pronoun_type
scores.pronoun_type <- function(x, ...) {

    out <- x[["rnp"]]
    attributes(out) <- list(
            class = c("table_score", class(out)),
            type = "pronoun_type_scores",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}


#' Question Counts
#' 
#' View pronoun_type counts.
#' 
#' pronoun_type Method for counts
#' @param x The \code{\link[qdap]{pronoun_type}} object.
#' @param \ldots ignored
#' @export
#' @method counts pronoun_type
counts.pronoun_type <- function(x, ...) {

    out <- x[["raw"]]
    attributes(out) <- list(
            class = c("table_count", class(out)),
            type = "pronoun_type_counts",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}

#' Question Counts
#' 
#' View \code{\link[qdap]{pronoun_type}} proportions.
#' 
#' pronoun_type Method for proportions
#' @param x The pronoun_type object.
#' @param \ldots ignored
#' @export
#' @method proportions pronoun_type
proportions.pronoun_type <- function(x, ...) {

    out <- x[["prop"]]
    attributes(out) <- list(
            class = c("table_proportion", class(out)),
            type = "pronoun_type_proportions",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}


#' Question Counts
#' 
#' View \code{\link[qdap]{pronoun_type}} preprocessed.
#' 
#' pronoun_type Method for preprocessed
#' @param x The pronoun_type object.
#' @param \ldots ignored
#' @export
#' @method preprocessed pronoun_type
preprocessed.pronoun_type <- function(x, ...) {

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

#' Prints a pronoun_type object
#' 
#' Prints a pronoun_type object
#' 
#' @param x The pronoun_type object
#' @param \ldots ignored
#' @export
#' @method print pronoun_type
print.pronoun_type <-
function(x, ...) {
    print(scores(x))
}


.pronouns <- list(
    I = c(" i'd ", " i'll ", " i'm ", " i've ", " i "),
    we = c(" we'd ", " we'll ", " we're ", " we've ", " we "),
    you = c(" you'd ",  " you'll ", " you're ", " you've ", " you ", " your "),
    he = c(" he'd ", " he'll ", " he's ", " he "),
    she = c(" she'd ", " she'll ", " she's ", " she "),
    they = c(" they'd ", " they'll ", " they're ", "they've ", " they "),
    it = c(" it'd ", " it'll ", " it's ", " it "),
    me = c(" me ", " my ", " mine "),
    us = c(" us ", " our ", " ours "),
    him = c(" him ", " his "),
    her = c(" her ", " hers "),
    them = c(" them "),
    their = c(" their ", "theirs ")
)
