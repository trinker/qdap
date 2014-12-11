#' Lexical Classification Score
#' 
#' Transcript apply lexical classification score (content to functional word 
#' proportion) by grouping variable(s) and optionally plot 
#' the breakdown of the model.  
#' 
#' @param text.var The text variable.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param order.by.lexical_classification logical.  If \code{TRUE} orders the 
#' results by #' lexical_classification score.
#' @param function.words A vector of function words.  Default is 
#' \code{\link[qdapDictionaries]{function.words}}.
#' @param bracket The bracket type to remove.  Use \code{NULL} to not remove 
#' bracketed substrings.  See \code{bracket} argument in 
#' \code{\link[qdap]{bracketX}} for bracket types.
#' @param \ldots Other arguments passed to \code{\link[qdap]{bracketX}}.
#' @details Content words (i.e., nouns, verbs, adjectives, and adverbs) tend to 
#' be the words speakers stresses in language use.  Whereas, functional words 
#' are the "glue" that holds the content together.  Speakers devote much less 
#' time and stress to these words (i.e., pronouns, articles, conjunctions, 
#' quantifiers, and prepositions).
#' @return A list containing at the following components: 
#' \item{content}{A \code{data.frame} of all content words used and corresponding frequencies} 
#' \item{functional}{A \code{data.frame} of all content words used and corresponding frequencies} 
#' \item{raw}{Sentence level descriptive statistics on content vs. functional word use} 
#' \item{lexical_classification}{Summarized (grouping variable level) descriptive statistics for content vs. functional word use} 
#' @references Chung, C. & Pennebaker, J. (2007). The Psychological Functions of Function Words. In K. Fiedler (Ed.) Social Communication (pp. 343-359). New York: Psychology Press.
#'
#' Pulvermuller, F. (1999). Words in the brain's language. Behavioral and Brain Sciences, 22, pp. 253-279. doi:10.1017/S0140525X9900182X
#'
#'  Segalowitz, S. J. & Lane, K. (2004). Perceptual fluency and lexical access for function versus content words. Behavioral and Brain Sciences, 27, 307-308. doi:10.1017/S0140525X04310071 
#'
#' Bell, A., Brenier, J. M., Gregory, M., Girand, C. & Jurafsky, D. (2009).  Predictability Effects on Durations of Content and Function Words in Conversational English.  Journal of Memory and Language, 60(1), 92-111. doi:10.1016/j.jml.2008.06.003
#' @keywords lexical_classification, parts-of-speech, functional, content
#' @export
#' @rdname lexical_classification
#' @examples
#' \dontrun{
#' lexical_classification("I did not like the dog.")
#' lexical_classification(DATA.SPLIT$state, DATA.SPLIT$person)
#' 
#' (out <- with(pres_debates2012, lexical_classification(dialogue, list(person, time))))
#' plot(out)
#' 
#' scores(out)
#' 
#' out2 <- preprocessed(out)
#' htruncdf(out2)
#' plot(out2)
#' 
#' plot(out[["content"]])
#' dev.new()
#' plot(out[["functional"]])
#' 
#' ## cloud of functional vs. content
#' ## Highlight Content Words
#' set.seed(10)
#' par(mar = c(0,0,0,0))
#' list(
#'         content = out[["content"]],
#'         functional = out[["functional"]]
#'     ) %>%
#'     list_df2df("type") %>%
#'     dplyr::mutate(colors = ifelse(type == "functional", "gray80", "blue")) %>%
#'     with(., wordcloud::wordcloud(
#'         word, 
#'         freq, 
#'         min.freq = 8, 
#'         random.order=FALSE,
#'         ordered.colors = TRUE,
#'         colors = colors
#'     )) 
#' mtext("2012 Presidential Debates:\nFunctional vs. Content Word Use", padj=1.25)
#' legend(
#'     .05, .12, bty = "n",
#'     legend = c("functional", "content"), 
#'     fill = c("gray80", "blue"),  
#'     cex = .7
#' )
#' 
#' ## Highlight Functional Words
#' set.seed(10)
#' par(mar = c(0,0,0,0))
#' list(
#'         content = out[["content"]],
#'         functional = out[["functional"]]
#'     ) %>%
#'     list_df2df("type") %>%
#'     dplyr::mutate(colors = ifelse(type == "functional", "red", "gray80")) %>%
#'     with(., wordcloud::wordcloud(
#'         word, 
#'         freq, 
#'         min.freq = 8, 
#'         random.order=FALSE,
#'         ordered.colors = TRUE,
#'         colors = colors
#'     )) 
#' mtext("2012 Presidential Debates:\nFunctional vs. Content Word Use", padj=1.25)
#' legend(
#'     .05, .12, bty = "n",
#'     legend = c("functional", "content"), 
#'     fill = c("red", "gray80"),  
#'     cex = .7
#' )
#' }
lexical_classification <- function(text.var, grouping.var = NULL,
    order.by.lexical_classification = TRUE,
    function.words = qdapDictionaries::function.words, 
    bracket = "all", ...) {

    lexical <- group <- word.count <- n.content <- n.functional <- freq <- ave.content.rate <- NULL

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

    DF <- data.frame(grouping, text.var = as.character(text.var), check.names = FALSE, 
        stringsAsFactors = FALSE, orig.row.num = seq_len(length(text.var)))
    DF[, "grouping"] <- factor(DF[, "grouping"])
    if (is.dp(text.var=DF[, "text.var"])){
        warning(paste0("\n  Some rows contain double punctuation.",
          "  Suggested use of sentSplit function."))
    }

    ## remove brackets
    if (!is.null(bracket)) {
        DF[["text.var"]] <- bracketX(DF[, "text.var"], bracket = bracket, ...)
    } 

    ## assign and count content vs. functional
    char02NA <- function(x){ 
        x[sapply(x, function(x) identical(character(0), x))] <- NA
        x
    }
    words <- lapply(DF[["text.var"]], bag_o_words)
    is_content <- lapply(words, function(x) !x %in% function.words)
    nan2na <- function(x) {
        sapply(x, function(y){
            if (length(y) > 1) return(y)
            if (is.nan(y)) return(NA)
            y
        })
    }

    DF2 <- data.frame(
        group = DF[, "grouping"], 
        markup = sapply(Map(function(x, y) paste(x, y, sep="/"), 
            words, lapply(is_content, as.numeric)), paste, collapse=" "),
        word.count=wc(DF[["text.var"]]), 
        content.rate = nan2na(100*sapply(is_content, mean, na.rm=TRUE)),
        n.content = sapply(is_content, sum, na.rm=TRUE),
        n.functional = sapply(is_content, function(x) sum(!x, na.rm=TRUE)),
        stringsAsFactors = FALSE
    )

    DF2[["words"]] <- words
    DF2[["lexical"]] <- lapply(is_content, as.numeric)
    DF2[DF2[["markup"]] == "", 2:8] <- NA
    DF2[["content"]] <- char02NA(Map(function(x, y) x[as.logical(y)], 
        DF2[["words"]], DF2[["lexical"]]))
    DF2[["functional"]] <- char02NA(Map(function(x, y) x[as.logical(y)], 
        DF2[["words"]], lapply(DF2[["lexical"]], function(x) !x)))


    ## calculate summary scores
    DF3 <- DF2 %>% 
        dplyr::group_by(group) %>%
        dplyr::summarise(
            word.count = sum(word.count, na.rm = TRUE),
            ave.content.rate = 100*mean(unlist(lexical), na.rm = TRUE),
            SE = SE(100*na.omit(unlist(lexical))),
            n.content = sum(n.content, na.rm = TRUE),
            n.functional = sum(n.functional, na.rm = TRUE),
            content = paste(na.omit(unlist(content)), collapse="_"),
            functional = paste(na.omit(unlist(functional)), collapse="_")
        ) 

    DF3[["content"]] <- char02NA(lapply(DF3[["content"]], function(x) strsplit(x, "_")[[1]]))
    DF3[["functional"]] <- char02NA(lapply(DF3[["functional"]], function(x) strsplit(x, "_")[[1]]))
    if (order.by.lexical_classification) {
        DF3 <- DF3 %>% 
            dplyr::arrange(-ave.content.rate)
        DF3[["group"]] <- factor(DF3[["group"]], levels = DF3[["group"]])
    }
    class(DF3) <- c("lexical_classification_by", "data.frame")
    colnames(DF3)[1] <- colnames(DF2)[1] <- G

    class(DF2) <- c("lexical_classification_sent", "data.frame")    

    content <- NA
    if (!all(is.na(unlist(DF2[["content"]])))) {
        content <- DF2[["content"]] %>%
            unlist %>%
            na.omit %>%
            table %>%
            as.matrix %>%
            matrix2df %>%
            setNames(c("word", "freq")) %>% 
            dplyr::arrange(-freq)

        class(content) <- c("lexical", class(content))
        attributes(content)[["type"]] <- "content"
    }

    functional <- NA
    if (!all(is.na(unlist(DF2[["functional"]])))) {
        functional <- DF2[["functional"]] %>%
            unlist %>%
            na.omit %>%
            table %>%
            as.matrix %>%
            matrix2df %>%
            setNames(c("word", "freq")) %>% 
            dplyr::arrange(-freq)

        class(functional) <- c("lexical", class(functional) )
        attributes(functional)[["type"]] <- "functional"

    }

    o <- list(content = content, functional = functional, 
        raw = DF2, lexical_classification = DF3)
    class(o) <- "lexical_classification"
    attributes(o)[["lexical_classification"]] <- lexical_classification

    text.env <- new.env(FALSE)
    text.env[["text.var"]] <- DF[["text.var"]]
    attributes(o)[["text.var"]] <- text.env
    group.env <- new.env(FALSE)
    group.env[["grouping.var"]] <- DF[["grouping"]]
    attributes(o)[["grouping.var"]] <- group.env
    o  
}

#' Prints a lexical_classification Object
#' 
#' Prints a lexical_classification_by  object.
#' 
#' @param x The lexical_classification_by object.
#' @param ave.digits The number of average lexical distribution proportion 
#' digits to print.
#' @param se.digits The number of standard error of the lexical distribution 
#' proportion digits to print.
#' @param trunc The width to truncate content/function word lists.
#' @param \ldots ignored
#' @method print lexical_classification_by
#' @export
print.lexical_classification_by <-
function(x, ave.digits = 1, se.digits = 1, trunc = 25, ...) {
  
    WD <- options()[["width"]]
    options(width=3000)

    class(x) <- "data.frame"

    if ("ave.content.rate" %in% colnames(x)) {
        x[["ave.content.rate"]] <- gsub("^\\.0+%$", "0%", 
            paste0(numformat(x[["ave.content.rate"]], 
            digits = ave.digits), "%"))
        x[["SE"]] <- numformat(x[["SE"]], digits = se.digits)
    }

    x[["content"]] <- sapply(x[["content"]], function(w) {
        if (length(w) == 1 && is.na(w)) return(w)
        paste(unlist(w), collapse=", ")
    })
    x[["functional"]] <- sapply(x[["functional"]], function(w) {
        if (length(w) == 1 && is.na(w)) return(w)
        paste(unlist(w), collapse=", ")
    })

    x[c("content", "functional")] <- lapply(c("content", "functional"), function(y) {
        sapply(x[[y]], function(z){
            if (nchar(z) < 1000) return(z)
            substring(z, 1, 1000)
        })
    })

    if (all(c("functional", "content") %in% colnames(x))) {
        x <- left_just(x, 7)
        x <- left_just(x, 8)
    }

    maxchar <- max(nchar(unlist(lapply(x[, 1:6], as.character))))
    trunc <- ifelse(maxchar > trunc, maxchar, trunc)
    x <- truncdf(x, trunc)

    x[[7]] <- sapply(as.character(x[[7]]), function(x) { 
        if (grepl("^NA\\s+$|\\s{2,}$", x)) {
            x
        } else {
            gsub(".{3}$", "\\.\\.\\.", x)
        }
    })
    x[[8]] <- sapply(as.character(x[[8]]), function(x) { 
        if (grepl("^NA\\s+$|\\s{2,}$", x)) {
            x
        } else {
            gsub(".{3}$", "\\.\\.\\.", x)
        }
    })

    print(x)
    options(width=WD)
}

#' Prints an lexical_classification Object
#' 
#' Prints an lexical_classification object.
#' 
#' @param x The lexical_classification object.
#' @param \ldots Other arguments passed to 
#' \code{\link[qdap]{print.lexical_classification_by}}.
#' @method print lexical_classification
#' @export
print.lexical_classification <- function(x, ...) {
    print(scores(x), ...)
}



#' Plots a lexical Object
#' 
#' Plots a lexical object.
#' 
#' @param x The lexical object.
#' @param min.freq Words with frequency below \code{min.freq} will not be plotted.
#' @param rot.per Proportion words with 90 degree rotation.
#' @param random.order logical.  If code{TRUE} plot words in random order. If \code{FALSE}, they will be plotted in decreasing frequency.
#' @param title The title of the plot.  Use \code{NULL} to eliminate.
#' @param title.color The color of the title.
#' @param \ldots Other arguments passed to \code{\link[wordcloud]{wordcloud}}.
#' @importFrom scales alpha
#' @method plot lexical
#' @export
plot.lexical <- function(x, min.freq=1, rot.per=0, random.order = FALSE, 
    title = TRUE, title.color = "blue", ...){

    wordcloud::wordcloud(x[["word"]], x[["freq"]], random.order = random.order,
        min.freq = min.freq, rot.per = rot.per, ...)

    if (!is.null(title)) {
        par(mar = c(0, 0, 2, 0))
        if (isTRUE(title)) title <- Caps(attributes(x)[["type"]])
        mtext(title, side = 3, col = title.color, padj=-1)
    }
}

#' Lexical Classification
#' 
#' \code{scores.lexical_classification} - View scores from \code{\link[qdap]{lexical_classification}}.
#' 
#' lexical_classification Method for scores
#' @param x The lexical_classification object.
#' @param \ldots ignored
#' @export
#' @method scores lexical_classification
scores.lexical_classification <- function(x, ...) {

    out <- x[["lexical_classification"]]
    attributes(out) <- list(
            class = c("lexical_classification_score", class(out)),
            type = "lexical_classification_scores",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}



#' Plots a lexical_classification Object
#' 
#' Plots a lexical_classification object as a heat map Gantt plot with lexical_classification over 
#' time (measured in words) and lexical_classification scores per sentence.  In the dotplot 
#' plot the black dots are the average lexical_classification per grouping variable.
#' 
#' @param x The lexical_classification object.
#' @param bar.size The size of the bars used in the Gantt plot.
#' @param low The color to be used for lower values.
#' @param mid The color to be used for mid-range values (default is a less 
#' striking color).
#' @param high The color to be used for higher values.
#' @param ave.lexical_classification.shape The shape of the average lexical_classification score used in the 
#' dot plot.
#' @param alpha Transparency level of points (ranges between 0 and 1).
#' @param shape The shape of the points used in the dot plot.
#' @param point.size The size of the points used in the dot plot.
#' @param jitter Amount of vertical jitter to add to the points.
#' @param nrow The number of rows in the dotplot legend (used when the number of 
#' grouping variables makes the legend too wide).  If \code{NULL} no legend if 
#' plotted.
#' @param na.rm logical. Should missing values be removed?
#' @param order.by.lexical_classification logical.  If \code{TRUE} the group lexical_classification plot 
#' will be ordered by average lexical_classification score, otherwise alphabetical order is 
#' assumed.
#' @param plot logical.  If \code{TRUE} the plot will automatically plot.  
#' The user may wish to set to \code{FALSE} for use in knitr, sweave, etc.
#' to add additional plot layers.
#' @param error.bars logical.  If \code{TRUE} error bars are added to the 
#' lexical_classification dot plot using the standard error of the mean lexical_classification score.
#' @param error.bar.height The height of the error bar ends.
#' @param error.bar.size The size/thickness of the error bars.
#' @param error.bar.color The color of the error bars.  If \code{NULL} each 
#' bar will be colored by grouping variable.
#' @param error.bar.alpha The alpha level of the error bars.
#' @param \ldots ignored
#' @return Invisibly returns the \code{ggplot2} objects that form the larger 
#' plot.  
#' @method plot lexical_classification
#' @importFrom gridExtra grid.arrange
#' @importFrom scales alpha
#' @importFrom qdapTools lookup
#' @importFrom ggplot2 ggplot aes geom_segment xlab ylab scale_colour_gradientn theme_bw guides geom_point guide_colorbar scale_color_discrete guide_legend
#' @export
plot.lexical_classification <- function(x, bar.size = 5, low = "blue", mid = "grey99", 
    high = "red", ave.lexical_classification.shape = "+", alpha = 1/4, shape = 19, 
    point.size = 2.5,  jitter = .1, nrow = NULL, na.rm = TRUE, 
    order.by.lexical_classification = TRUE, plot = TRUE, error.bars =TRUE, 
    error.bar.height = .5, error.bar.size = .5, error.bar.color = "black",
    error.bar.alpha = .6, ...){

    content.rate <- Lexical_classification <- group <- content.rate <- ave.content.rate <- unit <- NULL

    dat2 <- x[["lexical_classification"]] 
    dat <- as.data.frame(setNames(lapply(1:6, function(i) dat2[, i]), 
        colnames(dat2)[1:6]))
    dat2 <- x[["raw"]][, c(1, 4)]
    dat2[["dialogue"]] <- sapply(x[["raw"]][, 7], unbag)
    dat2[dat2[["dialogue"]] == "", "dialogue"] <- NA
    if (na.rm) {
       dat <- na.omit(dat)
       dat2 <- na.omit(dat2)
    }
    G <- names(dat)[1]
  
    colnames(dat2)[1] <- colnames(dat)[1] <-  "group"
    names(dat)[3] <- "content.rate"

    dat2 <- data.frame(dat2, with(dat2, 
        gantt(dialogue, list(group, seq_along(group)))))
    if (is.null(nrow)) {
        leg <- FALSE
        nrow <- 1
    } else {
        leg <- TRUE
    }

    ## reverse the levels so first factor level is on top
    dat2[["group"]] <- factor(dat2[["group"]], 
        levels = rev(dat[["group"]]))

    ## the filled lexical_classification Gantt plot
    nms <- paste(sapply(strsplit(G, "&")[[1]], Caps), collapse = " & ")
    XX <- ggplot(dat2, aes(color = content.rate)) + 
        geom_segment(aes(x=start, xend=end, y=group, yend=group), 
            size=bar.size) +
        xlab("Duration (sentences)") + ylab(nms) +
        scale_colour_gradientn(colours = c(low, mid, high), name="Content\nRate") +
        theme_bw() + 
        theme(
            plot.margin = unit(c(1, 1, 1, 1), "lines"),
            legend.position="bottom"
        ) + 
        guides(colour = guide_colorbar(barwidth = 9, barheight = .75, nbin=1000))

    ## order the ave. poalrity dotplot by ave. lexical_classification or factor level
    if (order.by.lexical_classification) {
        dat[["group"]] <- factor(dat[["group"]], levels = dat[order(dat[["content.rate"]]), 
            "group"])
        dat2[["group"]] <- factor(dat2[["group"]], 
            levels = dat[order(dat[["content.rate"]]), "group"])
    } else {
        ## reverse the levels so first factor level is on top
        dat2[["group"]] <- factor(dat2[["group"]], 
            levels = sort(unique(dat2[["group"]]), decreasing = TRUE))
        dat[["group"]] <- factor(dat[["group"]], 
            levels = sort(unique(dat[["group"]]), decreasing = TRUE))     
    }
    if (na.rm) {
       dat2 <- na.omit(dat2)
       dat <- na.omit(dat)
    }
    
    ## Plot the lexical_classification dotplot with optional error bars
    YY <- ggplot(dat2, aes(y=group, x=content.rate, colour = group)) + 
        geom_point(data = dat, aes(x=content.rate), shape = ave.lexical_classification.shape, 
            size = 6, show_guide=FALSE) +
        geom_point(alpha = alpha, shape = shape, 
            size = point.size, position = position_jitter(height = jitter)) 

    ## Optional Error Bars
    if (error.bars) {
        ## optional error.bar single color; if NULL colored by group
        if (!is.null(error.bar.color)) {
            YY <- YY + geom_errorbarh(data=dat, height = error.bar.height, alpha = error.bar.alpha,
                size = error.bar.size, color = error.bar.color, aes(x=content.rate, 
                    xmax = content.rate + SE, xmin = content.rate- SE))
        } else {
            YY <- YY + geom_errorbarh(data=dat, height = error.bar.height, alpha = error.bar.alpha, 
                size = error.bar.size, aes(x=content.rate, 
                    xmax = content.rate + SE, xmin = content.rate - SE))
        }
    }

    ## Add the black average lexical_classification point
    YY <- YY + geom_point(data = dat, aes(x=content.rate), shape = 19, 
            size = 1.5, colour = "black", show_guide=FALSE) +
        ylab(nms) + xlab("Average Content Rate") +
        scale_color_discrete(name= nms) 

    ## Legend for dotplot
    if (leg) {
        YY <- YY + theme(plot.margin = unit(c(-.25, 1, 1, 1), "lines"), 
            legend.position="bottom")  +
            guides(col = guide_legend(nrow = nrow, byrow = TRUE, 
                override.aes = list(shape = shape, alpha = 1)))
    } else {
        YY <- YY + theme(plot.margin = unit(c(-.25, 1, 1, 1), "lines"), 
            legend.position="none")       
    } 

    ## Logical plotting argument for use in knitr
    if (plot) {
        grid.arrange(XX, YY, nrow = 2)
    }
    invisible(list(p1 = XX, p2 = YY))
}

        
#' Prints a lexical_classification_score Object
#' 
#' Prints a lexical_classification_score object.
#' 
#' @param x The lexical_classification_score object.
#' @param digits The number of digits displayed if \code{values} is \code{TRUE}.
#' @param \ldots ignored
#' @method print lexical_classification_score
#' @export
print.lexical_classification_score <-
    function(x, digits = 3, ...) {

    class(x) <- c("lexical_classification_by", "data.frame")
    print(x, ...)

}


#' Lexical Classification
#' 
#' \code{preprocessed.lexical_classification} - View preprocessed from \code{\link[qdap]{lexical_classification}}.
#' 
#' lexical_classification Method for preprocessed.
#' @param x The lexical_classification object.
#' @param \ldots ignored
#' @export
#' @method preprocessed lexical_classification
preprocessed.lexical_classification <- function(x, ...) {
    out <- x[["raw"]]
    attributes(out) <- list(
            class = c("lexical_classification_preprocessed", class(out)),
            type = "lexical_classification_preprocessed",
            names = colnames(out),
            row.names = rownames(out),
            text.var = attributes(x)[["text.var"]]
    )
    out
}


#' Prints a lexical_classification_preprocessed Object
#' 
#' Prints a lexical_classification_preprocessed object.
#' 
#' @param x The lexical_classification_preprocessed object.
#' @param \ldots ignored
#' @method print lexical_classification_preprocessed
#' @export
print.lexical_classification_preprocessed <-
    function(x, ...) {

    class(x) <- "data.frame"
        
    WD <- options()[["width"]]
    options(width=3000)
    print(x)
    options(width=WD)
}


#' Plots a lexical_classification_score Object
#' 
#' Plots a lexical_classification_score object.
#' 
#' @param x The lexical_classification_score object.
#' @param error.bar.height The height of the error bar ends.
#' @param error.bar.size The size/thickness of the error bars.
#' @param error.bar.alpha The alpha level of the error bars.
#' @param \ldots ignored
#' @importFrom gridExtra grid.arrange
#' @importFrom scales alpha
#' @method plot lexical_classification_score
#' @export
plot.lexical_classification_score <- function(x, error.bar.height = .35, 
    error.bar.size = .5, error.bar.alpha = .3, ...){ 

    ave.content.rate <- n.content <- n.functional <- NULL
    
    character.count <- sentence.count <- word.count <- grvar <- 
        SE <- ave.polarity <- sd.polarity <- total.sentences <- NULL

    x  <- x[order(x[, "ave.content.rate"]), ]
    x[, 1] <- factor(x[, 1], levels = x[, 1])
    nms <- paste(sapply(strsplit(names(x)[1], "&")[[1]], Caps), collapse = " & ")
    names(x)[1] <- "grvar"

    dat <- as.data.frame(setNames(lapply(1:6, function(i) x[[i]]), colnames(x)[1:6])) 

    dat2 <- dat %>% 
        tidyr::gather(class, counts, c(n.content, n.functional)) %>%
        dplyr::mutate(prop = counts/word.count) %>%
        dplyr::group_by(grvar) %>%
        dplyr::mutate(
            position = Reduce('+', list(prop/2,cumsum(c(0,head(prop,-1))))),
            labs = paste0(numformat(100*round(prop, 4), 2), "%"),
            class = sapply(gsub("n\\.", "", class), Caps)
         )

    ##     plot1 <- ggplot2::ggplot(dat2, ggplot2::aes_string(x = "grvar", 
    ##             fill = "class", weight ="counts")) +
    ##         ggplot2::geom_bar(position = "fill") + 
    ##         ggplot2::scale_y_continuous(expand = c(0,0)) +
    ##         ggplot2::theme(
    ##             legend.position = "bottom", 
    ##             axis.ticks.x=ggplot2::element_blank(), 
    ##             axis.text.x=ggplot2::element_blank(),
    ##             legend.title=ggplot2::element_blank()
    ##         ) +
    ##         ggplot2::xlab(nms) +
    ##         ggplot2::ylab(NULL) + 
    ##         ggplot2::geom_text(aes_string(y="position", label="labs"), size=3, 
    ##             color = "grey30") + 
    ##         ggplot2::coord_flip() 

    plot2 <- ggplot2::ggplot(dat, ggplot2::aes_string(y = "grvar", x = "ave.content.rate")) +
        ggplot2::geom_point(ggplot2::aes_string(size="word.count"), color="grey40", alpha=.3) + 
        ggplot2::geom_errorbarh(ggplot2::aes(xmax = ave.content.rate + SE, 
                xmin = ave.content.rate - SE), 
                height = error.bar.height, size = error.bar.size, 
                alpha = error.bar.alpha) +
        ggplot2::geom_point(size=2) + 
        ggplot2::ylab(nms) +
        ggplot2::xlab("Average Sentence Content Rate") + 
        ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%")) +
        ggplot2::scale_size_continuous(name="Word\nCount") +
        ggplot2::theme(legend.key = element_rect(fill = NA)) +
        ggplot2::guides(size=guide_legend(title.hjust =0.5))

    print(plot2)
    invisible(plot2)
}


#' Plots a lexical_classification_preprocessed Object
#' 
#' Plots a lexical_classification_preprocessed object.
#' 
#' @param x The lexical_classification_preprocessed object.
#' @param jitter The amount to jitter the points by in the bocplots.
#' @param text.size The text size to use for plotting the mean in the boxplots.
#' @param alpha The alpha level to use for points.
#' @param ncol The number of columns to use for \code{\link[ggplot2]{facet_wrap}}.
#' @param \ldots ignored
#' @importFrom ggplot2 ggplot aes geom_point theme theme_minimal ylab xlab scale_size_continuous element_blank guides 
#' @importFrom scales alpha
#' @method plot lexical_classification_preprocessed
#' @export
plot.lexical_classification_preprocessed <- function(x, jitter=.1, 
    text.size=3.5, alpha = .3, ncol = 3, ...){ 

    content.rate <- NULL
    
    nms <- paste(sapply(strsplit(names(x)[1], "&")[[1]], 
        Caps), collapse = " & ")

    dat <- data.frame(group = x[[1]],
        word.count = x[["word.count"]], 
        content.rate = x[["content.rate"]],
        time = as.numeric(attributes(x)[["row.names"]]), 
        stringsAsFactors = FALSE
    )

    dat2 <- dat %>%
        dplyr::group_by_("group") %>%
        dplyr::summarise(ave=mean(content.rate, na.rm=TRUE)) %>%
        dplyr::arrange_("ave") %>%
        dplyr::mutate(labs=numformat(ave, 2))

    dat[["group"]] <- factor(dat[["group"]], levels=dat2[["group"]])
    dat2[["group"]] <- factor(dat2[["group"]], levels=dat2[["group"]])

    plot1 <- ggplot2::ggplot(dat, ggplot2::aes_string(x="group", y = "content.rate",
            group="group", color="group")) + 
        ggplot2::geom_jitter(alpha=alpha, h = 0, w = jitter) +
        ggplot2::geom_boxplot(fill=NA, outlier.shape=NA, size=.7) +
        ggplot2::geom_text(data=dat2, ggplot2::aes_string(y="ave", x="group", 
            label="labs"), vjust=1.2, color="grey40", size=text.size) +
        ggplot2::geom_point(data=dat2, ggplot2::aes_string(y="ave", x="group"), 
            shape=3, color="black")  + 
        ggplot2::coord_flip() + 
        ggplot2::xlab(nms) + 
        ggplot2::ylab("Content Rate") +
        ggplot2::guides(color=FALSE, alpha=FALSE) +
        theme_minimal()

    dat[["group"]] <- factor(dat[["group"]], levels=rev(dat2[["group"]]))

    plot2 <- ggplot2::ggplot(dat, ggplot2::aes_string(y = "content.rate",
            x="word.count", color="group")) +
        ggplot2::geom_hline(data=dat2, aes_string(yintercept = "ave"), 
            linetype=2, size=.7, alpha=.7) +
        ggplot2::geom_point(alpha=alpha)  + 
        ggplot2::geom_smooth() +
        ggplot2::facet_wrap(~group, ncol=ncol) +
        ggplot2::theme_minimal() + 
        ggplot2::theme(panel.grid = ggplot2::element_blank(),
            panel.margin = grid::unit(1, "lines")) +
        ggplot2::annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
        ggplot2::annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
        ggplot2::ylab("Content Rate") + 
        ggplot2::xlab("Word Count") +
        ggplot2::guides(color=FALSE, alpha=FALSE) +
        ggplot2::scale_color_manual(values = 
            rev(gg_color_hue(length(levels(dat[["group"]])))))

    gridExtra::grid.arrange(plot1, plot2, ncol=2)
}