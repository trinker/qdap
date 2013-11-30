#' Polarity Score (Sentiment Analysis)
#' 
#' \code{polarity} - Approximate the sentiment (polarity) of text by grouping 
#' variable(s).
#' 
#' @param text.var The text variable.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param polarity.frame A dataframe or environment containing a dataframe of 
#' positive/negative words and weights.
#' @param negators A character vector of terms reversing the intent of a 
#' positive or negative word.
#' @param amplifiers A character vector of terms that increase the 
#' intensity of a positive or negative word.
#' @param deamplifiers A character vector of terms that decrease the 
#' intensity of a positive or negative word.
#' @param question.weight The weighting of questions (values from 0 to 1).  
#' Default 0 corresponds with the belief that questions (pure questions) are not 
#' polarized.  A weight may be applied based on the evidence that the questions 
#' function with polarity.
#' @param amplifier.weight The weight to apply to amplifiers/deamplifiers (values 
#' from 0 to 1).  This value will multiply the polarized terms by 1 + this 
#' value.
#' @param n.before The number of words to consider as valence shifters before 
#' the polarized word.
#' @param n.after The number of words to consider as valence shifters after 
#' the polarized word.
#' @param rm.incomplete logical.  If \code{TRUE} text rows ending with qdap's 
#' incomplete sentence end mark (\code{|}) will be removed from the analysis.
#' @param digits Integer; number of decimal places to round when printing. 
#' @param \ldots Other arguments supplied to \code{\link[qdap]{strip}}.
#' @return Returns a list of:
#' \item{all}{A dataframe of scores per row with:
#' \itemize{
#'   \item  group.var - the grouping variable
#'   \item  wc - word count
#'   \item  polarity - sentence polarity score
#'   \item  pos.words - words considered positive
#'   \item  neg.words - words considered negative
#'   \item  text.var - the text variable}
#' }
#' \item{group}{A dataframe with the average polarity score by grouping variable:
#' \itemize{
#'   \item  group.var - the grouping variable
#'   \item  total.sentences - Total sentences spoken.
#'   \item  total.words - Total words used.
#'   \item  ave.polarity - The sum of all polarity scores for that group divided by number of sentences spoken.
#'   \item  sd.polarity - The standard deviation of that group's sentence level polarity scores.
#'   \item  stan.mean.polarity - A standardized polarity score calculated by taking the average polarity score for a group divided by the standard deviation.}
#' }
#' \item{digits}{integer value od number of digits to display; mostly internal 
#' use} 
#' @seealso \url{https://github.com/trestletech/Sermon-Sentiment-Analysis}
#' @note The polarity score is dependent upon the polarity dictionary used.  
#' This function defaults to the word polarity dictionary used by Hu, M., & 
#' Liu, B. (2004), however, this may not be appropriate for the context of 
#' children in a classroom.  The user may (is encouraged) to provide/augment the 
#' dictionary (see the \code{polarity_frame} function).  For instance the word 
#' "sick" in a high school setting may mean that something is good, whereas 
#' "sick" used by a typical adult indicates something is not right or negative 
#' connotation.
#' 
#' Also note that \code{\link[qdap]{polarity}} assumes you've run 
#' \code{\link[qdap]{sentSplit}}.
#' @details The equation used by the algorithm to assign value to polarity of 
#' each sentence fist utilizes the sentiment dictionary (Hu and Liu, 2004) to 
#' tag polarized words.  A context cluster (\eqn{x_i^{T}}) of words is pulled 
#' from around this polarized word (default 4 words before and two words after) 
#' to be considered as valence shifters.  The words in this context cluster are 
#' tagged as neutral (\eqn{x_i^{0}}), negator (\eqn{x_i^{N}}), amplifier 
#' (\eqn{x_i^{a}}), or de-amplifier (\eqn{x_i^{d}}). Neutral words hold no value 
#' in the equation but do affect word count (\eqn{n}).  Each polarized word is 
#' then weighted \eqn{w} based on the weights from the \code{polarity.frame} 
#' argument and then further weighted by the number and position of the valence 
#' shifters directly surrounding the positive or negative word.  The researcher 
#' may provide a weight \eqn{c} to be utilized with amplifiers/de-amplifiers 
#' (default is .8; deamplifier weight is constrained to -1 lower bound).  Last, 
#' these context cluster (\eqn{x_i^{T}}) are summed and divided by the square 
#' root of the word count (\eqn{\sqrt{n}}) yielding an unbounded polarity score 
#' (\eqn{\delta}).  Note that context clusters containing a comma before the 
#' polarized word will only consider words found after the comma.
#' 
#' \deqn{\delta=\frac{x_i^T}{\sqrt{n}}}
#'   
#' Where:
#' 
#' \deqn{x_i^T=\sum{((1 + c(x_i^{A} - x_i^{D}))\cdot w(-1)^{\sum{x_i^{N}}})}}
#' 
#' \deqn{x_i^{A}=\sum{(w_{neg}\cdot x_i^{a})}}
#' 
#' \deqn{x_{i}^D=\left\{\begin{array}{cc}
#' x_{i}^D & x_{i}^D \geq  -1         \\ 
#' -1 & x_{i}^D < -1
#' \end{array}\right.}
#' 
#' \deqn{x_i^{D}=\sum{(- w_{neg}\cdot x_i^{a} + x_i^{d})}}
#' 
#' \deqn{w_{neg}=\left\{\begin{array}{cc}
#' 1 & \sum{x_i^{N}} \bmod {2} >0         \\ 
#' 0 & \sum{x_i^{N}} \bmod {2} =0
#' \end{array}\right.}
#'     
#' @references Hu, M., & Liu, B. (2004). Mining opinion features in customer 
#' reviews. National Conference on Artificial Intelligence. 
#' 
#' \url{http://www.slideshare.net/jeffreybreen/r-by-example-mining-twitter-for}
#' @keywords sentiment, polarity
#' @export
#' @rdname polarity
#' @examples
#' \dontrun{
#' with(DATA, polarity(state, list(sex, adult)))
#' (poldat <- with(sentSplit(DATA, 4), polarity(state, person)))
#' names(poldat)
#' truncdf(poldat$all, 8)
#' poldat$group
#' poldat2 <- with(mraja1spl, polarity(dialogue, 
#'     list(sex, fam.aff, died)))
#' colsplit2df(poldat2$group)
#' plot(poldat)
#' 
#' poldat3 <- with(rajSPLIT, polarity(dialogue, person))
#' poldat3[["group"]][, "OL"] <- outlier_labeler(poldat3[["group"]][, 
#'     "ave.polarity"])
#' poldat3[["all"]][, "OL"] <- outlier_labeler(poldat3[["all"]][, 
#'     "polarity"])
#' head(poldat3[["group"]], 10)
#' htruncdf(poldat3[["all"]], 15, 8)
#' plot(poldat3)
#' plot(poldat3, nrow=4)
#' qheat(poldat3[["group"]][, -7], high="red", order.b="ave.polarity")
#' 
#' ## Create researcher defined polarity.frame
#' POLENV <- polarity_frame(positive.words, negative.words)
#' POLENV
#' ls(POLENV)[1:20]
#' }
polarity <- function (text.var, grouping.var = NULL, 
    polarity.frame = qdapDictionaries::env.pol, 
    negators = qdapDictionaries::negation.words, 
    amplifiers = qdapDictionaries::amplification.words, 
    deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, 
    amplifier.weight = .8, n.before = 4, n.after = 2, rm.incomplete = FALSE, 
    digits = 3, ...) {

    ## Save name of group vars column(s)
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

    ## create group vars
    if(is.null(grouping.var)){
        grouping <- rep("all", length(text.var))
    } else {
        if (is.list(grouping.var) & length(grouping.var) > 1) {
            grouping <- paste2(grouping.var)
        } else {
            grouping <- unlist(grouping.var)
        } 
    } 

    ## wrap groupvars and text var together into data.frame
    text.var <- as.character(text.var)
    DF <- data.frame(grouping, text.var, check.names = FALSE, 
        stringsAsFactors = FALSE)
    DF[, "text.var2"] <- DF[, "text.var"]

    ## remove incomplete sentences
    if (rm.incomplete) {
        DF <- end_inc(dataframe = DF, text.var = text.var, warning.report = FALSE)
    }

    ## warning about double punctuation
    if (is.dp(text.var=text.var)){
        warning(paste0("\n  Some rows contain double punctuation.",
          "  Suggested use of `sentSplit` function."))
    }

    ## replace commas for later consideration
    DF[, "text.var"] <- gsub(",", " longreplacementbreakoff ", DF[, "text.var"])

    ## An environment to look up polarized words
    if (!is.environment(polarity.frame)) {
        if (!is.data.frame(polarity.frame)) {
            stop(paste("Please supply a dataframe or environment", 
               "(see `polarity_frame`) to polarity.frame"))
        }
        polarity.frame <- hash(polarity.frame)
    }

    ## remove amplifiers also in polarity frame
    amplifiers <- amplifiers[!amplifiers %in% ls(polarity.frame)]
    deamplifiers <- deamplifiers[!deamplifiers %in% ls(polarity.frame)]

    ## create environment to lookup amps, de-amps and negators
    alter <- alter_env(negators, amplifiers, deamplifiers)

    ## grab the polarized/alter words to search for spaces and insert into textvar
    posneg <- ls(polarity.frame)
    words <- c(posneg, ls(alter))

    ## create sentence lengths
    counts <- unlist(lapply(DF[, "text.var"], function(x) length(bag_o_words(x))))
    DF[, "text.var"] <- space_fill(text.var = strip(DF[, "text.var"], 
        apostrophe.remove = FALSE, ... ), terms = words[grep("\\s", words)], 
        sep = "~~")

    ## split into bag of words per sentence
    TV <- lapply(lapply(DF[, "text.var"], bag_o_words, ...), function(x) {
        gsub("~~", " ", x)
    })

    ## Get position of polarized word (hits)
    hits <- lapply(TV, function(x) which(x %in% posneg))

    ## loop over the hits per sentence (nested loop) and apply the polarity_helper
    pols <- list()
    output <- list()
    polwords <- list()
    for (i in seq_along(hits)) {
        vlen <- ifelse(identical(hits[[i]], integer(0)), 1, length(hits[[i]]))
        pols[[i]] <- rep(0, vlen)
        output[[i]] <- rep(0, vlen)
        polwords[[i]] <- rep(NA, vlen)
        for(j in seq_along(hits[[i]])) {
            theoutputs <- polarity_helper(tv = TV[[i]], hit=hits[[i]][[j]], 
                polenv = polarity.frame, altenv = alter, count = counts[i],
                n.before = n.before, n.after = n.after, 
                amp.weight = amplifier.weight)
            output[[i]][j] <- theoutputs[[1]]
            pols[[i]][j] <- theoutputs[[2]]
            polwords[[i]][j] <- theoutputs[[3]]
        }
    }

    ## Construct sentence data.frame
    scores <- sapply(output, sum)/sqrt(counts)
    all <- data.frame(group.var =  DF[, "grouping"], wc = counts, 
        polarity = scores)

    ## Functions to grab pos and neg words and then add to all data.frame
    pwords <- function(x, y) {
        out <- y[x > 0]
        if (identical(out, character(0)) | identical(out, logical(0))) {
            return("-")
        }        
        out
    }
    nwords <- function(x, y) {
        out <- y[x < 0]
        if (identical(out, character(0)) | identical(out, logical(0))) {
            return("-")
        }
        out
    }
    all$pos.words <- mapply(pwords, pols, polwords)
    all$neg.words <-mapply(nwords, pols, polwords)
    all[, "text.var"] <- DF[, "text.var2"]
   
    ## Multiple polarity by question weights
    qweight <- ifelse(suppressWarnings(end_mark(all[, "text.var"])) %in% c("?", 
        "*?"), question.weight, 1)
    all[, "polarity"] <- qweight * all[, "polarity"]

    ## Create average polarity data.frame (group) from all data.frame
    sall <- split(all, all[, "group.var"])
    lall <- lapply(sall, function(x) {
        data.frame(total.words = sum(x[, "wc"]), 
            ave.polarity = mean(x[, "polarity"]),
            sd.polarity = sd(x[, "polarity"]), 
            stan.mean.polarity = mean(x[, "polarity"])/sd(x[, "polarity"]))
    })
    group <- data.frame(group = names(lall), 
        total.sentences = sapply(sall, nrow),
        do.call(rbind, lall), row.names = NULL)
    colnames(group)[1] <- colnames(all)[1] <- G
    o <- list(all = all, group = group, digits = digits)
    class(o) <- "polarity"
    return(o)
}



#' Polarity Score (Sentiment Analysis)
#' 
#' \code{polarity_frame} - Generate a polarity lookup environment or data.frame 
#' for use with the \code{polarity.frame} argument in the \code{polarity} 
#' function.
#' 
#' @param positives A character vector of positive words.
#' @param negatives A character vector of negative words.
#' @param pos.weights A vector of weights to weight each positive word by.  
#' Length must be equal to length of \code{postives} or length 1 (if 1 weight 
#' will be recycled). 
#' @param neg.weights A vector of weights to weight each negative word by.  
#' Length must be equal to length of \code{negatives} or length 1 (if 1 weight 
#' will be recycled). 
#' @param envir logical.  If \code{TRUE} a lookup table (a dataframe within 
#' an environment) is produced rather than a data.frame.
#' @export
#' @rdname polarity
polarity_frame <- function(positives, negatives, pos.weights = 1, 
    neg.weights = -1, envir = TRUE) {
    plen <- length(positives)
    nlen <- length(negatives)
    if (!length(plen) %in% c(length(positives), 1)) {
        stop("The length of positives and pos.weights must be equal")
    }
    if (!length(nlen) %in% c(length(negatives), 1)) {
        stop("The length of negatives and negative weights must be equal")
    }
    if (length(pos.weights) == 1) {
        pos.weights <- rep(pos.weights, plen)
    }
    if (length(neg.weights) == 1) {
        neg.weights <- rep(neg.weights, nlen)
    }
    dat <- data.frame(words = c(positives, negatives), polarity = c(pos.weights, 
        neg.weights))
    if (envir) {
        hash(dat)
    } else {
        dat
    }
}
#' Prints a polarity Object
#' 
#' Prints a polarity object.
#' 
#' @param x The polarity object.
#' @param digits Number of decimal places to print. 
#' @param \ldots ignored
#' @method print polarity
#' @S3method print polarity
print.polarity <- 
function(x, digits = NULL, ...) {
    message("POLARITY BY GROUP\n=================\n")
    WD <- options()[["width"]]
    options(width=3000)
    y <- x$group
    if (is.null(digits)) {
        digits <- x$digits
    }
    y[, "ave.polarity"] <- round(y[, "ave.polarity"], digits = digits)
    y[, "sd.polarity"] <- round(y[, "sd.polarity"], digits = digits)
    y[, "stan.mean.polarity"] <- round(y[, "stan.mean.polarity"], 
        digits = digits)
    print(y)
    options(width=WD)
}

#' Plots a polarity Object
#' 
#' Plots a polarity object as a heat map Gantt plot with polarity over 
#' time (measured in words) and polarity scores per sentence.  In the Gantt 
#' plot the black dots are the average polarity per grouping variable.
#' 
#' @param x The polarity object.
#' @param bar.size The size of the bars used in the Gantt plot.
#' @param low The color to be used for lower values.
#' @param mid The color to be used for mid-range values (default is a less 
#' striking color).
#' @param high The color to be used for higher values.
#' @param ave.polarity.shape The shape of the average polarity score used in the 
#' dot plot.
#' @param alpha Transparency level of points (ranges between 0 and 1).
#' @param shape The shape of the points used in the dot plot.
#' @param point.size The size of the points used in the dot plot.
#' @param jitter Amount of vertical jitter to add to the points.
#' @param nrow The number of rows in the dotplot legend (used when the number of 
#' grouping variables makes the legend too wide).  If \code{NULL} no legend if 
#' plotted.
#' @param na.rm logical. Should missing values be removed?
#' @param order.by.polarity logical.  If \code{TRUE} the group polarity plot 
#' will be ordered by average polarity score, otherwise alphabetical order is 
#' assumed.
#' @param plot logical.  If \code{TRUE} the plot will automatically plot.  
#' The user may wish to set to \code{FALSE} for use in knitr, sweave, etc.
#' to add additional plot layers.
#' @param error.bars logical.  If \code{TRUE} error bars are added to the 
#' polarity dot plot using the standard error of the mean polarity score.
#' @param error.bar.height The height of the error bar ends.
#' @param error.bar.size The size/thickness of the error bars.
#' @param error.bar.color The color of the error bars.  If \code{NULL} each 
#' bar will be colored by grouping variable.
#' @param \ldots ignored
#' @return Invisibly returns the \code{ggplot2} objects that form the larger 
#' plot.  
#' @method plot polarity
#' @importFrom gridExtra grid.arrange
#' @importFrom scales alpha
#' @importFrom ggplot2 ggplot aes geom_segment xlab ylab scale_colour_gradientn theme_bw guides geom_point guide_colorbar scale_color_discrete guide_legend
#' @S3method plot polarity
plot.polarity <- function(x, bar.size = 5, low = "red", mid = "grey99", 
    high = "blue", ave.polarity.shape = "+", alpha = 1/4, shape = 19, 
    point.size = 2.5,  jitter = .1, nrow = NULL, na.rm = TRUE, 
    order.by.polarity = TRUE, plot = TRUE, error.bars =TRUE, 
    error.bar.height = .5, error.bar.size = .5, error.bar.color = "black", 
    ...){
  
    Polarity <- group <- ave.polarity <- unit <- NULL
    dat <- x[["group"]][, 1:4]
    dat2 <- x[["all"]]
    if (na.rm) {
       dat <- na.omit(dat)
       dat2 <- na.omit(dat2)
    }
    G <- names(dat)[1]
    nms <- c("group", "dialogue", "word_count", "Polarity")
    names(dat)[c(1)] <-  nms[1]
    names(dat2)[c(1, 6, 2, 3)] <- nms
    dat2 <- data.frame(dat2, with(dat2, 
        gantt(dialogue, list(group, seq_along(group)))))
    if (is.null(nrow)) {
        leg <- FALSE
        nrow <- 1
    } else {
        leg <- TRUE
    }

    ## reverse the levels so first factor level is on top
    dat2$group <- factor(dat2$group, levels = rev(levels(dat2$group)))

    ## the filled polarity Gantt plot
    XX <- ggplot(dat2, aes(color = Polarity )) + 
        geom_segment(aes(x=start, xend=end, y=group, yend=group), 
            size=bar.size) +
        xlab("Duration (words)") + ylab(gsub("\\&", " & ", G)) +
        scale_colour_gradientn(colours = c(low, mid, high)) +
        theme_bw() + theme(legend.position="bottom") + 
        guides(colour = guide_colorbar(barwidth = 9, barheight = .75))

    ## order the ave. poalrity dotplot by ave. polarity or factor level
    if (order.by.polarity) {
        dat$group <- factor(dat$group, levels = rev(dat[order(dat$ave.polarity), 
            "group"]))
        dat2$group <- factor(dat2$group, 
            levels = rev(dat[order(dat$ave.polarity), "group"]))
    }
    if (na.rm) {
       dat2 <- na.omit(dat2)
       dat <- na.omit(dat)
    }

    ## Plot the polarity dotplot with optional error bars
    YY <- ggplot(dat2, aes(y=group, x=Polarity, colour = group)) + 
        geom_point(data = dat, aes(x=ave.polarity), shape = ave.polarity.shape, 
            size = 6, show_guide=FALSE) +
        geom_point(alpha = alpha, shape = shape, 
            size = point.size, position = position_jitter(height = jitter)) 

    ## Optional Error Bars
    if (error.bars) {
        se <-  tapply(dat2[, "Polarity"], dat2[ "group"], SE)
        dat[, "se"] <- lookup(dat[, "group"], names(se), se)

        ## optional error.bar single color; if NULL colored by group
        if (!is.null(error.bar.color)) {
            YY <- YY + geom_errorbarh(data=dat, height = error.bar.height, 
                size = error.bar.size, color = error.bar.color, aes(x=ave.polarity, 
                    xmax = ave.polarity + se, xmin = ave.polarity - se))
        } else {
            YY <- YY + geom_errorbarh(data=dat, height = error.bar.height, 
                size = error.bar.size, aes(x=ave.polarity, 
                    xmax = ave.polarity + se, xmin = ave.polarity - se))
        }
    }

    ## Add the black average polarity point
    YY <- YY + geom_point(data = dat, aes(x=ave.polarity), shape = 19, 
            size = 1.5, colour = "black", show_guide=FALSE) +
        ylab(gsub("\\&", " & ", G)) +
        scale_color_discrete(name= G) 

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

## Helper functions
SE <- function(x) sqrt(var(x)/length(x))

alter_env <- function(negators, amplifiers, deamplifiers) {
    n <- rep(1, length(negators))
    a <- rep(2, length(amplifiers))
    d <- rep(3, length(deamplifiers)) 
    hash(data.frame(words=c(negators, amplifiers, deamplifiers), 
        value=c(n, a, d)))
}

polarity_helper <- function(tv, hit, polenv, altenv, count, amp.weight, 
    n.before, n.after) {

    if (identical(hit, integer(0))) {
        return(list(0, 0))
    }
   
    ## Mark location of polarized word
    target <- ifelse((hit - n.before) < 1, hit, n.before + 1)
 
    ## Comma checks to remove polarized words preceeded by comma
    comma.check <- tv %in% "longreplacementbreakoff"
    if (sum(comma.check) > 0) {
        comma.loc <- which(comma.check)
        if (sum(comma.loc < target) > 0) {
            final.comma <- tail(comma.loc[comma.loc < target], 1)
            n.before <- hit - final.comma
            target <- ifelse((hit - n.before) < 1, hit, n.before + 1)
        }
    }

    ## Grab n1 words before polarized word and n2 after
    lower <- ifelse((hit - n.before) < 1, 1, hit - n.before)
    upper <- ifelse((hit + n.after) > count, count, hit + n.after)
    inds <- lower:upper
    words <- tv[inds]

    ## look in the hashtable at the polarity weights
    targ <- words[target]
    p <- hash_look(targ, polenv)

    ## determine contextual valence shifters (negators, deamplifiers and amplifiers)
    context <- hash_look(words[-target], altenv)
    if (!is.null(context)) {
        context <- unlist(lapply(split(context, context), length))
        ident <- function(x, y) if (!x %in% names(y)) 0 else y[names(y) == x]
        n <- ident("1", context)
        a <- ident("2", context)
        d <- ident("3", context)
        D <- (d + ifelse(n %% 2 != 0, 1, 0) * a) * ((-1) * amp.weight)
        D <- ifelse(D < -1, -1, D)        
        A <- (ifelse(n %% 2 == 0, 1, 0) * a * amp.weight)
    } else {
        D <- A <- n <- 0
    }   

    ## return the word group score and the polarity of the the polarized word
    list(x = (1 + (D + A)) * (p * (-1)^(2 + n)), y = p, z = targ)
}



