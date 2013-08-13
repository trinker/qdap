library(qdap)
library(data.table)

alter_env <- function(negators, amplifiers, deamplifiers) {
    n <- rep(1, length(negators))
    a <- rep(2, length(amplifiers))
    d <- rep(3, length(deamplifiers)) 
    hash(data.frame(words=c(negators, amplifiers, deamplifiers), value=c(n, a, d)))
}

polarity_frame <- function(positives, negatives, pos.weights = 1, 
    neg.weights = -1, envir = TRUE) {
    plen <- length(positives)
    nlen <- length(negatives)
    if (length(pos.weights) == 1) {
        pos.weights <- rep(pos.weights, plen)
    }
    if (length(neg.weights) == 1) {
        neg.weights <- rep(neg.weights, nlen)
    }
    dat <- data.frame(words = c(positives, negatives), polarity = c(pos.weights, neg.weights))
    if (envir) {
        hash(dat)
    } else {
        dat
    }
}

polarity_helper <- function(tv, hit, polenv, altenv, count, amp.weight, n.before, n.after) {

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
        A <- (ifelse(n %% 2 == 0, 1, 0) * a * amp.weight)
    } else {
        D <- A <- n <- 0
    }   

    ## return the word group score and the polarity of the the polarized word
    list(x = (1 + (D + A)) * (p * (-1)^(2 + n)), y = p, z = targ)
}
 

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

    ## create environment to lookup amps, de-amps and negators
    alter <- alter_env(negators, amplifiers, deamplifiers)

    ## grab the polarized/alter words to search for spaces and insert into textvar
    posneg <- ls(polarity.frame)
    words <- c(posneg, ls(alter))

    ## create sentence lengths
    counts <- unlist(lapply(DF[, "text.var"], function(x) length(bag.o.words(x))))
    DF[, "text.var"] <- space_fill(text.var = strip(DF[, "text.var"], ...), 
        terms = words[grep("\\s", words)], sep = "~~")

    ## split into bag of words per sentence
    TV <- lapply(lapply(DF[, "text.var"], bag.o.words, ...), function(x) {
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
                n.before = n.before, n.after = n.after, amp.weight = amplifier.weight)
            output[[i]][j] <- theoutputs[[1]]
            pols[[i]][j] <- theoutputs[[2]]
            polwords[[i]][j] <- theoutputs[[3]]
        }
    }

    ## Construct sentence data.frame
    scores <- sapply(output, sum)/counts
    all <- data.frame(group.var =  DF[, "grouping"], wc = counts, polarity = scores)

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
    qweight <- ifelse(suppressWarnings(end_mark(all[, "text.var"])) %in% c("?", "*?"), 
        question.weight, 1)
    all[, "polarity"] <- qweight * all[, "polarity"]

    ## Create average polarity data.frame (group) from all data.frame
    sall <- split(all, all[, "group.var"])
    lall <- lapply(sall, function(x) {
        data.frame(total.words = sum(x[, "wc"]), ave.polarity = mean(x[, "polarity"]),
            sd.polarity = sd(x[, "polarity"]), 
            stan.mean.polarity = mean(x[, "polarity"])/sd(x[, "polarity"]))
    })
    group <- data.frame(group = names(lall), total.sentences = sapply(sall, nrow),
        do.call(rbind, lall), row.names = NULL)
    colnames(group)[1] <- colnames(all)[1] <- G
    o <- list(all = all, group = group, digits = digits)
    class(o) <- "polarity"
    return(o)
}

DATA[c(2, 10:11),4] <-c("it's not, it's dumb.", 'Shall we move on?  Really Good then.', 'Really Good then.')
is.dp <- qdap:::is.dp

dat <- sentSplit(DATA, text.var="state")
dat[13, 6] <- "I had very little fun."

(poldat <- with(dat, polarity(state, person)))
class(poldat[[2]])


plot(poldat)

p_load(ggplot2, gridExtra, scales, RColorBrewer)

print.polarity <- function(x, digits = NULL, ...) {
    cat("POLARITY BY GROUP\n=================\n")
    WD <- options()[["width"]]
    options(width=3000)
    y <- x$group
    if (is.null(digits)) {
        digits <- x$digits
    }
    y[, "ave.polarity"] <- round(y[, "ave.polarity"], digits = digits)
    y[, "sd.polarity"] <- round(y[, "sd.polarity"], digits = digits)
    y[, "stan.mean.polarity"] <- round(y[, "stan.mean.polarity"], digits = digits)
    print(y)
    options(width=WD)
}

plot.polarity <- function(x, bar.size = 5, low = "red", mid = "grey99", 
    high = "blue", ave.polarity.shape = "+", alpha = 1/4, shape = 19, 
    point.size = 2.5,  jitter = .1, nrow = NULL, na.rm = TRUE, ...){
    Polarity <- group <- ave.polarity <- unit <- NULL
    dat <- x[["group"]]
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
        gantt(dialogue, list(group, seq_along(group)), plot = FALSE)))
    if (is.null(nrow)) {
        leg <- FALSE
        nrow <- 1
    } else {
        leg <- TRUE
    }
    XX <- ggplot(dat2, aes(color = Polarity )) + 
        geom_segment(aes(x=start, xend=end, y=group, yend=group), 
            size=bar.size) +
        xlab("Duration (words)") + ylab(gsub("\\&", " & ", G)) +
        scale_colour_gradientn(colours = c(low, mid, high)) +
        theme_bw() + theme(legend.position="bottom") + 
        guides(colour = guide_colorbar(barwidth = 9, barheight = .75))
    YY <- ggplot(dat2, aes(y=group, x=Polarity, colour = group)) + 
        geom_point(data = dat, aes(x=ave.polarity), shape = ave.polarity.shape, 
            size = 6, show_guide=FALSE) +
        geom_point(alpha = alpha, shape = shape, 
            size = point.size, position = position_jitter(height = jitter)) +
        geom_point(data = dat, aes(x=ave.polarity), shape = 19, 
            size = 1.5, colour = "black", show_guide=FALSE) +
        ylab(gsub("\\&", " & ", G)) +
        scale_color_discrete(name= G) 
    if (leg) {
        YY <- YY + theme(plot.margin = unit(c(-.25, 1, 1, 1), "lines"), 
            legend.position="bottom")  +
            guides(col = guide_legend(nrow = nrow, byrow = TRUE, 
                override.aes = list(shape = shape, alpha = 1)))
    } else {
        YY <- YY + theme(plot.margin = unit(c(-.25, 1, 1, 1), "lines"), 
            legend.position="none")       
    } 
    grid.arrange(XX, YY, nrow = 2)
    invisible(list(p1 = XX, p2 = YY))
}

plot.polarity_group <- function(x, ...){

browser()
    G <- names(dat)[1]
    nms <- c("group", "dialogue", "word_count", "Polarity")
    names(dat)[c(1)] <-  nms[1]
    names(dat2)[c(1, 6, 2, 3)] <- nms
    dat2 <- data.frame(dat2, with(dat2, 
        gantt(dialogue, list(group, seq_along(group)), plot = FALSE)))
    invisible(list(p1 = XX, p2 = YY))
}


