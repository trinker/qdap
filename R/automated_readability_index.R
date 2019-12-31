#' Readability Measures
#' 
#' \code{automated_readability_index} - Apply Automated Readability Index to 
#' transcript(s) by zero or more grouping variable(s).
#' 
#' @param text.var The text variable.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one output for all text.  Also takes a single grouping variable or a list of 1 
#' or more grouping variables.
#' @param rm.incomplete logical.  If \code{TRUE} removes incomplete sentences 
#' from the analysis.
#' @param \ldots Other arguments passed to \code{\link[qdap]{end_inc}}.
#' @return Returns a list of 2 dataframes: (1) Counts and (2) Readability.  
#' Counts are the raw scores used to calculate readability score and can be
#' accessed via \code{\link[qdap]{counts}}.  Readability is the dataframe
#' with the selected readability statistic by grouping variable(s) and can be 
#' access via \code{\link[qdap]{scores}}.  The \code{\link[qdap]{fry}} function 
#' returns a graphic representation of the readability as the 
#' \code{\link[qdap]{scores}} returns the information for graphing but not a
#' readability score.
#' @rdname Readability
#' @section Warning: Many of the indices (e.g., Automated Readability Index) 
#' are derived from word difficulty (letters per word) and sentence difficulty 
#' (words per sentence).  If you have not run the sentSplit function on your 
#' data the results may not be accurate.
#' @section Fry: The \code{fry} function is based on Fry's formula that randomly 
#' samples 3 100 word length passages.  If a group(s) in does not contain 300+ 
#' words they will not be included in the output.
#' @references Coleman, M., & Liau, T. L. (1975). A computer readability formula 
#' designed for machine scoring. Journal of Applied Psychology, Vol. 60, 
#' pp. 283-284.
#' 
#' Fry, E. B. (1968). A readability formula that saves time. Journal of Reading,
#' 11(7), 513-516, 575-578.
#' 
#' Fry, E. B. (1969). The readability graph validated at primary levels. The Reading
#' Teacher, 22(6), 534-538.
#' 
#' Flesch R. (1948). A new readability yardstick. Journal of Applied Psychology. 
#' Vol. 32(3), pp. 221-233. doi: 10.1037/h0057532.
#' 
#' Gunning, T. G. (2003). Building Literacy in the Content Areas. Boston: Allyn 
#' & Bacon.
#' 
#' McLaughlin, G. H. (1969). SMOG Grading: A New Readability Formula. 
#' Journal of Reading, Vol. 12(8), pp. 639-646. 
#' 
#' Smith, E. A. & Senter, R. J. (1967) Automated readability index. 
#' Technical Report AMRLTR-66-220, University of Cincinnati, Cincinnati, Ohio.
#' @export
#' @importFrom qdapTools matrix2df 
#' @examples
#' \dontrun{
#' AR1 <- with(rajSPLIT, automated_readability_index(dialogue, list(person, act)))
#' ltruncdf(AR1,, 15)
#' scores(AR1)
#' counts(AR1)
#' plot(AR1)
#' plot(counts(AR1))
#' 
#' AR2 <- with(rajSPLIT, automated_readability_index(dialogue, list(sex, fam.aff)))
#' ltruncdf(AR2,, 15)
#' scores(AR2)
#' counts(AR2)
#' plot(AR2)
#' plot(counts(AR2))
#' 
#' AR3 <- with(rajSPLIT, automated_readability_index(dialogue, person))
#' ltruncdf(AR3,, 15)
#' scores(AR3)
#' head(counts(AR3))
#' plot(AR3)
#' plot(counts(AR3))
#' 
#' CL1 <- with(rajSPLIT, coleman_liau(dialogue, list(person, act)))
#' ltruncdf(CL1, 20)
#' head(counts(CL1))
#' plot(CL1)
#' 
#' CL2 <- with(rajSPLIT, coleman_liau(dialogue, list(sex, fam.aff)))
#' ltruncdf(CL2)
#' plot(counts(CL2))
#' 
#' (SM1 <- with(rajSPLIT, SMOG(dialogue, list(person, act))))
#' plot(counts(SM1))
#' plot(SM1)
#' 
#' (SM2 <- with(rajSPLIT, SMOG(dialogue, list(sex, fam.aff))))
#' 
#' (FL1 <- with(rajSPLIT, flesch_kincaid(dialogue, list(person, act))))
#' plot(scores(FL1))
#' plot(counts(FL1))
#' 
#' (FL2 <-  with(rajSPLIT, flesch_kincaid(dialogue, list(sex, fam.aff))))
#' plot(scores(FL2))
#' plot(counts(FL2))
#' 
#' FR1 <- with(rajSPLIT, fry(dialogue, list(sex, fam.aff)))
#' scores(FR1)
#' plot(scores(FR1))
#' counts(FR1)
#' plot(counts(FR1))
#' 
#' FR2 <- with(rajSPLIT, fry(dialogue, person))
#' scores(FR2)
#' plot(scores(FR2))
#' counts(FR2)
#' plot(counts(FR2))
#' 
#' FR3 <- with(pres_debates2012, fry(dialogue, list(time, person)))
#' colsplit2df(scores(FR3))
#' plot(scores(FR3), auto.label = FALSE)
#' counts(FR3)
#' plot(counts(FR3))
#' 
#' library(ggplot2)
#' ggplot(colsplit2df(counts(FR3)), aes(sent.per.100.wrds, 
#'     syllables.per.100.wrds)) +
#'     geom_point(aes(fill=person), shape=21, size=3) +
#'     facet_grid(person~time)
#'     
#' LW1 <- with(rajSPLIT, linsear_write(dialogue, list(person, act)))
#' plot(scores(LW1))
#' plot(counts(LW1))
#' 
#' LW2 <- with(rajSPLIT, linsear_write(dialogue, list(sex, fam.aff)))
#' plot(scores(LW2), method="lm")
#' plot(counts(LW2))
#' }
automated_readability_index <-
function(text.var, grouping.var = NULL, rm.incomplete = FALSE, ...) {
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
    text <- as.character(text.var)
    DF <- stats::na.omit(data.frame(group = grouping, text.var = text, 
        stringsAsFactors = FALSE))
    if (rm.incomplete) {
        DF <- end_inc(dataframe = DF, text.var = text.var, ...)
    }   
    if (is.dp(text.var = DF[, "text.var"])) {
        warning(paste0("\n  Some rows contain double punctuation.", 
            "  Suggested use of sentSplit function."))
    }  
    if (is.empty(DF[["text.var"]])) {   
        badrows <- which.empty(DF[["text.var"]])
        DF <- DF[-c(badrows), ]
        warning("The following rows contained empty sentences and were removed:\n",
             paste(badrows, collapse=", "))                  
    }        
    DF$word.count <- word_count(DF$text.var, missing = 0)
    i <- as.data.frame(table(DF$group))
    DF$group <- DF$group[ , drop=TRUE]
    DF$tot.n.sent <- 1:nrow(DF)
    DF <- DF[with(DF, order(group, DF$tot.n.sent)), ]
    DF$character.count <- character_count(DF$text.var)
    DF2 <- stats::aggregate(word.count ~ group, DF, sum)
    DF2$sentence.count <- as.data.frame(table(DF$group))$Freq
    DF2$character.count <- stats::aggregate(character.count ~ 
        group, DF, sum)$character.count 
    ari <- function(tse, tw, tc) 4.71*(tc/tw) + .5*(tw/tse) - 21.43
    DF2$Automated_Readability_Index <- with(DF2, 
        ari(tse = sentence.count, tc = character.count, tw = word.count))
    rownames(DF) <- NULL
    names(DF2)[1] <- G
    o <- list(Counts = DF, Readability = DF2)
    class(o) <- c("automated_readability_index", class(DF2))
    o
}


#' Prints an automated_readability_index Object
#' 
#' Prints an automated_readability_index object.
#' 
#' @param x The automated_readability_index object.
#' @param digits The number of digits displayed if \code{values} is \code{TRUE}.
#' @param \ldots ignored
#' @method print automated_readability_index 
#' @export
print.automated_readability_index <- function(x, digits = 3, ...) {
    print(scores(x), digits = digits, ...)
}



#' Coleman Liau Readability
#' 
#' \code{coleman_liau} - Apply Coleman Liau Index to transcript(s) by zero or 
#' more grouping variable(s).
#' 
#' @rdname Readability
#' @export
coleman_liau <-
function(text.var, grouping.var = NULL, rm.incomplete = FALSE, ...) {
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
    text <- as.character(text.var)
    DF <- stats::na.omit(data.frame(group = grouping, text.var = text, 
        stringsAsFactors = FALSE))
    if (rm.incomplete) {
        DF <- end_inc(dataframe = DF, text.var = text.var, ...)
    }
    if (is.dp(text.var = DF[, "text.var"])) {
        warning(paste0("\n  Some rows contain double punctuation.", 
            "  Suggested use of sentSplit function."))
    }    
    if (is.empty(DF[["text.var"]])) {   
        badrows <- which.empty(DF[["text.var"]])
        DF <- DF[-c(badrows), ]
        warning("The following rows contained empty sentences and were removed:\n",
             paste(badrows, collapse=", "))                  
    }          
    DF$word.count <- word_count(DF$text.var, missing = 0, digit.remove = FALSE)
    i <- as.data.frame(table(DF$group))
    DF$group <- DF$group[ , drop=TRUE]
    DF$tot.n.sent <- 1:nrow(DF)
    DF <- DF[with(DF, order(group, DF$tot.n.sent)), ]
    DF$character.count <- character_count(DF$text.var, 
        apostrophe.remove = FALSE, digit.remove = FALSE)
    DF2 <- stats::aggregate(word.count ~ group, DF, sum)
    DF2$sentence.count <- as.data.frame(table(DF$group))$Freq
    DF2$character.count <- stats::aggregate(character.count ~ 
        group, DF, sum)$character.count 
    clf <- function(tse, tw, tc) (.0588*((100*tc)/tw)) - 
      (.296*((100*tse)/tw) ) - 15.8
    DF2$Coleman_Liau <- with(DF2, clf(tse = sentence.count, 
        tc = character.count, tw = word.count))
    names(DF2)[1] <- G
    rownames(DF) <- NULL    
    o <- list(Counts = DF, Readability = DF2)
    class(o) <- "coleman_liau"
    o
}


#' SMOG Readability
#' 
#' \code{SMOG} - Apply SMOG Readability to transcript(s) by zero or more grouping variable(s).
#' 
#' @rdname Readability
#' @param output A character vector character string indicating output type. 
#' One of "valid" (default and congruent with McLaughlin's intent) or "all". 
#' @export
SMOG <-
function(text.var, grouping.var = NULL, output = "valid", 
    rm.incomplete = FALSE, ...) {
    group <- NULL
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
    text <- as.character(text.var)
    DF <- stats::na.omit(data.frame(group = grouping, text.var = text, 
        stringsAsFactors = FALSE))
    if (rm.incomplete) {
        DF <- end_inc(dataframe = DF, text.var = text.var, ...)
    }
    if (is.dp(text.var = DF[, "text.var"])) {
        warning(paste0("\n  Some rows contain double punctuation.", 
            "  Suggested use of sentSplit function."))
    }   
    if (is.empty(DF[["text.var"]])) {   
        badrows <- which.empty(DF[["text.var"]])
        DF <- DF[-c(badrows), ]
        warning("The following rows contained empty sentences and were removed:\n",
             paste(badrows, collapse=", "))                  
    }          
    DF$word.count <- word_count(DF$text.var, missing = 0)
    i <- as.data.frame(table(DF$group))
    if (output == "valid") {
        DF <- subset(DF, group%in%as.character(i[i$Freq > 29, ][,'Var1']))
        if (nrow(DF) == 0) {
            stop("Not enough sentences for valid output.")
        }
    }
    DF$group <- DF$group[ , drop=TRUE]
    DF$tot.n.sent <- 1:nrow(DF)
    DF <- DF[with(DF, order(group, DF$tot.n.sent)), ]
    DF$polysyllable.count <- polysyllable_sum(DF$text.var)
    DF2 <- stats::aggregate(word.count ~ group, DF, sum)
    DF2$sentence.count <- as.data.frame(table(DF$group))$Freq
    DF2$polysyllable.count <- stats::aggregate(polysyllable.count ~ 
        group, DF, sum)$polysyllable.count   
    smog <- function(tse, tpsy) 1.043 * sqrt(tpsy * (30/tse)) + 3.1291 
    DF2$SMOG <- with(DF2, smog(tse = sentence.count, 
        tpsy = polysyllable.count))
    DF2$validity <- ifelse(DF2$sentence.count < 30, "n < 30", "valid")
    if(output == "valid") DF2$validity <- NULL
    names(DF2)[1] <- G
    o <- list(Counts = DF, Readability = DF2)
    class(o) <- "SMOG"
    o
}


#' Flesch-Kincaid Readability
#' 
#' \code{flesch_kincaid} - Flesch-Kincaid Readability to transcript(s) by zero or more 
#' grouping variable(s).
#' 
#' @rdname Readability
#' @export
flesch_kincaid <-
function(text.var, grouping.var = NULL, rm.incomplete = FALSE, ...) {
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
    text <- as.character(text.var)
    DF <- stats::na.omit(data.frame(group = grouping, text.var = text, 
        stringsAsFactors = FALSE))
    if (rm.incomplete) {
        DF <- end_inc(dataframe = DF, text.var = text.var, ...)
    }
    if (is.dp(text.var = DF[, "text.var"])) {
        warning(paste0("\n  Some rows contain double punctuation.", 
            "  Suggested use of sentSplit function."))
    }
    if (is.empty(DF[["text.var"]])) {   
        badrows <- which.empty(DF[["text.var"]])
        DF <- DF[-c(badrows), ]
        warning("The following rows contained empty sentences and were removed:\n",
             paste(badrows, collapse=", "))                  
    }          
    DF$word.count <- word_count(DF$text.var, missing = 0)
    DF$syllable.count <- syllable_sum(DF$text.var)
    DF$tot.n.sent <- 1:nrow(DF)
    DF <- DF[with(DF, order(group, DF$tot.n.sent)), ]
    DF2 <- stats::aggregate(word.count ~ group, DF, sum)
    DF2$sentence.count <- as.data.frame(table(DF$group))$Freq
    DF2$syllable.count <- stats::aggregate(syllable.count ~ group, DF, 
        sum)$syllable.count  
    fkgl <- function(tw, tse, tsy) 0.39 * (tw/tse) + 11.8 * (tsy/tw) - 15.59
    fre <- function(tw, tse, tsy) 206.835 - 1.015 * (tw/tse) - 84.6 * (tsy/tw)
    DF2$FK_grd.lvl <- with(DF2, fkgl(tw = word.count, 
        tse = sentence.count, tsy = syllable.count))
    DF2$FK_read.ease <- with(DF2, fre(tw = word.count, 
        tse = sentence.count, tsy = syllable.count))
    names(DF2)[1] <- G
    o <- list(Counts = DF, Readability = DF2)
    class(o) <- "flesch_kincaid"
    o
}


#' Fry Readability
#' 
#' \code{fry} - Apply Fry Readability to transcript(s) by zero or more 
#' grouping variable(s).
#' 
#' @rdname Readability
#' @param auto.label  logical.  If \code{TRUE} labels automatically added.  If 
#' \code{FALSE} the user clicks interactively.
#' @param grid logical.  If \code{TRUE} a micro grid is displayed, similar to 
#' Fry's original depiction, though this may make visualizing more difficult.
#' @param div.col The color of the grade level division lines.
#' @param plot logical.  If \code{TRUE} a graph is plotted corresponding to Fry's 
#' graphic representation.
#' @export
fry <-
function(text.var, grouping.var = NULL, rm.incomplete = FALSE, 
    auto.label = TRUE, grid = FALSE, div.col = "grey85", plot = TRUE, ...) {
    read.gr <- group <- NULL  
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
    text <- as.character(text.var)
    DF <- stats::na.omit(data.frame(group = grouping, text.var = text, 
        stringsAsFactors = FALSE))
    if (rm.incomplete) {
        DF <- end_inc(dataframe = DF, text.var = text.var, ...)
    }
    if (is.dp(text.var = DF[, "text.var"])) {
        warning(paste0("\n  Some rows contain double punctuation.", 
            "  Suggested use of sentSplit function."))
    }   
    if (is.empty(DF[["text.var"]])) {   
        badrows <- which.empty(DF[["text.var"]])
        DF <- DF[-c(badrows), ]
        warning("The following rows contained empty sentences and were removed:\n",
             paste(badrows, collapse=", "))                  
    }          
    DF$word.count <- word_count(DF$text.var, missing = 0)
    DF$tot.n.sent <- 1:nrow(DF)
    DF <- DF[with(DF, order(group, DF$tot.n.sent)), ]
    DF$read.gr <- unlist(by(DF$word.count, DF$group, partition))
    LIST <- names(which(tapply(DF$word.count, DF$group, function(x) {
        max(cumsum(x))
    }) >= 300))
    DF2 <- subset(DF, group %in% LIST & read.gr != "NA")
    DF2$group <- DF2$group[, drop = TRUE]
    DF2$sub <- with(DF2, paste(group, read.gr, sep = "_"))
    DF2b <- DF2[order(DF2$sub), ]
    DF2b$cumsum.gr <- unlist(tapply(DF2$word.count, DF2$sub, cumsum))
    DF2 <- DF2b[order(DF2b$group, DF2b$tot.n.sent), ]
    DF2$wo.last <- DF2$cumsum.gr - DF2$word.count
    DF2$hun.word <- 100 - DF2$wo.last
    DF2$frac.sent <- ifelse(DF2$hun.word/DF2$word.count > 1, 
        1, round(DF2$hun.word/DF2$word.count, digits = 3))   
    DF3b <- unique(data.frame(sub = DF2$sub, group = DF2$group))
    DF3 <- data.frame(sub = DF2$sub, group = DF2$group, 
        text.var = DF2$text.var, frac.sent = DF2$frac.sent)
    CHOICES <- tapply(as.character(DF3b$sub), DF3b$group, function(x) {
            if (length(x) < 3) return(NULL)
            sample(x, 3, replace = FALSE)
        }
    )
    ## drop any less than 300 words
    less_than_300 <- sapply(CHOICES, is.null)
    if (any(less_than_300)) {
        warning("The following groups contain < 300 words and were dropped:\n  ",
            paste(names(CHOICES[less_than_300]), collapse="', '")
        )
    }
    CHOICES <- CHOICES[!less_than_300]

    CHOICES <- as.character(unlist(CHOICES))
    DF4 <- DF3[which(DF3$sub %in% CHOICES), ]
    DF4$sub <- DF4$sub[, drop = TRUE]
    FUN <- function(x) paste(as.character(unlist(x)), collapse = " ")
    DF5 <- stats::aggregate(text.var ~ sub + group, DF4, FUN)
    sent.per.100 <- as.data.frame(tapply(DF2$frac.sent, DF2$sub, sum))
    names(sent.per.100) <- "x"
    DF5$sent.per.100 <- sent.per.100[as.character(DF5$sub), "x"] 
    hun.grab <- function(x) paste(unblanker(unlist(word_split(
        reducer(unlist(strip(x))))))[1:100], collapse = " ")
    DF5$syll.count <- syllable_sum(lapply(DF5$text.var, hun.grab))
    DF6 <- stats::aggregate(syll.count ~ group, DF5, mean)
    DF6$ave.sent.per.100 <- stats::aggregate(sent.per.100 ~ group, DF5, mean)[, 2]
    names(DF6) <- c(G, "ave.syll.per.100", "ave.sent.per.100")
    colnames(DF5)[c(2, 4, 5)] <- c(colnames(DF6)[1], "sent.per.100.wrds",
        "syllables.per.100.wrds")
    DF5 <- DF5[, -1]
    
    out <- list(Counts = DF5[, c(1, 3:4, 2)], Readability = DF6)
    attributes(out) <- list(
            class = "fry",
            names = names(out),
            plot_instructions = list(auto.label = auto.label, 
                grid = grid, div.col = div.col, plot = plot)
    )
    out
}


#' Linsear Write Readability
#' 
#' \code{linsear_write} - Apply Linsear Write Readability to transcript(s) by 
#' zero or more grouping variable(s).
#' @rdname Readability
#' @export 
linsear_write <-
function(text.var, grouping.var = NULL, rm.incomplete = FALSE, ...) {
    read.gr <- group <- NULL
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
    text <- as.character(text.var)
    DF <- stats::na.omit(data.frame(group = grouping, text.var = text, 
        stringsAsFactors = FALSE))
    if (rm.incomplete) {
        DF <- end_inc(dataframe = DF, text.var = text.var, ...)
    }
    if (is.dp(text.var = DF[, "text.var"])) {
        warning(paste0("\n  Some rows contain double punctuation.", 
            "  Suggested use of sentSplit function."))
    }   
    if (is.empty(DF[["text.var"]])) {   
        badrows <- which.empty(DF[["text.var"]])
        DF <- DF[-c(badrows), ]
        warning("The following rows contained empty sentences and were removed:\n",
             paste(badrows, collapse=", "))                  
    }          
    DF$word.count <- word_count(DF$text.var)
    DF$tot.n.sent <- 1:nrow(DF)
    DF <- DF[with(DF, order(group, DF$tot.n.sent)), ]
    DF$read.gr <- unlist(by(DF$word.count, DF$group, partition))
    LIST <- names(which(tapply(DF$word.count, DF$group, function(x) {
            max(cumsum(x))
        }
    ) >= 100))
    DF2 <- subset(DF, group %in% LIST & read.gr != "NA")
    DF2$group <- DF2$group[, drop = TRUE]
    DF2$sub <- with(DF2, paste(group, read.gr, sep = "_"))
    DF2b <- DF2[order(DF2$sub), ]
    DF2b$cumsum.gr <- unlist(tapply(DF2$word.count, DF2$sub, cumsum))
    DF2 <- DF2b[order(DF2b$group, DF2b$tot.n.sent), ]
    DF2$wo.last <- DF2$cumsum.gr - DF2$word.count
    DF2$hun.word <- 100 - DF2$wo.last
    DF2$frac.sent <- ifelse(DF2$hun.word/DF2$word.count > 1, 1, 
        DF2$hun.word/DF2$word.count)  
    DF3b <- unique(data.frame(sub = DF2$sub, group = DF2$group))
    DF3 <- data.frame(sub = DF2$sub, group = DF2$group, text.var = 
        DF2$text.var, frac.sent = DF2$frac.sent)
    CHOICES <- tapply(as.character(DF3b$sub), DF3b$group, function(x) {
            sample(x, 1, replace = FALSE)
        }
    )
    CHOICES <- as.character(unlist(CHOICES))
    DF4 <- DF3[which(DF3$sub %in% CHOICES), ]
    DF4$sub <- DF4$sub[, drop = TRUE]
    FUN <- function(x) paste(as.character(unlist(x)), collapse = " ")
    DF5 <- stats::aggregate(text.var ~ sub + group, DF4, FUN)
    sent.per.100 <- as.data.frame(tapply(DF2$frac.sent, DF2$sub, sum))
    names(sent.per.100) <- "x"
    DF5$sent.per.100 <- sent.per.100[as.character(DF5$sub), "x"]
    hun.grab <- function(x) paste(unblanker(unlist(word_split(reducer(
        unlist(strip(x))))))[1:100], collapse = " ")
    DF5$SYL.LIST <- lapply(DF5$text.var, function(x) unlist(syllable_count(
        hun.grab(x))$syllables))
    DF5$hard_easy_sum <- unlist(lapply(DF5$SYL.LIST, function(x) {
          sum(ifelse(x >= 3, 3, 1))
    }))
    DF5$HE_tsent_ratio <- unlist(DF5$hard_easy_sum)/DF5$sent.per.100
    DF5$Linsear_Write <- ifelse(DF5$sent.per.100 > 20, 
        DF5$HE_tsent_ratio/2, 
        (DF5$HE_tsent_ratio - 2)/2)
    DF5 <- DF5[, c(2, 4, 6, 8)]
    names(DF5) <- c(G, "sent.per.100", "hard_easy_sum", "Linsear_Write")
    names(DF)[1] <- names(DF5)[1]
    o <- list(Counts = DF, Readability = DF5)
    class(o) <- "linsear_write"
    o
}


#' Readability Measures
#' 
#' \code{scores.automated_readability_index} - View scores from \code{\link[qdap]{automated_readability_index}}.
#' 
#' automated_readability_index Method for scores
#' @param x The automated_readability_index object.
#' @param \ldots ignored
#' @export
#' @method scores automated_readability_index
scores.automated_readability_index <- function(x, ...) {

    out <- x[["Readability"]]
    attributes(out) <- list(
            class = c("readability_score", class(out)),
            type = "automated_readability_index_scores",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}

#' Prints a readability_score Object
#' 
#' Prints a readability_score object.
#' 
#' @param x The readability_score object.
#' @param digits The number of digits displayed if \code{values} is \code{TRUE}.
#' @param \ldots ignored
#' @method print readability_score
#' @export
print.readability_score <-
    function(x, digits = 3, ...) {
    class(x) <- "data.frame"
    WD <- options()[["width"]]
    options(width=3000)
    x[, -1] <- round(x[, -1], digits = digits)
    print(x)
    options(width=WD)
}


#' Readability Measures
#' 
#' \code{counts.automated_readability_index} - View counts from \code{\link[qdap]{automated_readability_index}}.
#' 
#' @param x The automated_readability_index object.
#' @param \ldots ignored
#' automated_readability_index Method for counts.
#' @export
#' @method counts automated_readability_index
counts.automated_readability_index <- function(x, ...) {
    out <- x[["Counts"]]
    attributes(out) <- list(
            class = c("readability_count", class(out)),
            type = "automated_readability_index_count",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}



#' Prints a readability_count Object
#' 
#' Prints a readability_count object.
#' 
#' @param x The readability_count object.
#' @param digits The number of digits displayed.
#' @param \ldots ignored
#' @method print readability_count
#' @export
print.readability_count <-
    function(x, digits = 3, ...) {
    class(x) <- "data.frame"
    WD <- options()[["width"]]
    options(width=3000)
    print(x)
    options(width=WD)
}




#' Plots a readability_count Object
#' 
#' Plots a readability_count object.
#' 
#' @param x The readability_count object.
#' @param alpha The alpha level to use for points.
#' @param \ldots ignored
#' @importFrom ggplot2 ggplot aes geom_point theme theme_minimal ylab xlab scale_size_continuous element_blank guides 
#' @importFrom scales alpha
#' @method plot readability_count
#' @export
plot.readability_count <- function(x, alpha = .3, ...){ 

    type <- attributes(x)[["type"]]

    switch(type,
        automated_readability_index_count = {
            word_counts(DF = x, x = "word.count", y = "character.count", 
                z = NULL, g = "group", alpha = alpha)
        },
        coleman_liau_count = {
            word_counts(DF = x, x = "word.count", y = "character.count", 
                z = NULL, g = "group", alpha = alpha)
        },
        SMOG_count = {
            word_counts(DF = x, x = "word.count", y = "polysyllable.count", 
                z = NULL, g = "group", alpha = alpha)
        },
        flesch_kincaid_count = {
            word_counts(DF = x, x = "word.count", y = "syllable.count", 
                z = NULL, g = "group", alpha = alpha)
        },
        fry_count = {
            
            word_counts3(DF = x, x = "sent.per.100.wrds", 
                y = "syllables.per.100.wrds", 
                z = NULL, g = colnames(x)[1])
        },        
        stop("Not a plotable readability count")
    )

}

#' Plots a readability_score Object
#' 
#' Plots a readability_score object.
#' 
#' @param x The readability_score object.
#' @param alpha The alpha level to be used for the points.
#' @param auto.label  logical.  For plotting \code{\link[qdap]{fry}} only, if 
#' \code{TRUE} labels automatically added.  If \code{FALSE} the user clicks 
#' interactively.
#' @param grid logical.  For plotting \code{\link[qdap]{fry}} only, if 
#' \code{TRUE} a micro grid is displayed similar to Fry's original depiction, 
#' though this makes visualizing more difficult.
#' @param div.col For plotting \code{\link[qdap]{fry}} only, the color of the 
#' grade level division lines.
#' @param \ldots ignored
#' @importFrom ggplot2 ggplot aes geom_smooth facet_wrap guide_colorbar geom_point theme ggplotGrob theme_bw ylab xlab scale_fill_gradient element_blank guides 
#' @importFrom gridExtra grid.arrange
#' @importFrom scales alpha
#' @method plot readability_score
#' @export
plot.readability_score <- function(x, alpha = .3, auto.label, 
    grid, div.col, ...){ 

    type <- attributes(x)[["type"]]

    switch(type,
        automated_readability_index_scores = {
            plot_automated_readability_index(x)
        },
        coleman_liau_scores = {
            plot_coleman_liau(x)
        },
        SMOG_scores = {
            plot_SMOG(x)
        },
        flesch_kincaid_scores = {
            plot_flesch_kincaid(x)
        },
        fry_scores = {
            ## Plotting attributes
            PA <- attributes(x)[["plot_instructions"]]
        
            if (missing(auto.label)) {
                auto.label <- PA[["auto.label"]]
            }
            if (missing(grid)) {
                grid <- PA[["grid"]]
            }
            if (missing(div.col)) {
                div.col <- PA[["div.col"]]
            }    
            plot_fry(x, auto.label = auto.label, grid = grid, 
                    div.col = div.col)
        },
        stop("Not a plotable readability score")
    )

}


plot_automated_readability_index <- function(x) {

    character.count <- sentence.count <- Coleman_Liau <- word.count <- grvar <- 
        Readability_count <-NULL

    names(x)[ncol(x)] <- "Readability_count"
    x  <- x[order(x[, "Readability_count"]), ]
    x[, 1] <- factor(x[, 1], levels = x[, 1])
    forlater <-  names(x)[1]
    names(x)[1] <- "grvar"
    plot1 <- ggplot2::ggplot(x, ggplot2::aes(fill = word.count, x = sentence.count, 
            y = character.count)) + 
        ggplot2::geom_point(size=2.75, shape=21, colour="grey65") +
        ggplot2::theme_bw() + 
        ggplot2::scale_fill_gradient(high="red", low="pink", name="Word\nCount") +
        ggplot2::ylab("Character Count") + 
        ggplot2::xlab("Sentence Count") + 
        ggplot2::theme(panel.grid = ggplot2::element_blank(),
            legend.position = "bottom") +
        ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = 10, barheight = .5)) 
    
    plot2 <- ggplot2::ggplot(x, ggplot2::aes(y = grvar, x = Readability_count)) +
        ggplot2::geom_point(size=2) + 
        ggplot2::ylab(gsub("&", " & ", forlater)) + 
        ggplot2::xlab("Automated Readability Index")

    gridExtra::grid.arrange(plot2, plot1, ncol=2)
}

#' Plots a automated_readability_index Object
#' 
#' Plots a automated_readability_index object.
#' 
#' @param x The readability_score object.
#' @param \ldots ignored
#' @importFrom ggplot2 ggplot aes guide_colorbar geom_point theme ggplotGrob theme_bw ylab xlab scale_fill_gradient element_blank guides 
#' @importFrom gridExtra grid.arrange
#' @export
#' @method plot automated_readability_index
plot.automated_readability_index <- function(x, ...){ 

    plot.readability_score(scores(x))

}

## Generic helper plotting function for counts
word_counts <- function(DF, x, y, z = NULL, g, alpha = .3) {

    NMS <- rename(c(x, y, z))

    AES <- ggplot2::aes(x=x, y=y, color=g)
    if (!is.null(z)) {
        AES[["size"]] <- g
    }
    MAX <- max(DF[, y])

    plot <- ggplot2::ggplot(data.frame(x=DF[, x], y=DF[, y], z=DF[, z], g=DF[, g]), AES) + 
        ggplot2::geom_point(alpha=alpha) + facet_wrap(~g) + 
        ggplot2::geom_smooth() + ylim(0, MAX + MAX*.05) +
        ggplot2::guides(color=FALSE) +
        ggplot2::ylab(NMS[2]) + 
        ggplot2::xlab(NMS[1]) +
        ggplot2::theme_minimal() + 
        ggplot2::theme(panel.grid = ggplot2::element_blank(),
            panel.spacing = unit(1, "lines")) +
        ggplot2::annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
        ggplot2::annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)


    if (!is.null(z)) {
        plot <- plot + ggplot2::scale_size_continuous(names=gsub(" ", "\n", NMS[3]))
    }

    suppressMessages(suppressWarnings(print(plot)))

}

word_counts2 <- function(DF, x, y, z = NULL, g, alpha = .3) {

    NMS <- rename(c(x, y, z))

    AES <- ggplot2::aes(x=x, y=y, color=g)
    if (!is.null(z)) {
        AES[["size"]] <- g
    }
    MAX <- max(DF[, y])

    plot <- ggplot2::ggplot(data.frame(x=DF[, x], y=DF[, y], z=DF[, z], g=DF[, g]), AES) + 
        ggplot2::geom_point(alpha=alpha) + 
        ggplot2::facet_wrap(~g) + 
        ggplot2::geom_smooth() + ylim(0, MAX + MAX*.05) +
        ggplot2::guides(color=FALSE) +
        ggplot2::ylab(NMS[2]) + 
        ggplot2::xlab(NMS[1]) +
        ggplot2::theme_minimal() + 
        ggplot2::theme(panel.grid = ggplot2::element_blank(),
            panel.spacing = unit(1, "lines")) +
        ggplot2::annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
        ggplot2::annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)


    if (!is.null(z)) {
        plot <- plot + ggplot2::scale_size_continuous(names=gsub(" ", "\n", NMS[3]))
    }

    invisible(suppressMessages(suppressWarnings(plot)))

}

word_counts3 <- function(DF, x, y, z = NULL, g) {

    NMS <- rename(c(x, y, z))

    AES <- ggplot2::aes(x=x, y=y, fill=g)
    if (!is.null(z)) {
        AES[["size"]] <- g
    }
    MAX <- max(DF[, y])

    avs <- qdapTools::matrix2df(do.call(rbind, lapply(split(DF[, 2:3], 
        DF[,1]), colMeans)), "g")

    plot <- ggplot2::ggplot(data.frame(x=DF[, x], y=DF[, y], z=DF[, z], g=DF[, g]), AES) + 
        ggplot2::geom_point(shape=21, size=3) + 
        ggplot2::facet_wrap(~g) + 
        ggplot2::guides(color=FALSE, fill=FALSE) +
        ggplot2::ylab(NMS[2]) + 
        ggplot2::xlab(NMS[1]) +
        ggplot2::geom_point(data=avs, shape=3, colour="black", ggplot2::aes_string(x=x, y=y, group="g")) 

    if (!is.null(z)) {
        plot <- plot + ggplot2::scale_size_continuous(names=gsub(" ", "\n", NMS[3]))
    }

    suppressMessages(suppressWarnings(print(plot)))

}


## Generic helper funciton for word_counts plotting to rename axi labels
rename <- function(x) {

    input <- c("word.count",  "character.count", "polysyllable.count",
        "syllable.count", "polysyl2word.ratio", "sent.per.100.wrds",
        "syllables.per.100.wrds")
    output <- c("Words Per Sentence", "Characters Per Sentence", 
        "Polysyllables Per Sentence", "Syllables Per Sentence",
        "Polysyllables Per Word", "Sentences Per 100 Words", 
        "Syllables Per 100 Words")
    mgsub(input, output, x)

}

#' Readability Measures
#' 
#' \code{scores.SMOG} - View scores from \code{\link[qdap]{SMOG}}.
#' 
#' SMOG Method for scores
#' @param x The SMOG object.
#' @param \ldots ignored
#' @export
#' @method scores SMOG
scores.SMOG <- function(x, ...) {

    out <- x[["Readability"]]
    attributes(out) <- list(
            class = c("readability_score", class(out)),
            type = "SMOG_scores",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}


#' Prints an SMOG Object
#' 
#' Prints an SMOG object.
#' 
#' @param x The SMOG object.
#' @param digits The number of digits displayed if \code{values} is \code{TRUE}.
#' @param \ldots ignored
#' @method print SMOG
#' @export
print.SMOG <- function(x, digits = 3, ...) {
    print(scores(x), digits = digits, ...)
}


#' Readability Measures
#' 
#' \code{counts.SMOG} - View counts from \code{\link[qdap]{SMOG}}.
#' 
#' SMOG Method for counts.
#' @param x The SMOG object.
#' @param \ldots ignored
#' @export
#' @method counts SMOG
counts.SMOG <- function(x, ...) {
    out <- x[["Counts"]]
    attributes(out) <- list(
            class = c("readability_count", class(out)),
            type = "SMOG_count",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}


#' Plots a SMOG Object
#' 
#' Plots a SMOG object.
#' 
#' @param x The readability_score object.
#' @param \ldots ignored
#' @importFrom ggplot2 ggplot aes geom_point theme ylab xlab scale_size_continuous  element_rect
#' @importFrom gridExtra grid.arrange
#' @export
#' @method plot SMOG
plot.SMOG <- function(x, ...){ 

    plot.readability_score(scores(x))

}


plot_SMOG <- function(x, ...){ 

    sentence.count <- SMOG <- word.count <- grvar <- NULL
    
    x  <- x[order(x[, "SMOG"]), ]
    x[, 1] <- factor(x[, 1], levels = x[, 1])
    forlater <-  names(x)[1]
    names(x)[1] <- "grvar"

    ggplot2::ggplot(x, ggplot2::aes(y = grvar, x = SMOG)) +
        ggplot2::geom_point(aes(size = word.count), color="grey75") +
        ggplot2::geom_point(size=2.2) + 
        ggplot2::ylab(gsub("&", " & ", forlater)) + 
        ggplot2::xlab("SMOG") +
        ggplot2::scale_size_continuous(name="  Word\n  Count") + 
        ggplot2::theme(legend.key = ggplot2::element_rect(fill = NA))

}

#' Readability Measures
#' 
#' \code{scores.flesch_kincaid} - View scores from \code{\link[qdap]{flesch_kincaid}}.
#' 
#' flesch_kincaid Method for scores
#' @param x The flesch_kincaid object.
#' @param \ldots ignored
#' @export
#' @method scores flesch_kincaid
scores.flesch_kincaid <- function(x, ...) {

    out <- x[["Readability"]]
    attributes(out) <- list(
            class = c("readability_score", class(out)),
            type = "flesch_kincaid_scores",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}


#' Prints an flesch_kincaid Object
#' 
#' Prints an flesch_kincaid object.
#' 
#' @param x The flesch_kincaid object.
#' @param digits The number of digits displayed if \code{values} is \code{TRUE}.
#' @param \ldots ignored
#' @method print flesch_kincaid
#' @export
print.flesch_kincaid <- function(x, digits = 3, ...) {
    print(scores(x), digits = digits, ...)
}


#' Readability Measures
#' 
#' \code{counts.flesch_kincaid} - View counts from \code{\link[qdap]{flesch_kincaid}}.
#' 
#' flesch_kincaid Method for counts.
#' @param x The flesch_kincaid object.
#' @param \ldots ignored
#' @export
#' @method counts flesch_kincaid
counts.flesch_kincaid <- function(x, ...) {
    out <- x[["Counts"]]
    attributes(out) <- list(
            class = c("readability_count", class(out)),
            type = "flesch_kincaid_count",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}


#' Plots a flesch_kincaid Object
#' 
#' Plots a flesch_kincaid object.
#' 
#' @param x The readability_score object.
#' @param \ldots ignored
#' @importFrom ggplot2 ggplot aes guide_colorbar geom_point theme ggplotGrob theme_bw ylab xlab scale_fill_gradient element_blank guides 
#' @importFrom gridExtra grid.arrange
#' @export
#' @method plot flesch_kincaid
plot.flesch_kincaid <- function(x, ...){ 

    plot.readability_score(scores(x))

}


plot_flesch_kincaid <- function(x, ...){ 

    character.count <- sentence.count <- FK_grd.lvl <- FK_read.ease <- 
        word.count <- grvar <- value <- syllable.count <- NULL

    x  <- x[order(x[, "FK_grd.lvl"]), ]
    x[, 1] <- factor(x[, 1], levels = x[, 1])
    forlater <-  names(x)[1]
    names(x)[1] <- "grvar"
    plot1 <- ggplot2::ggplot(x, ggplot2::aes(x = sentence.count, y = syllable.count)) + 
        ggplot2::geom_point(size=2.75, alpha=.3) +
        ggplot2::theme_bw() + 
        ggplot2::ylab("Syllable Count") + 
        ggplot2::xlab("Sentence Count") + 
        ggplot2::theme(panel.grid = ggplot2::element_blank())
    
    x2 <- melt(x[, c(1, 5:6)], id="grvar")
    x2[, "variable"] <- mgsub(c("FK_grd.lvl", "FK_read.ease"),
        c("Flesch Kincaid Grade Level", 
        "Flesch Kincaid Reading Ease"), x2[, "variable"])

    plot2 <- ggplot2::ggplot(x2, ggplot2::aes(y = grvar, x = value)) +
        ggplot2::geom_point(size=2) + 
        ggplot2::ylab(gsub("&", " & ", forlater)) + 
        ggplot2::xlab("score") +
        ggplot2::facet_grid(.~variable, scales="free_x") 

    gridExtra::grid.arrange(plot2, plot1, ncol=2, widths= grid::unit(c(.67, .33), "native"))
}

#' Readability Measures
#' 
#' \code{scores.linsear_write} - View scores from \code{\link[qdap]{linsear_write}}.
#' 
#' linsear_write Method for scores
#' @param x The linsear_write object.
#' @param \ldots ignored
#' @export
#' @method scores linsear_write
scores.linsear_write <- function(x, ...) {

    out <- x[["Readability"]]
    attributes(out) <- list(
            class = c("linsear_write_scores", class(out)),
            type = "linsear_write_scores",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}

#' Prints a linsear_write_scores Object
#' 
#' Prints a linsear_write_scores object.
#' 
#' @param x The linsear_write_scores object.
#' @param digits The number of digits displayed.
#' @param \ldots ignored
#' @method print linsear_write_scores
#' @export
print.linsear_write_scores <-
    function(x, digits = 3, ...) {
    class(x) <- "data.frame"
    WD <- options()[["width"]]
    options(width=3000)
    print(x)
    options(width=WD)
}


#' Prints an linsear_write Object
#' 
#' Prints an linsear_write object.
#' 
#' @param x The linsear_write object.
#' @param digits The number of digits displayed if \code{values} is \code{TRUE}.
#' @param \ldots ignored
#' @method print linsear_write
#' @export
print.linsear_write <- function(x, digits = 3, ...) {
    print(scores(x), digits = digits, ...)
}


#' Readability Measures
#' 
#' \code{counts.linsear_write} - View counts from \code{\link[qdap]{linsear_write}}.
#' 
#' linsear_write Method for counts.
#' @param x The linsear_write object.
#' @param \ldots ignored
#' @export
#' @method counts linsear_write
counts.linsear_write <- function(x, ...) {
    out <- x[["Counts"]]
    rownames(out) <- NULL
    
    attributes(out) <- list(
            class = c("linsear_write_count", class(out)),
            type = "linsear_write_count",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}

#' Prints a linsear_write_count Object
#' 
#' Prints a linsear_write_count object.
#' 
#' @param x The linsear_write_count object.
#' @param digits The number of digits displayed.
#' @param \ldots ignored
#' @method print linsear_write_count
#' @export
print.linsear_write_count <-
    function(x, digits = 3, ...) {
    class(x) <- "data.frame"
    WD <- options()[["width"]]
    options(width=3000)
    print(x)
    options(width=WD)
}


#' Plots a linsear_write_count Object
#' 
#' Plots a linsear_write_count object.
#' 
#' @param x The linsear_write_count object.
#' @param \ldots ignored
#' @importFrom ggplot2 scale_colour_manual ylab
#' @export
#' @method plot linsear_write_count
plot.linsear_write_count <- function(x, ...){ 

    x[, "Selected"] <- ifelse(is.na(x[, "read.gr"]), "Used", "Not Used")
    nms1 <- paste(sapply(unlist(strsplit(names(x)[1], "\\&")), 
        Caps), collapse = " & ")
    names(x)[1] <- "group"
    x[, "group"] <- paste(1:nrow(x), x[, "group"], sep="|||")

    dat <- gantt(x[, "text.var"], x[, "group"])
    dat[, "group"] <- sapply(strsplit(as.character(dat[, "group"]), "\\|\\|\\|"), "[", 2)
    dat[, "Selected"] <- ifelse(is.na(x[, "read.gr"]), "Used", "Not Used")

    plot1 <- gantt_wrap(dat, "group", fill.var = "Selected", plot = FALSE)

    plot1 + 
        ggplot2::scale_colour_manual(values=c("grey90", "red")) +
        ggplot2::ylab(nms1) 
}



#' Plots a linsear_write Object
#' 
#' Plots a linsear_write object.
#' 
#' @param x The readability_score object.
#' @param alpha The alpha level for the points and smooth fill in the 
#' scatterplot (length one or two; if two 1-points, 2-smooth fill).
#' @param \ldots ignored
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth theme ggplotGrob theme_bw ylab xlab
#' @importFrom gridExtra grid.arrange
#' @importFrom scales alpha
#' @export
#' @method plot linsear_write
plot.linsear_write <- function(x, alpha = .4, ...){ 

    graphics::plot(scores(x, alpha = alpha, ...))

}

#' Plots a linsear_write_scores Object
#' 
#' Plots a linsear_write_scores object.
#' 
#' @param x The readability_score object.
#' @param alpha The alpha level for the points and smooth fill in the 
#' scatterplot (length one or two; if two 1-points, 2-smooth fill).
#' @param \ldots Other arguments passed to \code{\link[ggplot2]{geom_smooth}}.
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth theme ggplotGrob theme_bw ylab xlab
#' @importFrom gridExtra grid.arrange
#' @importFrom scales alpha
#' @export
#' @method plot linsear_write_scores
plot.linsear_write_scores <- function(x, alpha = c(.4, .08), ...){ 

    hard_easy_sum <- Linsear_Write <- NULL
    
    character.count <- sent.per.100 <- Linsear_write <- 
        hard_easy_sumt <- grvar <- NULL
  
    x  <- x[order(x[, "Linsear_Write"]), ]
    x[, 1] <- factor(x[, 1], levels = x[, 1])
    forlater <-  names(x)[1]
    names(x)[1] <- "grvar"

    plot1 <- ggplot2::ggplot(x, ggplot2::aes(y = hard_easy_sum, 
            x = sent.per.100)) + 
        ggplot2::geom_smooth(fill="blue", colour="darkblue", size=1, 
            alpha = utils::tail(alpha, 1), ...) +
        ggplot2::geom_point(size=2.75, alpha = utils::head(alpha, 1)) +
        ggplot2::theme_bw() + 
        ggplot2::xlab("Sentence Per 100 Words") + 
        ggplot2::ylab("3 x Hard Words + Easy Words") + 
        ggplot2::theme(panel.grid = ggplot2::element_blank()) 
    
    plot2 <- ggplot2::ggplot(x, ggplot2::aes(y = grvar, x = Linsear_Write)) +
        ggplot2::geom_point(size=2) + 
        ggplot2::ylab(gsub("&", " & ", forlater)) + 
        ggplot2::xlab("Linsear Write")

    gridExtra::grid.arrange(plot2, plot1, ncol=2)
}

####=============Template================

#' Readability Measures
#' 
#' \code{scores.coleman_liau} - View scores from \code{\link[qdap]{coleman_liau}}.
#' 
#' coleman_liau Method for scores
#' @param x The coleman_liau object.
#' @param \ldots ignored
#' @export
#' @method scores coleman_liau
scores.coleman_liau <- function(x, ...) {

    out <- x[["Readability"]]
    attributes(out) <- list(
            class = c("readability_score", class(out)),
            type = "coleman_liau_scores",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}


#' Prints an coleman_liau Object
#' 
#' Prints an coleman_liau object.
#' 
#' @param x The coleman_liau object.
#' @param digits The number of digits displayed if \code{values} is \code{TRUE}.
#' @param \ldots ignored
#' @method print coleman_liau
#' @export
print.coleman_liau <- function(x, digits = 3, ...) {
    print(scores(x), digits = digits, ...)
}


#' Readability Measures
#' 
#' \code{counts.coleman_liau} - View counts from \code{\link[qdap]{coleman_liau}}.
#' 
#' coleman_liau Method for counts.
#' @param x The coleman_liau object.
#' @param \ldots ignored
#' @export
#' @method counts coleman_liau
counts.coleman_liau <- function(x, ...) {
    out <- x[["Counts"]]
    attributes(out) <- list(
            class = c("readability_count", class(out)),
            type = "coleman_liau_count",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}


#' Plots a coleman_liau Object
#' 
#' Plots a coleman_liau object.
#' 
#' @param x The readability_score object.
#' @param \ldots ignored
#' @importFrom ggplot2 ggplot aes guide_colorbar geom_point theme ggplotGrob theme_bw ylab xlab scale_fill_gradient element_blank guides 
#' @importFrom gridExtra grid.arrange
#' @export
#' @method plot coleman_liau
plot.coleman_liau <- function(x, ...){ 

    plot.readability_score(scores(x))

}


plot_coleman_liau <- function(x, ...){ 

    character.count <- sentence.count <- Coleman_Liau <- word.count <- grvar <- 
        NULL
    
    x  <- x[order(x[, "Coleman_Liau"]), ]
    x[, 1] <- factor(x[, 1], levels = x[, 1])
    forlater <-  names(x)[1]
    names(x)[1] <- "grvar"
    plot1 <- ggplot2::ggplot(x, ggplot2::aes(fill = word.count, x = sentence.count, 
            y = character.count)) + 
        ggplot2::geom_point(size=2.75, shape=21, colour="grey65") +
        ggplot2::theme_bw() + 
        ggplot2::scale_fill_gradient(high="red", low="pink", name="Word\nCount") +
        ggplot2::ylab("Character Count") + 
        ggplot2::xlab("Sentence Count") + 
        ggplot2::theme(panel.grid = ggplot2::element_blank(),
            legend.position = "bottom") +
        ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = 10, barheight = .5)) 
    
    plot2 <- ggplot2::ggplot(x, ggplot2::aes(y = grvar, x = Coleman_Liau)) +
        ggplot2::geom_point(size=2) + 
        ggplot2::ylab(gsub("&", " & ", forlater)) + 
        ggplot2::xlab("Coleman Liau")

    gridExtra::grid.arrange(plot2, plot1, ncol=2)
}

###==============End of template==========

plot_fry <- function(x, auto.label = TRUE, grid = FALSE, div.col = "grey85", ...) {

    ## Setting up the graph
    suppressWarnings(graphics::plot(1, 1, xlim = c(108, 182), ylim = c(2, 
        25), axes = FALSE, type = "n", 
        xlab = "Average number of syllables per 100 words", 
        ylab = "Average number of sentences per 100 words", 
        main = "Fry Graph for Estimating Reading Ages (grade level)", 
        xaxs = "i", yaxs = "i"))
    graphics::axis(1, at = 108:182, labels = TRUE)
    graphics::axis(1, at = seq(108, 182, by = 4), tcl = -1.1, labels = FALSE)
    graphics::axis(2, at = 2:25, labels = TRUE)
    graphics::axis(4, at = 2:25, labels = TRUE)
    if (grid){
        grid(nx = 74, ny = 46, lty = "solid", col = "gold")
        grid(nx = 37, ny = 23, lty = "solid", col = "gray65")
    }
    graphics::box()
    graphics::segments(108, 9.97, 133.9, 25, lwd = 2, col = div.col)
    graphics::segments(108, 7.7, 142.1, 25, lwd = 2, col = div.col)
    graphics::segments(108, 6.4, 147.8, 25, lwd = 2, col = div.col)
    graphics::segments(108, 5.61, 155, 25, lwd = 2, col = div.col)
    graphics::segments(108, 5.1, 160, 25, lwd = 2, col = div.col)
    graphics::segments(108, 3.8, 160.8, 25, lwd = 2, col = div.col)
    graphics::segments(111.5, 2, 167.5, 25, lwd = 2, col = div.col)
    graphics::segments(123, 2, 169, 25, lwd = 2, col = div.col)
    graphics::segments(136.3, 2, 169, 25, lwd = 2, col = div.col)
    graphics::segments(143.3, 2, 173.5, 25, lwd = 2, col = div.col)
    graphics::segments(148.15, 2, 178.2, 25, lwd = 2, col = div.col)
    graphics::segments(155.6, 2, 178.5, 25, lwd = 2, col = div.col)
    graphics::segments(161.9, 2, 178.4, 25, lwd = 2, col = div.col)
    graphics::segments(168.2, 2, 178.2, 25, lwd = 2, col = div.col)
    graphics::segments(173.9, 2, 178.7, 25, lwd = 2, col = div.col)
    graphics::segments(179.1, 2, 181.25, 25, lwd = 2, col = div.col)
    x1 <- c(108, 108, 128)
    y1 <- c(4.2, 2, 2)
    graphics::polygon(x1, y1, col = "darkblue", border = "darkblue")
    x2 <- c(143, 145, 150, 152, 155, 158, 162, 166, 172, 180, 181, 182, 182)
    y2 <- c(25, 18.3, 14.3, 12.5, 11.1, 10, 9.1, 8.3, 7.7, 7.55, 7.5, 7.5, 25)
    graphics::polygon(x2, y2, col = "darkblue", border = "darkblue")  
    graphics::text(120, 22.2, 1, col = "darkgreen", cex = 1.25)
    graphics::text(124, 17.9, 2, col = "darkgreen", cex = 1.25)
    graphics::text(126, 15.9, 3, col = "darkgreen", cex = 1.25)
    graphics::text(127, 14.4, 4, col = "darkgreen", cex = 1.25)
    graphics::text(128, 13.3, 5, col = "darkgreen", cex = 1.25)
    graphics::text(110.7, 5.5, 6, col = "darkgreen", cex = 1.25)
    graphics::text(115, 4.88, 7, col = "darkgreen", cex = 1.25)
    graphics::text(121, 3.78, 8, col = "darkgreen", cex = 1.25)
    graphics::text(132, 3.5, 9, col = "darkgreen", cex = 1.25)
    graphics::text(142, 3.5, 10, col = "darkgreen", cex = 1.25)
    graphics::text(147.7, 3.5, 11, col = "darkgreen", cex = 1.25)
    graphics::text(153.7, 3.5, 12, col = "darkgreen", cex = 1.25)
    graphics::text(160, 3.5, 13, col = "darkgreen", cex = 1.25)
    graphics::text(166.4, 3.5, 14, col = "darkgreen", cex = 1.25)
    graphics::text(171, 3.5, 15, col = "darkgreen", cex = 1.25)
    graphics::text(176.7, 3.5, 16, col = "darkgreen", cex = 1.25)
    graphics::text(180.7, 3.5, 17, col = "darkgreen", cex = 1.25)

    ## Actually plotting the points
    graphics::points(x[, "ave.syll.per.100"], x[, "ave.sent.per.100"], pch = 19, cex = 1, 
        col = "red")
    graphics::points(x[, "ave.syll.per.100"], x[, "ave.sent.per.100"], pch = 3, cex = 3, 
        col = "black")  
    if (!auto.label) {
        graphics::identify(x[, "ave.syll.per.100"], x[, "ave.sent.per.100"], x[, 1])
    } else {
        graphics::text(x[, "ave.syll.per.100"], x[, "ave.sent.per.100"], labels = x[, 1], 
            adj = c(1, -0.5))
    }  
}

#' Readability Measures
#' 
#' \code{scores.fry} - View scores from \code{\link[qdap]{fry}}.
#' 
#' fry Method for scores
#' @param x The fry object.
#' @param \ldots ignored
#' @export
#' @method scores fry
scores.fry <- function(x, ...) {

    out <- x[["Readability"]]
    attributes(out) <- list(
            class = c("readability_score", class(out)),
            type = "fry_scores",
            names = colnames(out),
            row.names = rownames(out),
            plot_instructions = attributes(x)[["plot_instructions"]]
    )
    out
}


#' Prints an fry Object
#' 
#' Prints an fry object.
#' 
#' @param x The fry object.
#' @param digits The number of digits displayed if \code{values} is \code{TRUE}.
#' @param auto.label  logical.  If \code{TRUE} labels automatically added.  If 
#' \code{FALSE} the user clicks interactively.
#' @param grid logical.  If \code{TRUE} a micro grid is displayed similar to 
#' Fry's original depiction, though this makes visualizing more difficult.
#' @param div.col The color of the grade level division lines.
#' @param plot logical.  If \code{TRUE} a graph is plotted corresponding to Fry's 
#' graphic representation.
#' @param \ldots ignored
#' @method print fry
#' @export
print.fry <- function(x, digits = 3, auto.label, 
    grid, div.col, plot, ...) {

    print(scores(x), digits = digits, ...)

    ## Plotting attributes
    PA <- attributes(x)[["plot_instructions"]]

    if (missing(auto.label)) {
        auto.label <- PA[["auto.label"]]
    }
    if (missing(grid)) {
        grid <- PA[["grid"]]
    }
    if (missing(div.col)) {
        div.col <- PA[["div.col"]]
    }
    if (missing(plot)) {
        plot <- PA[["plot"]]
    }

    if (plot) {
        plot_fry(scores(x), auto.label = auto.label, grid = grid, 
            div.col = div.col)
    }

}


#' Readability Measures
#' 
#' \code{counts.fry} - View counts from \code{\link[qdap]{fry}}.
#' 
#' fry Method for counts.
#' @param x The fry object.
#' @param \ldots ignored
#' @export
#' @method counts fry
counts.fry <- function(x, ...) {
    out <- x[["Counts"]]
    attributes(out) <- list(
            class = c("readability_count", class(out)),
            type = "fry_count",
            names = colnames(out),
            row.names = rownames(out)
    )
    out
}


