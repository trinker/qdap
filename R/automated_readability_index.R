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
#' access via \code{\link[qdap]{counts}} (\code{\link[qdap]{fry}} does not 
#' contain scores).  The \code{frey} function returns a graphic representation of 
#' the readability.
#' @rdname Readability
#' @section Warning: Many of the indices (e.g., Automated Readability Index) 
#' are derived from word difficulty (letters per word) and sentence difficulty 
#' (words per sentence).  If you have not run the sentSplit function on your 
#' data the results may not be accurate.
#' @references Coleman, M., & Liau, T. L. (1975). A computer readability formula 
#' designed for machine scoring. Journal of Applied Psychology, Vol. 60, 
#' pp. 283-284.
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
#' Senter, R. J., & Smith, E. A.. (1967) Automated readability index. 
#' Technical Report AMRLTR-66-220, University of Cincinnati, Cincinnati, Ohio.
#' @keywords readability, Automated Readability Index, Coleman Liau, SMOG, 
#' Flesch-Kincaid, Fry, Linsear Write
#' @export
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
#' counts(AR3)
#' plot(AR3)
#' plot(counts(AR3))
#' 
#' CL1 <- with(rajSPLIT, coleman_liau(dialogue, list(person, act)))
#' head(CL1)
#' CL2 <- with(rajSPLIT, coleman_liau(dialogue, list(sex, fam.aff)))
#' head(CL2)
#' plot(CL1)
#' 
#' SM1 <- with(rajSPLIT, SMOG(dialogue, list(person, act)))
#' head(SM1)
#' SM2 <- with(rajSPLIT, SMOG(dialogue, list(sex, fam.aff)))
#' head(SM2)
#' 
#' FL1 <- with(rajSPLIT, flesch_kincaid(dialogue, list(person, act)))
#' head(FL1)
#' FL2 <-  with(rajSPLIT, flesch_kincaid(dialogue, list(sex, fam.aff)))
#' head(FL2)
#' 
#' FR <- with(rajSPLIT, fry(dialogue, list(sex, fam.aff)))
#' htruncdf(FR$SENTENCES_USED)
#' head(FR$SENTENCE_AVERAGES)
#' 
#' LW1 <- with(rajSPLIT, linsear_write(dialogue, list(person, act)))
#' head(LW1)
#' LW2 <- with(rajSPLIT, linsear_write(dialogue, list(sex, fam.aff)))
#' head(LW2)
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
    DF <- na.omit(data.frame(group = grouping, text.var = text, 
        stringsAsFactors = FALSE))
    if (rm.incomplete) {
        DF <- end_inc(dataframe = DF, text.var = text.var, ...)
    }   
    if (is.dp(text.var = DF[, "text.var"])) {
        warning(paste0("\n  Some rows contain double punctuation.", 
            "  Suggested use of sentSplit function."))
    }    
    DF$word.count <- word_count(DF$text.var, missing = 0)
    i <- as.data.frame(table(DF$group))
    DF$group <- DF$group[ , drop=TRUE]
    DF$tot.n.sent <- 1:nrow(DF)
    DF <- DF[with(DF, order(group, DF$tot.n.sent)), ]
    DF$character.count <- character_count(DF$text.var)
    DF2 <- aggregate(word.count ~ group, DF, sum)
    DF2$sentence.count <- as.data.frame(table(DF$group))$Freq
    DF2$character.count <- aggregate(character.count ~ 
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
#' @S3method print automated_readability_index 
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
    DF <- na.omit(data.frame(group = grouping, text.var = text, 
        stringsAsFactors = FALSE))
    if (rm.incomplete) {
        DF <- end_inc(dataframe = DF, text.var = text.var, ...)
    }
    if (is.dp(text.var = DF[, "text.var"])) {
        warning(paste0("\n  Some rows contain double punctuation.", 
            "  Suggested use of sentSplit function."))
    }    
    DF$word.count <- word_count(DF$text.var, missing = 0, digit.remove = FALSE)
    i <- as.data.frame(table(DF$group))
    DF$group <- DF$group[ , drop=TRUE]
    DF$tot.n.sent <- 1:nrow(DF)
    DF <- DF[with(DF, order(group, DF$tot.n.sent)), ]
    DF$character.count <- character_count(DF$text.var, 
        apostrophe.remove = FALSE, digit.remove = FALSE)
    DF2 <- aggregate(word.count ~ group, DF, sum)
    DF2$sentence.count <- as.data.frame(table(DF$group))$Freq
    DF2$character.count <- aggregate(character.count ~ 
        group, DF, sum)$character.count 
    clf <- function(tse, tw, tc) (.0588*((100*tc)/tw)) - 
      (.296*((100*tse)/tw) ) - 15.8
    DF2$Coleman_Liau <- round(with(DF2, clf(tse = sentence.count, 
        tc = character.count, tw = word.count)), digits = 1)
    names(DF2)[1] <- G
    o <- list(Counts = DF, Readability = DF2)
    class(o) <- c("coleman_liau", class(DF2))
    o
}

#' Plots a coleman_liauObject
#' 
#' Plots a coleman_liauobject.
#' 
#' @param x The coleman_liauobject.
#' @param \ldots ignored
#' @importFrom ggplot2 ggplot aes guide_colorbar geom_point theme ggplotGrob theme_bw ylab xlab scale_fill_gradient element_blank guides 
#' @importFrom gridExtra grid.arrange
#' @method plot coleman_liau
#' @export
plot.coleman_liau<- function(x, ...){ 

    character.count <- sentence.count <- Coleman_Liau <- word.count <- grvar <- 
        NULL
    
    x  <- x[order(x[, "Coleman_Liau"]), ]
    x[, 1] <- factor(x[, 1], levels = x[, 1])
    forlater <-  names(x)[1]
    names(x)[1] <- "grvar"
    plot1 <- ggplot(x, aes(fill = word.count, x = sentence.count, 
        y = character.count)) + geom_point(size=2.75, shape=21, colour="grey65") +
        theme_bw() + 
        scale_fill_gradient(high="red", low="pink", name="Word\nCount") +
        ylab("Character Count") + 
        xlab("Sentence Count") + 
        theme(panel.grid = element_blank(),
            legend.position = "bottom") +
        guides(fill = guide_colorbar(barwidth = 10, barheight = .5)) 
    plot2 <- ggplot(x, aes(y = grvar, x = Coleman_Liau)) +
        geom_point(size=2) + 
        ylab(gsub("&", " & ", forlater)) + 
        xlab("Coleman Liau")

    grid.arrange(plot2, plot1, ncol=2)
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
    DF <- na.omit(data.frame(group = grouping, text.var = text, 
        stringsAsFactors = FALSE))
    if (rm.incomplete) {
        DF <- end_inc(dataframe = DF, text.var = text.var, ...)
    }
    if (is.dp(text.var = DF[, "text.var"])) {
        warning(paste0("\n  Some rows contain double punctuation.", 
            "  Suggested use of sentSplit function."))
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
    DF2 <- aggregate(word.count ~ group, DF, sum)
    DF2$sentence.count <- as.data.frame(table(DF$group))$Freq
    DF2$polysyllable.count <- aggregate(polysyllable.count ~ 
        group, DF, sum)$polysyllable.count   
    smog <- function(tse, tpsy) 1.043 * sqrt(tpsy * (30/tse)) + 3.1291 
    DF2$SMOG <- round(with(DF2, smog(tse = sentence.count, 
        tpsy = polysyllable.count)), digits = 1)
    DF2$validity <- ifelse(DF2$sentence.count < 30, "n < 30", "valid")
    if(output == "valid") DF2$validity <- NULL
    names(DF2)[1] <- G
    DF2
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
    DF <- na.omit(data.frame(group = grouping, text.var = text, 
        stringsAsFactors = FALSE))
    if (rm.incomplete) {
        DF <- end_inc(dataframe = DF, text.var = text.var, ...)
    }
    if (is.dp(text.var = DF[, "text.var"])) {
        warning(paste0("\n  Some rows contain double punctuation.", 
            "  Suggested use of sentSplit function."))
    }
    DF$word.count <- word_count(DF$text.var, missing = 0)
    DF$syllable.count <- syllable_sum(DF$text.var)
    DF$tot.n.sent <- 1:nrow(DF)
    DF <- DF[with(DF, order(group, DF$tot.n.sent)), ]
    DF2 <- aggregate(word.count ~ group, DF, sum)
    DF2$sentence.count <- as.data.frame(table(DF$group))$Freq
    DF2$syllable.count <- aggregate(syllable.count ~ group, DF, 
        sum)$syllable.count  
    fkgl <- function(tw, tse, tsy) 0.39 * (tw/tse) + 11.8 * (tsy/tw) - 15.59
    fre <- function(tw, tse, tsy) 206.835 - 1.015 * (tw/tse) - 84.6 * (tsy/tw)
    DF2$FK_grd.lvl <- round(with(DF2, fkgl(tw = word.count, 
        tse = sentence.count, tsy = syllable.count)), digits = 1)
    DF2$FK_read.ease <- round(with(DF2, fre(tw = word.count, 
        tse = sentence.count, tsy = syllable.count)), digits = 3)
    names(DF2)[1] <- G
    DF2
}


#' Fry Readability
#' 
#' \code{fry} - Apply Fry Readability to transcript(s) by zero or more 
#' grouping variable(s).
#' 
#' @rdname Readability
#' @param auto.label  logical.  If \code{TRUE} labels automatically added.  If 
#' \code{FALSE} the user clicks interactively.
#' @param grid logical.  If \code{TRUE} a micro grid is dsicplayed similar to 
#' Fry's original depiction, though this makes visualizing more difficult.
#' @param div.col The color of the grade level division lines.
#' @export
fry <-
function(text.var, grouping.var = NULL, auto.label = TRUE, 
    rm.incomplete = FALSE, grid = FALSE, div.col = "grey85", ...) {
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
    DF <- na.omit(data.frame(group = grouping, text.var = text, 
        stringsAsFactors = FALSE))
    if (rm.incomplete) {
        DF <- end_inc(dataframe = DF, text.var = text.var, ...)
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
            sample(x, 3, replace = FALSE)
        }
    )
    CHOICES <- as.character(unlist(CHOICES))
    DF4 <- DF3[which(DF3$sub %in% CHOICES), ]
    DF4$sub <- DF4$sub[, drop = TRUE]
    FUN <- function(x) paste(as.character(unlist(x)), collapse = " ")
    DF5 <- aggregate(text.var ~ sub + group, DF4, FUN)
    sent.per.100 <- as.data.frame(tapply(DF2$frac.sent, DF2$sub, sum))
    names(sent.per.100) <- "x"
    DF5$sent.per.100 <- sent.per.100[as.character(DF5$sub), "x"] 
    hun.grab <- function(x) paste(unblanker(unlist(word_split(
        reducer(unlist(strip(x))))))[1:100], collapse = " ")
    DF5$syll.count <- syllable_sum(lapply(DF5$text.var, hun.grab))
    DF6 <- aggregate(syll.count ~ group, DF5, mean)
    DF6$ave.sent.per.100 <- aggregate(sent.per.100 ~ group, DF5, mean)[, 2]
    suppressWarnings(plot(1, 1, xlim = c(108, 182), ylim = c(2, 
        25), axes = FALSE, type = "n", 
        xlab = "Average number of syllables per 100 words", 
        ylab = "Average number of sentences per 100 words", 
        main = "Fry Graph for Estimating Reading Ages (grade level)", 
        xaxs = "i", yaxs = "i"))
    axis(1, at = 108:182, labels = TRUE)
    axis(1, at = seq(108, 182, by = 4), tcl = -1.1, labels = FALSE)
    axis(2, at = 2:25, labels = TRUE)
    axis(4, at = 2:25, labels = TRUE)
    if (grid){
        grid(nx = 74, ny = 46, lty = "solid", col = "gold")
        grid(nx = 37, ny = 23, lty = "solid", col = "gray65")
    }
    box()
    segments(108, 9.97, 133.9, 25, lwd = 2, col = div.col)
    segments(108, 7.7, 142.1, 25, lwd = 2, col = div.col)
    segments(108, 6.4, 147.8, 25, lwd = 2, col = div.col)
    segments(108, 5.61, 155, 25, lwd = 2, col = div.col)
    segments(108, 5.1, 160, 25, lwd = 2, col = div.col)
    segments(108, 3.8, 160.8, 25, lwd = 2, col = div.col)
    segments(111.5, 2, 167.5, 25, lwd = 2, col = div.col)
    segments(123, 2, 169, 25, lwd = 2, col = div.col)
    segments(136.3, 2, 169, 25, lwd = 2, col = div.col)
    segments(143.3, 2, 173.5, 25, lwd = 2, col = div.col)
    segments(148.15, 2, 178.2, 25, lwd = 2, col = div.col)
    segments(155.6, 2, 178.5, 25, lwd = 2, col = div.col)
    segments(161.9, 2, 178.4, 25, lwd = 2, col = div.col)
    segments(168.2, 2, 178.2, 25, lwd = 2, col = div.col)
    segments(173.9, 2, 178.7, 25, lwd = 2, col = div.col)
    segments(179.1, 2, 181.25, 25, lwd = 2, col = div.col)
    x1 <- c(108, 108, 128)
    y1 <- c(4.2, 2, 2)
    polygon(x1, y1, col = "darkblue", border = "darkblue")
    x2 <- c(143, 145, 150, 152, 155, 158, 162, 166, 172, 180, 181, 182, 182)
    y2 <- c(25, 18.3, 14.3, 12.5, 11.1, 10, 9.1, 8.3, 7.7, 7.55, 7.5, 7.5, 25)
    polygon(x2, y2, col = "darkblue", border = "darkblue")  
    text(120, 22.2, 1, col = "darkgreen", cex = 1.25)
    text(124, 17.9, 2, col = "darkgreen", cex = 1.25)
    text(126, 15.9, 3, col = "darkgreen", cex = 1.25)
    text(127, 14.4, 4, col = "darkgreen", cex = 1.25)
    text(128, 13.3, 5, col = "darkgreen", cex = 1.25)
    text(110.7, 5.5, 6, col = "darkgreen", cex = 1.25)
    text(115, 4.88, 7, col = "darkgreen", cex = 1.25)
    text(121, 3.78, 8, col = "darkgreen", cex = 1.25)
    text(132, 3.5, 9, col = "darkgreen", cex = 1.25)
    text(142, 3.5, 10, col = "darkgreen", cex = 1.25)
    text(147.7, 3.5, 11, col = "darkgreen", cex = 1.25)
    text(153.7, 3.5, 12, col = "darkgreen", cex = 1.25)
    text(160, 3.5, 13, col = "darkgreen", cex = 1.25)
    text(166.4, 3.5, 14, col = "darkgreen", cex = 1.25)
    text(171, 3.5, 15, col = "darkgreen", cex = 1.25)
    text(176.7, 3.5, 16, col = "darkgreen", cex = 1.25)
    text(180.7, 3.5, 17, col = "darkgreen", cex = 1.25)
    with(DF6, points(syll.count, ave.sent.per.100, pch = 19, cex = 1, 
        col = "red"))
    with(DF6, points(syll.count, ave.sent.per.100, pch = 3, cex = 3, 
        col = "black"))   
    if (!auto.label) {
        with(DF6, identify(syll.count, ave.sent.per.100, group))
    } else {
        with(DF6, text(syll.count, ave.sent.per.100, labels = group, 
            adj = c(1, -0.5)))
    }      
    names(DF6) <- c(G, "ave.syll.per.100", "ave.sent.per.100")
    invisible(list(SENTENCES_USED = DF5, SENTENCE_AVERAGES = DF6))
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
    DF <- na.omit(data.frame(group = grouping, text.var = text, 
        stringsAsFactors = FALSE))
    if (rm.incomplete) {
        DF <- end_inc(dataframe = DF, text.var = text.var, ...)
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
        round(DF2$hun.word/DF2$word.count, digits = 3))  
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
    DF5 <- aggregate(text.var ~ sub + group, DF4, FUN)
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
        round(DF5$HE_tsent_ratio/2, digits = 2), 
        round((DF5$HE_tsent_ratio - 2)/2, digits = 2))
    DF5 <- DF5[, c(2, 4, 6, 8)]
    names(DF5) <- c(G, "sent.per.100", "hard_easy_sum", "Linsear_Write")
    DF5
}

#' Readability Measures
#' 
#' \code{scores} - Access the readability scores from a readability function output.
#' 
#' @rdname Readability
#' @export
#' @return \code{scores} - Returns a data.frame of the class "readability_score".
scores <-
function(x, ...){
    UseMethod("scores")
}

#' Readability Measures
#' 
#' \code{scores.automated_readability_index} - View scores from \code{\link[qdap]{automated_readability_index}}.
#' 
#' automated_readability_index Method for scores
#' @rdname Readability
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
#' @S3method print readability_score
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
#' \code{count} - Access the readability count from a readability function output.
#' 
#' @param x A readability object (output from a readability function).
#' @rdname Readability
#' @export
#' @return \code{count} - Returns a data.frame of the class "readability_count".
counts <-
function(x, ...){
    UseMethod("counts")
}

#' Readability Measures
#' 
#' \code{counts.automated_readability_index} - View counts from \code{\link[qdap]{automated_readability_index}}.
#' 
#' automated_readability_index Method for counts.
#' @rdname Readability
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
#' @S3method print readability_count
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
        stop("Not a plotable readability count")
    )

}

#' Plots a readability_score Object
#' 
#' Plots a readability_score object.
#' 
#' @param x The readability_score object.
#' @param alpha The alpha level to be used for the points.
#' @param \ldots ignored
#' @importFrom ggplot2 ggplot aes geom_smooth facet_wrap guide_colorbar geom_point theme ggplotGrob theme_bw ylab xlab scale_fill_gradient element_blank guides 
#' @importFrom gridExtra grid.arrange
#' @importFrom scales alpha
#' @method plot readability_score
#' @export
plot.readability_score <- function(x, alpha = .3, ...){ 

    type <- attributes(x)[["type"]]

    switch(type,
        automated_readability_index_scores = {
            plot_automated_readability_index(x)
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
    plot1 <- ggplot(x, aes(fill = word.count, x = sentence.count, 
        y = character.count)) + geom_point(size=2.75, shape=21, colour="grey65") +
        theme_bw() + 
        scale_fill_gradient(high="red", low="pink", name="Word\nCount") +
        ylab("Character Count") + 
        xlab("Sentence Count") + 
        theme(panel.grid = element_blank(),
            legend.position = "bottom") +
        guides(fill = guide_colorbar(barwidth = 10, barheight = .5)) 
    plot2 <- ggplot(x, aes(y = grvar, x = Readability_count)) +
        geom_point(size=2) + 
        ylab(gsub("&", " & ", forlater)) + 
        xlab("Automated Readability Index")

    grid.arrange(plot2, plot1, ncol=2)
}

#' Plots a automated_readability_index Object
#' 
#' Plots a automated_readability_index object.
#' 
#' @param x The readability_score object.
#' @param \ldots ignored
#' @importFrom ggplot2 ggplot aes guide_colorbar geom_point theme ggplotGrob theme_bw ylab xlab scale_fill_gradient element_blank guides 
#' @importFrom gridExtra grid.arrange
#' @method plot automated_readability_index
#' @export
plot.automated_readability_index <- function(x, ...){ 

    plot.readability_score(scores(x))

}

## Generic helper plotting function for counts
word_counts <- function(DF, x, y, z = NULL, g, alpha = .3) {


    NMS <- rename(c(x, y, z))

    AES <- aes(x=x, y=y, color=g)
    if (!is.null(z)) {
        AES[["size"]] <- g
    }
    MAX <- max(DF[, y])

    plot <- ggplot(data.frame(x=DF[, x], y=DF[, y], z=DF[, z], g=DF[, g]), AES) + 
        geom_point(alpha=alpha) + facet_wrap(~g) + 
        geom_smooth() + ylim(0, MAX + MAX*.05) +
        guides(color=FALSE) +
        ylab(NMS[2]) + xlab(NMS[1]) +
        theme_minimal() + 
        theme(panel.grid = element_blank(),
            panel.margin = unit(1, "lines")) +
        ggplot2::annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
        ggplot2::annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)


    if (!is.null(z)) {
        plot <- plot + scale_size_continuous(names=gsub(" ", "\n", NMS[3]))
    }

    suppressMessages(suppressWarnings(print(plot)))

}


## Generic helper funciton for word_counts plotting to rename axi labels
rename <- function(x) {

    input <- c("word.count",  "character.count")
    output <- c("Word Count", "Character Count")
    mgsub(input, output, x)

}