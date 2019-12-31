#' Rank Frequency Plot
#' 
#' \code{rank_freq_mplot} - Plot a faceted word rank versus frequencies by 
#' grouping variable(s).
#' 
#' @param text.var The text variable.        
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.  
#' @param ncol integer value indicating the number of columns in the facet wrap.
#' @param jitter Amount of horizontal jitter to add to the points.
#' @param log.freq logical.  If \code{TRUE} plots the frequencies in the natural 
#' log scale.
#' @param log.rank logical.  If \code{TRUE} plots the ranks in the natural log 
#' scale.
#' @param hap.col Color of the hapax legomenon points.
#' @param dis.col Color of the dis legomenon points.
#' @param alpha Transparency level of points (ranges between 0 and 1).
#' @param shape An integer specifying the symbol used to plot the points.
#' @param title Optional plot title.
#' @param digits Integer; number of decimal places to round.  
#' @param plot logical.  If \code{TRUE} provides a rank frequency plot.
#' @param words A vector of words.
#' @param frequencies A vector of frequencies corresponding to the words 
#' argument.
#' @param title.ext The title extension that extends: "Rank-Frequency Plot ..."
#' @param jitter.ammount Amount of horizontal jitter to add to the points.
#' @param log.scale logical.  If \code{TRUE} plots the rank and frequency as a 
#' log scale.
#' @return Returns a rank-frequency plot and a list of three dataframes:
#' \item{WORD_COUNTS}{The word frequencies supplied to 
#' \code{\link[qdap]{rank_freq_plot}} or created by 
#' \code{\link[qdap]{rank_freq_mplot}}.}
#' \item{RANK_AND_FREQUENCY_STATS}{A dataframe of rank and frequencies for the 
#' words used in the text.}
#' \item{LEGOMENA_STATS}{A dataframe displaying the percent hapax legomena and 
#' percent dis legomena of the text.}
#' @note \code{rank_freq_mplot} utilizes the ggplot2 package, whereas, 
#' \code{rank_freq_plot} employs base graphics.  \code{rank_freq_mplot} is more 
#' general & flexible; in most cases \code{rank_freq_mplot} should be preferred.
#' @rdname rank_freq_plot
#' @references Zipf, G. K. (1949). Human behavior and the principle of least 
#' effort. Cambridge, Massachusetts: Addison-Wesley. p. 1.
#' @export
#' @import RColorBrewer
#' @importFrom qdapTools text2color
#' @importFrom gridExtra grid.arrange
#' @importFrom scales alpha
#' @importFrom ggplot2 ggplot aes geom_point facet_wrap position_jitter theme_bw scale_color_manual theme element_blank guides guide_legend xlab ylab 
#' @examples
#' \dontrun{
#' #rank_freq_mplot EXAMPLES:
#' x1 <- rank_freq_mplot(DATA$state, DATA$person, ncol = 2, jitter = 0)
#' ltruncdf(x1, 10)
#' x2 <- rank_freq_mplot(mraja1spl$dialogue, mraja1spl$person, ncol = 5, 
#'     hap.col = "purple")
#' ltruncdf(x2, 10)
#' invisible(rank_freq_mplot(mraja1spl$dialogue, mraja1spl$person, ncol = 5, 
#'     log.freq = FALSE, log.rank = FALSE, jitter = .6))
#' invisible(rank_freq_mplot(raj$dialogue, jitter = .5, alpha = 1/15))
#' invisible(rank_freq_mplot(raj$dialogue, jitter = .5, shape = 19, alpha = 1/15))
#' 
#' #rank_freq_plot EXAMPLES:
#' mod <- with(mraja1spl , word_list(dialogue, person, cut.n = 10, 
#'     cap.list=unique(mraja1spl$person)))         
#' x3 <- rank_freq_plot(mod$fwl$Romeo$WORD, mod$fwl$Romeo$FREQ, title.ext = 'Romeo')  
#' ltruncdf(x3, 10)
#' ltruncdf(rank_freq_plot(mod$fwl$Romeo$WORD, mod$fwl$Romeo$FREQ, plot = FALSE)           , 10)
#' invisible(rank_freq_plot(mod$fwl$Romeo$WORD, mod$fwl$Romeo$FREQ, title.ext = 'Romeo',     
#'     jitter.ammount = 0.15, hap.col = "darkgreen", dis.col = "purple"))                  
#' invisible(rank_freq_plot(mod$fwl$Romeo$WORD, mod$fwl$Romeo$FREQ, title.ext = 'Romeo',  
#'     jitter.ammount = 0.5, log.scale=FALSE))  
#' invisible(lapply(seq_along(mod$fwl), function(i){
#'     dev.new()
#'     rank_freq_plot(mod$fwl[[i]]$WORD, mod$fwl[[i]]$FREQ, 
#'         title.ext = names(mod$fwl)[i], jitter.ammount = 0.5, log.scale=FALSE)
#' }))
#' }
rank_freq_mplot <-
function(text.var, grouping.var = NULL, ncol =4, jitter = 0.2, log.freq = TRUE, 
    log.rank = TRUE, hap.col = "red", dis.col = "blue", alpha = 1, shape = 1, 
    title = "Rank-Frequency Plot", digits = 2, plot = TRUE) {
    plotrank <- plotfreq <- cols <- freq <- NULL
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
    L1 <- word_list(text.var, grouping)
    L2 <- lapply(seq_along(L1[["fwl"]]), function(i){
        if (nrow(L1[["fwl"]][[i]]) == 1 & all(is.na(L1[["fwl"]][[i]][1, ]))) {
            return(NULL)
        } 
        dat <- data.frame(group = names(L1[["fwl"]])[i], L1[["fwl"]][[i]]) 
        names(dat)[2:3] <- c("word", "freq")
        X <- tapply(dat$freq, dat$freq, length)
        ZIPF <- data.frame(group = names(L1[["fwl"]])[i],
            n.words = as.numeric(rev(X)), 
            freq = as.numeric(rev(rownames(X))), 
            rank = as.numeric(rev(nrow(X):1)))
        list(ZIPF, dat)
    })
    DF1 <- do.call(rbind, unlist(lapply(L2, "[", 2), recursive = FALSE))
    DF2 <- do.call(rbind, unlist(lapply(L2, "[", 1), recursive = FALSE))
    DF3 <- DF2[rep(seq_len(dim(DF2)[1]), DF2[, "n.words"]), ]
    DF3$cols <- text2color(DF3$freq, c(1, 2), c(hap.col, dis.col, "black"))
    if (log.freq) {
        DF3$plotfreq  <-  log(DF3$freq)
    } else {
        DF3$plotfreq  <- DF3$freq
    }
    if (log.rank) {
        DF3$plotrank  <- log(DF3$rank) 
    } else {
        DF3$plotrank <- DF3$rank
    }
    gg <- ggplot(DF3, aes(x = plotrank, y = plotfreq)) + 
        geom_point(aes(colour = cols), shape = shape, alpha = alpha,
            position=position_jitter(width = jitter, height = 0)) +
        facet_wrap(~group, ncol = ncol, scales = "free") + theme_bw() +
        scale_color_manual(values = c("black", dis.col, hap.col),
            breaks=c("black", dis.col, hap.col), 
            labels=c("> 2", "Dis Legomenon", "Hapax Legomenon"),
            name = "Word Use") +
        theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_blank(),
            panel.border = element_rect(colour = "black")) + 
        guides(colour = guide_legend(override.aes = list(shape = 15, alpha = 1)))
    gg <- gg + xlab(ifelse(log.rank, "Rank (log scale)", "Rank")) + 
        ylab(ifelse(log.freq, "Frequency (log scale)", "Frequency"))
    if (!is.null(title)) {
        gg <- gg + ggtitle(title)
    }
    if (plot) {
        print(gg)
    }
    L3 <- invisible(lapply(split(DF2, DF2$group), function(x) {
        hapax <- 100 * (x[x[, "freq"] == 1, "n.words"]/sum(x[, "n.words"]))
        dis <- 100 * (x[x[, "freq"] == 2, "n.words"]/sum(x[, "n.words"]))
        round(c(hapax_legomenon = hapax, dis_legomenon = dis), digits = digits)
    }))
    DF4 <- data.frame(group = levels(DF2$group), do.call(rbind, L3), 
        row.names = NULL)
    names(DF4)[1] <- G
    list(WORD_COUNTS = DF1, RANK_AND_FREQUENCY_STATS = DF2, 
        LEGOMENA_STATS = DF4)
}

#' Rank Frequency Plot
#' 
#' \code{rank_freq_plot} - Plot word rank versus frequencies.
#' 
#' @rdname rank_freq_plot
#' @export
rank_freq_plot <- 
function(words, frequencies, plot = TRUE, title.ext = NULL,
    jitter.ammount = 0.1, log.scale = TRUE, hap.col = "red", dis.col = "blue") {
    freq <- NULL
    frequencies <- as.numeric(as.character(frequencies))
    original.data <- data.frame(words = words, freq = frequencies)
    X <- tapply(words, frequencies, function(x) length(x))
    ZIPF <- data.frame(n.words = as.numeric(rev(X)), 
        freq = as.numeric(rev(rownames(X))), 
        rank = as.numeric(rev(nrow(X):1)))
    rownames(ZIPF) <- 1:nrow(ZIPF)
    title <- if (is.null(title.ext)) 
        NULL else paste(c("for", title.ext), collapse = " ")
    ZIPF2 <- ZIPF[rep(seq(dim(ZIPF)[1]), ZIPF$n.words), ]
    ZIPF <- transform(ZIPF, per.of.tot = round(freq/sum(ZIPF2$freq) * 
        100, digits = 3))
    if (plot) {
        if (log.scale) {
            with(ZIPF2, plot(jitter(log(rank), amount = jitter.ammount), 
                log(freq), ylab = "Frequency (log scale)", 
                xlab = "Rank (log scale)", 
                main = paste("Rank-Frequency Plot", title, collapse = " "), 
                col = ifelse(freq == 1, hap.col, ifelse(freq == 2, 
                dis.col, "black"))))
        } else {
            with(ZIPF2, plot(jitter(rank, amount = jitter.ammount), 
                freq, ylab = "Frequency", xlab = "Rank", 
                main = paste("Rank-Frequency Plot", title, collapse = " "), 
                col = ifelse(freq == 1, hap.col, ifelse(freq == 2, 
                    dis.col, "black"))))
        }
    } else {
        NULL
    }
    percent_hapax_legomena <- round(ZIPF[ZIPF$freq == 1, 
        "n.words"]/sum(ZIPF$n.words) * 100, digits = 3)
    percent_dis_legomena <- round(ZIPF[ZIPF$freq == 2, 
        "n.words"]/sum(ZIPF$n.words) * 100, digits = 3)
    list(WORD_COUNTS = original.data, RANK_AND_FREQUENCY_STATS = ZIPF, 
        LEGOMENA_STATS = c(percent_hapax_legomena = percent_hapax_legomena, 
        percent_dis_legomena = percent_dis_legomena))
}
