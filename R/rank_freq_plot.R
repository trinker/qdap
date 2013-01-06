#' Rank Frequency Plot
#' 
#' Plot word rank versus frequencies.
#' 
#' @param words A vector of words.
#' @param frequencies A vector of frequencies corresponding to the words argument.
#' @param plot logical.  If TRUE provides a rank frequency plot.
#' @param title.ext The title extension that extends: "Rank-Frequency Plot ..."
#' @param jitter.ammount Ammount of horizontal jitter to add to the points.
#' @param log.scale logical.  If TRUE plots the rank and frequency as a log 
#' scale.
#' @param hap.col Color of the hapax legomenon points.
#' @param dis.col Color of the dis legomenon points.
#' @return Returns a rank-frequency plot and a list of three dataframes:
#' \item{ORIGINAL_DATA}{The word frquencies supplied to \code{rank_freq_plot}.}
#' \item{RANK_AND_FREQUENCY_STATS}{A dataframe of rank and frequencies for the 
#' words used in the text.}
#' \item{LEGOMENA_STATS}{A dataframe displaying the percent_hapax_legomena and 
#' percent_dis_legomena of the text.}
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references Zipf, G. K. (1949). Human behavior and the principle of least 
#' effort. Cambridge, Massachusetts: Addison-Wesley. p. 1.
#' @keywords Zipf, rank-frequency
#' @export
#' @examples
#' \dontrun{
#' mod <- with(mraja1spl , word_list(dialogue, person, cut.n = 10, 
#'     cap.list=unique(DF$person)))         
#' rank_freq_plot(mod$fwl$Romeo$WORD, mod$fwl$Romeo$FREQ, title.ext = 'Romeo')  
#' rank_freq_plot(mod$fwl$Romeo$WORD, mod$fwl$Romeo$FREQ, plot = FALSE)           
#' rank_freq_plot(mod$fwl$Romeo$WORD, mod$fwl$Romeo$FREQ, title.ext = 'Romeo',     
#'     jitter.ammount = 0.15, hap.col = "darkgreen", dis.col = "purple")                    
#' rank_freq_plot(mod$fwl$Romeo$WORD, mod$fwl$Romeo$FREQ, title.ext = 'Romeo',  
#'     jitter.ammount = 0.5, log.scale=FALSE)  
#' lapply(seq_along(mod$fwl), function(i){
#'     dev.new()
#'     rank_freq_plot(mod$fwl[[i]]$WORD, mod$fwl[[i]]$FREQ, 
#'         title.ext = names(mod$fwl)[i], jitter.ammount = 0.5, log.scale=FALSE)
#' }) 
#' }
rank_freq_plot <-
function(words, frequencies, plot = TRUE, title.ext = NULL,
    jitter.ammount = 0.1, log.scale = TRUE, hap.col = "red", dis.col = "blue") {
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
                main = paste("Rank-Frequency Plot", 
                title, collapse = " "), col = ifelse(freq == 1, 
                hap.col, ifelse(freq == 2, dis.col, "black"))))
        }
    } else {
        NULL
    }
    percent_hapax_legomena <- round(ZIPF[ZIPF$freq == 1, 
        "n.words"]/sum(ZIPF$n.words) * 100, digits = 3)
    percent_dis_legomena <- round(ZIPF[ZIPF$freq == 2, 
        "n.words"]/sum(ZIPF$n.words) * 100, digits = 3)
    list(ORIGINAL_DATA = original.data, RANK_AND_FREQUENCY_STATS = ZIPF, 
        LEGOMENA_STATS = c(percent_hapax_legomena = percent_hapax_legomena, 
        percent_dis_legomena = percent_dis_legomena))
}
