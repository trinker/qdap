#' Polarity Score (Sentiment Analysis)
#' 
#' \code{polarity} - Approximate the sentiment (polarity) of text by grouping 
#' variable(s).
#' 
#' @param text.var The text variable.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param polarity.frame A dataframe or hash key of positive/negative words and 
#' weights.
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
#' @param constrain logical.  If \code{TRUE} polarity values are constrained to 
#' be between -1 and 1 using the following transformation:
#' 
#' \deqn{\left[\left( 1 - \frac{1}{exp(\delta)}\right ) \cdot 2 \right] - 1}{((1 - (1/(1 + exp(polarity)))) * 2) - 1}
#' 
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
#' dictionary (see the \code{sentiment_frame} function).  For instance the word 
#' "sick" in a high school setting may mean that something is good, whereas 
#' "sick" used by a typical adult indicates something is not right or negative 
#' connotation (\strong{deixis}).
#' 
#' Also note that \code{\link[qdap]{polarity}} assumes you've run 
#' \code{\link[qdap]{sentSplit}}.
#' @details The equation used by the algorithm to assign value to polarity of 
#' each sentence fist utilizes the sentiment dictionary (Hu and Liu, 2004) to 
#' tag polarized words.  A context cluster (\eqn{x_i^{T}}{x_i^T}) of words is 
#' pulled from around this polarized word (default 4 words before and two words 
#' after) to be considered as valence shifters.  The words in this context 
#' cluster are tagged as neutral (\eqn{x_i^{0}}{x_i^0}), negator 
#' (\eqn{x_i^{N}}{x_i^N}), amplifier (\eqn{x_i^{a}}{x_i^a}), or de-amplifier 
#' (\eqn{x_i^{d}}{x_i^d}). Neutral words hold no value 
#' in the equation but do affect word count (\eqn{n}).  Each polarized word is 
#' then weighted \eqn{w} based on the weights from the \code{polarity.frame} 
#' argument and then further weighted by the number and position of the valence 
#' shifters directly surrounding the positive or negative word.  The researcher 
#' may provide a weight \eqn{c} to be utilized with amplifiers/de-amplifiers 
#' (default is .8; deamplifier weight is constrained to -1 lower bound).  Last, 
#' these context cluster (\eqn{x_i^{T}}{x_i^T}) are summed and divided by the 
#' square root of the word count (\eqn{\sqrt{n}}{\sqrtn}) yielding an unbounded 
#' polarity score (\eqn{\delta}{C}).  Note that context clusters containing a 
#' comma before the polarized word will only consider words found after the 
#' comma.
#' 
#' \deqn{\delta=\frac{x_i^T}{\sqrt{n}}}{C=x_i^2/\sqrt(n)}
#'   
#' Where:
#' 
#' \deqn{x_i^T=\sum{((1 + c(x_i^{A} - x_i^{D}))\cdot w(-1)^{\sum{x_i^{N}}})}}{x_i^T=\sum((1 + c * (x_i^A - x_i^D)) * w(-1)^(\sumx_i^N))}
#' 
#' \deqn{x_i^{A}=\sum{(w_{neg}\cdot x_i^{a})}}{x_i^A=\sum(w_neg * x_i^a)}
#' 
#' \deqn{x_i^D = \max(x_i^{D'}, -1)}{x_i^D = max(x_i^D', -1)}
#' 
#' \deqn{x_i^{D'}= \sum{(- w_{neg}\cdot x_i^{a} + x_i^{d})}}{x_i^D'=\sum(- w_neg * x_i^a + x_i^d)}
#' 
#' \deqn{w_{neg}= \left(\sum{x_i^{N}}\right) \bmod {2}}{w_neg= (\sumx_i^N) mod 2}
#'     
#' @references Hu, M., & Liu, B. (2004). Mining opinion features in customer 
#' reviews. National Conference on Artificial Intelligence. 
#' 
#' https://www.slideshare.net/jeffreybreen/r-by-example-mining-twitter-for
#' 
#' http://hedonometer.org/papers.html Links to papers on hedonometrics
#' @export
#' @importFrom qdapTools hash hash_look
#' @rdname polarity
#' @examples
#' \dontrun{
#' with(DATA, polarity(state, list(sex, adult)))
#' (poldat <- with(sentSplit(DATA, 4), polarity(state, person)))
#' counts(poldat)
#' scores(poldat)
#' plot(poldat)
#' 
#' poldat2 <- with(mraja1spl, polarity(dialogue, 
#'     list(sex, fam.aff, died)))
#' colsplit2df(scores(poldat2))
#' plot(poldat2)
#' plot(scores(poldat2))
#' cumulative(poldat2)
#' 
#' poldat3 <- with(rajSPLIT, polarity(dialogue, person))
#' poldat3[["group"]][, "OL"] <- outlier_labeler(scores(poldat3)[, 
#'     "ave.polarity"])
#' poldat3[["all"]][, "OL"] <- outlier_labeler(counts(poldat3)[, 
#'     "polarity"])
#' htruncdf(scores(poldat3), 10)
#' htruncdf(counts(poldat3), 15, 8)
#' plot(poldat3)
#' plot(poldat3, nrow=4)
#' qheat(scores(poldat3)[, -7], high="red", order.b="ave.polarity")
#' 
#' ## Create researcher defined sentiment.frame
#' POLKEY <- sentiment_frame(positive.words, negative.words)
#' POLKEY
#' c("abrasive", "abrupt", "happy") %hl% POLKEY
#' 
#' # Augmenting the sentiment.frame
#' mycorpus <- c("Wow that's a raw move.", "His jokes are so corny")
#' counts(polarity(mycorpus))
#' 
#' POLKEY <- sentiment_frame(c(positive.words, "raw"), c(negative.words, "corny"))
#' counts(polarity(mycorpus, polarity.frame=POLKEY))
#' 
#' ## ANIMATION
#' #===========
#' (deb2 <- with(subset(pres_debates2012, time=="time 2"),
#'     polarity(dialogue, person)))
#' 
#' bg_black <- Animate(deb2, neutral="white", current.speaker.color="grey70")
#' print(bg_black, pause=.75)
#' 
#' bgb <- vertex_apply(bg_black, label.color="grey80", size=20, color="grey40")
#' bgb <- edge_apply(bgb, label.color="yellow")
#' print(bgb, bg="black", pause=.75)
#' 
#' ## Save it
#' library(animation)
#' library(igraph)
#' library(plotrix)
#' 
#' loc <- folder(animation_polarity)
#' 
#' ## Set up the plotting function
#' oopt <- animation::ani.options(interval = 0.1)
#' 
#' FUN <- function() {
#'     Title <- "Animated Polarity: 2012 Presidential Debate 2"
#'     Legend <- c(-1.1, -1.25, -.2, -1.2)
#'     Legend.cex <- 1
#'     lapply(seq_along(bgb), function(i) {
#'         par(mar=c(2, 0, 1, 0), bg="black")
#'         set.seed(10)
#'         plot.igraph(bgb[[i]], edge.curved=TRUE)
#'         mtext(Title, side=3, col="white")
#'         color.legend(Legend[1], Legend[2], Legend[3], Legend[4],
#'               c("Negative", "Neutral", "Positive"), attributes(bgb)[["legend"]],
#'               cex = Legend.cex, col="white")
#'         animation::ani.pause()
#'     })
#' }
#' 
#' FUN()
#' 
#' ## Detect OS
#' type <- if(.Platform$OS.type == "windows") shell else system
#' 
#' saveHTML(FUN(), autoplay = FALSE, loop = TRUE, verbose = FALSE,
#'     ani.height = 500, ani.width=500,
#'     outdir = file.path(loc, "new"), single.opts =
#'     "'controls': ['first', 'play', 'loop', 'speed'], 'delayMin': 0")
#' 
#' ## Detect OS
#' type <- if(.Platform$OS.type == "windows") shell else system
#' 
#' saveHTML(FUN(), autoplay = FALSE, loop = TRUE, verbose = FALSE,
#'     ani.height = 1000, ani.width=650,
#'     outdir = loc, single.opts =
#'     "'controls': ['first', 'play', 'loop', 'speed'], 'delayMin': 0")
#'     
#'  ## Animated corresponding text plot
#'  Animate(deb2, type="text")
#'  
#' #=====================#
#' ## Complex Animation ##
#' #=====================#
#' library(animation)
#' library(grid)
#' library(gridBase)
#' library(qdap)
#' library(qdapTools)
#' library(igraph)
#' library(plotrix)
#' library(gridExtra)
#' 
#' deb2dat <- subset(pres_debates2012, time=="time 2")
#' deb2dat[, "person"] <- factor(deb2dat[, "person"])
#' (deb2 <- with(deb2dat, polarity(dialogue, person)))
#' 
#' ## Set up the network version
#' bg_black <- Animate(deb2, neutral="white", current.speaker.color="grey70")
#' bgb <- vertex_apply(bg_black, label.color="grey80", size=30, label.size=22,
#'     color="grey40")
#' bgb <- edge_apply(bgb, label.color="yellow")
#' 
#' ## Set up the bar version
#' deb2_bar <- Animate(deb2, as.network=FALSE)
#' 
#' ## Generate a folder
#' loc2 <- folder(animation_polarity2)
#' 
#' ## Set up the plotting function
#' oopt <- animation::ani.options(interval = 0.1)
#' 
#' 
#' FUN2 <- function(follow=FALSE, theseq = seq_along(bgb)) {
#' 
#'     Title <- "Animated Polarity: 2012 Presidential Debate 2"
#'     Legend <- c(.2, -1.075, 1.5, -1.005)
#'     Legend.cex <- 1
#' 
#'     lapply(theseq, function(i) {
#'         if (follow) {
#'             png(file=sprintf("%s/images/Rplot%s.png", loc2, i), 
#'                 width=650, height=725)
#'         }
#'         ## Set up the layout
#'         layout(matrix(c(rep(1, 9), rep(2, 4)), 13, 1, byrow = TRUE))
#' 
#'         ## Plot 1
#'         par(mar=c(2, 0, 2, 0), bg="black")
#'         #par(mar=c(2, 0, 2, 0))
#'         set.seed(20)
#'         plot.igraph(bgb[[i]], edge.curved=TRUE)
#'         mtext(Title, side=3, col="white")
#'         color.legend(Legend[1], Legend[2], Legend[3], Legend[4],
#'               c("Negative", "Neutral", "Positive"), attributes(bgb)[["legend"]],
#'               cex = Legend.cex, col="white")
#' 
#'         ## Plot2
#'         plot.new()              
#'         vps <- baseViewports()
#' 
#'         uns <- unit(c(-1.3,.5,-.75,.25), "cm")
#'         p <- deb2_bar[[i]] + 
#'             theme(plot.margin = uns,
#'                 text=element_text(color="white"),
#'                 plot.background = element_rect(fill = "black", 
#'                     color="black")) 
#'         print(p,vp = vpStack(vps$figure,vps$plot))
#'         animation::ani.pause()
#' 
#'         if (follow) {
#'             dev.off()
#'         }
#'     })
#' 
#' }
#' 
#' FUN2()
#' 
#' ## Detect OS
#' type <- if(.Platform$OS.type == "windows") shell else system
#' 
#' saveHTML(FUN2(), autoplay = FALSE, loop = TRUE, verbose = FALSE,
#'     ani.height = 1000, ani.width=650,
#'     outdir = loc2, single.opts =
#'     "'controls': ['first', 'play', 'loop', 'speed'], 'delayMin': 0")
#' 
#' FUN2(TRUE)
#' 
#' #=====================#
#' library(animation)
#' library(grid)
#' library(gridBase)
#' library(qdap)
#' library(qdapTools)
#' library(igraph)
#' library(plotrix)
#' library(gplots)
#' 
#' deb2dat <- subset(pres_debates2012, time=="time 2")
#' deb2dat[, "person"] <- factor(deb2dat[, "person"])
#' (deb2 <- with(deb2dat, polarity(dialogue, person)))
#' 
#' ## Set up the network version
#' bg_black <- Animate(deb2, neutral="white", current.speaker.color="grey70")
#' bgb <- vertex_apply(bg_black, label.color="grey80", size=30, label.size=22,
#'     color="grey40")
#' bgb <- edge_apply(bgb, label.color="yellow")
#' 
#' ## Set up the bar version
#' deb2_bar <- Animate(deb2, as.network=FALSE)
#' 
#' ## Set up the line version
#' deb2_line <- plot(cumulative(deb2_bar))
#' 
#' ## Generate a folder
#' loc2b <- folder(animation_polarity2)
#' 
#' ## Set up the plotting function
#' oopt <- animation::ani.options(interval = 0.1)
#' 
#' FUN2 <- function(follow=FALSE, theseq = seq_along(bgb)) {
#' 
#'     Title <- "Animated Polarity: 2012 Presidential Debate 2"
#'     Legend <- c(.2, -1.075, 1.5, -1.005)
#'     Legend.cex <- 1
#' 
#'     lapply(theseq, function(i) {
#'         if (follow) {
#'             png(file=sprintf("%s/images/Rplot%s.png", loc2b, i),
#'                 width=650, height=725)
#'         }
#'         ## Set up the layout
#'         layout(matrix(c(rep(1, 9), rep(2, 4)), 13, 1, byrow = TRUE))
#' 
#'         ## Plot 1
#'         par(mar=c(2, 0, 2, 0), bg="black")
#'         #par(mar=c(2, 0, 2, 0))
#'         set.seed(20)
#'         plot.igraph(bgb[[i]], edge.curved=TRUE)
#'         mtext(Title, side=3, col="white")
#'         color.legend(Legend[1], Legend[2], Legend[3], Legend[4],
#'               c("Negative", "Neutral", "Positive"), attributes(bgb)[["legend"]],
#'               cex = Legend.cex, col="white")
#' 
#'         ## Plot2
#'         plot.new()
#'         vps <- baseViewports()
#' 
#'         uns <- unit(c(-1.3,.5,-.75,.25), "cm")
#'         p <- deb2_bar[[i]] +
#'             theme(plot.margin = uns,
#'                 text=element_text(color="white"),
#'                 plot.background = element_rect(fill = "black",
#'                     color="black"))
#'         print(p,vp = vpStack(vps$figure,vps$plot))
#'         animation::ani.pause()
#' 
#'         if (follow) {
#'             dev.off()
#'         }
#'     })
#' 
#' }
#' 
#' FUN2()
#' 
#' ## Detect OS
#' type <- if(.Platform$OS.type == "windows") shell else system
#' 
#' saveHTML(FUN2(), autoplay = FALSE, loop = TRUE, verbose = FALSE,
#'     ani.height = 1000, ani.width=650,
#'     outdir = loc2b, single.opts =
#'     "'controls': ['first', 'play', 'loop', 'speed'], 'delayMin': 0")
#' 
#' FUN2(TRUE)
#' 
#' ## Increased complexity
#' ## --------------------
#' 
#' ## Helper function to cbind ggplots
#' cbinder <- function(x, y){
#' 
#'     uns_x <- unit(c(-1.3,.15,-.75,.25), "cm")
#'     uns_y <- unit(c(-1.3,.5,-.75,.15), "cm")
#' 
#'     x <- x + theme(plot.margin = uns_x,
#'         text=element_text(color="white"),
#'         plot.background = element_rect(fill = "black",
#'         color="black")
#'     )
#' 
#'     y <- y + theme(plot.margin = uns_y,
#'         text=element_text(color="white"),
#'         plot.background = element_rect(fill = "black", 
#'         color="black")
#'     )
#' 
#'     plots <- list(x, y)
#'     grobs <- list()
#'     heights <- list()
#'     
#'     for (i in 1:length(plots)){
#'         grobs[[i]] <- ggplotGrob(plots[[i]])
#'         heights[[i]] <- grobs[[i]]$heights[2:5]
#'     }
#'     
#'     maxheight <- do.call(grid::unit.pmax, heights)
#'     
#'     for (i in 1:length(grobs)){
#'          grobs[[i]]$heights[2:5] <- as.list(maxheight)
#'     }
#'     
#'     do.call("arrangeGrob", c(grobs, ncol = 2))
#' }
#' 
#' deb2_combo <- Map(cbinder, deb2_bar, deb2_line)
#' 
#' ## Generate a folder
#' loc3 <- folder(animation_polarity3)
#' 
#' 
#' FUN3 <- function(follow=FALSE, theseq = seq_along(bgb)) {
#' 
#'     Title <- "Animated Polarity: 2012 Presidential Debate 2"
#'     Legend <- c(.2, -1.075, 1.5, -1.005)
#'     Legend.cex <- 1
#' 
#'     lapply(theseq, function(i) {
#'         if (follow) {
#'             png(file=sprintf("%s/images/Rplot%s.png", loc3, i),
#'                 width=650, height=725)
#'         }
#'         ## Set up the layout
#'         layout(matrix(c(rep(1, 9), rep(2, 4)), 13, 1, byrow = TRUE))
#' 
#'         ## Plot 1
#'         par(mar=c(2, 0, 2, 0), bg="black")
#'         #par(mar=c(2, 0, 2, 0))
#'         set.seed(20)
#'         plot.igraph(bgb[[i]], edge.curved=TRUE)
#'         mtext(Title, side=3, col="white")
#'         color.legend(Legend[1], Legend[2], Legend[3], Legend[4],
#'               c("Negative", "Neutral", "Positive"), attributes(bgb)[["legend"]],
#'               cex = Legend.cex, col="white")
#' 
#'         ## Plot2
#'         plot.new()
#'         vps <- baseViewports()
#'         p <- deb2_combo[[i]]
#'         print(p,vp = vpStack(vps$figure,vps$plot))
#'         animation::ani.pause()
#' 
#'         if (follow) {
#'             dev.off()
#'         }
#'     })
#' }
#' 
#' FUN3()
#' 
#' type <- if(.Platform$OS.type == "windows") shell else system
#' 
#' saveHTML(FUN3(), autoplay = FALSE, loop = TRUE, verbose = FALSE,
#'     ani.height = 1000, ani.width=650,
#'     outdir = loc3, single.opts =
#'     "'controls': ['first', 'play', 'loop', 'speed'], 'delayMin': 0")
#' 
#' FUN3(TRUE)
#' 
#' ##-----------------------------##
#' ## Constraining between -1 & 1 ##
#' ##-----------------------------##
#' ## The old behavior of polarity constrained the output to be between -1 and 1
#' ## this can be replicated via the `constrain = TRUE` argument:
#' 
#' polarity("really hate anger")
#' polarity("really hate anger", constrain=TRUE)
#' 
#' #==================#
#' ## Static Network ##
#' #==================#
#' (poldat <- with(sentSplit(DATA, 4), polarity(state, person)))
#' m <- Network(poldat)
#' m
#' print(m, bg="grey97", vertex.color="grey75")
#' 
#' print(m, title="Polarity Discourse Map", title.color="white", bg="black",
#'     legend.text.color="white", vertex.label.color = "grey70", 
#'     edge.label.color="yellow")
#'     
#' ## or use themes:
#' dev.off()
#' m + qtheme()
#' m + theme_nightheat
#' dev.off()
#' m+ theme_nightheat(title="Polarity Discourse Map")
#' 
#' #===============================#
#' ## CUMULATIVE POLARITY EXAMPLE ##
#' #===============================#
#' #        Hedonometrics          #           
#' #===============================#
#' poldat4 <- with(rajSPLIT, polarity(dialogue, act, constrain = TRUE))
#' 
#' polcount <- na.omit(counts(poldat4)$polarity)
#' len <- length(polcount)
#' 
#' cummean <- function(x){cumsum(x)/seq_along(x)}
#' 
#' cumpolarity <- data.frame(cum_mean = cummean(polcount), Time=1:len)
#' 
#' ## Calculate background rectangles
#' ends <- cumsum(rle(counts(poldat4)$act)$lengths)
#' starts <- c(1, head(ends + 1, -1))
#' rects <- data.frame(xstart = starts, xend = ends + 1, 
#'     Act = c("I", "II", "III", "IV", "V"))
#' 
#' library(ggplot2)
#' ggplot() + theme_bw() +
#'     geom_rect(data = rects, aes(xmin = xstart, xmax = xend, 
#'         ymin = -Inf, ymax = Inf, fill = Act), alpha = 0.17) +
#'     geom_smooth(data = cumpolarity, aes(y=cum_mean, x = Time)) +
#'     geom_hline(y=mean(polcount), color="grey30", size=1, alpha=.3, linetype=2) + 
#'     annotate("text", x = mean(ends[1:2]), y = mean(polcount), color="grey30", 
#'         label = "Average Polarity", vjust = .3, size=3) +
#'     geom_line(data = cumpolarity, aes(y=cum_mean, x = Time), size=1) +
#'     ylab("Cumulative Average Polarity") + xlab("Duration") +
#'     scale_x_continuous(expand = c(0,0)) +
#'     geom_text(data=rects, aes(x=(xstart + xend)/2, y=-.04, 
#'         label=paste("Act", Act)), size=3) + 
#'     guides(fill=FALSE) +
#'     scale_fill_brewer(palette="Set1")
#' }
polarity <- function (text.var, grouping.var = NULL, 
    polarity.frame = qdapDictionaries::key.pol, constrain = FALSE,
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

    ## A hash key to look up polarized words
    if (!is.hash(polarity.frame)) {
        if (!is.data.frame(polarity.frame)) {
            stop(paste("Please supply a dataframe or hash_key", 
               "(see `sentiment_frame`) to polarity.frame"))
        }
        polarity.frame <- hash(polarity.frame)
    }

    ## remove amplifiers also in polarity frame
    amplifiers <- amplifiers[!amplifiers %in% ls(polarity.frame)]
    deamplifiers <- deamplifiers[!deamplifiers %in% ls(polarity.frame)]

    ## create hash key to lookup amps, de-amps and negators
    alter <- alter_env(negators, amplifiers, deamplifiers)

    ## grab the polarized/alter words to search for spaces and insert into textvar
    posneg <- polarity.frame[[1]]
    words <- c(posneg, alter[[1]])

    ## create sentence lengths
    counts <- unlist(lapply(DF[, "text.var"], function(x) length(bag_o_words(x))))
    DF[, "text.var"] <- space_fill(text.var = strip(DF[, "text.var"], 
        apostrophe.remove = FALSE, ... ), terms = words[grep("\\s", words)], 
        sep = "~~")

    ## split into bag of words per sentence
    TV <- lapply(lapply(DF[, "text.var"], bag_o_words2, char.keep = "~~", ...), function(x) {
        gsub("~~", " ", x)
    })

    ## Get position of polarized word (hits)
    hits <- lapply(TV, function(x) which(x %in% gsub("~~", " ", posneg)))

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
    counts <- unlist(lapply(DF[, "text.var2"], function(x) length(bag_o_words(x))))
    scores <- sapply(output, sum)/sqrt(counts)
    all <- data.frame(group.var =  DF[, "grouping"], wc = counts, 
        polarity = scores, stringsAsFactors = FALSE)

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
    all$pos.words <- mapply(pwords, pols, polwords, SIMPLIFY = FALSE)
    all$neg.words <-mapply(nwords, pols, polwords, SIMPLIFY = FALSE)
    all[, "text.var"] <- DF[, "text.var2"]
   
    ## Multiple polarity by question weights
    qweight <- ifelse(suppressWarnings(end_mark(all[, "text.var"])) %in% c("?", 
        "*?"), question.weight, 1)
    pols <- all[, "polarity"] <- qweight * all[, "polarity"]

    ## constrain to -1 to 1
    if (constrain) {
        all[, "polarity"] <- constrain(all[, "polarity"])    
    }
   
    ## Create average polarity data.frame (group) from all data.frame
    sall <- split(all, all[, "group.var"])  
    lall <- lapply(sall, function(x) {
        data.frame(total.words = sum(x[, "wc"], na.rm = TRUE), 
            ave.polarity = mean(x[, "polarity"], na.rm = TRUE),
            sd.polarity = stats::sd(x[, "polarity"], na.rm = TRUE), 
            stan.mean.polarity = mean(x[, "polarity"], na.rm = TRUE)/
                stats::sd(x[, "polarity"], na.rm = TRUE))
    })
    group <- data.frame(group = names(lall), 
        total.sentences = sapply(sall, nrow),
        do.call(rbind, lall), row.names = NULL)
    colnames(group)[1] <- colnames(all)[1] <- G
    o <- list(all = all, group = group)
    attributes(o) <- list(
            class = c("polarity", class(o)),
            names = names(o),
            digits = digits,
            constrained = constrain,
            unconstrained.polarity = pols
    )
    return(o)
}


#' Polarity
#' 
#' \code{scores.polarity} - View scores from \code{\link[qdap]{polarity}}.
#' 
#' polarity Method for scores
#' @param x The polarity object.
#' @param \ldots ignored
#' @export
#' @method scores polarity
scores.polarity <- function(x, ...) {

    out <- x[["group"]]
    attributes(out) <- list(
            class = c("polarity_score", class(out)),
            type = "polarity_scores",
            names = colnames(out),
            row.names = rownames(out),
            digits = attributes(x)[["digits"]]
    )
    out
}

#' Prints a polarity_score Object
#' 
#' Prints a polarity_score object.
#' 
#' @param x The polarity_score object.
#' @param digits The number of digits displayed if \code{values} is \code{TRUE}.
#' @param \ldots ignored
#' @method print polarity_score
#' @export
print.polarity_score <-
    function(x, digits = 3, ...) {

    WD <- options()[["width"]]
    options(width=3000)

    class(x) <- "data.frame"
    if ("ave.polarity" %in% colnames(x)) {    
        x[, "ave.polarity"] <- round(x[, "ave.polarity"], digits = digits)
    }   
    if ("sd.polarity" %in% colnames(x)) {    
        x[, "sd.polarity"] <- round(x[, "sd.polarity"], digits = digits)
    }        
    if ("stan.mean.polarity" %in% colnames(x)) {    
        x[, "stan.mean.polarity"] <- round(x[, "stan.mean.polarity"], 
            digits = digits)
    }    

    print(x)
    options(width=WD)
}

        
#' Prints an polarity Object
#' 
#' Prints an polarity object.
#' 
#' @param x The polarity object.
#' @param digits The number of digits displayed if \code{values} is \code{TRUE}.
#' @param \ldots ignored
#' @method print polarity
#' @export
print.polarity <- function(x, digits = 3, ...) {
    print(scores(x), digits = digits, ...)
}


#' Polarity
#' 
#' \code{counts.polarity} - View counts from \code{\link[qdap]{polarity}}.
#' 
#' polarity Method for counts.
#' @param x The polarity object.
#' @param \ldots ignored
#' @export
#' @method counts polarity
counts.polarity <- function(x, ...) {
    out <- x[["all"]]
    attributes(out) <- list(
            class = c("polarity_count", class(out)),
            type = "polarity_count",
            names = colnames(out),
            row.names = rownames(out),
            digits = attributes(x)[["digits"]]
    )
    out
}

#' Prints a polarity_count Object
#' 
#' Prints a polarity_count object.
#' 
#' @param x The polarity_count object.
#' @param digits The number of digits displayed.
#' @param \ldots ignored
#' @method print polarity_count
#' @export
print.polarity_count <-
    function(x, digits = 3, ...) {

    class(x) <- "data.frame"
        
    if ("polarity" %in% colnames(x)) {    
        x[, "polarity"] <- round(x[, "polarity"], digits = digits)
    }
    WD <- options()[["width"]]
    options(width=3000)
    print(x)
    options(width=WD)
}



#' Plots a polarity Object
#' 
#' Plots a polarity object as a heat map Gantt plot with polarity over 
#' time (measured in words) and polarity scores per sentence.  In the dotplot 
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
#' @importFrom qdapTools lookup
#' @importFrom ggplot2 ggplot aes geom_segment xlab ylab scale_colour_gradientn theme_bw guides geom_point guide_colorbar scale_color_discrete guide_legend
#' @export
plot.polarity <- function(x, bar.size = 5, low = "blue", mid = "grey99", 
    high = "red", ave.polarity.shape = "+", alpha = 1/4, shape = 19, 
    point.size = 2.5,  jitter = .1, nrow = NULL, na.rm = TRUE, 
    order.by.polarity = TRUE, plot = TRUE, error.bars =TRUE, 
    error.bar.height = .5, error.bar.size = .5, error.bar.color = "black", 
    ...){
  
    start <- end <- Polarity <- group <- ave.polarity <- unit <- NULL
    dat <- x[["group"]][, 1:4]
    dat2 <- x[["all"]]
    if (na.rm) {
       dat <- stats::na.omit(dat)
       dat2 <- stats::na.omit(dat2)
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
    dat2$group <- factor(dat2$group, levels = sort(unique(dat2$group), decreasing = TRUE))

    ## the filled polarity Gantt plot
    XX <- ggplot(dat2, aes(color = Polarity )) + 
        geom_segment(aes(x=start, xend=end, y=group, yend=group), 
            size=bar.size) +
        xlab("Duration (sentences)") + ylab(gsub("\\&", " & ", G)) +
        scale_colour_gradientn(colours = c(low, mid, high)) +
        theme_bw() + theme(legend.position="bottom") + 
        guides(colour = guide_colorbar(barwidth = 9, barheight = .75, nbin=1000))

    ## order the ave. poalrity dotplot by ave. polarity or factor level
    if (order.by.polarity) {
        dat$group <- factor(dat$group, levels = rev(dat[order(dat$ave.polarity), 
            "group"]))
        dat2$group <- factor(dat2$group, 
            levels = rev(dat[order(dat$ave.polarity), "group"]))
    } else {
        ## reverse the levels so first factor level is on top
        dat2$group <- factor(dat2$group, levels = sort(unique(dat2$group), decreasing = TRUE))
        dat$group <- factor(dat$group, levels = sort(unique(dat$group), decreasing = TRUE))     
    }
    if (na.rm) {
       dat2 <- stats::na.omit(dat2)
       dat <- stats::na.omit(dat)
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
SE <- function(x) sqrt(stats::var(x)/length(x))

alter_env <- function(negators, amplifiers, deamplifiers) {
    n <- rep(1, length(negators))
    a <- rep(2, length(amplifiers))
    d <- rep(3, length(deamplifiers)) 
    hash(data.frame(words=c(negators, amplifiers, deamplifiers), 
        value=c(n, a, d), stringsAsFactors = FALSE))
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
            final.comma <- utils::tail(comma.loc[comma.loc < target], 1)
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

#' Plots a polarity_count Object
#' 
#' Plots a polarity_count object as a heat map Gantt plot with polarity over 
#' time (measured in words) and polarity scores per sentence.  In the dotplot 
#' plot the black dots are the average polarity per grouping variable.
#' 
#' @param x The polarity_count object.
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
#' @method plot polarity_count
#' @importFrom gridExtra grid.arrange
#' @importFrom scales alpha
#' @importFrom ggplot2 ggplot aes geom_segment xlab ylab scale_colour_gradientn theme_bw guides geom_point guide_colorbar scale_color_discrete guide_legend
#' @export
plot.polarity_count <- function(x, bar.size = 5, low = "blue", mid = "grey99", 
    high = "red", ave.polarity.shape = "+", alpha = 1/4, shape = 19, 
    point.size = 2.5,  jitter = .1, nrow = NULL, na.rm = TRUE, 
    order.by.polarity = TRUE, plot = TRUE, error.bars =TRUE, 
    error.bar.height = .5, error.bar.size = .5, error.bar.color = "black", 
    ...){
  
    start <- end <- Polarity <- group <- ave.polarity <- unit <- NULL

    dat2 <- data.frame(x)
    dat <- do.call(rbind, lapply(split(data.frame(x), x[, 1]), function(x2) {
     
        data.frame(group = x2[1, 1], total.sentences = nrow(x2), 
            total.words = sum(x2[, "wc"], na.rm = TRUE), 
            ave.polarity = mean(x2[, "polarity"], na.rm = TRUE))
    
    }))
    names(dat)[1] <- names(dat2)[1]

    if (na.rm) {
       dat <- stats::na.omit(dat)
       dat2 <- stats::na.omit(dat2)
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
        guides(colour = guide_colorbar(barwidth = 9, barheight = .75, nbin=1000))

    ## order the ave. poalrity dotplot by ave. polarity or factor level
    if (order.by.polarity) {
        dat$group <- factor(dat$group, levels = rev(dat[order(dat$ave.polarity), 
            "group"]))
        dat2$group <- factor(dat2$group, 
            levels = rev(dat[order(dat$ave.polarity), "group"]))
    }
    if (na.rm) {
       dat2 <- stats::na.omit(dat2)
       dat <- stats::na.omit(dat)
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
        gridExtra::grid.arrange(XX, YY, nrow = 2)
    }
    invisible(list(p1 = XX, p2 = YY))
}


#' Plots a polarity_score Object
#' 
#' Plots a polarity_score object.
#' 
#' @param x The polarity_score object.
#' @param error.bar.height The height of the error bar ends.
#' @param error.bar.size The size/thickness of the error bars.
#' @param error.bar.alpha The alpha level of the error bars.
#' @param \ldots ignored
#' @importFrom ggplot2 ggplot aes geom_smooth facet_wrap geom_errorbarh guide_colorbar geom_point theme ggplotGrob theme_bw ylab xlab scale_fill_gradient element_blank guides 
#' @importFrom gridExtra grid.arrange
#' @importFrom scales alpha
#' @method plot polarity_score
#' @export
plot.polarity_score <- function(x, error.bar.height = .35, 
    error.bar.size = .5, error.bar.alpha = .3, ...){ 

    character.count <- sentence.count <- word.count <- grvar <- 
        SE <- ave.polarity <- sd.polarity <- total.sentences <- NULL

    x  <- x[order(x[, "ave.polarity"]), ]
    x[, 1] <- factor(x[, 1], levels = x[, 1])
    forlater <-  names(x)[1]
    names(x)[1] <- "grvar"
    x[, "SE"] <- sqrt((x[, "sd.polarity"]^2)/x[, "total.sentences"])

    plot1 <- ggplot(x, aes(fill = sd.polarity, x = ave.polarity, 
        y = total.sentences)) + geom_point(size=2.75, shape=21, colour="grey65") +
        theme_bw() + 
        scale_fill_gradient(high="red", low="pink", name="Polaity\nVariability") +
        ylab("Numer of Sentences") + 
        xlab("Average Polarity") + 
        theme(panel.grid = element_blank(),
            legend.position = "bottom") +
        guides(fill = guide_colorbar(barwidth = 10, barheight = .5)) 
    plot2 <- ggplot(x, aes(y = grvar, x = ave.polarity)) +
        geom_errorbarh(aes(xmax = ave.polarity + SE, xmin = ave.polarity - SE), 
                height = error.bar.height, size = error.bar.size, 
                alpha = error.bar.alpha) +
        geom_point(size=2) + 
        ylab(gsub("&", " & ", forlater)) + 
        xlab("Average Polarity")

    grid.arrange(plot2, plot1, ncol=2)
    invisible(list(plot1=plot2, plot2=plot1))
}


## Capture edges from an igraph object
edge_capture <- function(iobj) {

    data.frame(do.call(rbind, 
        strsplit(bracketX(capture.output(E(iobj)))[-c(1:2)], 
        " -> ")), stringsAsFactors = FALSE)

}

## generate a to and from column based on a column of froms
from_to_End <- function(x) {

    data.frame(from=as.character(x), to=c(as.character(x[-1]), 
        "End"), stringsAsFactors = FALSE)

}

## aggregate polarity scores as you iterate through rows
agg_pol <- function(a) {
    b <- list_df2df(lapply(split(a[, c("polarity", "wc", "id")], a[, "from|to"]), function(x) {
       data.frame(polarity=mean(x[, 1], na.rm = TRUE), 
           wc=sum(x[, 2], na.rm = TRUE), id=max(x[, 3], na.rm = TRUE))
   }), "group")
    b[, "polarity"][is.nan(b[, "polarity"])] <- NA
    b[, "prop_wc"] <- b[, "wc"]/sum(b[, "wc"], na.rm = TRUE)
    b
}

Animate_polarity_net <- function(x, negative = "blue", positive = "red", 
    neutral = "yellow", edge.constant, 
    wc.time = TRUE, time.constant = 1, title = NULL, digits = 3, 
    current.color = "black", current.speaker.color, non.speaker.color = NA, ...){

    qsep <- "|-|qdap|-|"

    brks <- c(-10:-2, seq(-1, -.6, by=.01), seq(-.5, 0, by=.001), 
        seq(.001, .5, by=.001), seq(.6, 1, by=.01), 2:10)
    max.color.breaks <- length(brks)

    y2 <- y <- counts(x)
    condlens <- rle(as.character(y[, 1]))
    y[, "temp"] <- rep(paste0("X", pad(1:length(condlens[[2]]))),
        condlens[[1]])
    y[, "ave.polarity"] <- stats::ave(y[, 3], y[, "temp"], FUN=mean)

    ## Add to  and from columns
    y <- cbind(y, from_to_End(y[, 1]))

    ## repeat last to column to match with split sentence (i.e.
    ## we don't want an edge to return to the node it leaves
    tos <- split(y[, "to"], y[, "temp"])
    tos_lens <- sapply(tos, length)
    y[, "to"] <- rep(sapply(tos, utils::tail, 1), tos_lens)
  
    ## make a combined from|to column
    y[, "from|to"] <- paste2(y[, c("from", "to")], sep=qsep)

    ## add id column
    y[, "id"] <- 1:nrow(y)

    ## get aggregated values iterating through rows
    ## sum wc, max(id),  prop_wc
    list_polarity <- lapply(1:nrow(y), function(i) {
        agg_pol(y[1:i, , drop=FALSE])
    })

    ## combine into a dataframe by turn of talk
    df_polarity <- list_df2df(list_polarity, "turn")

    ## set up color gradients
    colfunc <- grDevices::colorRampPalette(c(negative, neutral, positive))
    cols <- colfunc(max.color.breaks)
   
    ## add colors to df_polarity based on agrgegated 
    ## average polarity per edge
    cuts <- cut(df_polarity[, "polarity"], brks)

    df_polarity[, "color"] <- cuts %l% data.frame(cut(brks, brks), cols, 
        stringsAsFactors = FALSE)

    ## split it back into the iterative per row 
    ## dataframes of aggregated values
    list_polarity <- lapply(split(df_polarity[, -1], df_polarity[, 1]), 
        function(x) {
            y <- colsplit2df(x, sep=qsep)
            colnames(y)[1:2] <- c("from", "to")
            y
    })

    ## create a single network plot with all values
    dat <- sentCombine(y[, "text.var"], y[, "from"])
    theplot <- discourse_map(dat[, "text.var"], dat[, "from"], 
        ...)[["plot"]]

    ## generate edge constant of needed
    if (missing(edge.constant)) {
        edge.constant <- length(unique(y[, 1])) * 2.5
    }

    ## Add colors from the aggregated list of average polarities
    ## and output a corresponding list of network plots
    new_pol_nets <- lapply(list_polarity, colorize, theplot)

    ## Add edge weights etc to each graph
    igraph_objs <- stats::setNames(lapply(seq_along(new_pol_nets), 
        function(i, grp =new_pol_nets, len=length(unique(y[, 1])), sep=qsep){

        ## limit the edge weights (widths) of first 5 plots)
        if (i %in% 1:5) {
            edge.constant <- edge.constant/(len/i)
        }

        ## calculate edge widths
        cur <- list_polarity[[i]]
        cur[, "width"] <- edge.constant*cur[, "prop_wc"]

        ## get current edge
        cur_edge <- which.max(cur[, "id"])
        cur_edge2 <- max(cur[, "id"])

        ## create current edge label and polarity sign
        cur_pol <- y[y[, "id"] == cur_edge2, "ave.polarity"]
        symb <- ifelse(cur_pol == 0, "", ifelse(cur_pol < 0, "-", "+"))
        lab <- numbformat(cur_pol, digits)
        lab <- ifelse(symb == "", "0", sprintf("%s (%s)", lab, symb))
        E(grp[[i]])$label <- NA
        curkey <- data.frame(paste2(cur[cur_edge, 1:2], sep="|-|qdap|-|"), lab, 
            stringsAsFactors = FALSE)

        ## Set up widths and colors
        tcols <- cur[, c("from", "to", "color"), drop=FALSE]
        widths <- cur[, c("from", "to", "width"), drop=FALSE]
        widths[, "width"] <- ceiling(widths[, "width"])
        ekey <- paste2(edge_capture(grp[[i]]), sep=sep)
        ckey <- colpaste2df(tcols, 1:2, sep = sep, keep.orig=FALSE)[, 2:1]
        wkey <- colpaste2df(widths, 1:2, sep = sep, keep.orig=FALSE)[, 2:1]
        E(grp[[i]])$width <- NAer(ekey %l% wkey, 1)
        #plot(grp[[i]], edge.curved=TRUE)
        E(grp[[i]])$color <- ekey %l% ckey
        E(grp[[i]])$label <- ekey %l% curkey
        V(grp[[i]])$frame.color <- NA
        if (!is.null(current.speaker.color)) {
            spkkey <- data.frame(as.character(cur[cur_edge, 1]), current.speaker.color, 
                stringsAsFactors = FALSE)
            V(grp[[i]])$frame.color <- V(grp[[i]])$name %l% spkkey
        }
        V(grp[[i]])$frame.color[is.na(V(grp[[i]])$frame.color)] <- non.speaker.color

        ## change edge label color
        E(grp[[i]])$label.color <- current.color
        ##ekey %l% data.frame(curkey[1, 1], current.color)
            
        grp[[i]]
    }), paste0("Turn_", pad(1:nrow(y))))

    timings <- round(exp(y2[, "wc"]/(max(y2[, "wc"])/time.constant)))
    if(wc.time) {
        igraph_objs <- rep(igraph_objs, timings)
    }

    ## starts with a blank object
    igraph_objs <- rep(igraph_objs, c(2, rep(1, length(igraph_objs) - 1)))
    len <- nchar(char2end(names(igraph_objs)[1], "_"))
    names(igraph_objs)[1] <- sprintf("turn_%s", paste(rep(0, len), collapse=""))

    uncol <- E(igraph_objs[[1]])$color
    E(igraph_objs[[1]])$color <- NA
    E(igraph_objs[[1]])$label.color <- NA
    E(igraph_objs[[1]])$label <- NA
    V(igraph_objs[[1]])$frame.color <- non.speaker.color    

    ## end with no label or frame color
    igraph_objs <- rep(igraph_objs, c(rep(1, length(igraph_objs) - 1), 2))
    E(igraph_objs[[length(igraph_objs)]])$label.color <- NA
    E(igraph_objs[[length(igraph_objs)]])$label <- NA
    V(igraph_objs[[length(igraph_objs)]])$frame.color <- non.speaker.color
    
    ## add class info
    class(igraph_objs) <- "animated_polarity"
    attributes(igraph_objs)[["title"]] <- title
    attributes(igraph_objs)[["timings"]] <- timings
    attributes(igraph_objs)[["type"]] <- "network"
    attributes(igraph_objs)[["legend"]] <- cols
    attributes(igraph_objs)[["data"]] <- list_polarity
    igraph_objs
}


Animate_polarity_bar <- function(x, wc.time = TRUE, time.constant = 1, 
    digits = 3, ave.color.line = "red", ...) {

    input <- counts(x)
    ord <- scores(x)[order(scores(x)[, "ave.polarity"]), 1]
 
    grp <- colnms1 <- colnames(input)[1]
    colnames(input)[1] <- "group"
    input[, "group"] <- factor(input[, "group"], levels = ord)
    listdat <- lapply(1:nrow(input), function(i) {
        row_dat(input[1:i, ])
    })
    thedat <- list_df2df(listdat, "row")
    rng <- range(thedat[, "ave.polarity"], na.rm=TRUE)

    theplot <- ggbar(listdat[[length(listdat)]], grp = colnms1, rng = rng)

    ggplots <- stats::setNames(lapply(seq_along(listdat), function(i, aplot=theplot) {
        listdat[[i]][, "group"] <- factor(listdat[[i]][, "group"], levels=ord)

        tot_ave_pol <- mean(listdat[[i]][, "ave.polarity"], na.rm = TRUE)
        titlepol <- numbformat(tot_ave_pol, digits)

        aplot[["labels"]][["title"]] <- paste(sprintf("Average Discourse Polarity:  %s", 
            titlepol), sprintf("%sCurrent Speaker:   %s", paste(rep(" ", 15), 
            collapse=""), input[i, 1]))

        aplot[["data"]] <- listdat[[i]]
        aplot + geom_hline(yintercept=tot_ave_pol, size=1, color=ave.color.line) 
        }), paste0("turn_", pad(1:length(listdat))))

    timings <- round(exp(input[, "wc"]/(max(input[, "wc"])/time.constant)))
    if(wc.time) {
        ggplots <- rep(ggplots, timings)
    }

    ## starts with a blank object and end match the network Animate
    theplot[["data"]][, "ave.polarity"] <- NaN
    ggplots <- unlist(list(list(theplot), ggplots, 
        ggplots[length(ggplots)]), recursive=FALSE)

    len <- nchar(char2end(names(ggplots)[1], "_"))
    names(ggplots)[1] <- sprintf("turn_%s", paste(rep(0, len), collapse=""))

    ## add class info
    class(ggplots) <- "animated_polarity"
    attributes(ggplots)[["timings"]] <- timings
    attributes(ggplots)[["type"]] <- "bar"
    attributes(ggplots)[["legend"]] <- NULL
    attributes(ggplots)[["data"]] <- listdat
    ggplots
}


row_dat <- function(input) {    
    list_df2df(lapply(split(input, input[, "group"]), function(x) {
           data.frame(wc = sum(x[, "wc"], na.rm = TRUE), 
               ave.polarity = mean(x[, "polarity"], na.rm = TRUE))
        }), "group")
}


ggbar <- function(dat, grp = grp, rng = rng) {

    padding <- diff(rng)*.1

    ggplot(dat, aes_string(x="group"))  +
        geom_hline(yintercept=0, size=1.5, color="grey50", linetype="dashed") + 
#        geom_hline(yintercept=tot_ave_pol, size=1, color=ave.color.line) + 
        geom_bar(aes_string(weight="ave.polarity")) +
        ylab("Average Polarity") + 
        xlab(paste(sapply(unlist(strsplit(grp, "&")), Caps), collapse = " ")) +
        ylim(c(rng[1] - padding, rng[2] + padding)) + theme_bw() +
        ggtitle(sprintf("Average Discourse Polarity:  %s", "")) +
        theme(axis.text.x=element_text(angle = 90, vjust = .4, hjust = 1, size=11),
            plot.title=element_text(hjust=0, size=11, color="grey60")) + 
        scale_x_discrete(drop=FALSE)

}

Animate_polarity_text <- function(x, wc.time = TRUE, time.constant = 2, 
    width,  just, coord, ...) {
  
    y <- counts(x)
    txt <- lapply(y[["text.var"]], function(x){
            paste(strwrap(x, width), collapse="\n")
        }) %>% unlist

    theplot <- ggplot2::ggplot(data.frame(x=0:1, y=0:1), ggplot2::aes(x, x, y=y)) + 
        ggplot2::geom_blank() + ggplot2::theme_bw() +
        ggplot2::theme( 
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            axis.text = ggplot2::element_blank()
        ) + 
        ggplot2::ylab(NULL) + 
        ggplot2::xlab(NULL) 

    ggplots <- lapply(txt, function(z){
        theplot + ggplot2::annotate("text", x = coord[1], 
            y = coord[2], label = z, vjust = just[2], hjust = just[1], ...)
    })

    timings <- round(exp(y[["wc"]]/(max(y[["wc"]], na.rm=TRUE)/time.constant)))

    if(wc.time) {
        ggplots <- rep(ggplots, replace_nan(timings, is.na, 1))
    }

    ## starts with a blank object and end match the network Animate
    ggplots <- unlist(list(list(theplot), ggplots, 
        list(theplot)), recursive=FALSE)

    ## add class info
    class(ggplots) <- "animated_polarity"
    attributes(ggplots)[["timings"]] <- timings
    attributes(ggplots)[["type"]] <- "text"
    attributes(ggplots)[["legend"]] <- NULL
    attributes(ggplots)[["data"]] <- NULL
    ggplots
}

#' Animate Polarity
#' 
#' \code{Animate.polarity} - Animate a \code{\link[qdap]{polarity}} object.
#' 
#' polarity Method for Animate
#' @param x A \code{\link[qdap]{polarity}} object.
#' @param negative The color to use for negative polarity.
#' @param positive The color to use for positive polarity.
#' @param neutral The color to use for neutral polarity.
#' @param edge.constant A constant to multiple edge width by.
#' @param wc.time logical.  If \code{TRUE} weights duration of frame by word 
#' count.
#' @param time.constant A constant to divide the maximum word count by.  Time
#' is calculated by `round(exp(WORD COUNT/(max(WORD COUNT)/time.constant)))`.  
#' Therefore a larger constant will make the difference between the large and 
#' small word counts greater.
#' @param title The title to apply to the animated image(s).
#' @param digits The number of digits to use in the current turn of talk 
#' polarity.
#' @param width The width to break text at if \code{type = "text"}.
#' @param current.color The color to use for the current turn of talk polarity.
#' @param current.speaker.color The color for the current speaker.
#' @param non.speaker.color The color for the speakers not currently speaking.
#' @param ave.color.line The color to use for the average color line if 
#' \code{type = "network"}.
#' @param type  Character string of either \code{"network"} (as a network 
#' plot), \code{"bar"} (as a bar plot), or \code{"text"} (as a simple 
#' colored text plot).
#' @param coord The x/y coordinate to plot the test if \code{type = "text"}.
#' @param just The \code{hjust} and \code{vjust} values to use for the text if 
#' \code{type = "text"}.
#' @param \ldots Other arguments passed to \code{\link[qdap]{discourse_map}} or
#' \code{\link[ggplot2]{annotate}} if \code{type = "text"}.
#' @note The width of edges is based on words counts on that edge until that 
#' moment divided by total number of words used until that moment.  Thicker 
#' edges tend to thin as time passes.  The actual duration the current edge 
#' stays as the \code{current.color} is based on word counts for that particular 
#' flow of dialogue divided by total dialogue (words) used.  The edge label is
#' the current polarity for that turn of talk (an aggregation of the sub 
#' sentences of the current turn of talk).  The coloring of the current edge 
#' polarity is produced at th sentence level, therefor a label may indicate a 
#' positive current turn of talk, while the coloring may indicate a negative 
#' sentences.
#' @import igraph
#' @importFrom qdapTools %l% list_df2df
#' @importFrom ggplot2 ggplot geom_hline geom_bar ylab xlab theme ggtitle theme_bw ylim element_text scale_x_discrete 
#' @export
#' @method Animate polarity
Animate.polarity <- function(x, negative = "blue", positive = "red", 
    neutral = "yellow", edge.constant, wc.time = TRUE, time.constant = 2,
    title = NULL, digits = 3, width = 65, current.color = "black", 
    current.speaker.color = NULL, non.speaker.color = NA, 
    ave.color.line = "red", type = "network", coord = c(.0, .5), just = c(.0, .5), 
    ...){

    switch(type,
        network = {
            Animate_polarity_net(x = x, negative = negative, positive = positive, 
                neutral = neutral, edge.constant = edge.constant, wc.time = wc.time, 
                time.constant = time.constant, title = title, digits = digits, 
                current.speaker.color = current.speaker.color,
                current.color = current.color, ...)
        },
        bar = {
            Animate_polarity_bar(x = x, wc.time = wc.time, 
                time.constant = time.constant, digits = digits, 
                ave.color.line = ave.color.line, ...)         
        },
        text = {
           Animate_polarity_text(x = x, wc.time = wc.time, 
               time.constant = time.constant, width = width,  
               coord = coord, just = just, ...)
        }, stop("`type` must be \"network\", \"bar\", or \"text\"")
    )
}


#' Prints an animated_polarity  Object
#' 
#' Prints an animated_polarity  object.
#' 
#' @param x The animated_polarity  object.
#' @param title The title of the plot.
#' @param layout \pkg{igraph} \code{layout} to use.
#' @param seed The seed to use in plotting the graph.
#' @param pause The length of time to pause between plots.
#' @param legend The coordinates of the legend. See 
#' \code{\link[plotrix]{color.legend}} for more information.
#' @param legend.cex character expansion factor. \code{NULL} and \code{NA} are 
#' equivalent to 1.0. See \code{\link[graphics]{mtext}} for more information.
#' @param bg The color to be used for the background of the device region. See
#' \code{\link[graphics]{par}} for more information. 
#' @param net.legend.color The text legend color for the network plot.
#' @param \ldots Other Arguments passed to \code{\link[igraph]{plot.igraph}}.
#' @import igraph
#' @importFrom plotrix color.legend
#' @method print animated_polarity 
#' @export
print.animated_polarity <- function(x, title = NULL, 
    seed = sample(1:10000, 1), layout=layout.auto, pause = 0, 
    legend = c(-.5, -1.5, .5, -1.45), legend.cex=1, bg=NULL, 
    net.legend.color = "black", ...){
    
    if (is.null(title)) {
        title <- attributes(x)[["title"]]
    }

    switch(attributes(x)[["type"]],
        network = {
            invisible(lapply(x, function(y) {
                set.seed(seed)
                graphics::par(bg = bg)
                plot.igraph(y, edge.curved=TRUE, layout=layout, ...)
                if (!is.null(title)) {
                    graphics::mtext(title, side=3)
                }
                if (!is.null(legend)) {
                    color.legend(legend[1], legend[2], legend[3], legend[4], 
                        c("Negative", "Neutral", "Positive"), attributes(x)[["legend"]], 
                        cex = legend.cex, col = net.legend.color)
                }
                if (pause > 0) Sys.sleep(pause)
            })) 
        },
        bar = {
            invisible(lapply(x, print))
        },
        text = {
            invisible(lapply(x, print))
        }, stop("`type` must be \"network\", \"bar\", or \"text\"")
    )  
   
}



#' Plots an animated_polarity  Object
#' 
#' Plots an animated_polarity  object.
#' 
#' @param x The animated_polarity  object.
#' @param \ldots Other arguments passed to \code{print.animated_polarity }.
#' @method plot animated_polarity 
#' @export
plot.animated_polarity  <- function(x, ...){ 

    print(x, ...)

}

constrain <- function(x) ((1 - (1/(1 + exp(x)))) * 2) - 1

#' Network Polarity
#' 
#' \code{Network.polarity} - Network a \code{\link[qdap]{polarity}} object.
#' 
#' polarity Method for Network
#' @param x A \code{\link[qdap]{polarity}} object.
#' @param negative The color to use for negative polarity.
#' @param positive The color to use for positive polarity.
#' @param neutral The color to use for neutral polarity.
#' @param edge.constant A constant to multiple edge width by.
#' @param title The title to apply to the Networked image(s).
#' @param digits The number of digits to use in the current turn of talk 
#' polarity.
#' @param \ldots Other arguments passed to \code{\link[qdap]{discourse_map}}.
#' @import igraph
#' @importFrom qdapTools %l% 
#' @export
#' @method Network polarity
Network.polarity <- function(x, negative = "blue", positive = "red", 
    neutral = "yellow", edge.constant, title = NULL, digits = 3, ...){

    qsep <- "|-|qdap|-|"

    brks <- c(-10:-2, seq(-1, -.6, by=.01), seq(-.5, 0, by=.001), 
        seq(.001, .5, by=.001), seq(.6, 1, by=.01), 2:10)
    max.color.breaks <- length(brks)

    y2 <- y <- counts(x)
    condlens <- rle(as.character(y[, 1]))
    y[, "temp"] <- rep(paste0("X", pad(1:length(condlens[[2]]))),
        condlens[[1]])
    y[, "ave.polarity"] <- stats::ave(y[, 3], y[, "temp"], FUN=mean)

    ## Add to  and from columns
    y <- cbind(y, from_to_End(y[, 1]))

    ## repeat last to column to match with split sentence (i.e.
    ## we don't want an edge to return to the node it leaves
    tos <- split(y[, "to"], y[, "temp"])
    tos_lens <- sapply(tos, length)
    y[, "to"] <- rep(sapply(tos, utils::tail, 1), tos_lens)
  
    ## make a combined from|to column
    y[, "from|to"] <- paste2(y[, c("from", "to")], sep=qsep)

    ## add id column
    y[, "id"] <- 1:nrow(y)

    ## get aggregated values 
    ## sum wc, max(id),  prop_wc
    the_polarity <- agg_pol(y[1:nrow(y), , drop=FALSE])

    ## set up color gradients
    colfunc <- grDevices::colorRampPalette(c(negative, neutral, positive))
    cols <- colfunc(max.color.breaks)
   
    ## add colors to df_polarity based on agrgegated 
    ## average polarity per edge
    cuts <- cut(the_polarity[, "polarity"], brks)

    the_polarity[, "color"] <- cuts %l% data.frame(cut(brks, brks), cols, 
        stringsAsFactors = FALSE)

    ## split it back into the from --> to
    ## dataframes of aggregated values
    thedat <- splitter(the_polarity, sep=qsep)

    ## create a single network plot with all values
    dat <- sentCombine(y[, "text.var"], y[, "from"])
    theplot <- discourse_map(dat[, "text.var"], dat[, "from"], 
        ...)[["plot"]]

    ## generate edge constant of needed
    if (missing(edge.constant)) {
        edge.constant <- length(unique(y[, 1])) * 2.5
    }

    ## Add colors from the aggregated list of average polarities
    ## and output a corresponding list of network plots
    new_pol_net <- colorize(thedat, theplot)
 
    ## calculate edge widths
    thedat[, "width"] <- edge.constant*thedat[, "prop_wc"]

    ## get current edge
    cur_edge <- which.max(thedat[, "id"])
    cur_edge2 <- max(thedat[, "id"])

    ## create current edge label and polarity sign
    lab <- numbformat(thedat[, "polarity"], digits)
    curkey <- data.frame(paste2(thedat[, c("from", "to")], sep=qsep), lab, 
        stringsAsFactors = FALSE)

    ## Set up widths and colors
    tcols <- thedat[, c("from", "to", "color"), drop=FALSE]
    widths <- thedat[, c("from", "to", "width"), drop=FALSE]
    widths[, "width"] <- ceiling(widths[, "width"])
    ekey <- paste2(edge_capture(theplot), sep=qsep)
    ckey <- colpaste2df(tcols, 1:2, sep = qsep, keep.orig=FALSE)[, 2:1]
    wkey <- colpaste2df(widths, 1:2, sep = qsep, keep.orig=FALSE)[, 2:1]
    E(theplot)$width <- NAer(ekey %l% wkey, 1)
    #plot(grp[[i]], edge.curved=TRUE)
    E(theplot)$color <- ekey %l% ckey
    E(theplot)$label <- ekey %l% curkey
    
    ## add class info
    class(theplot) <- c("Network", class(theplot))
    attributes(theplot)[["title"]] <- title
    attributes(theplot)[["legend.gradient"]] <- cols
    attributes(theplot)[["network.type"]] <- "polarity"
    attributes(theplot)[["legend.label"]] <- c("Negative", "Neutral", "Positive")
    attributes(theplot)[["n.color.breaks"]] <- max.color.breaks
    attributes(theplot)[["color.locs"]] <- as.numeric(cuts)    
    theplot
}

splitter <- function(x, sep) {
        y <- colsplit2df(x, sep=sep)
        colnames(y)[1:2] <- c("from", "to")
        y
}

## function to added color to edges in network plot
colorize <- function(x, y) {
    E(y)$color <- paste2(edge_capture(y), sep="|-|qdap|-|") %l%
        data.frame(edge=paste2(x[, 1:2], sep="|-|qdap|-|"), cols=x[, "color"], 
            stringsAsFactors = FALSE)
    y
}


#' \code{cumulative.polarity} - Generate polarity over time (duration in 
#' sentences).
#' @rdname cumulative
#' @export
#' @method cumulative polarity
cumulative.polarity <- function(x, ...){
    
    keeps <- !is.na(counts(x)[["polarity"]])
    y <- counts(x)[["polarity"]][keeps]

    out <- list(cumulative_average_polarity = cummean(y))

    class(out) <- "cumulative_polarity"
    attributes(out)[["constrained"]] <- attributes(x)[["constrained"]]
    out

}

cummean <- function(x){
    cumsum(x)/seq_along(x)
}

#' Plots a cumulative_polarity Object
#' 
#' Plots a cumulative_polarity object.
#' 
#' @param x The cumulative_polarity object.
#' @param \ldots ignored
#' @method plot cumulative_polarity 
#' @export
plot.cumulative_polarity <- function(x, ...){

    len <- length(x[[1]])
    cumpolarity <- data.frame(cum_mean = x[[1]], Time = 1:len, drop=TRUE) 

    ggplot2::ggplot() + ggplot2::theme_bw() +
        ggplot2::geom_smooth(data = cumpolarity, ggplot2::aes_string(y="cum_mean", 
            x = "Time")) +
        ggplot2::geom_hline(y=mean(x[[1]]), color="grey30", size=1, alpha=.3, linetype=2) + 
        ggplot2::annotate("text", x = len/2, y = mean(x[[1]]), color="grey30", 
            label = "Average Polarity", vjust = .3, size=4) +
        ggplot2::geom_line(data = cumpolarity, ggplot2::aes_string(y="cum_mean", 
            x = "Time"), size=1) +
        ggplot2::ylab("Cumulative Average Polarity") + 
        ggplot2::xlab("Duration") +
        ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, len))

}

#' Prints a cumulative_polarity Object
#' 
#' Prints a cumulative_polarity  object.
#' 
#' @param x The cumulative_polarity object.
#' @param \ldots ignored
#' @method print cumulative_polarity
#' @export
print.cumulative_polarity <- function(x, ...) {
    print(plot.cumulative_polarity(x, ...))
}

#' \code{cumulative.animated_polarity} - Generate animated polarity over time 
#' (duration in sentences).
#' @rdname cumulative
#' @export
#' @method cumulative animated_polarity
cumulative.animated_polarity <- function(x, ...) {

    if(attributes(x)[["network"]]) {
        stop("Output must be from an `Animate.polarity` when `network = FALSE`")
    }

    out <- c(0, unlist(lapply(x, grab_ave_polarity), use.names = FALSE))
    avepol <- utils::tail(out, 1)
    len <- length(out)
    
    output <- data.frame(cum_mean = out, Time = 1:len, drop=TRUE) 

    class(output) <- c("cumulative_animated_polarity", class(output))
    attributes(output)[["length"]] <- len
    attributes(output)[["average.polarity"]] <- avepol    
    attributes(output)[["range"]] <- x[[1]][["scales"]][["scales"]][[1]][["limits"]]  
    output
}

#' Plots a cumulative_animated_polarity Object
#' 
#' Plots a cumulative_animated_polarity object.
#' 
#' @param x The cumulative_animated_polarity object.
#' @param \ldots ignored
#' @method plot cumulative_animated_polarity 
#' @export
plot.cumulative_animated_polarity <- function(x, ...){
   
    output <- lapply(1:nrow(x), function(i) {

        ggplot2::ggplot() + ggplot2::theme_bw() +
            ggplot2::geom_line(data = x[1:i, ,drop=FALSE], ggplot2::aes_string(y="cum_mean", 
                x = "Time"), size=1) +
            ggplot2::geom_hline(yintercept=0, size=1.5, color="grey50", linetype="dashed") + 
            ggplot2::geom_hline(y=attributes(x)[["average.polarity"]], 
                color="grey30", size=1, alpha=.3) + 
            ggplot2::ylab("Cumulative Average Polarity") + 
            ggplot2::xlab("Duration") +
            ggplot2::scale_x_continuous(expand = c(0, 0), 
                limits = c(0, attributes(x)[["length"]])) +
            ggplot2::ylim(range(x[["cum_mean"]])) +
            ggplot2::annotate("point", y = x[i, "cum_mean"], 
                x =x[i, "Time"], colour = "red", size = 1.5) 
    })

    output[[1]][["layers"]][[4]][["geom_params"]][["colour"]] <- NA
    output[[length(output)]] <- output[[length(output)]] + 
        ggplot2::geom_smooth(data = x, 
            ggplot2::aes_string(y="cum_mean", x = "Time")) 

    output
}

#' Prints a cumulative_animated_polarity Object
#' 
#' Prints a cumulative_animated_polarity  object.
#' 
#' @param x The cumulative_animated_polarity object.
#' @param \ldots ignored
#' @method print cumulative_animated_polarity
#' @export
print.cumulative_animated_polarity <- function(x, ...) {
    print(plot.cumulative_animated_polarity(x, ...))
}

grab_ave_polarity <- function(x, left="Average Discourse Polarity:", 
    right = "Current Speaker:") {
    
    genXtract(x[["labels"]][["title"]], left, right) %>% 
    Trim() %>% 
    as.numeric() 
}
