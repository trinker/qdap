#' Phrase Nets
#' 
#' Create Many Eyes style phrase nets.
#' 
#' @param text.var The text variable.
#' @param freq The minimum word frequency occurrence.
#' @param r The minimum correlation value
#' @param edge.constant A constant to multiple the edges by.
#' @param vertex.constant A constant to multiple the vertex label sizes by.
#' @param \ldots Other arguments passed to \code{\link[qdap]{Filter}}.
#' @return Returns an igraph object.
#' @note While Many Eyes
#' phrase nets inspired this function the two outputs are not identical.  The
#' \code{\link[qdap]{phrase_net}} function operates off of correlations between 
#' words in sentences.
#' @references http://trinker.github.io/many-eye/
#' @keywords phrase_net
#' @export
#' @importFrom reshape2 melt
#' @import igraph
#' @importFrom qdapTools lookup
#' @examples
#' \dontrun{
#' x <- "Questions must be at least 2 days old to be eligible for a bounty.
#'     There can only be 1 active bounty per question at any given time.
#'     Users must have at least 75 reputation to offer a bounty, and may
#'     only have a maximum of 3 active bounties at any given time. The
#'     bounty period lasts 7 days. Bounties must have a minimum duration of
#'     at least 1 day. After the bounty ends, there is a grace period of 24
#'     hours to manually award the bounty. If you do not award your bounty
#'     within 7 days (plus the grace period), the highest voted answer
#'     created after the bounty started with at least 2 upvotes will be
#'     awarded half the bounty amount. If there's no answer meeting that
#'     criteria, the bounty is not awarded to anyone. If the bounty was
#'     started by the question owner, and the question owner accepts an
#'     answer during the bounty period, and the bounty expires without an
#'     explicit award - we assume the bounty owner liked the answer they
#'     accepted and award it the full bounty amount at the time of bounty
#'     expiration. In any case, you will always give up the amount of
#'     reputation specified in the bounty, so if you start a bounty, be sure
#'     to follow up and award your bounty to the best answer! As an
#'     additional bonus, bounty awards are immune to the daily reputation
#'     cap and community wiki mode."
#' 
#' phrase_net(sent_detect(x), r=.5)
#' library(igraph)
#' plot(phrase_net(sent_detect(x), r=.5), edge.curved = FALSE)
#' 
#' ## Declaration of Independence Example
#' y <- readLines("http://www.constitution.org/usdeclar.txt")
#' y <- paste(y[grep("When, in the", y):length(y)], collapse=" ")
#' phrase_net(sent_detect(y), r=.7)
#' 
#' 
#' ## Multiple grouping variables
#' z <- lapply(split(raj.act.1$dialogue, raj.act.1$person), paste, collapse = " ")
#' par(mfrow=c(2, 5), mai = c(.05, 0.15, 0.15, 0.15))
#' lapply(seq_along(z), function(i) {
#'     x <- try(phrase_net(sent_detect(z[i]), r=.6))
#'     if (!inherits(x, "try-error")) {
#'         print(x)
#'         box()
#'         mtext(names(z)[i])
#'     }
#' }) 
#' 
#' 
#' lapply(seq_along(z), function(i) {
#'     x <- try(phrase_net(sent_detect(z[i]), r=.6))
#'     if (!inherits(x, "try-error")) {
#'         dev.new()
#'         print(x)
#'         mtext(names(z)[i], padj=-1, cex=1.7, col="red")
#'     }
#' }) 
#' }
phrase_net <- function(text.var, freq=4, r = .35, 
    edge.constant = 6, vertex.constant = 3,...) {

    Filtered_dat <- Filter(all_words(text.var), freq=freq, ...)
    text.var <- sentSplit(data.frame(text=text.var), "text")
    cor_dat <- word_cor(text.var[["text"]], text.var[["tot"]],
        word=Filtered_dat[[1]], r=NULL)
    
    X2 <- stats::na.omit(melt(cor_dat))
    colnames(X2)[1:2] <-c("to", "from")
    X2 <- X2[X2[, "value"] >= r, ]
    X2 <- X2[X2[, "to"] != X2[, "from"], ]
    X2[, -3] <- t(apply(X2[, -3], 1, sort))
    X2 <- unique(X2)

    g <- graph.data.frame(X2, directed=FALSE)
    E(g)$width <- X2[, "value"] * edge.constant
    E(g)$width <- E(g)$width + (1 - min(E(g)$width))
    V(g)$size <- 0
    Filtered_dat[, "vweight"] <- Filtered_dat[, 2] *
        (vertex.constant/max(Filtered_dat[, 2]))
    V(g)$label.cex <- lookup(V(g)$name, Filtered_dat[, -2])
    V(g)$label.cex <- V(g)$label.cex + (1 - min(V(g)$label.cex))
    class(g) <- c("phrase_net", class(g))
    return(g)
}


#' Prints a phrase_net Object
#' 
#' Prints a phrase_net object.
#' 
#' @param x The phrase_net object.
#' @param edge.curved logical.  If \code{TRUE} edges are plotted with curves.
#' @param \ldots Other Arguments passed to \code{\link[igraph]{plot.igraph}}.
#' @method print phrase_net
#' @export
print.phrase_net <- function(x, edge.curved = TRUE, ...) {
    plot.igraph(x, edge.curved = edge.curved, ...)
}

