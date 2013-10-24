#' Convert/Generate Term Document Matrix or Document Term Matrix
#' 
#' \code{tdm} - Create term document matrices from raw text or \code{wfm} for 
#' use with other text analysis packages.
#'
#' @param text.var The text variable or a \code{wfm} object.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param \ldots Other arguments passed to \code{wfm}.
#' @details Similar to the \code{tm} package's 
#' \code{\link[tm]{TermDocumentMatrix}}/\code{\link[tm]{DocumentTermMatrix}} 
#' though not identical.
#' @export
#' @importFrom reshape2 melt
#' @rdname tdm
#' @examples
#' \dontrun{
#' x <- wfm(DATA$state, DATA$person)
#' tdm(x)
#' dtm(x)
#' library(lsa)
#' lsa(tdm(x), dims=dimcalc_share())
#' lsa(tdm(DATA$state, DATA$person), dims=dimcalc_share())
#' 
#' ## Correspondence Analysis
#' library(ca)
#' 
#' dat <- pres_debates2012
#' dat <- dat[dat$person %in% qcv(ROMNEY, OBAMA), ]
#' 
#' speech <- stemmer(dat$dialogue)
#' mytable1 <- with(dat, tdm(speech, list(person, time), stopwords = Top25Words))
#' 
#' fit <- ca(mytable)
#' summary(fit)
#' plot(fit)
#' plot3d.ca(fit, labels=1)
#' 
#' 
#' mytable2 <- with(dat, tdm(speech, list(person, time), stopwords = Top200Words))
#' 
#' fit2 <- ca(mytable2)
#' summary(fit2)
#' plot(fit2)
#' plot3d.ca(fit2, labels=1)
#' }
tdm <- function(text.var, grouping.var = NULL, ...) {
    if (!is(text.var, "true.matrix")) {
        text.var <- wfm(text.var = text.var, grouping.var = grouping.var, output = "raw", ...)
    }
    d <- melt(text.var)
    colnames(d)[1:2] <- c("Terms", "Docs")
    xtabs(value ~ Terms + Docs, d)
}

#' Convert/Generate Term Document Matrix or Document Term Matrix
#' 
#' \code{dtm} - Create document term matrices from raw text or \code{wfm} for 
#' use with other text analysis packages.
#' 
#' @rdname tdm
#' @export
dtm <- function(text.var, grouping.var = NULL, ...) {
    t(tdm(text.var = text.var, grouping.var = grouping.var, ...))
}
