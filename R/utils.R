## rbinds and fills empties with zero
## http://stackoverflow.com/a/19804707/1000343
rbind_qdap <- function(x) {
    allnames <- unique(unlist(lapply(x, names)))
    data.frame(do.call(rbind, lapply(x, function(df) {
        not <- allnames[!allnames %in% names(df)]
        df[, not] <- 0
        df
    })), row.names = NULL)

}


## Chosing colour pallette that matches ggplot2's default colour pallette
## Compliments of John Colby
## http://stackoverflow.com/a/8197703/1000343
gg_color_hue <- function(n) {
    hues <- seq(15, 375, length=n+1)
    hcl(h=hues, l=65, c=100)[1:n]
}
