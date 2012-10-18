trans.venn <-
function(text.var, grouping.var, stopwords = Top200Words, 
    title = TRUE, title.font = NULL, title.color = "black", 
    title.cex = NULL, title.name = NULL, legend = TRUE, 
    legend.cex = .8, legend.location = "bottomleft", 
    legend.text.col = "black", legend.horiz = FALSE, ...) {
    if (is.list(grouping.var)) {
      m <- unlist(as.character(substitute(grouping.var))[-1])
      m <- sapply(strsplit(m, "$", fixed = TRUE), 
                  function(x) x[length(x)])
      NAME <- paste(m, collapse = "&")
    } else {
      G <- as.character(substitute(grouping.var))
      NAME <- G[length(G)]
    }
    group.var <- if (is.list(grouping.var) & length(grouping.var)>1) {
      apply(data.frame(grouping.var), 1, function(x){
        if (any(is.na(x))){
          NA
        } else {
          paste(x, collapse = ".")
        }
      }
      )
    } else {
      grouping.var
    }
    word.list <- qda(text.var, group.var, stopwords = stopwords)[["cwl"]]
    all <- unique(unlist(word.list))
    C1 <- lapply(word.list, function(x) as.numeric(all%in%x))
    Counts <- do.call(cbind, C1) 
    v <- venneuler::venneuler(Counts)
    if (title) {
        if (!is.null(title.name)){
            title.name <- title.name
        } else {
            title.name <- "Venn Diagram"
        }
    } else {
        title.name <- ""
    }
    plot(v, main = title.name, cex.main = title.cex, col.main = title.color,
        family = title.font, ...)
    if (legend) {
        cols <- col.fn(v$colors)
        par(mar = rep(0, 4), xpd = TRUE)
        legend(legend.location[1], legend.location[2], horiz = legend.horiz,
            legend = names(C1), fill = cols, cex = legend.cex, 
            text.col = legend.text.col)
        par(mar = c(5, 4, 4, 2) + 0.1, xpd = TRUE)
    }
    invisible(v)
}