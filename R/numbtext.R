#helper function used in cm_df.transcript (not exported)
numbtext <-
function(text.var, width=80, txt.file = NULL, 
    indent = 4, lengths = NULL, name=NULL, skip = TRUE, space = 2) {
    if (is.null(lengths)){
        lengths <- 1:length(text.var)
    }
    char <- ifelse(skip, "\n", "")
    zz <- matrix(c(lengths, as.character(text.var)), 
        nrow=2, byrow=TRUE)
    OW <- options()$width
    options(width=width)
    ind <- paste(rep(" ", indent), collapse = "")
    dimnames(zz) <- list(c(rep(ind, nrow(zz))), c(rep("", ncol(zz))))
    if(!is.null(name)) {
        message(paste0("\n", name, ":"))
    }
    print(zz, quote = FALSE)
    if (!is.null(txt.file)){
        if(!is.null(name)) {
            cat(paste0(char, name, sprintf(":%s", paste(rep("\n", space), 
                collapse = ""))), file=txt.file, append = TRUE)
        }
        sink(file=txt.file, append = TRUE)
        print(zz, quote = FALSE)
        sink()
    }
    options(width=OW)
} 
