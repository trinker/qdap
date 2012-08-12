#' Formality score
#' 
#' Transcript apply formality score by grouping variable(s)
#' 
#' @param text.var The text variable.
#' @param group.vars The grouping variables.  Default NULL generates formality score for all text.  Also takes a single grouping variable or a list of 1 or more grouping variables.
#' @param plot logical. Provides a visualization for the results
#' @param sort.by.formality logical.  If TURE orders the results by formality score.
#' @param digits The number of digits displayed
#' @param point.pch The plotting symbol.
#' @param point.cex  The plotting symbol size.
#' @param point.colors A vector of colors (length of two) to plot word count and formality score
#' @param bar.colors A palette of colors to supply to the bars in the visualization
#' @param min.wrdcnt A minimum word count threshold that must be achieved to be considered in the results.  Default includes all subgroups.
#' @return returns a bject of the class "word.list".  This is a list of 5 word lists: complete word list (raw words; "cwl"), stop word list (same as rwl with stop words removed; "swl"), frequency word list (a data frame of words and correspnding frequency counts; "fwl"), fequency stopword word list (same as fwl but with stopwords removed; "fswl") and reduced frequency stopword word list (same as fswl but truncated to n rows; "rfswl").
#' @references Heylighen, F., & Dewaele, J.M. (2002). Variation in the contextuality of language: An empirical measure. Context in Context, Special issue of Foundations of Science, 7 (3), 293-340.
#' @keywords formality explicit
#' @examples
#' rajDEM <- key_merge(raj, raj.demographics, 'person')
#' with(rajDEM, formality(rajPOS, list(sex, died), plot=T))
#'

formality <- function(text.var, grouping.var = NULL, plot = FALSE,                   
    sort.by.formality = TRUE, digits = 2, point.pch = 20, point.cex = .5,            
    point.colors = c("gray65", "red"), bar.colors = NULL, min.wrdcnt = NULL){        
    G <- if(is.null(grouping.var)) {                                                 
             gv <- TRUE                                                              
             "all"                                                                   
         } else {                                                                    
             gv <- FALSE                                                             
             if (is.list(grouping.var)) {                                            
                 m <- unlist(as.character(substitute(grouping.var))[-1])             
                 m <- sapply(strsplit(m, "$", fixed=TRUE), function(x) x[length(x)]) 
                     paste(m, collapse="&")                                          
             } else {                                                                
                  G <- as.character(substitute(grouping.var))                        
                  G[length(G)]                                                       
             }                                                                       
         }                                                                           
    grouping.var <- if(is.null(grouping.var)){                                       
        rep("all", length(text.var))                                                 
    } else {                                                                         
    if(is.list(grouping.var) & length(grouping.var)>1) {                             
         apply(data.frame(grouping.var), 1, function(x){                             
                     if (any(is.na(x))){                                             
                         NA                                                          
                     }else{                                                          
                         paste(x, collapse = ".")                                    
                     }                                                               
                 }                                                                   
             )                                                                       
        } else {                                                                     
            unlist(grouping.var)                                                     
        }                                                                            
    }                                                                                
    if (!gv) {                                                                       
        pos.list <- pos.by(text.var = text.var,                                      
            grouping.var = grouping.var, digits = digits)                            
    } else {                                                                         
        pos.list <- suppressWarnings(pos.by(text.var = text.var,                     
            grouping.var = NULL, digits = digits))                                   
    }                                                                                
    text.var <- pos.list$text                                                        
    WOR <- word.count(text.var)                                                      
    X <- pos.list[["pos.by.freq"]]                                                   
    nameX <- rownames(X)                                                             
    X <- data.frame(X)                                                               
    xn <- nrow(X)                                                                    
    X$JI <- rep(0, xn)                                                               
    X$JK <- rep(0, xn)                                                               
    article <- function(x) {                                                         
        if (identical(x, character(0))) {                                            
            return(0)                                                                
        } else {                                                                     
            WORDS <- stopwords(x, stopwords = NULL,                                  
                unlist = FALSE, strip = TRUE)                                        
            sapply(WORDS, function(x) sum(x %in% c("the", "an", "a"),                
                na.rm = TRUE ))                                                      
        }                                                                            
    }                                                                                
    if (!gv){                                                                        
        stv <- split(text.var, grouping.var)                                         
        stv <- stv[sapply(stv, function(x) !identical(x, character(0)))]             
        articles <- unlist(lapply(stv, function(x){                                  
                sum(article(x))                                                      
            }                                                                        
        ))                                                                           
    } else {                                                                         
        articles <- sum(article(text.var))                                           
    }                                                                                
    if (!is.null(X$DT)) {                                                            
        PD <- X$DT-articles                                                          
    }                                                                                
    DF1 <- DF2 <- data.frame(                                                        
        noun = rowSums(X[, names(X) %in% c("NN", "NNS", "NNP", "NNPS",               
            "POS", "JI", "JK")]),                                                    
        adj = rowSums(cbind(X[, names(X) %in% c("CD", "JJ", "JJR", "JJS",            
            "JI", "JK")], PD)),                                                      
        prep = rowSums(X[, names(X) %in% c("IN", "RP", "TO", "JI", "JK")]),          
        articles = articles,                                                         
        pronoun = rowSums(X[, names(X) %in% c("PRP", "PRP$", "PRP.", "WDT",          
            "WP", "WP$", "WP.", "JI", "JK", "EX")]),                                 
        verb = rowSums(X[, names(X) %in% c("MD", "VB", "VBD", "VBG",                 
            "VBN", "VBP", "VBZ", "JI", "JK")]),                                      
        adverb = rowSums(X[, names(X) %in% c("RB", "RBR", "RBS", "WRB",              
            "JI", "JK")]),                                                           
        interj = rowSums(X[, names(X) %in% c("UH", "JI", "JK")]))                    
    DF1RS <- rowSums(DF1)       
    if (!gv) {                                                                       
        WOR <- sapply(split(WOR, grouping.var), sum, na.rm = TRUE)                   
    } else {                                                                         
        WOR <- sum(WOR, na.rm=TRUE)                                                  
    } 
    DF2$other <- DF1$other <- WOR - DF1RS                                                  
    DF1 <- do.call(rbind, lapply(1:nrow(DF1), function(i) 100*(DF1[i, ]/WOR[i])))  
    FOR <- (rowSums(cbind(DF1$noun, DF1$article, DF1$adj, DF1$prep)) -               
        rowSums(cbind(DF1$pronoun, DF1$verb, DF1$adverb, DF1$interj)) + 100)/2                                                                                      
    if(!gv) {                                                                        
        FOR <- data.frame(replace = X[, 1], word.count = WOR, formality = FOR)       
        colnames(FOR)[1] <- G                                                        
    } else {                                                                         
        FOR <- data.frame(replace = X[, 1], word.count = WOR,                        
            formality = FOR)                                                         
        colnames(FOR)[1] <- G                                                        
    }                                                                                
    FOR[, "formality"] <- round(FOR[, "formality"], digits = digits)                 
    if (!gv & sort.by.formality) {                                                   
        FOR <- FOR[order(-FOR$formality), ]                                          
        rownames(FOR) <- NULL                                                        
    }                                                                                
    if (!gv) {                                                                       
        prop.by <- data.frame(var=names(WOR),                                        
            word.count = WOR,                                                        
            apply(DF1, 2, round, digits = digits))                                   
        freq.by <- data.frame(var=names(WOR),                                        
            word.count = WOR, DF2)                                                   
    } else {                                                                         
        prop.by <- data.frame(var="all",                                             
            word.count = sum(WOR, na.rm = TRUE), DF1)                                
        freq.by <- data.frame(var="all",                                             
            word.count = sum(WOR, na.rm = TRUE), DF2)                                
    }                                                                                
    colnames(prop.by)[1] <- colnames(freq.by)[1] <- colnames(FOR)[1]                 
    rownames(prop.by) <- rownames(freq.by) <- NULL                                   
    o <- unclass(pos.list)                                                           
    o$form.freq.by <- freq.by                                                        
    o$form.prop.by <- prop.by                                                        
    o$formality <- FOR                                                               
    dat <- reshape(freq.by,                                                          
        direction="long",                                                            
        varying=list(c(3:11)),                                                       
        idvar= names(freq.by)[c(1:2)],                                                  
        timevar="pos",                                                               
        v.names=c("freq"),                                                           
        times =names(freq.by)[-c(1:2)])                                              
    colnames(dat)[1] <- "grouping"   
    ON <- sum(dat[, "pos"] == "other")
    dat[, "form.class"] <- c(rep(c("formal", "contectual"), 
        each = (nrow(dat) - ON)/2), rep("other", ON))      
    dat <- dat[rep(seq_len(dim(dat)[1]), dat[, 4]), -4]                              
    dat[, "pos"] <- factor(dat[, "pos"], levels=unique(dat[, "pos"]))                
    dat[, "form.class"] <- factor(dat[, "form.class"],                               
        levels=unique(dat[, "form.class"]))                                          
    row.names(dat) <- NULL                                                           
    o$pos.reshaped <- dat                                                            
    if (!is.null(min.wrdcnt)){                                                       
        dat <- dat[dat[, "word.count"] > min.wrdcnt, ,drop = TRUE]                   
        dat[, 1] <- factor(dat[, 1])                                                 
        FOR <- FOR[FOR[, "word.count"] > min.wrdcnt, ,drop = TRUE]                   
    }                                                                                
    if (plot) {                                                                      
        suppressWarnings(require(ggplot2))                                           
        suppressWarnings(require(gridExtra))                                         
        YY <- ggplot(dat, aes(grouping,  fill=form.class)) +                         
            geom_bar(position='fill') +                                              
            coord_flip() +  labs(fill=NULL) +                                        
            ylab("proportion") + xlab(G)  +                                          
            opts(title = "Percent Contextual-Formal",                                
                legend.position = 'bottom')                                          
            if (!is.null(bar.colors)) {                                              
                YY <- YY + suppressWarnings(scale_fill_brewer(palette=bar.colors))   
            }          
        dat2 <- dat[dat[, "pos"] != "other", ] 
        dat2[, "pos"] <- factor(dat2[, "pos"])
        dat2[, "form.class"] <- factor(dat2[, "form.class"])                                                          
        XX <- ggplot(data=dat2, aes(grouping,  fill=pos)) +                           
            geom_bar(position='fill') + coord_flip() +                               
            facet_grid(~form.class, scales="free", margins = TRUE) +                 
            scale_x_discrete(drop=F) +  labs(fill=NULL) +                            
            ylab("proportion") + xlab(G)  +                                          
            scale_fill_discrete(name = "", breaks=levels(dat2$pos),                   
                labels=c("noun", "adjective", "preposition",                         
                "articles", "pronoun", "verb", "adverb", "interjection")) +          
            opts(title = "Percent Parts of Speech By Contextual-Formal",             
                legend.position = 'bottom')                                          
            if (!is.null(bar.colors)) {                                              
                XX <- XX + scale_fill_brewer(palette=bar.colors,                     
                    name = "", breaks=levels(dat2$pos),                               
                    labels=c("noun", "adjective", "preposition",                     
                    "articles", "pronoun", "verb", "adverb", "interjection"))        
            }                                                                        
        names(FOR)[1] <- "grouping"                                                  
        buffer <- diff(range(FOR$formality))*.05                                     
        ZZ <- ggplot(data=FOR, aes(grouping,  formality, size=word.count)) +         
            geom_point(colour=point.colors[1]) + coord_flip()+                       
            geom_text(aes(label = word.count), vjust = 1.2, size = 3,                
                position = "identity",colour = "grey30") +                           
            labs(size="word count") +                                                
            opts(title = "F Measure (Formality)", legend.position = 'bottom') +      
            scale_y_continuous(limits=c(min(FOR$formality)-buffer,                   
                max(FOR$formality) + buffer)) +                                      
            scale_size_continuous(range = c(1, 8)) + xlab(G)  +                      
            if (point.pch == "|") {                                                  
                geom_text(aes(label = "|"), colour=point.colors[2], size=point.cex,  
                    position = "identity", hjust = .25, vjust = .25)                 
            } else {                                                                 
                geom_point(colour=point.colors[2], shape=point.pch, size=point.cex)  
            }                                                                        
            suppressWarnings(gridExtra::grid.arrange(YY, XX,                         
                ZZ, widths=c(.25, .45, .3), ncol=3))                                 
    }                                                                                
    class(o) <- "formality.measure"                                                  
    return(o)                                                                        
} 