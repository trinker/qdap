library(igraph)

## Repeated Measures (EXAMPLE 1)
##------------------------------

## First merge data and map to discourse per act 
## to separate networks

dat <- key_merge(raj, raj.demographics)
list_dat <- split(dat, dat$act)
plot_dat <- lapply(list_dat, function(x) with(x, discourse_map(dialogue, person)))

opar <- par()$mar
par(mfrow=c(3, 2), mar=c(0, 0, 3, 0))

lapply(seq_along(plot_dat), function(i){
    plot(plot_dat[[i]])
    mtext(paste("Act", names(plot_dat)[i]), side=3)
})


## Repeated Measures (EXAMPLE 2)
##------------------------------

fam_key <- data.frame(fam=unique(raj.demographics$fam.aff),
    cols=qcv(blue, grey10, red, orange), 
    stringsAsFactors = FALSE)

pdf("test.pdf", width=13, height=21)
par(mfrow=c(3, 2), mar=c(0, 1, 3, 1))
lapply(seq_along(plot_dat), function(i){

    THE_PLOT <- visual(plot_dat[[i]])

    V(THE_PLOT)$sex <- V(THE_PLOT)$name %l% raj.demographics[, 1:2]
    V(THE_PLOT)$color <- ifelse(V(THE_PLOT)$sex=="f", "pink", "lightblue")
    V(THE_PLOT)$family <- V(THE_PLOT)$name %l+% raj.demographics[, c(1, 3)]
    V(THE_PLOT)$label.color <- lookup(V(THE_PLOT)$family, fam_key)

    plot(THE_PLOT, edge.curved=TRUE)
    mtext(paste("Act", names(plot_dat)[i]), side=3)
})
frame()
bords <- rep("black", 7)
bords[3] <- "white"
legend(.22, .9, c("Female", "Male", NA, as.character(fam_key[, 1])), 
    fill=c("pink", "lightblue", NA, fam_key[, 2]),  border=bords, cex=4) 

dev.off()
## Reset graphics margins
par(mar=opar)




