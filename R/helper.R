helper <-
function(data, res.col="person",start.col="start",
    end.col="end", res.colors=rainbow(40), title=NULL, box.color = "black", 
    xlab = "Duration", ylab=NA, y2 = NULL){
    op <- par("mar")
    if (is.null(y2)){
        par(mar = op + c(0,3,0,0)) 
    } else {
        par(mar = op + c(0,3,0,2.2)) 
    }
    on.exit(par(mar = c(5, 4, 4, 2) + 0.1))
    minval <- min(data[,start.col])
    maxval <- max(data[,end.col])
    res.colors <- rev(res.colors)
    resources <- sort(unique(data[,res.col]),decreasing=T)
    plot(c(minval,maxval),
       c(0.5,length(resources)+0.5),
       type="n", xlab=xlab,ylab=ylab,yaxt="n" , main = title, 
       cex.main = 1)
    axis(side=2,at=1:length(resources),labels=resources,las=1)
    if (!is.null(y2)){
        axis(side=4,at=1:length(y2),labels=rev(y2),las=1) 
    }
    for (i in 1:length(resources)) {
        yTop <- i+0.1
        yBottom <- i-0.1
        subset <- data[data[,res.col] == resources[i],]
        for(r in 1:nrow(subset)) {
            color <- res.colors[((i-1)%%length(res.colors))+1]
            start <- subset[r,start.col]
            end <- subset[r,end.col]
            rect(start,yBottom,end,yTop,col=color, border= box.color)
        }
    }
}
