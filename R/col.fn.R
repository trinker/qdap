#Currently used as a helper function in converting the colors from the venn 
#diagrams 
col.fn <-
function(col, alpha=0.3) {
    col<- hcl(col * 360, 130, 60)
    col <- col2rgb(col)/255
    col <- rgb(col[1, ], col[2, ], col[3, ], alpha)
    col
}
