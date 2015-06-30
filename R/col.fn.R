#Currently used as a helper function in converting the colors from the venn 
#diagrams 
col.fn <-
function(col, alpha=0.3) {
    col<- grDevices::hcl(col * 360, 130, 60)
    col <- grDevices::col2rgb(col)/255
    col <- grDevices::rgb(col[1, ], col[2, ], col[3, ], alpha)
    col
}
