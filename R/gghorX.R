gghorX <-
function(ggplot.object){    
    guide_grid_orig <- ggplot2:::guide_grid                                 
    guide_grid_no_hline <- function(theme, x.minor, x.major, 
        y.minor, y.major) {
        x.minor <- setdiff(x.minor, x.major)              
        y.minor <- setdiff(y.minor, y.major)                                                                              
        ggname("grill", grobTree(                          
            theme_render(theme, "panel.background"),   
            if (length(x.minor) > 0) theme_render(       
                theme, "panel.grid.minor", name = "x",           
                x = rep(x.minor, each=2), y = rep(0:1, length(x.minor)), 
                id.lengths = rep(2, length(x.minor))                  
            ),                                               
            if (length(x.major) > 0) theme_render(              
                theme, "panel.grid.major", name = "x",           
                x = rep(x.major, each=2), y = rep(0:1, length(x.major)),
                id.lengths = rep(2, length(x.major))                  
            )                                                     
        ))                                                         
    }                                                          
    environment(guide_grid_no_hline) <- environment(ggplot2:::guide_grid)
    assignInNamespace("guide_grid", guide_grid_no_hline, ns="ggplot2") 
    print(ggplot.object)             
    assignInNamespace("guide_grid", guide_grid_orig, ns="ggplot2") 
}
