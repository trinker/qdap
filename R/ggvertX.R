ggvertX <-
function(ggplot.object){
    guide_grid_orig <- ggplot2:::guide_grid
    guide_grid_no_vline <- function(theme, x.minor, x.major, y.minor, 
        y.major) {
        x.minor <- setdiff(x.minor, x.major)
        y.minor <- setdiff(y.minor, y.major)
        ggname("grill", grobTree(
            theme_render(theme, "panel.background"),
            if (length(y.minor) > 0) theme_render(
                theme, "panel.grid.minor", name = "y",
                x = rep(0:1, length(y.minor)), y = rep(y.minor, each=2), 
                id.lengths = rep(2, length(y.minor))
            ),
            if(length(y.major) > 0) theme_render(
                theme, "panel.grid.major", name = "y",
                x = rep(0:1, length(y.major)), y = rep(y.major, each=2), 
                id.lengths = rep(2, length(y.major))
           )
        ))
    }
    environment(guide_grid_no_vline) <- environment(ggplot2:::guide_grid)
    assignInNamespace("guide_grid", guide_grid_no_vline, ns="ggplot2")
    print(ggplot.object) 
    assignInNamespace("guide_grid", guide_grid_orig, ns="ggplot2")
}
