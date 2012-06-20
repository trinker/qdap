csv.dump <-
function(..., dir = NULL, open = FALSE){
    x <- match.call(expand.dots = FALSE)
    z <- as.character(x[[2]])
    x2 <- list(...)
    names(x2) <- z
    y <- qdap::folder(folder.name = dir)
    files <- paste0(y, "/", z, ".csv")
    lapply(seq_along(x2), function(i){
        write.table(x2[i], file = files[i],  sep = ",", col.names = T, 
        row.names=F, qmethod = "double")
        }
    )
    if(open){
        if (.Platform['OS.type'] == "windows"){
            shell.exec(y)
        } else {
            system(paste(Sys.getenv("R_BROWSER"), y))
        }
    }
}
