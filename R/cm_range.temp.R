cm_range.temp <- 
function(codes) {
    cat(paste("list(\n",
        paste0("    ", paste0("\b", paste(codes, 
            collapse = " = c(),\n    "), " = c()")), "\n)\n")
    )
    x <- matrix(c("list(", 
        paste0("    ", codes[1:(length(codes)-1)], " = c(),"),
        paste0("    ", codes[length(codes)], " = c()"),
        ")"), ncol = 1)
    if (Sys.info()["sysname"] == "Windows") {                
        writeClipboard(x, format = 1)                        
    }                                                        
    if (Sys.info()["sysname"] == "Darwin") {                 
        j <- pipe("pbcopy", "w")                             
        writeLines(x, con = j)                               
        close(j)                                             
    }                   
}