cm_range.temp <-
function(codes) {
    cat(paste("list(\n",
        paste0("    ", paste0("\b", paste(codes, 
            collapse = " = c(),\n    "), " = c()")), "\n)\n")
    )
}
