qview <-
function(dataframe,...){
    x<-as.character(substitute(dataframe))
    cat(paste(rep("=", 72), collapse=""), "\n",  
        "n = ",nrow(dataframe),"          # of vars = ",
        ncol(dataframe), "           ", x, "\n",
        "\b", paste(rep("=", 72), collapse=""), "\n"); 
    return(head(dataframe,...))
}
