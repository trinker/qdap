
#==========
#staticdocs
#==========
#packages
library(highlight); library(qdap); library(staticdocs); library(acc.roxygen2)

#STEP 1: create static doc
build_package(package="C:/Users/trinker/GitHub/qdap", 
    base_path="C:/Users/trinker/Desktop/qdap/", examples = TRUE)

#STEP 2: reshape index
path <- "C:/Users/trinker/Desktop/qdap"
path2 <- paste0(path, "/index.html")
extras <- qcv(right.just, coleman_liau, flesch_kincaid, fry, 
    linsear_write, SMOG, syn, mgsub, adjmat, wc)
expand_statdoc(path2, to.icon = extras)

#STEP 3: move to trinker.guthub
file <- "C:/Users/trinker/GitHub/trinker.github.com/qdap"
delete(file)
file.copy(path, file)
delete(path)