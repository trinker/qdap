
#==========
#staticdocs
#==========
#packages
library(highlight); library(qdap); library(staticdocs); library(acc.roxygen2)

#STEP 1: create static doc  
#right now examples are FALSE in the future this will be true
#in the future qdap2 will be the go to source
build_package(package="C:/Users/trinker/GitHub/qdap", 
    base_path="C:/Users/trinker/Desktop/qdap/", examples = FALSE)

#STEP 2: reshape index
path <- "C:/Users/trinker/Desktop/qdap"
path2 <- paste0(path, "/index.html")
rdme <- "C:/Users/trinker/GitHub/qdap/inst/extra_statdoc/readme.R"
extras <- qcv(right.just, coleman_liau, flesch_kincaid, fry, 
    linsear_write, SMOG, syn, mgsub, adjmat, wc, char.table, wfdf)
expand_statdoc(path2, to.icon = extras, readme = rdme)

#STEP 3: move to trinker.guthub
file <- "C:/Users/trinker/GitHub/trinker.github.com/"
delete(paste0(file, "qdap"))
file.copy(path, file, TRUE, TRUE)
delete(path)