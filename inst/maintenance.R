#========
# BUILD
#========
source("inst/build.R")

#==========================
# Run unit tests
#==========================
devtools::test()

#========================
# Delete manuals Rd files
#========================
mans <- file.path(getwd(), "man")
delete(file.path(mans, dir(mans)))

#========================
#staticdocs dev version
#========================
#packages
# library(devtools); install_github("trinker/qdap"); install_github("hadley/staticdocs")
# install_github("trinker/acc.roxygen2")
library(highlight); library(staticdocs)

#STEP 1: create static doc  
#right now examples are FALSE in the future this will be true
#in the future qdap2 will be the go to source
build_site(pkg="C:/Users/trinker/GitHub/qdap",launch = FALSE)
library(qdap); library(acc.roxygen2)

#STEP 2: reshape index
path <- "inst/web"
path2 <- file.path(path, "/index.html")
rdme <- "C:/Users/trinker/GitHub/qdap/inst/extra_statdoc/readme.R"
extras <- qcv(right_just, coleman_liau, flesch_kincaid, fry, 
    linsear_write, SMOG, syn, mgsub, adjmat, wc, wfdf, mcsv_w, dtm,  
    "polarity_frame", "boolean_search", "stem_words", 
    "stem2df", colpaste2df, rm_stop, sent_detect, as.wfm, "%bs%",
    Filter.wfm, weight.wfdf, weight.wfm, wfm_combine, wfm_expanded, 
    Filter.character, Filter.DocumentTermMatrix, synonyms_frame,
    syn_frame, edge_apply, end_mark_by, wfm.wfdf, wfm.character, wfm.factor,
    unbag, breaker, word_split, exclude.DocumentTermMatrix, "%ex%", 
    exclude.TermDocumentMatrix, exclude.default, exclude.list, exclude.wfm,
    lcolsplit2df, termco_d, "%sw%",
    as.wfm.Corpus, as.wfm.DocumentTermMatrix, as.wfm.TermDocumentMatrix, 
    as.wfm.data.frame, as.wfm.default, as.wfm.matrix, as.wfm.wfdf, wfm.Corpus,
    weight.word_proximity, which_misspelled, check_spelling_interactive, 
    "Text", "Text&lt;-", sent_detect_nlp)

expand_statdoc(path2, to.icon = extras, readme = rdme, 
    combine = qcv(character_table, char_table))

x <- readLines(path2)
x[grepl("<h2>Authors</h2>", x)] <- paste(c("<h2>Author</h2>", 
    rep("<h2>Contributor</h2>", 2)),
    c("Tyler W. Rinker", "Dason Kurkiewicz", "Bryan Goodrich"))

cat(paste(x, collapse="\n"), file=path2)
v_to <- file.path(path, "vignettes/qdap_vignette.html")
v_from <- "inst/Rmd_vignette/qdap_vignette.html"
file.copy(v_from, v_to, overwrite = TRUE)

#STEP 3: move to trinker.guthub
library(reports)
file <- "C:/Users/trinker/GitHub/trinker.github.com/"
incoming <- file.path(file, "qdap_dev")
delete(incoming)
file.copy(path, file, TRUE, TRUE)
file.rename(file.path(file, "web"), incoming)
## delete(path)
#==========================
#staticdocs current version
#==========================
#packages
library(highlight); library(qdap); library(staticdocs); library(acc.roxygen2)

#STEP 1: create static doc  
#right now examples are FALSE in the future this will be true
#in the future qdap2 will be the go to source
build_site(pkg="C:/Users/trinker/GitHub/qdap")

#STEP 2: reshape index
path <- "inst/web"
path2 <- paste0(path, "/index.html")
rdme <- "C:/Users/trinker/GitHub/qdap/inst/extra_statdoc/readme.R"
extras <- qcv(right_just, coleman_liau, flesch_kincaid, fry, 
    linsear_write, SMOG, syn, mgsub, adjmat, wc, wfdf, mcsv_w, dtm,  
    "polarity_frame", "boolean_search", "stem_words", 
    "stem2df", colpaste2df, rm_stop, sent_detect, as.wfm, "%bs%",
    Filter.wfm, weight.wfdf, weight.wfm, wfm_combine, wfm_expanded, 
    Filter.character, Filter.DocumentTermMatrix, synonyms_frame,
    syn_frame, edge_apply, end_mark_by, wfm.wfdf, wfm.character, wfm.factor,
    unbag, breaker, word_split, exclude.DocumentTermMatrix, "%ex%", 
    exclude.TermDocumentMatrix, exclude.default, exclude.list, exclude.wfm,
    lcolsplit2df, termco_d, "%sw%",
    as.wfm.Corpus, as.wfm.DocumentTermMatrix, as.wfm.TermDocumentMatrix, 
    as.wfm.data.frame, as.wfm.default, as.wfm.matrix, as.wfm.wfdf, wfm.Corpus,
    weight.word_proximity, which_misspelled, check_spelling_interactive, 
    "Text", "Text&lt;-", sent_detect_nlp)


expand_statdoc(path2, to.icon = extras, readme = rdme, 
    combine = qcv(character_table, char_table))


path <- "inst/web"
path2 <- file.path(path, "index.html")
x <- readLines(path2)
x[grepl("<h2>Authors</h2>", x)] <- paste(c("<h2>Author</h2>", 
    rep("<h2>Contributor</h2>", 2)),
    c("Tyler W. Rinker", "Dason Kurkiewicz", "Bryan Goodrich"))
cat(paste(x, collapse="\n"), file=path2)
v_to <- file.path(path, "vignettes/qdap_vignette.html")
v_from <- "inst/Rmd_vignette/qdap_vignette.html"
file.copy(v_from, v_to, overwrite = TRUE)

#STEP 3: move to trinker.guthub
library(reports)
file <- "C:/Users/trinker/GitHub/trinker.github.com"
incoming <- file.path(file, "qdap")
# delete(incoming); file.copy(path, file, TRUE, TRUE); file.rename(file.path(file, "web"), incoming)
#delete(path2)
## https://github.com/trinker/qdap/releases

#==========================
#move project directions
#==========================
outpdf <- paste0(getwd(), "/inst/extdata/docs/")
inpdf <- paste0(getwd(), "/inst/pdf_gen/PROJECT_WORKFLOW_GUIDE.pdf")
file.copy(inpdf, outpdf,, TRUE)
file.copy(inpdf, "C:/Users/trinker/Dropbox/Public/packages",, TRUE)

#==========================
#Check spelling
#==========================
path <- file.path(getwd(), "R")
txt <- suppressWarnings(lapply(file.path(path, dir(path)), readLines))
txt <- lapply(txt, function(x) x[substring(x, 1, 2) == "#'"])
new <- lapply(1:length(txt), function(i){
    c("\n", dir(path)[i], "=========", txt[[i]])
})
out <- paste(unlist(new), collapse="\n")
cat(out, file=file.path(path.expand("C:/Users/trinker/Desktop"), "spelling.doc"))

#==========================
#Get Examples to run
#==========================
library(acc.roxygen2)
examples(path = "C:/Users/trinker/GitHub/qdap/R/")


#==========================
# Vignette copy
#==========================
  path <- file.path("C:/Users/trinker/GitHub", "qdap_2.1.1.tar.gz")
  install.packages(path,  repos = NULL, type="source")

  browseVignettes(package = 'qdap')

knitr::knit2html("inst/Rmd_vignette/qdap_vignette.Rmd", 
    stylesheet='inst/Rmd_vignette/css/style.css')


knitr::knit2html("inst/Rmd_vignette/qdap_vignette.Rmd", 
    "inst/Rmd_vignette/qdap_vignette.html",
    stylesheet='inst/Rmd_vignette/css/style.css',
    options=c("use_xhtml","smartypants","mathjax","highlight_code", "base64_images"))

reports::delete("figure")

knitr::knit2html("vignettes/qdap_vignette.Rmd", 
    "vignettes/qdap_vignette.html")


#root <- system.file("doc/qdap_vignette.html", package = "qdap")
#root <- "C:/Users/trinker/GitHub/qdap/vignettes/qdap_vignette.html"
#new <- "C:/Users/trinker/Dropbox/Public/packages"
#file.copy(root, new, TRUE, TRUE)

root <- "vignettes/tm_package_compatibility.pdf"
new <- "C:/Users/trinker/Dropbox/Public/packages"
file.copy(root, new, TRUE, TRUE)
#==========================
# NEWS.md
#==========================
update_news()


#==========================
# NEWS new version
#==========================
x <- c("BUG FIXES", "NEW FEATURES", "MINOR FEATURES", "IMPROVEMENTS", "CHANGES")
cat(paste(x, collapse = "\n\n"), file="clipboard")

#==============================
# Copy from Current R to R_dev
#==============================
r2dev()

#========================================================

library(qdap)
library(ggplot2)
library(pacman)
library(reports)

options(repos="http://cran.rstudio.com/")

options(rstudio.markdownToHTML =
  function(inputFile, outputFile) {
    require(markdown)
    markdownToHTML(inputFile, outputFile, stylesheet=file.path(getwd(), "css/style.css"))
  }
)


r2dev <- function(pack = "qdap", rver = "3.0.1", dev = "C:/R/R-devel/library") {
    cur <- file.path(paste0("C:/R/R-", rver), "library", pack)
    if (file.exists(file.path(dev, pack))) {
        unlink(file.path(dev, pack), recursive = TRUE, force = FALSE)
    }
    file.copy(cur, dev, recursive = TRUE)
    message(paste("dev version of", pack, "updated"))
}


