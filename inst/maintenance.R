#========================
# Delete manuals Rd files
#========================
mans <- file.path(getwd(), "man")
delete(file.path(mans, dir(mans)))
#========================
#staticdocs dev version
#========================
#packages
# library(devtools); install_github("qdap", "trinker"); install_github("staticdocs", "hadley")
# install_github("acc.roxygen2", "trinker")
library(highlight); library(qdap); library(staticdocs); library(acc.roxygen2)

#STEP 1: create static doc  
#right now examples are FALSE in the future this will be true
#in the future qdap2 will be the go to source
build_site(pkg="C:/Users/trinker/GitHub/qdap")

#STEP 2: reshape index
path <- "inst/web"
path2 <- file.path(path, "/index.html")
rdme <- "C:/Users/trinker/GitHub/qdap/inst/extra_statdoc/readme.R"
extras <- qcv(right_just, coleman_liau, flesch_kincaid, fry, 
    linsear_write, SMOG, syn, mgsub, adjmat, wc, wfdf, mcsv_w, dtm, "%ha%", 
    "hash_look", "%l%", "%l+%", "polarity_frame", "boolean_search", "stem_words", 
    "stem2df", colpaste2df, rm_stop, sent_detect, matrix2df, as.wfm, "%bs%",
    Filter.wfm, weight.wfdf, weight.wfm, wfm_combine, wfm_expanded, 
    dissimilarity, Filter.character, Filter.DocumentTermMatrix, synonyms_frame,
    syn_frame, edge_apply, end_mark_by)

expand_statdoc(path2, to.icon = extras, readme = rdme, 
    combine = qcv(character_table, char_table))

x <- readLines(path2)
x[grepl("<h2>Authors</h2>", x)] <- paste(c("<h2>Author</h2>", 
    rep("<h2>Contributor</h2>", 2)),
    c("Tyler W. Rinker", "Dason Kurkiewicz", "Bryan Goodrich"))

cat(paste(x, collapse="\n"), file=path2)


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
    linsear_write, SMOG, syn, mgsub, adjmat, wc, wfdf, mcsv_w, dtm, "%ha%", 
    "hash_look", "%l%", "%l+%", "polarity_frame", "boolean_search", "stem_words", 
    "stem2df", colpaste2df, rm_stop, sent_detect, matrix2df, as.wfm, "%bs%",
    Filter.wfm, weight.wfdf, weight.wfm, wfm_combine, wfm_expanded, 
    dissimilarity, Filter.character, Filter.DocumentTermMatrix,synonyms_frame,
    syn_frame, edge_apply, end_mark_by)

expand_statdoc(path2, to.icon = extras, readme = rdme, 
    combine = qcv(character_table, char_table))


path <- "inst/web"
path2 <- file.path(path, "index.html")
x <- readLines(path2)
x[grepl("<h2>Authors</h2>", x)] <- paste(c("<h2>Author</h2>", 
    rep("<h2>Contributor</h2>", 2)),
    c("Tyler W. Rinker", "Dason Kurkiewicz", "Bryan Goodrich"))
cat(paste(x, collapse="\n"), file=path2)

#STEP 3: move to trinker.guthub
library(reports)
file <- "C:/Users/trinker/GitHub/trinker.github.com"
incoming <- file.path(file, "qdap")
# delete(incoming); file.copy(path, file, TRUE, TRUE); file.rename(file.path(file, "web"), incoming)
#delete(path2)

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

#=============================
#Update Name 2 Gender Data Set
#=============================
x <- "http://www.census.gov/genealogy/www/data/1990surnames/dist.female.first"
y <- "http://www.census.gov/genealogy/www/data/1990surnames/dist.male.first"

library(qdap)
female.names <- read.table(x, stringsAsFactors = FALSE, col.names = qcv(name, per.freq, cum.freq, rank))
female.names$gender <- "F"
str(female.names)
head(female.names)

male.names <- read.table(y, stringsAsFactors = FALSE, col.names = qcv(name, per.freq, cum.freq, rank))
male.names$gender <- "M"
str(male.names)
head(male.names)

nms <- rbind(female.names, male.names)
nms$gender <- factor(nms$gender)
str(nms)
head(nms)

nms$gender2 <- as.character(nms$gender)
nms$gender2[nms$name %in% intersect(female.names[, 1], male.names[, 1])] <- "B"
like <- sapply(unique(nms$name[nms$gender2 == "B"]), function(x) {
    y <- na.omit(nms[nms$name == x, ])
    y[which.max(y[, "per.freq"]), "gender"]
})
nms$pred.sex <- lookup(nms$name, names(like), like)
nms$pred.sex[is.na(nms$pred.sex)] <- nms$gender2[is.na(nms$pred.sex)]


nms$gender2 <- factor(nms$gender2)
nms$pred.sex <- factor(nms$pred.sex)
str(nms)

NAMES <- nms
NAMES_SEX <- unique(nms[, qcv(name, gender2, pred.sex)])

y <- NAMES_SEX[order(NAMES_SEX[, 1]), ]
NAMES_LIST <- lapply(LETTERS, function(x){
    na.omit(y[x == substring(y[, 1], 1, 1), ])
})
names(NAMES_LIST) <- LETTERS

#==========================
# Vignette copy
#==========================
#path <- file.path("C:/Users/trinker/GitHub", "qdap_1.0.0.tar.gz")
#install.packages(path,  repos = NULL, type="source")

##  browseVignettes(package = 'qdap')

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
