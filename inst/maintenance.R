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
build_package(package="C:/Users/trinker/GitHub/qdap", 
    base_path="C:/Users/trinker/Desktop/qdap_dev/", examples = FALSE)

#STEP 2: reshape index
path <- "C:/Users/trinker/Desktop/qdap_dev"
path2 <- paste0(path, "/index.html")
rdme <- "C:/Users/trinker/GitHub/qdap/inst/extra_statdoc/readme.R"
extras <- qcv(right.just, coleman_liau, flesch_kincaid, fry, 
    linsear_write, SMOG, syn, mgsub, adjmat, wc, wfdf, mcsv_w, dtm, "%ha%", 
    "hash_look", "%l%", "polarity_frame")
expand_statdoc(path2, to.icon = extras, readme = rdme, 
    combine = qcv(character.table, char.table))

#STEP 3: move to trinker.guthub
library(reports)
file <- "C:/Users/trinker/GitHub/trinker.github.com/"
delete(paste0(file, "qdap_dev"))
file.copy(path, file, TRUE, TRUE)
delete(path)
#==========================
#staticdocs current version
#==========================
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
    linsear_write, SMOG, syn, mgsub, adjmat, wc, wfdf, mcsv_w, dtm)
expand_statdoc(path2, to.icon = extras, readme = rdme, 
    combine = qcv(character.table, char.table))

#STEP 3: move to trinker.guthub
library(reports)
file <- "C:/Users/trinker/GitHub/trinker.github.com/"
delete(paste0(file, "qdap"))
file.copy(path, file, TRUE, TRUE)
delete(path)

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
root <- "C:/Users/trinker/GitHub/qdap/vignettes/qdap_vignette.html"
new <- "C:/Users/trinker/Dropbox/Public/packages"
file.copy(root, new, TRUE, TRUE)

#==========================
# NEWS.md
#==========================
News <- readLines("NEWS")
library(qdap)

News <- mgsub(
    c("<", ">", "&lt;major&gt;.&lt;minor&gt;.&lt;patch&gt;", "BUG FIXES", 
        "NEW FEATURES", "MINOR FEATURES", "CHANGES", " TRUE ", " FALSE ", 
        " NULL ", ":m:"), 
    c("&lt;", "&gt;", "<b>&lt;major&gt;.&lt;minor&gt;.&lt;patch&gt;</b>", 
        "<b>BUG FIXES</b>", "<b>NEW FEATURES</b>", "<b>MINOR FEATURES</b>", 
        "<b>CHANGES</b>", " `TRUE` ", " `FALSE` ", " `NULL` ", 
        "&#58;&#109;&#58;"), 
    News, trim = FALSE)

News <- sub(pattern="issue *# *([0-9]+)", 
    replacement="<a href=\"https://github.com/trinker/qdap/issues/\\1\">issue #\\1</a>", 
    x=News)

cat(paste(News, collapse = "\n"), file = "NEWS.md")


#==========================
# NEWS new version
#==========================
x <- c("BUG FIXES", "NEW FEATURES", "MINOR FEATURES", "CHANGES")
cat(paste(x, collapse = "\n\n"), file="clipboard")

#==============================
# Copy from Current R to R_dev
#==============================
cur <- "C:/R/R-3.0.1/library/qdap"
dev <- "C:/R/R-devel/library"
if (file.exists(file.path(dev, "qdap"))) {
    unlink(file.path(dev, "qdap"), recursive = TRUE, force = FALSE)
}
file.copy(cur, dev, recursive = TRUE)




+-