## ----setup, include=FALSE------------------------------------------------
# set global chunk options
library(knitr)

## ----eval=FALSE----------------------------------------------------------
#  browseVignettes(package = 'qdap')

## ----eval=FALSE----------------------------------------------------------
#  build_qdap_vignette()

## ----eval=FALSE----------------------------------------------------------
#  build_qdap_vignette(TRUE)

## ----eval=FALSE----------------------------------------------------------
#  path <- system.file("Rmd_vignette", package = "qdap")
#  path2 <- system.file("doc", package = "qdap")
#  
#  fls <- paste0("qdap_vignette", c(".Rmd", ".html", ".R"))
#  new <- file.path(path, fls)
#  old <- file.path(path2, fls)
#  
#  knitr::knit(new[1], output = old[3], tangle=TRUE)
#  knitr::knit2html(new[1], output = old[2], stylesheet=file.path(path, 'css/style.css'),
#      options=c("use_xhtml","smartypants","mathjax","highlight_code", "base64_images"))
#  file.copy(new[1], old[1], overwrite = TRUE)

## ----eval=FALSE----------------------------------------------------------
#  knitr::knit(new[1], output = old[3], tangle=TRUE)
#  file.copy(new[1], old[1], overwrite = TRUE)
#  
#  url <- paste0("https://raw.githubusercontent.com/trinker/qdap",
#      "/master/inst/Rmd_vignette/qdap_vignette.html")
#  
#  bin <- RCurl::getBinaryURL(url, ssl.verifypeer = FALSE)
#  temp <- tempdir()
#  con <- file(old[2], open = "wb")
#  writeBin(bin, con)
#  close(con)

