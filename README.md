# qdap
====
![qdapicon](https://dl.dropbox.com/u/61803503/qdapicon.png)   
qdap (Quantitative Discourse Analysis Package) is an R package designed to assist in quantitative discourse analysis.  The package stands as a bridge between qualitative transcripts of dialogue and statistical analysis and visualization.

## Installation

Currently there isn't a release on [CRAN](http://cran.r-project.org/).


You can, however, download the [zip ball](https://github.com/trinker/qdap/zipball/master) or [tar ball](https://github.com/trinker/qdap/tarball/master), decompress and run `R CMD INSTALL` on it, or use the **devtools** package to install the development version:

```r
# install.packages("devtools")

library(devtools)
install_github("qdap", "trinker")
```

Note: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) and [devtools](http://CRAN.R-project.org/package=devtools) to install this way.

Note: Mac users must install `openNLP` before attempting to install `qdap`
```r
install.packages("openNLP", type = "source")
```
For a variety of qdap help files and videos [click here](https://github.com/trinker/qdap/wiki).
