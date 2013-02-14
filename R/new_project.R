#' Project Template
#' 
#' Generate a project template to increase efficiency.
#' 
#' @param project A character vector of the project name.
#' @param path The path to where the project should be created.  Default is the 
#' current working directory.
#' @details The project template includes these main directories and scripts:
#' \itemize{
#' \item{ANALYSIS}{ - A directory containing the following analysis scripts:
#' \itemize{
#'     \item{01_clean_data.R}{ * initial cleaning of raw transcripts}
#'     \item{02_analysis_I.R}{ * initial analysis}
#'     \item{03_plots.R}{ * plotting script}
#'     }
#' }
#' \item{CLEANED_TRANSCRIPTS}{ - A directory to store the cleaned transcripts (If the transcripts are already cleaned you may choose to not utilize the RAW_TRANSCRIPTS directory)}
#' \item{CM_DATA}{ - A directory to export/import scripts for cm_xxx family of functions}
#' \item{CODEBOOK}{ - A directory to store coding conventions or demographics data:
#' \itemize{
#'     \item{KEY.csv}{ * A blank template for demographic information}
#'     }  
#' }
#' \item{CORRESPONDENCE}{ - A directory to store correspondence and agreements with the client
#' \itemize{
#'     \item{CONTACT_INFO.txt}{ * A txt file to put research team members' contact information}     
#'     \item{project_directions.pdf}{ * A pdf explaining the structure of the project template}
#'     } 
#' }
#' \item{DATA}{ - A directory to store cleaned data (generally .RData format)}
#' \item{DATA_FOR_REVIEW}{ - A directory to put data that may need to be altered or needs to be inspected more closely}
#' \item{DOCUMENTS}{ - A directory to store documents related to the project}
#' \item{PLOTS}{ - A directory to store plots}
#' \item{RAW_TRANSCRIPTS}{ - A directory to store the raw transcripts}
#' \item{REPORTS}{ - A directory to house reports; contains:
#' \itemize{
#'     \item{report_1.rnw}{ * A latex rnw file for use with \href{http://yihui.name/knitr/}{knitr}}
#'     \item{project.bib}{ * A latex bibtex file}
#'     \item{preamble.tex}{ * A tex file that \code{report_1.rnw} references to generate the preamble}
#'     }
#' }
#' \item{TABLES}{ - A directory to export tables to}  
#' \item{WORD_LISTS}{ - A directory to store word lists that can be sourced and supplied to functions}
#' \item{.Rprofile}{ - Performs certan tasks such as loading libraries, data and sourcing functions upon startup in \href{http://www.rstudio.com/}{RStudio}} 
#' \item{extra_functions.R}{ - A script to store user made functions related to the project}
#' \item{LOG.txt}{ - A text file documenting project changes/needs etc.}
#' \item{xxx.Rproj}{ - A project file used by \href{http://www.rstudio.com/}{RStudio}} 
#' \item{TO_DO.txt}{ - A text file documenting project tasks}
#' }
#' 
#' The template comes with a .Rproj file and .Rstudio file.  This makes 
#' operating in \href{http://www.rstudio.com/}{RStudio} very easy.  The file can 
#' be kept on the desktop or a git application such as \href{https://github.com/}{github},
#' \href{https://bitbucket.org/}{bitbucket} or \href{https://www.dropbox.com/}{dropbox}, 
#' depending on what the client/research team is comfortable utilizing. 
#' 
#' @return Creates a project template.
#' @keywords replace
#' @export
new_project <- function(project = "new", path = getwd()) {
    WD <- getwd()
    on.exit(setwd(WD))
    if(file.exists(paste0(path, "/", project))) {
        cat(paste0("\"", paste0(path, "/", project), 
            "\" already exists:\nDo you want to overwrite?\n\n"))
        ans <- menu(c("Yes", "No")) 
        if (ans == "2") {
            stop("q_workflow aborted")
        } else {
            delete(paste0(path, "/", project))
        }
    }
    x <- suppressWarnings(invisible(folder(folder.name=paste0(path, "/", project))))
    setwd(x)
    ANALYSIS <- CODEBOOK <- DATA <- DATA_FOR_REVIEW <- NULL
    RAW_TRANSCRIPTS <- PLOTS <- TABLES <- CM_DATA <- WORD_LISTS <- NULL
    REPORTS <- CORRESPONDENCE <- DOCUMENTS <- CLEANED_TRANSCRIPTS <- NULL
    y <- invisible(folder(ANALYSIS, CODEBOOK, DATA, 
        DATA_FOR_REVIEW, RAW_TRANSCRIPTS, PLOTS, TABLES, CM_DATA, 
        WORD_LISTS, REPORTS, CORRESPONDENCE, DOCUMENTS, CLEANED_TRANSCRIPTS))
    cat(file=paste0(x, "/", "extra_functions.R"))
    cat(file=paste0(x, "/", "TO_DO.txt"))
    cat(paste0("Project \"", project, "\" created: ", Sys.time(), "\n"), 
        file=paste0(x, "/", "LOG.txt"))
    invisible(folder(folder.name=paste0(y[[4]], "/", "ALREADY_REVIEWED")))
    cat(paste0("library(qdap)\ndir_map(\"", 
        y[[13]], "\")\n\n\n\n", 
    "len <- length(dir(\"", y[[13]], "\"))\n",
    "L1 <- lapply(paste0(\"dat\", 1:len), function(x) get(x))\n", 
    "names(L1) <- paste0(\"dat\", 1:len)\n",
    "\n\n\n\nsave( , file = \"", y[[3]], 
        "/cleaned.RData\")\n"), file=paste0(y[[1]], "/", 
        "01_clean_data.R"))
    cat(paste0("library(qdap, ggplot2, grid, scales)\nsource(\"",
        paste0(x, "/", "extra_functions.R"), "\")\n",
        paste0("load(\"", y[[3]], "/cleaned.RData", "\")")),
        file=paste0(y[[1]], "/", "02_analysis_I.R"))
    cat(paste0("library(qdap, ggplot2, grid, scales)\nsource(\"",
        paste0(x, "/", "extra_functions.R"), "\")\n",
        paste0("load(\"", y[[3]], "/cleaned.RData", "\")\n"),
        paste0("setwd(\"", y[[6]], "\")\n")),
        file=paste0(y[[1]], "/", "03_plots.R"))
    root <- system.file("extdata/docs", package = "qdap")
    pdfloc <- paste0(root, "/project_directions.pdf")
    invisible(file.copy(pdfloc, y[[11]]))
    pdfloc2 <- paste0(root, "/report_1.rnw")
    invisible(file.copy(pdfloc2, y[[10]]))
    pdfloc3 <- paste0(root, "/preamble.tex")
    invisible(file.copy(pdfloc3, y[[10]]))
    pdfloc4 <- paste0(root, "/TEMP.txt")
    invisible(file.copy(pdfloc4, x))
    invisible(file.rename(paste0(x, "/TEMP.txt"), 
        paste0(x, "/",  project, ".Rproj")))
    doc1 <- system.file("CITATION", package = "qdap")
    cite <- readLines(doc1)
    cite2 <- cite[4:10]
    cite2 <- gsub("= \"", "= {", cite2)
    cite2 <- gsub("\",", "},", cite2)
    cite2[1] <- "@MANUAL{Rinker,"
    cite2[8] <- "}\n"
    cite2 <- paste(cite2, collapse="\n")
    cat(cite2, file = paste0(y[[10]], "/project.bib"))
    info <- c("PROJECT NAME: Project", 
        "ANALYST: analyst_name<analyst@email> 555-555-5555[skype: analyst_guy12]",
        paste0("PROJECT MEMBERS:\n    john doe<j.doe@email> 555-555-5555[skype: jd156]\n",
        "    jane doe<jane@email> 555-555-5555[skype: jd157]\n", 
        "    nth_member<member_nth@email> 555-555-5555[skype: nmem123]\n"),
        paste("PROJECT CREATED:", Sys.time())
    )
    info <- paste(info, collapse = "\n\n")
    cat(info, file=paste0(y[[11]], "/", "CONTACT_INFO.txt"))
    write.csv(data.frame(person=""), file=paste0(y[[2]], "/", "KEY.csv"), 
        row.names = FALSE)
    rpro <- c("#load the packages used",
        "library(ggplot2)",
        "library(reshape2)",
        "library(plyr)",
        "library(grid)",
        "library(scales)",
        "library(RColorBrewer)",
        "library(qdap)",
        "",
        "WD <- getwd()",
        "",
        "#load functions into workspace",
        "source(paste0(WD, \"/extra_functions.R\"))",
        "",
        "#load data into work space",
        "dat <- paste0(WD, \"/DATA/\", dir(paste0(WD, \"/DATA/\")))",
        "dat2 <- dat[tools::file_ext(dat) %in% c(\"txt\", \"R\", \"r\")]",
        "if (!identical(dat2, character(0))) {",              
        "try(lapply(dat2, source))",
        "}",              
        "dat <- dat[tools::file_ext(dat) == \"RData\"]",
        "if (!identical(dat, character(0))) {",
        "    lapply(dat, load)",
        "}")
    cat(paste(rpro, collapse = "\n"), file = paste0(x, "/.Rprofile"))
    cat(paste0("Project \"", project, "\" created:\n",
        x, "\n"))    
}