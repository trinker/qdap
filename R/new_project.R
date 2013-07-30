#' Project Template
#' 
#' Generate a project template to increase efficiency.
#' 
#' @param project A character vector of the project name.
#' @param path The path to where the project should be created.  Default is the 
#' current working directory.
#' @param \ldots Other arguments passed to \code{\link[reports]{new_report}}.
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
#' \item{CORRESPONDENCE}{ - A directory to store correspondence and agreements with the client:
#' \itemize{
#'     \item{CONTACT_INFO.txt}{ * A txt file to put research team members' contact information}     
#'     } 
#' }
#' \item{DATA}{ - A directory to store cleaned data (generally .RData format)}
#' \item{DATA_FOR_REVIEW}{ - A directory to put data that may need to be altered or needs to be inspected more closely}
#' \item{DOCUMENTS}{ - A directory to store documents related to the project}
#' \item{PLOTS}{ - A directory to store plots}
#' \item{PROJECT_WORKFLOW_GUIDE.pdf}{ * A pdf explaining the structure of the project template}
#' \item{RAW_DATA}{ - A directory to store non-transcript data related to the project:
#' \itemize{
#'     \item{AUDIO}{ * A directory to put audio files (or shortcuts)}     
#'     \item{FIELD_NOTES}{ * A directory to put audio files (or shortcuts)}   
#'     \item{PAPER_ARTIFACTS}{ * A directory to put paper artifacts}  
#'     \item{PHOTOGRAPHS}{ * A directory to put photographs}  
#'     \item{VIDEO}{ * A directory to put video files (or shortcuts)}  
#'     } 
#' }
#' \item{RAW_TRANSCRIPTS}{ - A directory to store the raw transcripts}
#' \item{REPORTS}{ - A directory with report and presentation related tools.  Please see the \cr \href{https://dl.dropbox.com/u/61803503/packages/REPORT_WORKFLOW_GUIDE.pdf}{REPORT_WORKFLOW_GUIDE.pdf} for more details}
#' \item{TABLES}{ - A directory to export tables to}  
#' \item{WORD_LISTS}{ - A directory to store word lists that can be sourced and supplied to functions}
#' \item{.Rprofile}{ - Performs certain tasks such as loading libraries, data and sourcing functions upon startup in \href{http://www.rstudio.com/}{RStudio}} 
#' \item{extra_functions.R}{ - A script to store user made functions related to the project
#' \itemize{
#'     \item{email}{ * A function to view, and optionally copy to the clipboard, emails for the client/lead researcher, analyst and/or other project members (information taking from ~/CORRESPONDENCE/CONTACT_INFO.txt file)}
#'     \item{todo}{ * A function to view, and optionally copy to the clipboard, non-completed tasks from the \code{TO_DO.txt} file}
#'     }
#' }
#' \item{LOG}{ - A text file documenting project changes/needs etc.}
#' \item{xxx.Rproj}{ - A project file used by \href{http://www.rstudio.com/}{RStudio}; clicking this will open the project in RStudio.} 
#' \item{TO_DO}{ - A text file documenting project tasks}
#' }
#' 
#' The template comes with a .Rproj file.  This makes operating in 
#' \href{http://www.rstudio.com/}{RStudio} very easy.  The file can be kept on 
#' the desktop or a git application such as \href{https://github.com/}{github},
#' \href{https://bitbucket.org/}{bitbucket} or \href{https://www.dropbox.com/}{dropbox}, 
#' depending on what the client/research team is comfortable utilizing. 
#' 
#' @return Creates a project template.
#' @keywords project, workflow
#' @export
#' @import reports
new_project <- function(project = "new", path = getwd(), ...) {
    WD <- getwd()
    on.exit(setwd(WD))
    if(file.exists(paste0(path, "/", project))) {
        cat(paste0("\"", paste0(path, "/", project), 
            "\" already exists:\nDo you want to overwrite?\n\n"))
        ans <- menu(c("Yes", "No")) 
        if (ans == "2") {
            stop("new_project aborted")
        } else {
            delete(paste0(path, "/", project))
        }
    }
    x <- suppressWarnings(invisible(folder(folder.name=paste0(path, "/", project))))
    setwd(x)
    ANALYSIS <- CODEBOOK <- DATA <- DATA_FOR_REVIEW <- RAW_DATA <- NULL
    RAW_TRANSCRIPTS <- PLOTS <- TABLES <- CM_DATA <- WORD_LISTS <- NULL
    CORRESPONDENCE <- DOCUMENTS <- CLEANED_TRANSCRIPTS <- NULL
    y <- invisible(folder(ANALYSIS, CODEBOOK, DATA, DATA_FOR_REVIEW, 
        RAW_TRANSCRIPTS, PLOTS, TABLES, CM_DATA, WORD_LISTS, CORRESPONDENCE, 
        DOCUMENTS, CLEANED_TRANSCRIPTS, RAW_DATA))
    todo <- paste("#when a task is complete put - in front of the item",
        "#Use hanging indent",
        "1. Task 1", sep = "\n")
    cat(todo, file=paste0(x, "/", "TO_DO"))
    cat(paste0("Project \"", project, "\" created: ", Sys.time(), "\n"), 
        file=paste0(x, "/", "LOG"))
    invisible(folder(folder.name=paste0(y[[4]], "/", "ALREADY_REVIEWED")))
    dats <- c("AUDIO", "VIDEO", "FIELD_NOTES", "INTERVIEWS", "PAPER_ARTIFACTS", 
        "PHOTOGRAPHS")
    invisible(folder(folder.name=paste0(y[[13]], "/", dats)))
    cat(paste0("library(qdap)\n",
        "dir_map(file.path(getwd(), \"CLEANED_TRANSCRIPTS\")\n\n\n\n", 
        "len <- length(dir(file.path(getwd(), \"CLEANED_TRANSCRIPTS\")))\n",
        "L1 <- lapply(paste0(\"dat\", 1:len), function(x) get(x))\n", 
        "names(L1) <- paste0(\"dat\", 1:len)\n",
        "\n\n\n\nsave( , file = file.path(getwd(), \"DATA/cleaned.RData\"))\n"), 
        file=paste0(y[[1]], "/", "01_clean_data.R"))
    cat(paste0("lapply(c(\"qdap\", \"ggplot2\", \"grid\", \"scales\"), require, character.only = T)\n", 
        "source(file.path(getwd(), \"extra_functions.R\"))\n",
        "load(file.path(getwd(), \"DATA/cleaned.RData\"))\n"),
        file=paste0(y[[1]], "/", "02_analysis_I.R"))
    cat(paste0("lapply(c(\"qdap\", \"ggplot2\", \"grid\", \"scales\"), require, character.only = T)\n",
        "source(file.path(getwd(), \"extra_functions.R\"))\n",
        "load(file.path(getwd(), \"DATA/cleaned.RData\"))\n",
        "setwd(file.path(getwd(), \"PLOTS\"))\n"),
        file=paste0(y[[1]], "/", "03_plots.R"))
    root <- system.file("extdata/docs", package = "qdap")
    pdfloc <- paste0(root, "/PROJECT_WORKFLOW_GUIDE.pdf")
    invisible(file.copy(pdfloc, x))
    pdfloc4 <- paste0(root, "/TEMP.txt")
    invisible(file.copy(pdfloc4, x))
    invisible(file.rename(paste0(x, "/TEMP.txt"), 
        paste0(x, "/",  project, ".Rproj")))
    pdfloc5 <- paste0(root, "/extra_functions.R")
    invisible(file.copy(pdfloc5, x))
    info <- c("PROJECT NAME: Project", 
        "CLIENT/LEAD RESEARCHER: lead_researcher<numero_uno@email> 555-555-5555[skype: num1]",
        "ANALYST: analyst_name<analyst@email> 555-555-5555[skype: analyst_guy12]",
        paste0("PROJECT MEMBERS:\n    john doe<j.doe@email> 555-555-5555[skype: jd156]\n",
        "    jane doe<jane@email> 555-555-5555[skype: jd157]\n", 
        "    nth_member<member_nth@email> 555-555-5555[skype: nmem123]\n"),
        paste("PROJECT CREATED:", Sys.time())
    )
    info <- paste(info, collapse = "\n\n")
    cat(info, file=paste0(y[[10]], "/", "CONTACT_INFO"))
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
        "library(reports)",              
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
        "    try(lapply(dat2, source))",
        "}",              
        "dat <- dat[tools::file_ext(dat) == \"RData\"]",
        "if (!identical(dat, character(0))) {",
        "    lapply(dat, load)",
        "}")
    cat(paste(rpro, collapse = "\n"), file = paste0(x, "/.Rprofile"))
        cat(paste(rpro, collapse = "\n"), file = paste0(x, "/.Rprofile"))
    invisible(new_report(c("REPORTS", project), ...))
    o <- paste0("Project \"", project, "\" created:\n", x, "\n") 
    class(o) <- "qdapProj"
    return(o) 
}

#' Prints a qdapProj Object
#' 
#' Prints a qdapProj object.
#' 
#' @param x The qdapProj object.
#' @param \ldots ignored
#' @method print qdapProj
#' @S3method print qdapProj
print.qdapProj <-
function(x, ...) {
    class(x) <- NULL
    cat(x)
}
