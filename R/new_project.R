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
#'     \item{01_clean_data.R}{ * intial cleaning of raw transcripts}
#'     \item{02_analysis_I.R}{ * intial analysis}
#'     \item{03_plots.R}{ * plotting script}
#'     }
#' }
#' \item{CM_DATA}{ - A directory to export/import scripts for cm_xxx family of 
#'     functions}
#' \item{CODEBOOK}{ - A directory to store coding conventions or demographics 
#'    data}
#' \item{CORRESPONDENCE}{ - A directory to store correspondence and agreements 
#'     with the client}
#' \item{DATA_CLEANED}{ - A directory to store cleaned data (generally .RData 
#'     format)}
#' \item{DATA_FOR_REVIEW}{ - A directory to put data that may need to be altered 
#'     or needs to be inspected more closely}
#' \item{DOCUMENTS}{ - A directory to store documents related tot he project}
#' \item{PLOTS}{ - A directory to store plots}
#' \item{RAW_TRANSCRIPTS}{ - A directory to store the raw transcripts}
#' \item{REPORTS}{ - A directory to house reports}
#' \item{TABLES}{ - A directory to export tables to}  
#' \item{WORD_LISTS}{ - A directory to store word lists that can be sourced and 
#'     supplied to functions}
#' \item{extra_functions.R}{ - A script to store user made functions related to the 
#'     project}
#' \item{LOG.txt}{ - A text file documenting project changes/needs etc.}
#' \item{TO_DO.txt}{ - A text file documenting project tasks}
#' }
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
    x <- invisible(folder(folder.name=paste0(path, "/", project)))
    setwd(x)
    y <- invisible(folder(ANALYSIS, CODEBOOK, DATA_CLEANED, 
        DATA_FOR_REVIEW, RAW_TRANSCRIPTS, PLOTS, TABLES, CM_DATA, 
        WORD_LISTS, REPORTS, CORRESPONDENCE, DOCUMENTS))
    cat(file=paste0(x, "/", "extra_functions.R"))
    cat(file=paste0(x, "/", "TO_DO.txt"))
    cat(paste0("Project \"", project, "\" created: ", Sys.time(), "\n"), 
        file=paste0(x, "/", "LOG.txt"))
    invisible(folder(folder.name=paste0(y[[4]], "/", "ALREADY_REVIEWED")))
    cat(paste0("library(qdap)\ndir_map(\"", 
        y[[5]], "\")\n\n\n\n", 
    "len <- length(dir(\"", y[[5]], "\"))\n",
    "L1 <- lapply(paste0(\"dat\", 1:len), function(x) get(x))\n", 
    "names(L1) <- paste0(\"dat\", 1:len)\n",
    "\n\n\n\nsave( , file = \"", y[[3]], 
        "/cleaned.RData\")\n"), file=paste0(y[[1]], "/", 
        "01_clean_data.R"))
    cat(paste0("library(qdap, ggplot2, grid, scales)\nsource(\"",
        paste0(x, "/", "extra_functions.R"), "\")\n",
        "load(\"", paste0(y[[1]], "/", "01_clean_data.R"), 
        "\")\n"), file=paste0(y[[1]], "/", "02_analysis_I.R"))
    cat(paste0("library(qdap, ggplot2, grid, scales)\nsource(\"",
        paste0(x, "/", "extra_functions.R"), "\")\n",
        "load(\"", paste0(y[[1]], "/", "01_clean_data.R"), 
        "\")\n"), file=paste0(y[[1]], "/", "03_plots.R"))
    cat(paste0("Project \"", project, "\" created:\n",
        x, "\n"))
}
