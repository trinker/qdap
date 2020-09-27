#' Project Template
#' 
#' Generate a project template to increase efficiency.
#' 
#' @param project A character vector of the project name.
#' @param path The path to where the project should be created.  Default is the 
#' current working directory.
#' @param open logical.  If \code{TRUE} the project will be opened in RStudio.  
#' The default is to test if \code{new_project} is being used in the global 
#' environment, if it is then the project directory will be opened.  
#' @param \ldots ignored.
#' @details The project template includes these main directories and scripts:
#' \itemize{
#' \item{CODEBOOK}{ - A directory to store coding conventions or demographics data:
#' \itemize{
#'     \item{KEY.csv}{ - A blank template for demographic information}
#'     }  
#' }
#' \item{CORRESPONDENCE}{ - A directory to store correspondence and agreements with the client:
#' \itemize{
#'     \item{CONTACT_INFO.txt}{ - A text file to put research team members' contact information}     
#'     } 
#' }
#' \item{DATA}{ - A directory to store data:}
#' \itemize{
#'     \item{CLEANED_TRANSCRIPTS}{ - A directory to store the cleaned transcripts (If the transcripts are already cleaned you may choose to not utilize the RAW_TRANSCRIPTS directory)}     
#'     \item{CM_DATA}{ - A directory to export/import scripts for cm_xxx family of functions} 
#'     \item{DATA_FOR_REVIEW}{ - A directory to put data that may need to be altered or needs to be inspected more closely} 
#'     \item{RAW_DATA}{ - A directory to store non-transcript data related to the project:
#'         \itemize{
#'           \item{ANALYTIC_MEMOS}{ - A directory to put audio files (or shortcuts)}     
#'           \item{AUDIO}{ - A directory to put audio files (or shortcuts)}     
#'           \item{FIELD_NOTES}{ - A directory to put audio files (or shortcuts)}   
#'           \item{PAPER_ARTIFACTS}{ - A directory to put paper artifacts}  
#'           \item{PHOTOGRAPHS}{ - A directory to put photographs}  
#'           \item{VIDEO}{ - A directory to put video files (or shortcuts)}  
#'         } 
#'     } 
#'     \item{TRANSCRIPTS}{ - A directory to put transcription data:
#'         \itemize{
#'             \item{CLEANED_TRANSCRIPTS}{ - A directory to store the cleaned transcripts (If the transcripts are already cleaned you may choose to not utilize the RAW_TRANSCRIPTS directory)}     
#'             \item{RAW_TRANSCRIPTS}{ - A directory to store the raw transcripts}
#'         } 
#'     } 
#' }
#' \item{DOCUMENTATION}{ - A directory to store documents related to the project}
#' \item{PLOTS}{ - A directory to store plots}
#' \item{REPORTS}{ - A directory with report and presentation related tools.}
#' \item{SCRIPTS}{ - A directory to store scripts; already contains the following:
#' \itemize{
#'     \item{01_clean_data.R}{ - initial cleaning of raw transcripts}
#'     \item{02_analysis_I.R}{ - initial analysis}
#'     \item{03_plots.R}{ - plotting script}
#'     }
#' }
#' \item{TABLES}{ - A directory to export tables to}  
#' \item{WORD_LISTS}{ - A directory to store word lists that can be sourced and supplied to functions}
#' \item{extra_functions.R}{ - A script to store user made functions related to the project
#' \itemize{
#'     \item{email}{ - A function to view, and optionally copy to the clipboard, emails for the client/lead researcher, analyst and/or other project members (information taking from ~/CORRESPONDENCE/CONTACT_INFO.txt file)}
#'     \item{todo}{ - A function to view, and optionally copy to the clipboard, non-completed tasks from the \code{TO_DO.txt} file}
#'     }
#' }
#' \item{LOG}{ - A text file documenting project changes/needs etc.}
#' \item{PROJECT_WORKFLOW_GUIDE.pdf}{ - A pdf explaining the structure of the project template}
#' \item{xxx.Rproj}{ - A project file used by RRtudio; clicking this will open the project in RStudio.} 
#' \item{TO_DO}{ - A text file documenting project tasks}
#' }
#' 
#' The template comes with a .Rproj file.  This makes operating in 
#' RStudio very easy.  The file can be kept on 
#' the desktop or a git application such as github,
#' bitbucket or dropbox, 
#' depending on what the client/research team is comfortable utilizing. 
#' 
#' @return Creates a project template.
#' @export
#' @importFrom tools file_ext
new_project <- function(project = "new", path = getwd(), 
    open = is.global(2),  ...) {

    ## Replace spaces in path with underscores
    project <- sub("'", "", gsub("\\s+", "_", project))

    ## get the working directory and save for later
    WD <- getwd()
    on.exit(setwd(WD))

    ## handle if the directory already exists
    if(file.exists(file.path(path, project))) {
        message(paste0("\"", paste0(path, "/", project), 
            "\" already exists:\nDo you want to overwrite?\n"))
        ans <- utils::menu(c("Yes", "No")) 
        if (ans == "2") {
            stop("new_project aborted")
        } else {
            delete(paste0(path, "/", project))
        }
    }

    ## Create the main directory and set wd to there
    x <- suppressWarnings(invisible(folder(folder.name=file.path(path, 
        project))))
    setwd(x)

    ## NULL to variables not defined
    SCRIPTS <- CODEBOOK <- DATA <- DATA_FOR_REVIEW <- RAW_DATA <- NULL
    RAW_TRANSCRIPTS <- PLOTS <- TABLES <- CM_DATA <- WORD_LISTS <- NULL
    CORRESPONDENCE <- DOCUMENTATION <- CLEANED_TRANSCRIPTS <- NULL

    ## Add level 2 folders
    y <- invisible(folder(SCRIPTS, CODEBOOK, DATA,  
        PLOTS, TABLES, WORD_LISTS, CORRESPONDENCE, 
        DOCUMENTATION))

    ## Create a to do file
    todo <- paste("#when a task is complete put - in front of the item",
        "#Use hanging indent",
        "1. Task 1", sep = "\n")
    cat(todo, file=paste0(x, "/", "TO_DO"))

    ## Create a project log
    cat(paste0("Project \"", project, "\" created: ", Sys.time(), "\n"), 
        file=paste0(x, "/", "LOG"))

    ## Subdirectories in DATA folder
    data_nms <- c("RAW_DATA", "DATA_FOR_REVIEW", "CM_DATA", 
        "TRANSCRIPTS", "CLEANED_DATA")
    datas <- invisible(folder(folder.name=file.path(y[[3]], data_nms)))

    ## Add RAW_DATA subfolders    
    dats <- c("ANALYTIC_MEMOS", "AUDIO", "VIDEO", "FIELD_NOTES", "INTERVIEWS", 
        "PAPER_ARTIFACTS", "PHOTOGRAPHS")
    invisible(folder(folder.name=file.path(datas[[1]], dats)))

    ## Add TRANSCRIPTS subfolders    
    trans <- c("RAW_TRANSCRIPTS", "CLEANED_TRANSCRIPTS")
    invisible(folder(folder.name=file.path(datas[[4]], trans)))

    ## Add directory for data already reviewed
    invisible(folder(folder.name=file.path(datas[[2]], "ALREADY_REVIEWED")))

    ## Add first script (CLEANING DATA) in SCRIPTS folder
    cat(paste0("## Load required packages\npacks <- c(\"qdap\")\n",
        "invisible(lapply(packs, library, character.only=TRUE))\n\n",
        "trans_dir <- \"DATA/TRANSCRIPTS/CLEANED_TRANSCRIPTS\"\n",
        "dir_map(trans_dir)\n\n\n\n", 
        "len <- length(dir(trans_dir))\n",
        "L1 <- lapply(paste0(\"dat\", 1:len), function(x) get(x))\n", 
        "names(L1) <- paste0(\"dat\", 1:len)\n",
        "\n\n\n\nsave( , file = \"DATA/CLEANED_DATA/cleaned.RData\")\n"), 
        file=file.path(y[[1]], "01_clean_data.R"))

    ## Add 2nd script (02_analysis_I) in SCRIPTS folder
    cat(paste0("## Load required packages\n",
        "packs <- c(\"qdap\", \"ggplot2\", \"grid\", \"scales\")\n",
        "invisible(lapply(packs, library, character.only=TRUE))\n\n",
        "source(\"extra_functions.R\")\n",
        "load(\"DATA/CLEANED_DATA/cleaned.RData\")\n"),
        file=paste0(y[[1]], "/", "02_analysis_I.R"))

    ## Add 3rd script (03_plots) in SCRIPTS folder
    cat(paste0("## Load required packages\n",
        "packs <- c(\"qdap\", \"ggplot2\", \"grid\", \"scales\")\n",
        "invisible(lapply(packs, library, character.only=TRUE))\n\n",
        "source(\"extra_functions.R\")\n",
        "load(\"DATA/CLEANED_DATA/cleaned.RData\")\n"),
        file=file.path(y[[1]], "03_plots.R"))

    ## Add Project Workflow guide PDF
    root <- system.file("extdata/docs", package = "qdap")
    pdfloc <- file.path(root, "PROJECT_WORKFLOW_GUIDE.pdf")
    invisible(file.copy(pdfloc, x))

    ## Create .rproj
    pdfloc4 <- file.path(root, "TEMP.txt")
    invisible(file.copy(pdfloc4, x))
    invisible(file.rename(file.path(x, "TEMP.txt"), 
        file.path(x, paste0(project, ".Rproj"))))

    ## Create extra_functions.R
    pdfloc5 <- file.path(root, "extra_functions.R")
    invisible(file.copy(pdfloc5, x))

    ## Create CONTACT_INFO in the CORRESPONDENCE folder
    info <- c("PROJECT NAME: Project", 
        "CLIENT/LEAD RESEARCHER: lead_researcher<numero_uno@email> 555-555-5555[skype: num1]",
        "ANALYST: analyst_name<analyst@email> 555-555-5555[skype: analyst_guy12]",
        paste0("PROJECT MEMBERS:\n    john doe<j.doe@email> 555-555-5555[skype: jd156]\n",
        "    jane doe<jane@email> 555-555-5555[skype: jd157]\n", 
        "    nth_member<member_nth@email> 555-555-5555[skype: nmem123]\n"),
        paste("PROJECT CREATED:", Sys.time())
    )
    info <- paste(info, collapse = "\n\n")
    cat(info, file=paste0(y[[7]], "/", "CONTACT_INFO"))

    ## Create a KEY.csv codebook
    utils::write.csv(data.frame(person=""), file=paste0(y[[2]], "/", "KEY.csv"), 
        row.names = FALSE)
 

    ## Create the reports folder  with `new_report`
    invisible(dir.create("REPORTS"))

    o <- paste0("Project \"", project, "\" created:\n", x, "\n") 
    class(o) <- "qdapProj"


    ## Open Project in RStudio
    if (open) {
        open_project(file.path(x, project, paste0(project, ".Rproj")))
    }    
    
    return(o) 
}

#' Prints a qdapProj Object
#' 
#' Prints a qdapProj object.
#' 
#' @param x The qdapProj object.
#' @param \ldots ignored
#' @method print qdapProj
#' @export
print.qdapProj <-
function(x, ...) {
    class(x) <- NULL
    message(x)
}

wheresRstudio <- 
function() {
    myPaths <- c("rstudio",  "~/.cabal/bin/rstudio", 
        "~/Library/Haskell/bin/rstudio", "C:\\PROGRA~1\\RStudio\\bin\\rstudio.exe",
        "C:\\RStudio\\bin\\rstudio.exe", "/Applications/RStudio.app/Contents/MacOS/RStudio")
    panloc <- Sys.which(myPaths)
    temp <- panloc[panloc != ""]
    if (identical(names(temp), character(0))) {
        ans <- readline("RStudio not installed in one of the typical locations.\n 
            Do you know where RStudio is installed? (y/n) ")
        if (ans == "y") {
                temp <- readline("Enter the (unquoted) path to RStudio: ")
        } else {
            if (ans == "n") {
                stop("RStudio not installed or not found.")
            }
        }
    } 
    short.path <- which.min(unlist(lapply(gregexpr("RStudio", temp), "[[", 1)))
    temp[short.path] 
}

open_project <- function(Rproj.loc) {
    action <- paste(wheresRstudio(), Rproj.loc)
    message("Preparing to open project!")
    try(system(action, wait = FALSE, ignore.stderr = TRUE))
}



#' Easy File Handling
#' 
#' \code{delete} - Deletes files and directories.
#' 
#' @param file The name of the file in the working directory or the path to the 
#' file to be deleted.  If \code{NULL} provides a menu of files from the working 
#' directory.
#' @param \ldots The name(s) of the folder to be created.  If both \ldots and
#' \code{folder.name} are \code{NULL} creates a file in the working directory 
#' with the creation date and time stamp.
#' @param folder.name A character vector of the name(s) of the folder to be 
#' created.  Default \code{NULL}  (if the \ldots  is \code{NULL} too) creates a 
#' file in the working directory with the creation date and time stamp.  Use 
#' this argument only if the directory names contain spaces.
#' @return \code{delete} permanently removes a file/directory.
#' @seealso  \code{\link[base]{unlink}}, 
#' \code{\link[base]{file.remove}}, 
#' \code{\link[base]{dir.create}}
#' @rdname file_handling
#' @export
#' @examples
#' \dontrun{
#' (x <- folder("DELETE.ME"))
#' which(dir() == "DELETE.ME")
#' delete("DELETE.ME")
#' which(dir() == "DELETE.ME")
#' 
#' folder("the/next/big/thing", "hello world", "now/is/the/time")
#' 
#' folder(cat, dog)
#' lapply(c("cat", "dog"), delete)
#' }
delete <-
function(file = NULL) {
    x <- if (is.null(file)) {
        utils::menu(dir())
    } else {
        file
    }
    unlink(x, recursive = TRUE, force = FALSE)
}

#' Create Folder
#' 
#' \code{folder} - Create a folder/directory.
#' 
#' @return \code{folder} creates a folder/directory.
#' @rdname file_handling
#' @export
folder <- function(..., folder.name = NULL) {
    if (!is.null(folder.name)) {
        x <- strsplit(folder.name, split = ", ")
    } else {
        x <- substitute(...())
    }
    if (!is.null(x)) {
        x <- unblanker(scrubber(unlist(lapply(x, function(y) {
            as.character(y)}))))
    }
    if (is.null(x)) {
        hfolder()
    } else {
        if (length(x) == 1) {
            hfolder(x)
        } else {
            lapply(x, function(z) {
                hfolder(z)
            })
        }
    }
}

hfolder <- function(folder.name = NULL) {
    if (is.null(folder.name)) {
        FN <- mgsub(c(":", " "), c(".", "_"), 
            substr(Sys.time(), 1, 19))
    } else {
        FN <-folder.name
    }
    parts <- unlist(strsplit(FN, "/"))
    if (length(parts) == 1) {
        x <- paste(getwd(), "/", FN, sep = "")
    } else {

        ## If nested path (multiple directories created)
        if (!file.exists(dirname(FN))) {

            y <- FN
            z <- length(parts)
            for (i in rev(seq_along(parts))) {
                if(file.exists(y)) {
                    z <- z + 1
                    break
                }
                y <- dirname(paste(parts[1:i], collapse ="/"))
                z <- z - 1
            }
            
            for (i in z:(length(parts) - 1)) {
                suppressWarnings(dir.create(paste(parts[1:i], collapse ="/")))
            }
        
        }
        x <- FN
    }
    dir.create(x)
    return(x)
}

unblanker <-
function(x)subset(x, nchar(x)>0)