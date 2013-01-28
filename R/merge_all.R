## This function was intially useful to the functioning of qdap but currently 
## does not fit the workflow of a qdap user
# Merge Multiple Data Sets
# 
# Merge multiple data sets together.
# 
# @param frames Multiple dataframes to merge together.
# @param by Specifications of the common column(s).
# @param na.replace Value to replace missing values with.
# @return Returns a dataframe with multiple dataframes merged together.
# @seealso \code{\link[base]{merge}}
# @keywords multimerge
# @references \url{http://stackoverflow.com/questions/9551555/combine-a-series-of-data-frames-and-create-new-columns-for-data-in-each}
# @export
# @examples
# \dontrun{
# #Create three dataframe                                                                     
# Week_1_sheet <- read.table(text="ID Gender  DOB Absences Unexcused_Absences Lates           
# 1  1      M 1997        5                  1    14                                          
# 2  2      F 1998        4                  2     3", header=TRUE)                           
#                                                                                             
# Week_2_sheet <- read.table(text="ID Gender  DOB Absences Unexcused_Absences Lates           
# 1  1      M 1997        2                  1    10                                          
# 2  2      F 1998        8                  2     2                                          
# 3  3      M 1998        8                  2     2", header=TRUE)                           
#                                                                                             
# Week_3_sheet <- read.table(text="ID Gender  DOB Absences Unexcused_Absences Lates           
# 1  1      M 1997        2                  1    10                                          
# 2  2      F 1998        8                  2     2", header=TRUE)                           
#                                                                                             
# #Consolidate them into a list                                                               
# WEEKlist <- list(Week_1_sheet , Week_2_sheet , Week_3_sheet)                                
#                                                                                             
# names(WEEKlist) <- LETTERS[1:3]                                                             
#                                                                                             
# #change names of columns that may overlap with other data frame yet not have 
# #duplicate data 
# lapply(seq_along(WEEKlist), function(x) {                                                   
#     y <- names(WEEKlist[[x]]) #do this to avoid repeating this 3 times                      
#     names(WEEKlist[[x]]) <<- c(y[1:3], paste(y[4:length(y)], ".", x, sep=""))}              
# )  #notice the assignment to the environment                                                 
#                                                                                             
#                                                                                             
# merge_all(frames=WEEKlist, by=c('ID', 'Gender', 'DOB'))                                     
# merge_all(frames=WEEKlist, by=1:3, na.replace = 0) 
# }

merge_all <-
function(frames, by, na.replace = NA) {
    DF <- Reduce(function(x, y) {merge(x, y, by = by, all = TRUE)}, frames)
    return(NAer(DF, replace = na.replace))
}
