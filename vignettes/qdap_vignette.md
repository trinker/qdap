<!--
%\VignetteEngine{knitr}
%\VignetteIndexEntry{A Markdown Vignette with knitr}
-->



# qdap Package Vignette
# Tyler Rinker

qdap (<a href="http://github.com/trinker/qdap">Rinker, 2013</a>) is an R package designed to assist in quantitative discourse analysis. The package stands as a bridge between qualitative transcripts of dialogue and statistical analysis and visualization.  qdap was born out of a frustration with current discourse analysis programs. Packaged programs are a closed system, meaning the researcher using the method has little, if any, influence on the program applied to her data.

R already has thousands of excellent packages for statistics and visualization. qdap is designed to stand as a bridge between the qualitative discourse of a transcript and the computational power and freedom that R offers. As qdap returns the power to the researcher it will also allow the researcher to be more efficient and thus effective and productive in data analysis.  The qdap package provides researchers with the tools to analyze data and more importantly is a dynamic system governed by the data, shaped by theory, and continuously refined by the field.

...if you can dream up an analysis then qdap and R can help get you there.

<div style="width:367.5px;margin:auto;">
    <p><img src="https://dl.dropbox.com/u/61803503/qdap_logo.png" width="350" height="250"></p>
</div>


The following vignette is a loose road map for utilizing the tools provided by qdap.  

<hr>
<h3 id="toc">Select from sections below:</h3>

<div style="float: left; width: 50%;">
<ul>
<div>1.  <a href="#project">Starting a New Project</a>
    </div> 
<div>2.  <a href="#import_export">Import/Export Discourse Data</a>
    </div> 
<div>3.  <a href="#tools">Generic qdap Tools</a>
    </div> 
<div>4.  <a href="#cleaning">Cleaning/Preparing the Data</a>
    </div> 
<div>5.  <a href="#viewing">View the Data</a>
    </div> 
<div>6.  <a href="#reshaping">Reshaping the Data</a>
    </div> 
<div>7.  <a href="#word">Extract/Analyze Words</a>
    </div> 
<div>8.  <a href="#coding">Qualitative Coding System</a>
    </div> 
<div>9.  <a href="#counts">Word Counts and Descriptive Statistics</a>
    </div> 
<div>10.  <a href="#measures">Word Measures and Scoring</a>
    </div> 
<div>11.  <a href="#visualization">Visualizing Discourse Data</a>
    </div> 
<div>12.  <a href="#id">ID Sentences</a>
    </div> 
<div>13.  <a href="#data">Data Sets</a>
    </div> 
<div>14.  <a href="#dict">Dictionaries and Word Lists</a>
    </div> 
<div>15.  <a href="#install">Installation Issues</a>
    </div> 


</ul>
</div>
<div style="float: right; width: 50%;">
<ul>
<div><b>Symbol Conventions:</b></div>  
<div><font size="5" color="gold">&diams;</font> = Example (R code)    </div> 
<div><b><font size="5" color="firebrick">[YT]</font></b> = Video Demo (click to watch)    </div> 
</ul>
</div>
<br style="clear:both;"/>




<h3 id="project">Starting a New Project <a href="http://youtu.be/u8AJiyMffmc" target="_blank" style="text-decoration: none"><b><font size="5" color="#B22222">[YT]</font></b></a>
</h3>


The following functions will be utilized in this section (click to view more):    

<form action="http://trinker.github.io/qdap_dev/new_project.html" target="_blank">
    <input type="submit" value="new_project"> - Project Template
</form>

<hr>

The function <a href="http://trinker.github.io/qdap_dev/new_project.html" target="_blank"><code>new_project</code></a>
 is designed to generate project template of multiple nested directories that organize and guide the researcher through a qualitative study, from data collection to analysis and report/presentation generation.  This workflow framework will enable the researcher to be better organized and more efficient in all stages of the research process.  <a href="http://trinker.github.io/qdap_dev/new_project.html" target="_blank"><code>new_project</code></a>
 utilizes the <a href="http://cran.r-project.org/web/packages/reports/reports.pdf" target="_blank">reports package</a>
 (<a href="http://github.com/trinker/reports">Rinker, 2013b</a>) 

Please see the following links for PDF descriptions of the contents of the <a href="http://trinker.github.io/qdap_dev/new_project.html" target="_blank"><code>new_project</code></a>
 and the reports directory. </br></br>

<div style="text-align: center;">
<table width="30%" style="text-align: center;margin: 0px auto;">
<colgroup>
<col width="110" />
<col width="110" />
</colgroup>
<tr>
<tr style="text-align: center;">
<td style="text-align: center;">Project<br> Workflow</td>
<td style="text-align: center;">Report<br> Workflow</td>
</tr>
<tr>
<td style="text-align: center; onClick="document.location.href='https://copy.com/4VekuLlUqix0CfSw/PROJECT_WORKFLOW_GUIDE.pdf?download=1';">
<a href="https://copy.com/4VekuLlUqix0CfSw/PROJECT_WORKFLOW_GUIDE.pdf?download=1';"><img src="http://drupal.org/files/project-images/Download%20Views%20PDF_2.png" width="50" height="75"><br></a>
<a href="https://copy.com/4VekuLlUqix0CfSw/PROJECT_WORKFLOW_GUIDE.pdf?download=1" target="_blank">click here</a>
<td style="text-align: center; onClick="https://copy.com/csVvdAm2vikGlkIU/REPORT_WORKFLOW_GUIDE.pdf?download=1';">
<p><a href="https://copy.com/csVvdAm2vikGlkIU/REPORT_WORKFLOW_GUIDE.pdf?download=1';"  target="_blank"><img src="http://drupal.org/files/project-images/Download%20Views%20PDF_2.png" width="50" height="75"><br></a>
<a href="https://copy.com/csVvdAm2vikGlkIU/REPORT_WORKFLOW_GUIDE.pdf?download=1" target="_blank">click here</a></p></td>
</tr>
</table>
</div>

**extra_functions** <a href="http://youtu.be/yuFyz7IW0Us" target="_blank" style="text-decoration: none"><b><font size="5" color="#B22222">[YT]</font></b></a>
 </br>    
The <a href="http://trinker.github.io/qdap_dev/new_project.html" target="_blank"><code>new_project</code></a>
 template is designed to be utilized with <a href="http://www.rstudio.com/ide/download/" target="_blank">RStudio</a>
.  Upon clicking the `xxx.Rproj` file the template will be loaded into RStudio.  The .Rprofile script will be sourced upon start up, allowing the user to automatically load packages, functions, etc. related to the project.  The file `extra_functions.R` is sourced, loading custom functions.  Already included are two functions, `email` and `todo`, used to generate project member emails and track project tasks.  This auto sourcing greatly enhances efficiency in workflow.


<h3 id="import_export">Import/Export Discourse Data</h3>

The following functions will be utilized in this section (click to view more):    

<form action="http://trinker.github.io/qdap_dev/condense.html" target="_blank">
    <input type="submit" value="condense"> - Condense Dataframe Columns
</form>

<form action="http://trinker.github.io/qdap_dev/dir_map.html" target="_blank">
    <input type="submit" value="dir_map"> - Map Transcript Files from a Directory to a Script
</form>

<form action="http://trinker.github.io/qdap_dev/mcsv_r.html" target="_blank">
    <input type="submit" value="mcsv_r"><input type="submit" value="mcsv_w"> - Read/Write Multiple csv Files at a Time
</form>

<form action="http://trinker.github.io/qdap_dev/read.transcript.html" target="_blank">
    <input type="submit" value="read.transcript"> - Read Transcripts Into R
</form>

<h3 id="tools">Generic qdap Tools</h3>

The following functions will be utilized in this section (click to view more):    

<form action="http://trinker.github.io/qdap_dev/blank2NA.html" target="_blank">
    <input type="submit" value="blank2NA"> - Replace Blanks in a dataframe
</form>

<form action="http://trinker.github.io/qdap_dev/capitalizer.html" target="_blank">
    <input type="submit" value="capitalizer"> - Capitalize Select Words
</form>

<form action="http://trinker.github.io/qdap_dev/duplicates.html" target="_blank">
    <input type="submit" value="duplicates"> - Find Duplicated Words in a Text String
</form>

<form action="http://trinker.github.io/qdap_dev/hash.html" target="_blank">
    <input type="submit" value="hash"> - Hash/Dictionary Lookup
</form>

<form action="http://trinker.github.io/qdap_dev/hms2sec.html" target="_blank">
    <input type="submit" value="hms2sec"> - Convert h:m:s to Seconds
</form>

<form action="http://trinker.github.io/qdap_dev/sec2hms.html" target="_blank">
    <input type="submit" value="sec2hms"> - Convert Seconds to h:m:s
</form>

<form action="http://trinker.github.io/qdap_dev/lookup.html" target="_blank">
    <input type="submit" value="lookup"> - Hash Table/Dictionary Lookup
</form>

<form action="http://trinker.github.io/qdap_dev/qcv.html" target="_blank">
    <input type="submit" value="qcv"> - Quick Character Vector
</form>

<form action="http://trinker.github.io/qdap_dev/replacer.html" target="_blank">
    <input type="submit" value="replacer"> - Replace Cells in a Matrix or Data Frame
</form>

<form action="http://trinker.github.io/qdap_dev/Search.html" target="_blank">
    <input type="submit" value="Search"> - Search Columns of a Data Frame
</form>

<form action="http://trinker.github.io/qdap_dev/text2color.html" target="_blank">
    <input type="submit" value="text2color"> - Map Words to Colors
</form>

<form action="http://trinker.github.io/qdap_dev/url_dl.html" target="_blank">
    <input type="submit" value="url_dl"> - Download Instructional Documents
</form>

<form action="http://trinker.github.io/qdap_dev/v.outer.html" target="_blank">
    <input type="submit" value="v.outer"> - Vectorized Version of outer
</form>



<h3 id="cleaning">Cleaning/Preparing the Data</h3>

The following functions will be utilized in this section (click to view more):    

<form action="http://trinker.github.io/qdap_dev/bracketX.html" target="_blank">
    <input type="submit" value="bracketX"><input type="submit" value="bracketXtract"><input type="submit" value="genX"><input type="submit" value="genXtract"> - <a href="#bracket">Bracket/General Chunk Extraction</a>
     
</form>

<form action="http://trinker.github.io/qdap_dev/beg2char.html" target="_blank">
    <input type="submit" value="beg2char"><input type="submit" value="char2end"> - Grab Begin/End of Sting to Character
</form>

<form action="http://trinker.github.io/qdap_dev/clean.html" target="_blank">
    <input type="submit" value="clean"> - Remove Escaped Characters
</form>

<form action="http://trinker.github.io/qdap_dev/incomplete.replace.html" target="_blank">
    <input type="submit" value="incomplete.replace"><input type="submit" value="incomp"> - Denote Incomplete End Marks With "|"
</form>

<form action="http://trinker.github.io/qdap_dev/multigsub.html" target="_blank">
    <input type="submit" value="multigsub"><input type="submit" value="mgsub"> - Multiple gsub
</form>

<form action="http://trinker.github.io/qdap_dev/name2sex.html" target="_blank">
    <input type="submit" value="name2sex"> - Names to Gender Prediction
</form>

<form action="http://trinker.github.io/qdap_dev/potential_NA.html" target="_blank">
    <input type="submit" value="potential_NA"> - Search for Potential Missing Values
</form>

<form action="http://trinker.github.io/qdap_dev/qprep.html" target="_blank">
    <input type="submit" value="qprep"> - Quick Preparation of Text
</form>

<form action="http://trinker.github.io/qdap_dev/replace_abbreviation.html" target="_blank">
    <input type="submit" value="replace_abbreviation"> - Replace Abbreviations
</form>

<form action="http://trinker.github.io/qdap_dev/replace_contraction.html" target="_blank">
    <input type="submit" value="replace_contraction"> - Replace Contractions
</form>

<form action="http://trinker.github.io/qdap_dev/replace_number.html" target="_blank">
    <input type="submit" value="replace_number"> - Replace Numbers With Text Representation
</form>

<form action="http://trinker.github.io/qdap_dev/replace_symbol.html" target="_blank">
    <input type="submit" value="replace_symbol"> - Replace Symbols With Word Equivalents
</form>

<form action="http://trinker.github.io/qdap_dev/rm_row.html" target="_blank">
    <input type="submit" value="rm_row"><input type="submit" value="rm_empty_row"> - Remove Rows That Contain Markers
</form>

<form action="http://trinker.github.io/qdap_dev/scrubber.html" target="_blank">
    <input type="submit" value="scrubber"> - Clean Imported Text
</form>

<form action="http://trinker.github.io/qdap_dev/space_fill.html" target="_blank">
    <input type="submit" value="space_fill"> - Replace Spaces
</form>

<form action="http://trinker.github.io/qdap_dev/spaste.html" target="_blank">
    <input type="submit" value="spaste"> - Add Leading/Trailing Spaces
</form>

<form action="http://trinker.github.io/qdap_dev/stemmer.html" target="_blank">
    <input type="submit" value="stemmer"><input type="submit" value="stem.words"><input type="submit" value="stem2df"> - Stem Text
</form>

<form action="http://trinker.github.io/qdap_dev/Trim.html" target="_blank">
    <input type="submit" value="Trim"> - Remove Leading/Trailing White Space
</form>
<hr>

<h4 id="bracket">Bracket/General Chunk Extraction</h4>
<font size="5" color="gold">&diams;</font> **Extracting Chunks 1**- *bracketX/bracketXtract* <font size="5" color="gold">&diams;</font>

```r
## A fake data set
examp <- structure(list(person = structure(c(1L, 2L, 1L, 3L), .Label = c("bob", 
    "greg", "sue"), class = "factor"), text = c("I love chicken [unintelligible]!", 
    "Me too! (laughter) It's so good.[interrupting]", "Yep it's awesome {reading}.", 
    "Agreed. {is so much fun}")), .Names = c("person", "text"), row.names = c(NA, 
    -4L), class = "data.frame")

examp
```

```
##   person                                           text
## 1    bob               I love chicken [unintelligible]!
## 2   greg Me too! (laughter) It's so good.[interrupting]
## 3    bob                    Yep it's awesome {reading}.
## 4    sue                       Agreed. {is so much fun}
```

```r
bracketX(examp$text, "square")
```

```
## [1] "I love chicken!"                  "Me too! (laughter) It's so good."
## [3] "Yep it's awesome {reading}."      "Agreed. {is so much fun}"
```

```r
bracketX(examp$text, "curly")
```

```
## [1] "I love chicken [unintelligible]!"              
## [2] "Me too! (laughter) It's so good.[interrupting]"
## [3] "Yep it's awesome."                             
## [4] "Agreed."
```

```r
bracketX(examp$text, c("square", "round"))
```

```
## [1] "I love chicken!"             "Me too! It's so good."      
## [3] "Yep it's awesome {reading}." "Agreed. {is so much fun}"
```

```r
bracketX(examp$text)
```

```
## [1] "I love chicken!"       "Me too! It's so good." "Yep it's awesome."    
## [4] "Agreed."
```

```r


bracketXtract(examp$text, "square")
```

```
## $square1
## [1] "unintelligible"
## 
## $square2
## [1] "interrupting"
## 
## $square3
## character(0)
## 
## $square4
## character(0)
```

```r
bracketXtract(examp$text, "curly")
```

```
## $curly1
## character(0)
## 
## $curly2
## character(0)
## 
## $curly3
## [1] "reading"
## 
## $curly4
## [1] "is so much fun"
```

```r
bracketXtract(examp$text, c("square", "round"))
```

```
## [[1]]
## [1] "unintelligible"
## 
## [[2]]
## [1] "interrupting" "laughter"    
## 
## [[3]]
## character(0)
## 
## [[4]]
## character(0)
```

```r
bracketXtract(examp$text, c("square", "round"), merge = FALSE)
```

```
## $square
## $square[[1]]
## [1] "unintelligible"
## 
## $square[[2]]
## [1] "interrupting"
## 
## $square[[3]]
## character(0)
## 
## $square[[4]]
## character(0)
## 
## 
## $round
## $round[[1]]
## character(0)
## 
## $round[[2]]
## [1] "laughter"
## 
## $round[[3]]
## character(0)
## 
## $round[[4]]
## character(0)
```

```r
bracketXtract(examp$text)
```

```
## $all1
## [1] "unintelligible"
## 
## $all2
## [1] "laughter"     "interrupting"
## 
## $all3
## [1] "reading"
## 
## $all4
## [1] "is so much fun"
```

```r
bracketXtract(examp$text, with = TRUE)
```

```
## $all1
## [1] "[unintelligible]"
## 
## $all2
## [1] "(laughter)"     "[interrupting]"
## 
## $all3
## [1] "{reading}"
## 
## $all4
## [1] "{is so much fun}"
```

```r

paste2(bracketXtract(examp$text, "curly"), " ")
```

```
## [1] "reading is so much fun"
```


The researcher may need a more general extraction method that allows for any left/right boundaries to be specified.  This is useful in that many qualitative transciption/coding programs have specific syntax for various dialogue markup for events that must be parsed from the data set.  The <a href="http://trinker.github.io/qdap_dev/genX.html" target="_blank"><code>genX</code></a>
 and <a href="http://trinker.github.io/qdap_dev/genXtract.html" target="_blank"><code>genXtract</code></a>
 functions have such capabilities.

<font size="5" color="gold">&diams;</font> **Extracting Chunks 2**- *genX/genXtract* <font size="5" color="gold">&diams;</font>

```r
DATA$state  ## Look at the difference in number 1 and 10
```

```
##  [1] "Computer is fun. Not too fun."        
##  [2] "No it's not, it's dumb."              
##  [3] "What should we do?"                   
##  [4] "You liar, it stinks!"                 
##  [5] "I am telling the truth!"              
##  [6] "How can we be certain?"               
##  [7] "There is no way."                     
##  [8] "I distrust you."                      
##  [9] "What are you talking about?"          
## [10] "Shall we move on?  Good then."        
## [11] "I'm hungry.  Let's eat.  You already?"
```

```r

genX(DATA$state, c("is", "we"), c("too", "on"))
```

```
##  [1] "Computer fun."                      
##  [2] "No it's not, it's dumb."            
##  [3] "What should we do?"                 
##  [4] "You liar, it stinks!"               
##  [5] "I am telling the truth!"            
##  [6] "How can we be certain?"             
##  [7] "There is no way."                   
##  [8] "I distrust you."                    
##  [9] "What are you talking about?"        
## [10] "Shall? Good then."                  
## [11] "I'm hungry. Let's eat. You already?"
```

```r

## A fake data set
x <- c("Where is the /big dog#?", "I think he's @arunning@b with /little cat#.")
x
```

```
## [1] "Where is the /big dog#?"                    
## [2] "I think he's @arunning@b with /little cat#."
```

```r

genXtract(x, c("/", "@a"), c("#", "@b"))
```

```
## [[1]]
## [1] "big dog"
## 
## [[2]]
## [1] "little cat" "running"
```

```r

## A fake data set
x2 <- c("Where is the L1big dogL2?", "I think he's 98running99 with L1little catL2.")
x2
```

```
## [1] "Where is the L1big dogL2?"                    
## [2] "I think he's 98running99 with L1little catL2."
```

```r

genXtract(x2, c("L1", 98), c("L2", 99))
```

```
## [[1]]
## [1] "big dog"
## 
## [[2]]
## [1] "little cat" "running"
```



<h3 id="viewing">View the Data</h3>

The following functions will be utilized in this section (click to view more):    

<form action="http://trinker.github.io/qdap_dev/htruncdf.html" target="_blank">
    <input type="submit" value="truncdf"><input type="submit" value="htruncdf"><input type="submit" value="ltruncdf"> - Dataframe Viewing
</form>

<form action="http://trinker.github.io/qdap_dev/qview.html" target="_blank">
    <input type="submit" value="qview"> - Quick View Dataframe View
</form>


<form action="http://trinker.github.io/qdap_dev/left.just.html" target="_blank">
    <input type="submit" value="left.just"><input type="submit" value="right.just"> - Text Justification
</form>

<form action="http://trinker.github.io/qdap_dev/strWrap.html" target="_blank">
    <input type="submit" value="strWrap"> - Wrap Character Strings to Format Paragraphs
</form>

<h3 id="reshaping">Reshaping the Data</h3>

The following functions will be utilized in this section (click to view more):    

<form action="http://trinker.github.io/qdap_dev/adjacency_matrix.html" target="_blank">
    <input type="submit" value="adjacency_matrix"><input type="submit" value="adjmat"> - Takes a Matrix and Generates an Adjacency Matrix
</form>

<form action="http://trinker.github.io/qdap_dev/colSplit.html" target="_blank">
    <input type="submit" value="colSplit"> - Separate a Column Pasted by `paste2`
</form>

<form action="http://trinker.github.io/qdap_dev/colsplit2df.html" target="_blank">
    <input type="submit" value="colsplit2df"><input type="submit" value="lcolsplit2df">  - Wrapper for colSplit that Returns Dataframe(s)
</form>

<form action="http://trinker.github.io/qdap_dev/gantt.html" target="_blank">
    <input type="submit" value="gantt"> - Generate Unit Spans 
</form>

<form action="http://trinker.github.io/qdap_dev/gantt_rep.html" target="_blank">
    <input type="submit" value="gantt_rep"> - Generate Unit Spans for Repeated Measures
</form>

<form action="http://trinker.github.io/qdap_dev/key_merge.html" target="_blank">
    <input type="submit" value="key_merge"> - Merge Demographic Information with Person/Text Transcript
</form>

<form action="http://trinker.github.io/qdap_dev/paste2.html" target="_blank">
    <input type="submit" value="paste2"> - Paste an Unspecified Number Of Text Columns
</form>

<form action="http://trinker.github.io/qdap_dev/prop.html" target="_blank">
    <input type="submit" value="prop"> - Convert Raw Numeric Matrix or Data Frame to Proportions
</form>

<form action="http://trinker.github.io/qdap_dev/qcombine.html" target="_blank">
    <input type="submit" value="qcombine"> - Combine Columns
</form>

<form action="http://trinker.github.io/qdap_dev/sentSplit.html" target="_blank">
    <input type="submit" value="sentSplit"><input type="submit" value="sentCombine"> - Sentence Splitting
</form>

<form action="http://trinker.github.io/qdap_dev/TOT.html" target="_blank">
    <input type="submit" value="TOT"> - Turns of Talk Labelling
</form>

<form action="http://trinker.github.io/qdap_dev/speakerSplit.html" target="_blank">
    <input type="submit" value="speakerSplit"> - Break and Stretch if Multiple Persons per Cell
</form>

<h3 id="word">Extract/Analyze Words</h3>

The following functions will be utilized in this section (click to view more):    

<form action="http://trinker.github.io/qdap_dev/all_words.html" target="_blank">
    <input type="submit" value="all_words"> - Searches Text Column for Words
</form>

<form action="http://trinker.github.io/qdap_dev/bag.o.words.html" target="_blank">
    <input type="submit" value="bag.o.words"><input type="submit" value="breaker"><input type="submit" value="word.split"> - Bag of Words
</form>

<form action="http://trinker.github.io/qdap_dev/common.html" target="_blank">
    <input type="submit" value="common"> - Find Common Words Between Groups
</form>

<form action="http://trinker.github.io/qdap_dev/exclude.html" target="_blank">
    <input type="submit" value="exclude"> - Exclude Elements From a Vector
</form>

<form action="http://trinker.github.io/qdap_dev/ngrams.html" target="_blank">
    <input type="submit" value="ngrams"> - Generate ngrams
</form>

<form action="http://trinker.github.io/qdap_dev/stopwords.html" target="_blank">
    <input type="submit" value="stopwords"> - Remove Stopwords
</form>

<form action="http://trinker.github.io/qdap_dev/strip.html" target="_blank">
    <input type="submit" value="strip"> - Strip Text
</form>

<form action="http://trinker.github.io/qdap_dev/synonyms.html" target="_blank">
    <input type="submit" value="synonyms"><input type="submit" value="syn"> - Search For Synonyms
</form>

<form action="http://trinker.github.io/qdap_dev/word_associate.html" target="_blank">
    <input type="submit" value="word_associate"> - Find Associated Words
</form>

<form action="http://trinker.github.io/qdap_dev/word_diff_list.html" target="_blank">
    <input type="submit" value="word_diff_list"> - Differences In Word Use Between Groups
</form>

<form action="http://trinker.github.io/qdap_dev/word_list.html" target="_blank">
    <input type="submit" value="word_list"> - Raw Word Lists/Frequency Counts
</form>

<h3 id="coding">Qualitative Coding System</h3>

The following functions will be utilized in this section (click to view more):    

<form action="http://trinker.github.io/qdap_dev/cm_code.blank.html" target="_blank">
    <input type="submit" value="cm_code.blank"> - Blank Code Transformation
</form>

<form action="http://trinker.github.io/qdap_dev/cm_code.combine.html" target="_blank">
    <input type="submit" value="cm_code.combine"> - Combine Codes
</form>

<form action="http://trinker.github.io/qdap_dev/cm_code.exclude.html" target="_blank">
    <input type="submit" value="cm_code.exclude"> - Exclude Codes
</form>

<form action="http://trinker.github.io/qdap_dev/cm_code.overlap.html" target="_blank">
    <input type="submit" value="cm_code.overlap"> - Find Co-occurrence Between Codes
</form>

<form action="http://trinker.github.io/qdap_dev/cm_code.transform.html" target="_blank">
    <input type="submit" value="cm_code.transform"> - Transform Codes
</form>

<form action="http://trinker.github.io/qdap_dev/cm_combine.dummy.html" target="_blank">
    <input type="submit" value="cm_combine.dummy"> - Find Co-occurrence Between Dummy Codes
</form>

<form action="http://trinker.github.io/qdap_dev/cm_df.fill.html" target="_blank">
    <input type="submit" value="cm_df.fill"> - Range Coding
</form>

<form action="http://trinker.github.io/qdap_dev/cm_df.temp.html" target="_blank">
    <input type="submit" value="cm_df.temp"> - Break Transcript Dialogue into Blank Code Matrix
</form>

<form action="http://trinker.github.io/qdap_dev/cm_df.transcript.html" target="_blank">
    <input type="submit" value="cm_df.transcript"> - Transcript With Word Number
</form>

<form action="http://trinker.github.io/qdap_dev/cm_df2long.html" target="_blank">
    <input type="submit" value="cm_df2long"> - Transform Codes to Start-End Durations
</form>

<form action="http://trinker.github.io/qdap_dev/cm_distance.html" target="_blank">
    <input type="submit" value="cm_distance"> - Distance Matrix Between Codes
</form>

<form action="http://trinker.github.io/qdap_dev/cm_dummy2long.html" target="_blank">
    <input type="submit" value="cm_dummy2long"> - Convert cm_combine.dummy Back to Long
</form>

<form action="http://trinker.github.io/qdap_dev/cm_long2dummy.html" target="_blank">
    <input type="submit" value="cm_long2dummy"> - Stretch and Dummy Code cm_xxx2long
</form>

<form action="http://trinker.github.io/qdap_dev/cm_range.temp.html" target="_blank">
    <input type="submit" value="cm_range.temp"> - Range Code Sheet
</form>

<form action="http://trinker.github.io/qdap_dev/cm_range2long.html" target="_blank">
    <input type="submit" value="cm_range2long"> - Transform Codes to Start-End Durations
</form>

<form action="http://trinker.github.io/qdap_dev/cm_time.temp.html" target="_blank">
    <input type="submit" value="cm_time.temp"> - Time Span Code Sheet
</form>

<form action="http://trinker.github.io/qdap_dev/cm_time2long.html" target="_blank">
    <input type="submit" value="cm_time2long"> - Transform Codes to Start-End Times
</form>


<h3 id="counts">Word Counts and Descriptive Statistics</h3>

The following functions will be utilized in this section (click to view more):    

<form action="http://trinker.github.io/qdap_dev/distTab.html" target="_blank">
    <input type="submit" value="distTab"> - SPSS Style Frequency Tables
</form>

<form action="http://trinker.github.io/qdap_dev/multiscale.html" target="_blank">
    <input type="submit" value="multiscale"> - Nested Standardization
</form>

<form action="http://trinker.github.io/qdap_dev/outlier.detect.html" target="_blank">
    <input type="submit" value="outlier.detect"> - Detect Outliers in Text
</form>

<form action="http://trinker.github.io/qdap_dev/outlier.labeler.html" target="_blank">
    <input type="submit" value="outlier.labeler"> - Locate Outliers in Numeric String
</form>

<form action="http://trinker.github.io/qdap_dev/pos.html" target="_blank">
    <input type="submit" value="pos"><input type="submit" value="pos.by"><input type="submit" value="pos.tags"> - Parts of Speech Tagging
</form>

<form action="http://trinker.github.io/qdap_dev/question_type.html" target="_blank">
    <input type="submit" value="question_type"> - Count of Question Type
</form>

<form action="http://trinker.github.io/qdap_dev/syllable.sum.html" target="_blank">
    <input type="submit" value="syllable.sum"><input type="submit" value="combo_syllable.sum"><input type="submit" value="polysyllable.sum"><input type="submit" value="syllable.count"> - Syllabication
</form>

<form action="http://trinker.github.io/qdap_dev/tdm.html" target="_blank">
    <input type="submit" value="tdm"><input type="submit" value="dtm"> - Convert/Generate Term Document Matrix or Document Term Matrix
</form>

<form action="http://trinker.github.io/qdap_dev/termco.html" target="_blank">
    <input type="submit" value="termco"><input type="submit" value="term.match"><input type="submit" value="termco.d"><input type="submit" value="termco2mat"> - Search For and Count Terms
</form>

<form action="http://trinker.github.io/qdap_dev/termco.c.html" target="_blank">
    <input type="submit" value="termco.c"> - Combine Columns from a termco Object
</form>

<form action="http://trinker.github.io/qdap_dev/wfm.html" target="_blank">
    <input type="submit" value="wfm"><input type="submit" value="wfdf"><input type="submit" value="wf.combine"><input type="submit" value="wfm.expanded"> - Word Frequency Matrix
</form>

<form action="http://trinker.github.io/qdap_dev/word.count.html" target="_blank">
    <input type="submit" value="word.count"><input type="submit" value="wc"> - Word Counts
</form>

<form action="http://trinker.github.io/qdap_dev/character.count.html" target="_blank">
    <input type="submit" value="character.count"><input type="submit" value="character.table"><input type="submit" value="char.table"> - Character Counts
</form>

<form action="http://trinker.github.io/qdap_dev/word_stats.html" target="_blank">
    <input type="submit" value="word_stats"> - Descriptive Word Statistics
</form>


<h3 id="measures">Word Measures and Scoring</h3>

The following functions will be utilized in this section (click to view more):    

<form action="http://trinker.github.io/qdap_dev/automated_readability_index.html" target="_blank">
    <input type="submit" value="automated_readability_index"><input type="submit" value="coleman_liau"><input type="submit" value="flesch_kincaid"><input type="submit" value="fry"><input type="submit" value="linsear_write"><input type="submit" value="SMOG"> - Readability Measures
</form>

<form action="http://trinker.github.io/qdap_dev/dissimilarity.html" target="_blank">
    <input type="submit" value="dissimilarity"> - Dissimilarity Statistics
</form>

<form action="http://trinker.github.io/qdap_dev/diversity.html" target="_blank">
    <input type="submit" value="diversity"> - Diversity Statistics
</form>

<form action="http://trinker.github.io/qdap_dev/formality.html" target="_blank">
    <input type="submit" value="formality"> - Formality Score
</form>

<form action="http://trinker.github.io/qdap_dev/kullback.leibler.html" target="_blank">
    <input type="submit" value="kullback.leibler"> - Kullback Leibler Statistic
</form>

<form action="http://trinker.github.io/qdap_dev/polarity.html" target="_blank">
    <input type="submit" value="polarity"> - Polarity Score (Sentiment Analysis)
</form>


<h3 id="visualization">Visualizing Discourse Data</h3>

The following functions will be utilized in this section (click to view more):    

<form action="http://trinker.github.io/qdap_dev/gradient_cloud.html" target="_blank">
    <input type="submit" value="gradient_cloud"> - Gradient Word Cloud
</form>

<form action="http://trinker.github.io/qdap_dev/gantt_plot.html" target="_blank">
    <input type="submit" value="gantt_plot"><input type="submit" value="gantt_wrap">  - Gantt Plot
</form>


<form action="http://trinker.github.io/qdap_dev/qheat.html" target="_blank">
    <input type="submit" value="qheat"> - Quick Heatmap
</form>

<form action="http://trinker.github.io/qdap_dev/rank_freq_mplot.html" target="_blank">
    <input type="submit" value="rank_freq_mplot"><input type="submit" value="rank_freq_plot"> - Rank Frequency Plot
</form>

<form action="http://trinker.github.io/qdap_dev/tot_plot.html" target="_blank">
    <input type="submit" value="tot_plot"> - Visualize Word Length by Turn of Talk
</form>

<form action="http://trinker.github.io/qdap_dev/trans.cloud.html" target="_blank">
    <input type="submit" value="trans.cloud"> - Word Clouds by Grouping Variable
</form>

<form action="http://trinker.github.io/qdap_dev/trans.venn.html" target="_blank">
    <input type="submit" value="trans.venn"> - Venn Diagram by Grouping Variable
</form>

<form action="http://trinker.github.io/qdap_dev/word.network.plot.html" target="_blank">
    <input type="submit" value="word.network.plot"> - Word Network Plot
</form>


<h3 id="id">ID Sentences</h3>

The following functions will be utilized in this section (click to view more):    

<form action="http://trinker.github.io/qdap_dev/end_inc.html" target="_blank">
    <input type="submit" value="end_inc"> - Test for Incomplete Sentences
</form>

<form action="http://trinker.github.io/qdap_dev/end_mark.html" target="_blank">
    <input type="submit" value="end_mark"> - Sentence End marks
</form>

<form action="http://trinker.github.io/qdap_dev/imperative.html" target="_blank">
    <input type="submit" value="imperative"> - Intuitively Remark Sentences as Imperative
</form>

<form action="http://trinker.github.io/qdap_dev/NAer.html" target="_blank">
    <input type="submit" value="NAer"> - Replace Missing Values (NA)
</form>

<h3 id="data">Data Sets</h3>

The following functions will be utilized in this section (click to view more):    

<form action="http://trinker.github.io/qdap_dev/DATA.html" target="_blank">
    <input type="submit" value="DATA"> - Fictitious Classroom Dialogue
</form>

<form action="http://trinker.github.io/qdap_dev/DATA2.html" target="_blank">
    <input type="submit" value="DATA2"> - Fictitious Repeated Measures Classroom Dialogue
</form>

<form action="http://trinker.github.io/qdap_dev/pres_debates2012.html" target="_blank">
    <input type="submit" value="pres_debates2012"> - 2012 U.S. Presidential Debates
</form>

<form action="http://trinker.github.io/qdap_dev/pres_debate_raw2012.html" target="_blank">
    <input type="submit" value="pres_debate_raw2012"> - First 2012 U.S. Presidential Debate
</form>

<form action="http://trinker.github.io/qdap_dev/mraja1.html" target="_blank">
    <input type="submit" value="mraja1"> - Romeo and Juliet: Act 1 Dialogue Merged with Demographics
</form>

<form action="http://trinker.github.io/qdap_dev/mraja1spl.html" target="_blank">
    <input type="submit" value="mraja1spl"> - Romeo and Juliet: Act 1 Dialogue Merged with Demographics and Split
</form>

<form action="http://trinker.github.io/qdap_dev/raj.act.1.html" target="_blank">
    <input type="submit" value="raj.act.1"> - Romeo and Juliet: Act 1
</form>

<form action="http://trinker.github.io/qdap_dev/raj.act.2.html" target="_blank">
    <input type="submit" value="raj.act.2"> - Romeo and Juliet: Act 2
</form>

<form action="http://trinker.github.io/qdap_dev/raj.act.3.html" target="_blank">
    <input type="submit" value="raj.act.3"> - Romeo and Juliet: Act 3
</form>

<form action="http://trinker.github.io/qdap_dev/raj.act.4.html" target="_blank">
    <input type="submit" value="raj.act.4"> - Romeo and Juliet: Act 4
</form>

<form action="http://trinker.github.io/qdap_dev/raj.act.5.html" target="_blank">
    <input type="submit" value="raj.act.5"> - Romeo and Juliet: Act 5
</form>

<form action="http://trinker.github.io/qdap_dev/raj.demographics.html" target="_blank">
    <input type="submit" value="raj.demographics"> - Romeo and Juliet Demographics
</form>

<form action="http://trinker.github.io/qdap_dev/raj.html" target="_blank">
    <input type="submit" value="raj"> - Romeo and Juliet (Unchanged & Complete)
</form>

<form action="http://trinker.github.io/qdap_dev/rajPOS.html" target="_blank">
    <input type="submit" value="rajPOS"> - Romeo and Juliet Split in Parts of Speech
</form>

<form action="http://trinker.github.io/qdap_dev/rajSPLIT.html" target="_blank">
    <input type="submit" value="rajSPLIT"> - Romeo and Juliet (Complete & Split)
</form>


<h3 id="dict">Dictionaries and Word Lists</h3>
<h3 id="install">Installation Issues</h3>

<h4>Java Issues</h3>
  
<p>If there is a discrepancy between the <a href="https://dl.dropbox.com/u/61803503/rjava_warning.txt">R and Java architectures</a> you will have to <a href="http://www.java.com/en/download/manual.jsp">download</a> the appropriate version of Java compatible with the version of R you're using.    

For more see <a href="http://www.r-statistics.com/2012/08/how-to-load-the-rjava-package-after-the-error-java_home-cannot-be-determined-from-the-registry/" target="_blank">Tal Galili's blog post</a> regarding rJava issues.


<hr>
## Acknowledgements

The qdap package was my first R package and a learning process. Several people contributed immensely to my learning. I'd like to particularly thank <a href="https://github.com/Dasonk/" target="_blank">Dason Kurkiewicz</a>
 for his constant mentoring/assistance in learning the R language, GitHub and package development as well as collaboration on numerous qdap functions. Thank you to <a href="https://twitter.com/bryangoodrich" target="_blank">Bryan Goodrich</a>
 for his teaching, feedback and collaboration on serveral qdap functions. Thank you to <a href="https://github.com/hadley" target="_blank">Dr. Hadley Wickham</a>
 for roxygen2, ggplot2, devtools and GitHub repos which I referenced often. I'd also like to thank the many folks at <a href="http://www.talkstats.com/" target="_blank">talkstats.com</a>
 and <a href="http://stackoverflow.com/questions/tagged/r" target="_blank">stackoverflow.com</a>
 for their help in answering many R questions related to qdap.


## Improvements

If the reader spots an error in this Vignette or would like to suggest an improvement please contact me @ Tyler Rinker&lt;<a href="mailto:tyler.rinker@gmail.com" target="_blank">tyler.rinker@gmail.com</a>&gt;.  To submit bug reports and feature requests related to the qdap package please visit <a href="https://github.com/trinker/qdap/issues?state=open" target="_blank">qdap's GitHub issues page</a>
.

<hr> 

*<em><font size="3">Vignette created with the reports package (<a href="http://github.com/trinker/reports">Rinker, 2013b</a>)</font><em>





## References

- Tyler Rinker,   (2013) qdap: Quantitative Discourse Analysis Package.  <a href="http://github.com/trinker/qdap">http://github.com/trinker/qdap</a>
- Tyler Rinker,   (2013) reports: Package to asssist in report writing.  <a href="http://github.com/trinker/reports">http://github.com/trinker/reports</a>

