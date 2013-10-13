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


The following vignette is a loose chronological road map for utilizing the tools provided by qdap.  

<hr>
<h3 id="toc">Select from sections below:</h3>

<div style="float: left; width: 50%;">
<ul>
<div>1.  <a href="#project">Starting a New Project</a>    </div> 
<div>2.  <a href="#import_export">Import/Export Discourse Data</a>    </div> 
<div>3.  <a href="#viewing">View the Data</a>    </div> 
<div>4.  <a href="#tools">Generic qdap Tools</a>    </div> 
<div>5.  <a href="#cleaning">Cleaning/Preparing the Data</a>    </div> 
<div>6.  <a href="#reshaping">Reshaping the Data</a>    </div> 
<div>7.  <a href="#word">Extract Words</a>    </div> 
<div>8.  <a href="#coding">Qualitative Coding System</a>    </div> 
<div>9.  <a href="#counts">Word Counts and Descriptive Statistics</a>    </div> 
<div>10.  <a href="#measures">Word Measures and Scoring</a>    </div> 
<div>11.  <a href="#visualization">Visualizing Discourse Data</a>    </div> 
<div>12.  <a href="#id">ID Sentences</a>    </div> 
<div>13.  <a href="#data">Data Sets</a>    </div> 
<div>14.  <a href="#dict">Dictionaries and Word Lists</a>    </div> 
<div>15.  <a href="#install">Installation Issues</a>    </div> 


</ul>
</div>
<div style="float: right; width: 50%;">
<ul>
<div><b>Symbol Conventions:</b></div>  
<div><font size="5" color="orange">&diams;</font> = Example (R code)    </div> 
<div><b><font size="5" color="firebrick">[YT]</font></b> = Video Demo (click to watch)    </div> 
</ul>
</div>
<br style="clear:both;"/>




<h3 id="project">Starting a New Project <a href="http://youtu.be/u8AJiyMffmc" target="_blank" style="text-decoration: none"><b><font size="5" color="#B22222">[YT]</font></b></a>
</h3>

<div class="funs">
The following functions will be utilized in this section (click to view more):    

<form action="http://trinker.github.io/qdap_dev/new_project.html" target="_blank">
    <input type="submit" value="new_project"> - Project Template
</form>
</div>


The function <a href="http://trinker.github.io/qdap_dev/new_project.html" target="_blank"><code>new_project</code></a> is designed to generate project template of multiple nested directories that organize and guide the researcher through a qualitative study, from data collection to analysis and report/presentation generation.  This workflow framework will enable the researcher to be better organized and more efficient in all stages of the research process.  <a href="http://trinker.github.io/qdap_dev/new_project.html" target="_blank"><code>new_project</code></a> utilizes the <a href="http://cran.r-project.org/web/packages/reports/reports.pdf" target="_blank">reports package</a> (<a href="http://github.com/trinker/reports">Rinker, 2013b</a>) 

Please see the following links for PDF descriptions of the contents of the <a href="http://trinker.github.io/qdap_dev/new_project.html" target="_blank"><code>new_project</code></a> and the reports directory. </br></br>

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

<h4 id="extra">extra_functions <a href="http://youtu.be/yuFyz7IW0Us" target="_blank" style="text-decoration: none"><b><font size="5" color="#B22222">[YT]</font></b></a>
</h4>    
The <a href="http://trinker.github.io/qdap_dev/new_project.html" target="_blank"><code>new_project</code></a> template is designed to be utilized with <a href="http://www.rstudio.com/ide/download/" target="_blank">RStudio</a>.  Upon clicking the `xxx.Rproj` file the template will be loaded into RStudio.  The .Rprofile script will be sourced upon start up, allowing the user to automatically load packages, functions, etc. related to the project.  The file `extra_functions.R` is sourced, loading custom functions.  Already included are two functions, `email` and `todo`, used to generate project member emails and track project tasks.  This auto sourcing greatly enhances efficiency in workflow.


<h3 id="import_export">Import/Export Discourse Data</h3>

<div class="funs">
The following functions will be utilized in this section (click to view more):    

<form action="http://trinker.github.io/qdap_dev/condense.html" target="_blank">
    <input type="submit" value="condense"> - <a href="#mcsv">Condense Dataframe Columns</a>
</form>

<form action="http://trinker.github.io/qdap_dev/dir_map.html" target="_blank">
    <input type="submit" value="dir_map"> - <a href="#readin">Map Transcript Files from a Directory to a Script</a>
</form>

<form action="http://trinker.github.io/qdap_dev/mcsv_r.html" target="_blank">
    <input type="submit" value="mcsv_r"><input type="submit" value="mcsv_w"> - <a href="#mcsv">Read/Write Multiple csv Files at a Time</a>
</form>

<form action="http://trinker.github.io/qdap_dev/read.transcript.html" target="_blank">
    <input type="submit" value="read.transcript"> - <a href="#readin">Read Transcripts Into R</a>
</form>
</div>

<h4 id="readin">Reading In Transcript Data <a href="http://youtu.be/UxgOScggLBg" target="_blank" style="text-decoration: none"><b><font size="5" color="#B22222">[YT]</font></b></a>
</h4>    

This subsection covers how to read in transcript data.  Generally the researcher will have data stored as a .docx (Microsoft Word or Open/Libre Office) or .xlsx/.csv (spreadsheet format).  It is of great importance that the researcher manually writes/parses their transcripts to avoid potential analysis problems later.  All sentences should contain appropriate qdap punctuation (declarative = ., interrogative = ?, exclamatory = !, interupted = | or <a href="http://trinker.github.io/qdap_dev/imperative.html" target="_blank"><code>imperative</code></a> = *., *?, *!, *|).  Additionally, if a sentence contains an endmark/punctuation it should have accompanying text/dialogue.  Two functions are useful for reading in data, <a href="http://trinker.github.io/qdap_dev/read.transcript.html" target="_blank"><code>read.transcript</code></a> and <a href="http://trinker.github.io/qdap_dev/dir_map.html" target="_blank"><code>dir_map</code></a>.  <a href="http://trinker.github.io/qdap_dev/read.transcript.html" target="_blank"><code>read.transcript</code></a> detects file type (.docx/.csv/.xlsx) and reads in a single transcipt whereas <a href="http://trinker.github.io/qdap_dev/dir_map.html" target="_blank"><code>dir_map</code></a> generates code that utilizes <a href="http://trinker.github.io/qdap_dev/read.transcript.html" target="_blank"><code>read.transcript</code></a> for each of the multiple transcripts in a single directory.  Note that <a href="http://trinker.github.io/qdap_dev/read.transcript.html" target="_blank"><code>read.transcript</code></a> expects a two column formatted transcript (usually with person on the left and dialogue on the right).

Five arguments are of particular importance to read.transcript: 

<table>
<tr><td><code>file</code></td>
<td><p>The name of the file which the data are to be
read from. Each row of the table appears as one line of
the file. If it does not contain an absolute path, the
file name is relative to the current working directory,
<code>getwd()</code>.</p></td></tr>
<tr><td><code>col.names</code></td>
<td>
<p>A character vector specifying the column
names of the transcript columns.</p>
</td></tr>
<tr><td><code>header</code></td>
<td>
<p>logical.  If <code>TRUE</code> the file contains
the names of the variables as its first line.</p>
</td></tr>
<tr><td><code>sep</code></td>
<td>
<p>The field separator character. Values on each
line of the file are separated by this character.  The
default of <code>NULL</code> instructs
<code><a href="read.transcript.html">read.transcript</a></code> to use a separator
suitable for the file type being read in.</p>
</td></tr>
<tr><td><code>skip</code></td>
<td>
<p>Integer; the number of lines of the data file
to skip before beginning to read data.</p>
</td></tr>
</table>

Often transcripts contain extraneous material at the top and the argument <font face="courier">skip = ?</font> must be used to skip these extra lines.  Some sort of unique separator must also be used to separate the person column from the text column.  By defualt <font face="courier">sep = ":"</font> is assumed.  If your transcripts do no contain a separator one must be inserted manually.  Also note that the researcher may want to prepare the transcripts with brackets to denote non spoken annotations as well dialogue that is read rather than spoken.  For more on bracket parsing see <a href="#bracket">Bracket/General Chunk Extraction</a>.

<div class="middleDiv">
<b><font size="4" color="red">Note: It is important that all sentences contain valid qdap punctuation (<font face="courier">.</font>, <font face="courier">?</font>, <font face="courier">!</font>, <font face="courier">|</font>) in your transcripts. Many qdap functions are dependant upon this assumption.</font></b>
</div>

<font size="5" color="orange">&diams;</font> **Reading In Data**- *read.transcript* <font size="5" color="orange">&diams;</font>
<pre><code class="r">## Location of sample transcripts from the qdap package
(doc1 <- system.file("extdata/transcripts/trans1.docx", package = "qdap"))
(doc2 <- system.file("extdata/transcripts/trans2.docx", package = "qdap"))
(doc3 <- system.file("extdata/transcripts/trans3.docx", package = "qdap"))
(doc4 <- system.file("extdata/transcripts/trans4.xlsx", package = "qdap"))</code></pre>

<pre><code class="r">dat1 <- read.transcript(doc1)
truncdf(dat1, 40)</code></pre>

<pre><code>##                  X1                                       X2
## 1      Researcher 2                         October 7, 1892.
## 2         Teacher 4 Students it's time to learn. [Student di
## 3 Multiple Students        Yes teacher we're ready to learn.
## 4     [Cross Talk 3                                      00]
## 5         Teacher 4 Let's read this terrific book together. </code></pre>


<pre><code class="r">dat2 <- read.transcript(doc1, col.names = c("person", "dialogue"))
truncdf(dat2, 40)</code></pre>

<pre><code>##              person                                 dialogue
## 1      Researcher 2                         October 7, 1892.
## 2         Teacher 4 Students it's time to learn. [Student di
## 3 Multiple Students        Yes teacher we're ready to learn.
## 4     [Cross Talk 3                                      00]
## 5         Teacher 4 Let's read this terrific book together. </code></pre>

<pre><code class="r">dat2b <- rm_row(dat2, "person", "[C") #remove bracket row
truncdf(dat2b, 40)</code></pre>

<pre><code>##              person                                 dialogue
## 1      Researcher 2                         October 7, 1892.
## 2         Teacher 4 Students it's time to learn. [Student di
## 3 Multiple Students        Yes teacher we're ready to learn.
## 4         Teacher 4 Let's read this terrific book together. </code></pre>


<pre><code class="r">## Be aware of the need to `skip` non transcript lines
## Incorrect read; Needed to use `skip`
read.transcript(doc2)</code></pre>

<pre><code>Error in data.frame(X1 = speaker, X2 = pvalues, stringsAsFactors = FALSE) : 
  arguments imply differing number of rows: 7, 8</code></pre>


<pre><code class="r">## Correct: Used `skip`
dat3 <- read.transcript(doc2, skip = 1)
truncdf(dat3, 40)</code></pre>

<pre><code>##                  X1                                       X2
## 1      Researcher 2                         October 7, 1892.
## 2         Teacher 4 Students it's time to learn. [Student di
## 3 Multiple Students        Yes teacher we're ready to learn.
## 4     [Cross Talk 3                                      00]
## 5         Teacher 4 Let's read this terrific book together. 
</code></pre>

<pre><code class="r">## Be Aware of the `sep` Used
## Incorrect Read; Wrong `sep` Provided (used defualt `:`)
read.transcript(doc3, skip = 1)</code></pre>

<pre><code>##Dialogue and Person Columns Mixed Inappropriately
## X1
## 1 [Cross Talk 3
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              X2
## 1 Teacher 4-Students it's time to learn. [Student discussion; unintelligible] Multiple Students-Yes teacher we're ready to learn. 00] Teacher 4-Let's read this terrific book together. It's called Moo Baa La La La and what was I going to ... Oh yes The story is by Sandra Boynton. A cow says Moo. A Sheep says Baa. Three singing pigs say LA LA LA! "No, no!" you say, that isn't right. The pigs say oink all day and night. Rhinoceroses snort and snuff. And little dogs go ruff ruff ruff! Some other dogs go bow wow wow! And cats and kittens say Meow! Quack! Says the duck. A horse says neigh. It's quiet now. What do you say?
</code></pre>


<pre><code class="r">## Correct `sep` Used
dat4 <- read.transcript(doc3, sep = "-", skip = 1)
truncdf(dat4, 40)</code></pre>

<pre><code>##                  X1                                       X2
## 1         Teacher 4 Students it's time to learn. [Student di
## 2 Multiple Students Yes teacher we're ready to learn. [Cross
## 3         Teacher 4 Let's read this terrific book together. </code></pre>


<pre><code class="r">## Read In .xlsx Data
dat5 <- read.transcript(doc4)
truncdf(dat5, 40)</code></pre>

<pre><code>##                   V1                                       V2
## 1      Researcher 2:                         October 7, 1892.
## 2         Teacher 4:             Students it's time to learn.
## 3               <NA>                                     <NA>
## 4 Multiple Students:        Yes teacher we're ready to learn.
## 5               <NA>                                     <NA>
## 6         Teacher 4: Let's read this terrific book together. 
</code></pre>

<pre><code class="r">## Reading In Text
trans <- "sam: Computer is fun. Not too fun.
greg: No it's not, it's dumb.
teacher: What should we do?
sam: You liar, it stinks!"

read.transcript(text=trans)</code></pre>

<pre><code>##        V1                            V2
## 1     sam Computer is fun. Not too fun.
## 2    greg         No its not, its dumb.
## 3 teacher            What should we do?
## 4     sam          You liar, it stinks!
</code></pre>

The <a href="http://trinker.github.io/qdap_dev/dir_map.html" target="_blank"><code>dir_map</code></a> function enables the researcher to produce multiple lines of code, one line with <a href="http://trinker.github.io/qdap_dev/read.transcript.html" target="_blank"><code>read.transcript</code></a> for each file in a directory, which is then optionally copied to the clipboard for easy insertion into a script.  Note that setting the argument <font face="courier">use.path = FALSE</font> may allow the code to be more portable in that a static path is not suppplied the the <a href="http://trinker.github.io/qdap_dev/read.transcript.html" target="_blank"><code>read.transcript</code></a> scripts.

<font size="5" color="orange">&diams;</font> **Reading In Data**- *dir_map* <font size="5" color="orange">&diams;</font>

<pre><code class="r">(DIR <- system.file("extdata/transcripts", package = "qdap"))
dir_map(DIR)</code></pre>

...will produce...

<pre><code>dat1 <- read.transcript('~/extdata/transcripts/trans1.docx', col.names = c('person', 'dialogue'), skip = 0)
dat2 <- read.transcript('~/extdata/transcripts/trans2.docx', col.names = c('person', 'dialogue'), skip = 0)
dat3 <- read.transcript('~/extdata/transcripts/trans3.docx', col.names = c('person', 'dialogue'), skip = 0)
dat4 <- read.transcript('~/extdata/transcripts/trans4.xlsx', col.names = c('person', 'dialogue'), skip = 0)</code></pre>


<h4 id="mcsv">Reading/Writing Multiple .csv Files <a href="http://youtu.be/aeZKJGEfD7U" target="_blank" style="text-decoration: none"><b><font size="5" color="#B22222">[YT]</font></b></a>
</h4>    

The <font face="courier">mcsv_x</font> family of functions are utilized to read (<a href="http://trinker.github.io/qdap_dev/mcsv_r.html" target="_blank"><code>mcsv_r</code></a>) and write (<a href="http://trinker.github.io/qdap_dev/mcsv_w.html" target="_blank"><code>mcsv_w</code></a>) multiple csv files at once.  <a href="http://trinker.github.io/qdap_dev/mcsv_w.html" target="_blank"><code>mcsv_w</code></a> takes an arbitrary number of dataframes and outputs them to the supplied directory( <font face="courier">dir = ?</font>).  An attempt will be made to output the dataframes from qdap functions that output lists of dataframes.  Note that dataframes that contain columns that are lists must be condensed prior to writing with other R dataframe writing functions (e.g., `write.csv`) using the <a href="http://trinker.github.io/qdap_dev/condense.html" target="_blank"><code>condense</code></a> function.  By default <a href="http://trinker.github.io/qdap_dev/mcsv_w.html" target="_blank"><code>mcsv_w</code></a> attempts to utilize <a href="http://trinker.github.io/qdap_dev/condense.html" target="_blank"><code>condense</code></a>.

The <a href="http://trinker.github.io/qdap_dev/mcsv_r.html" target="_blank"><code>mcsv_r</code></a> function reads multiple files at once and then assigns then dataframes to identically named objects (minus the file extension) in the global environment.  Additionally, all of the dataframes that are read in are also assigned to an inclusive list (name `L1` by defualt).

<font size="5" color="orange">&diams;</font> **Reading and Writing Multiple csvs** <font size="5" color="orange">&diams;</font>


```r
## Make new minimal data sets
mtcarsb <- mtcars[1:5, ]; CO2b <- CO2[1:5, ]

## Write multiple csvs and assign the directory path to `a`
a <- mcsv_w(mtcarsb, CO2b, dir="foo")

## New data sets gone from .GlobalEnv
rm("mtcarsb", "CO2b")  

## View the files in `a` and assign to `nms`
(nms <- dir(a))

## Read in and notice the dataframes have been assigned in .GlobalEnv
mcsv_r(file.path(a, nms))
mtcarsb; CO2b
L1

## The dataframe anmes and list of dataframe can be altered
mcsv_r(file.path(a, nms), a.name = paste0("bot", 1:2), l.name = "bots_stink")
bot1; bot2
bots_stink

## Clean up
delete("foo")
```


<font size="5" color="orange">&diams;</font> **Writing Lists of Dataframes to csvs** <font size="5" color="orange">&diams;</font>

```r
## poldat and termco produce lists of dataframes
poldat <- with(DATA, polarity(state, person))
term <- c("the ", "she", " wh")
termdat <- with(raj.act.1,  termco(dialogue, person, term))

## View the lists of dataframes
str(poldat); str(termdat)

## Write the lists of dataframes to csv
mcsv_w(poldat, termdat, mtcars, CO2, dir="foo2")

## Clean up
delete("foo2")
```


<h3 id="viewing">View the Data</h3>

<div class="funs">
The following functions will be utilized in this section (click to view more):    

<form action="http://trinker.github.io/qdap_dev/data_viewing.html" target="_blank">
    <input type="submit" value="truncdf"><input type="submit" value="htruncdf"><input type="submit" value="ltruncdf"><input type="submit" value="qview"> - <a href="#trunc">Truncated Dataframe Viewing</a>
</form>

<form action="http://trinker.github.io/qdap_dev/data_viewing.html" target="_blank">
    <input type="submit" value="lview"> - <a href="#unclass">Unclass qdap Object to View List of Dataframes</a>
</form>

<form action="http://trinker.github.io/qdap_dev/left_just.html" target="_blank">
    <input type="submit" value="left_just"><input type="submit" value="right_just"> - <a href="#just">Text Justification</a>
</form>

<form action="http://trinker.github.io/qdap_dev/Search.html" target="_blank">
    <input type="submit" value="Search"> - <a href="#search">Search Columns of a Dataframe</a>
</form>
</div>

The nature of dialogue data makes it large and cumbersome to view in R.  This section explores qdap tools designed for more comfortable viewing of R dialogue oriented text dataframes.  

<h4 id="trunc">Truncated Dataframe Viewing</h4> 

The <a href="http://trinker.github.io/qdap_dev/htruncdf.html" target="_blank"><code>_truncdf</code></a> family of functions (trunc + dataframe = <a href="http://trinker.github.io/qdap_dev/data_viewing.html" target="_blank"><code>truncdf</code></a>) are designed to truncate the width of columns and number of rows in dataframes and lists of dataframes.  The <font face="courier">l</font> and <font face="courier">h</font> in front of <font face="courier">trunc</font> stands for <b><font color="blue">l</font>ist</b> and <b><font color="blue">h</font>ead</b> and are extensions of <a href="http://trinker.github.io/qdap_dev/data_viewing.html" target="_blank"><code>truncdf</code></a>.  <a href="http://trinker.github.io/qdap_dev/data_viewing.html" target="_blank"><code>qview</code></a> is a wrapper for <a href="http://trinker.github.io/qdap_dev/data_viewing.html" target="_blank"><code>htruncdf</code></a> that also displays number of rows, columns, and the dataframe name.


<font size="5" color="orange">&diams;</font> **Truncated Data Viewing** <font size="5" color="orange">&diams;</font>


```r
truncdf(raj[1:10, ])
```

```
##     person   dialogue act
## 1  Sampson Gregory, o   1
## 2  Gregory No, for th   1
## 3  Sampson I mean, an   1
## 4  Gregory Ay, while    1
## 5  Sampson I strike q   1
## 6  Gregory But thou a   1
## 7  Sampson A dog of t   1
## 8  Gregory To move is   1
## 9  Sampson A dog of t   1
## 10 Gregory That shows   1
```

```r
truncdf(raj[1:10, ], 40)
```

```
##     person                                 dialogue act
## 1  Sampson Gregory, o my word, we'll not carry coal   1
## 2  Gregory      No, for then we should be colliers.   1
## 3  Sampson  I mean, an we be in choler, we'll draw.   1
## 4  Gregory Ay, while you live, draw your neck out o   1
## 5  Sampson           I strike quickly, being moved.   1
## 6  Gregory But thou art not quickly moved to strike   1
## 7  Sampson A dog of the house of Montague moves me.   1
## 8  Gregory To move is to stir; and to be valiant is   1
## 9  Sampson A dog of that house shall move me to sta   1
## 10 Gregory That shows thee a weak slave; for the we   1
```

```r
htruncdf(raj)
```

```
##     person   dialogue act
## 1  Sampson Gregory, o   1
## 2  Gregory No, for th   1
## 3  Sampson I mean, an   1
## 4  Gregory Ay, while    1
## 5  Sampson I strike q   1
## 6  Gregory But thou a   1
## 7  Sampson A dog of t   1
## 8  Gregory To move is   1
## 9  Sampson A dog of t   1
## 10 Gregory That shows   1
```

```r
htruncdf(raj, 20)
```

```
##     person   dialogue act
## 1  Sampson Gregory, o   1
## 2  Gregory No, for th   1
## 3  Sampson I mean, an   1
## 4  Gregory Ay, while    1
## 5  Sampson I strike q   1
## 6  Gregory But thou a   1
## 7  Sampson A dog of t   1
## 8  Gregory To move is   1
## 9  Sampson A dog of t   1
## 10 Gregory That shows   1
## 11 Sampson True; and    1
## 12 Gregory The quarre   1
## 13 Sampson 'Tis all o   1
## 14 Gregory The heads    1
## 15 Sampson Ay, the he   1
## 16 Gregory They must    1
## 17 Sampson Me they sh   1
## 18 Gregory 'Tis well    1
## 19 Sampson My naked w   1
## 20 Gregory How! turn    1
```

```r
htruncdf(raj, ,20)
```

```
##     person             dialogue act
## 1  Sampson Gregory, o my word,    1
## 2  Gregory No, for then we shou   1
## 3  Sampson I mean, an we be in    1
## 4  Gregory Ay, while you live,    1
## 5  Sampson I strike quickly, be   1
## 6  Gregory But thou art not qui   1
## 7  Sampson A dog of the house o   1
## 8  Gregory To move is to stir;    1
## 9  Sampson A dog of that house    1
## 10 Gregory That shows thee a we   1
```

```r
ltruncdf(rajPOS, width = 4)
```

```
## $text
##   data
## 1 Greg
## 2 No, 
## 3 I me
## 4 Ay, 
## 5 I st
## 6 But 
## 
## $POStagged
##   POSt POSt word
## 1 greg c("N    8
## 2 no/D c("D    7
## 3 i/PR c("P    9
## 4 ay/N c("N   11
## 5 i/VB c("V    5
## 6 but/ c("C    8
## 
## $POSprop
##   wrd. prop prop prop prop prop prop prop prop prop prop prop prop prop
## 1    8    0    0    0    0    0    0    0 12.5    0    0    0    0   25
## 2    7    0    0    0    0 14.2    0    0 14.2    0    0    0 14.2    0
## 3    9    0    0    0    0 11.1    0    0 11.1    0    0    0    0 11.1
## 4   11    0    0    0    0 9.09    0    0 27.2    0    0    0    0 27.2
## 5    5    0    0    0    0    0    0    0    0    0    0    0    0   20
## 6    8    0    0 12.5    0 12.5    0    0    0    0    0    0    0 12.5
##   prop prop prop prop prop prop prop prop prop prop prop prop prop prop
## 1    0    0 12.5    0    0    0 12.5   25    0    0    0    0    0 12.5
## 2    0    0 14.2    0    0 14.2    0 14.2    0    0    0    0    0 14.2
## 3    0    0    0    0    0 22.2    0 11.1    0    0    0    0    0 22.2
## 4    0    0    0    0    0 9.09 9.09    0    0    0    0    0    0 9.09
## 5    0    0    0    0    0    0    0   20    0    0    0    0    0    0
## 6    0    0    0    0    0    0    0   25    0    0    0 12.5    0 12.5
##   prop prop prop prop prop prop prop prop prop
## 1    0    0    0    0    0    0    0    0    0
## 2    0    0    0    0    0    0    0    0    0
## 3    0    0    0 11.1    0    0    0    0    0
## 4    0    0    0 9.09    0    0    0    0    0
## 5    0   20   40    0    0    0    0    0    0
## 6 12.5    0    0    0    0    0    0    0    0
## 
## $POSfreq
##   wrd. , . CC CD DT EX FW IN JJ JJR JJS MD NN NNP NNPS NNS PDT POS PRP
## 1    8 0 0  0  0  0  0  0  1  0   0   0  0  2   0    0   1   0   0   0
## 2    7 0 0  0  0  1  0  0  1  0   0   0  1  0   0    0   1   0   0   1
## 3    9 0 0  0  0  1  0  0  1  0   0   0  0  1   0    0   0   0   0   2
## 4   11 0 0  0  0  1  0  0  3  0   0   0  0  3   0    0   0   0   0   1
## 5    5 0 0  0  0  0  0  0  0  0   0   0  0  1   0    0   0   0   0   0
## 6    8 0 0  1  0  1  0  0  0  0   0   0  0  1   0    0   0   0   0   0
##   PRP$ RB RBR RBS RP TO UH VB VBD VBG VBN VBP VBZ WDT WP WP$ WRB
## 1    1  2   0   0  0  0  0  1   0   0   0   0   0   0  0   0   0
## 2    0  1   0   0  0  0  0  1   0   0   0   0   0   0  0   0   0
## 3    0  1   0   0  0  0  0  2   0   0   0   1   0   0  0   0   0
## 4    1  0   0   0  0  0  0  1   0   0   0   1   0   0  0   0   0
## 5    0  1   0   0  0  0  0  0   0   1   2   0   0   0  0   0   0
## 6    0  2   0   0  0  1  0  1   1   0   0   0   0   0  0   0   0
## 
## $POSrnp
##   wrd. , .   CC CD   DT EX FW   IN JJ JJR JJS   MD   NN NNP NNPS  NNS PDT
## 1    8 0 0    0  0    0  0  0 1(12  0   0   0    0 2(25   0    0 1(12   0
## 2    7 0 0    0  0 1(14  0  0 1(14  0   0   0 1(14    0   0    0 1(14   0
## 3    9 0 0    0  0 1(11  0  0 1(11  0   0   0    0 1(11   0    0    0   0
## 4   11 0 0    0  0 1(9.  0  0 3(27  0   0   0    0 3(27   0    0    0   0
## 5    5 0 0    0  0    0  0  0    0  0   0   0    0 1(20   0    0    0   0
## 6    8 0 0 1(12  0 1(12  0  0    0  0   0   0    0 1(12   0    0    0   0
##   POS  PRP PRP$   RB RBR RBS RP   TO UH   VB  VBD  VBG  VBN  VBP VBZ WDT
## 1   0    0 1(12 2(25   0   0  0    0  0 1(12    0    0    0    0   0   0
## 2   0 1(14    0 1(14   0   0  0    0  0 1(14    0    0    0    0   0   0
## 3   0 2(22    0 1(11   0   0  0    0  0 2(22    0    0    0 1(11   0   0
## 4   0 1(9. 1(9.    0   0   0  0    0  0 1(9.    0    0    0 1(9.   0   0
## 5   0    0    0 1(20   0   0  0    0  0    0    0 1(20 2(40    0   0   0
## 6   0    0    0 2(25   0   0  0 1(12  0 1(12 1(12    0    0    0   0   0
##   WP WP$ WRB
## 1  0   0   0
## 2  0   0   0
## 3  0   0   0
## 4  0   0   0
## 5  0   0   0
## 6  0   0   0
## 
## $percent
##   data
## 1 TRUE
## 
## $zero.replace
##   data
## 1    0
```


<pre><code class="r">qview(raj)</code></pre>

<pre><code>## ========================================================================
## nrow =  840           ncol =  3             raj
## ========================================================================
##     person   dialogue act
## 1  Sampson Gregory, o   1
## 2  Gregory No, for th   1
## 3  Sampson I mean, an   1
## 4  Gregory Ay, while    1
## 5  Sampson I strike q   1
## 6  Gregory But thou a   1
## 7  Sampson A dog of t   1
## 8  Gregory To move is   1
## 9  Sampson A dog of t   1
## 10 Gregory That shows   1</code></pre>

<pre><code class="r">qview(CO2)</code></pre>

<pre><code>## ========================================================================
## nrow =  84           ncol =  5             CO2
## ========================================================================
##    Plant   Type  Treatment conc uptake
## 1    Qn1 Quebec nonchilled   95     16
## 2    Qn1 Quebec nonchilled  175   30.4
## 3    Qn1 Quebec nonchilled  250   34.8
## 4    Qn1 Quebec nonchilled  350   37.2
## 5    Qn1 Quebec nonchilled  500   35.3
## 6    Qn1 Quebec nonchilled  675   39.2
## 7    Qn1 Quebec nonchilled 1000   39.7
## 8    Qn2 Quebec nonchilled   95   13.6
## 9    Qn2 Quebec nonchilled  175   27.3
## 10   Qn2 Quebec nonchilled  250   37.1</code></pre>

<h4 id="unclass">Unclass qdap Object to View List of Dataframes</h4> 

Many qdap objects are lists that print as a single dataframe, though the rest of the objects in the list are available.  The <a href="http://trinker.github.io/qdap_dev/data_viewing.html" target="_blank"><code>lview</code></a> function unclasses the object and assigns "list".



```r
lview(question_type(DATA.SPLIT$state, DATA.SPLIT$person))
```

```
## $raw
##        person                    raw.text n.row endmark
## 4     teacher          What should we do?     4       ?
## 7       sally      How can we be certain?     7       ?
## 10      sally What are you talking about?    10       ?
## 11 researcher           Shall we move on?    11       ?
## 15       greg                You already?    15       ?
##                      strip.text              q.type
## 4            what should we do                 what
## 7        how can we be certain                  how
## 10  what are you talking about                 what
## 11            shall we move on                shall
## 15                 you already  implied_do/does/did
## 
## $count
##       person tot.quest what how shall implied_do/does/did
## 1       greg         1    0   0     0                   1
## 2 researcher         1    0   0     1                   0
## 3      sally         2    1   1     0                   0
## 4    teacher         1    1   0     0                   0
## 5        sam         0    0   0     0                   0
## 
## $prop
##       person tot.quest what how shall implied_do/does/did
## 1       greg         1    0   0     0                 100
## 2 researcher         1    0   0   100                   0
## 3      sally         2   50  50     0                   0
## 4    teacher         1  100   0     0                   0
## 5        sam         0    0   0     0                   0
## 
## $rnp
##       person tot.quest    what    how   shall implied_do/does/did
## 1       greg         1       0      0       0             1(100%)
## 2 researcher         1       0      0 1(100%)                   0
## 3      sally         2  1(50%) 1(50%)       0                   0
## 4    teacher         1 1(100%)      0       0                   0
## 5        sam         0       0      0       0                   0
## 
## $inds
## [1]  4  7 10 11 15
## 
## $missing
## integer(0)
## 
## $percent
## [1] TRUE
## 
## $zero.replace
## [1] 0
```


<h4 id="just">Text Justification</h4> 

By defualt text data (character vectors) are displayed as right justified in R.  This can be difficult and unnatural to read, particularly as the length of the sentences increase.  The <a href="http://trinker.github.io/qdap_dev/left_just.html" target="_blank"><code>left_just</code></a> function creates a more natural left justification of text.  Note that <a href="http://trinker.github.io/qdap_dev/left_just.html" target="_blank"><code>left_just</code></a> inserts spaces to achieve the justification. This could interfere with analysis and therefore the output from <a href="http://trinker.github.io/qdap_dev/left_just.html" target="_blank"><code>left_just</code></a> should only be used for visualization purposes, not analysis.

<font size="5" color="orange">&diams;</font> **Justified Data Viewing** <font size="5" color="orange">&diams;</font>    


```r
## The unnatural state of R text data
DATA
```

```
##        person sex adult                                 state code
## 1         sam   m     0         Computer is fun. Not too fun.   K1
## 2        greg   m     0               No it's not, it's dumb.   K2
## 3     teacher   m     1                    What should we do?   K3
## 4         sam   m     0                  You liar, it stinks!   K4
## 5        greg   m     0               I am telling the truth!   K5
## 6       sally   f     0                How can we be certain?   K6
## 7        greg   m     0                      There is no way.   K7
## 8         sam   m     0                       I distrust you.   K8
## 9       sally   f     0           What are you talking about?   K9
## 10 researcher   f     1         Shall we move on?  Good then.  K10
## 11       greg   m     0 I'm hungry.  Let's eat.  You already?  K11
```

```r
## left jsut to the rescue
left_just(DATA)
```

```
##    person     sex adult state                                 code
## 1  sam        m   0     Computer is fun. Not too fun.         K1  
## 2  greg       m   0     No it's not, it's dumb.               K2  
## 3  teacher    m   1     What should we do?                    K3  
## 4  sam        m   0     You liar, it stinks!                  K4  
## 5  greg       m   0     I am telling the truth!               K5  
## 6  sally      f   0     How can we be certain?                K6  
## 7  greg       m   0     There is no way.                      K7  
## 8  sam        m   0     I distrust you.                       K8  
## 9  sally      f   0     What are you talking about?           K9  
## 10 researcher f   1     Shall we move on?  Good then.         K10 
## 11 greg       m   0     I'm hungry.  Let's eat.  You already? K11
```

```r
## Left just select column(s)
left_just(DATA, c("sex", "state"))
```

```
##        person sex adult state                                 code
## 1         sam m       0 Computer is fun. Not too fun.           K1
## 2        greg m       0 No it's not, it's dumb.                 K2
## 3     teacher m       1 What should we do?                      K3
## 4         sam m       0 You liar, it stinks!                    K4
## 5        greg m       0 I am telling the truth!                 K5
## 6       sally f       0 How can we be certain?                  K6
## 7        greg m       0 There is no way.                        K7
## 8         sam m       0 I distrust you.                         K8
## 9       sally f       0 What are you talking about?             K9
## 10 researcher f       1 Shall we move on?  Good then.          K10
## 11       greg m       0 I'm hungry.  Let's eat.  You already?  K11
```

```r
left_just(CO2[1:15,])
```

```
##    Plant Type   Treatment  conc uptake
## 1  Qn1   Quebec nonchilled 95   16    
## 2  Qn1   Quebec nonchilled 175  30.4  
## 3  Qn1   Quebec nonchilled 250  34.8  
## 4  Qn1   Quebec nonchilled 350  37.2  
## 5  Qn1   Quebec nonchilled 500  35.3  
## 6  Qn1   Quebec nonchilled 675  39.2  
## 7  Qn1   Quebec nonchilled 1000 39.7  
## 8  Qn2   Quebec nonchilled 95   13.6  
## 9  Qn2   Quebec nonchilled 175  27.3  
## 10 Qn2   Quebec nonchilled 250  37.1  
## 11 Qn2   Quebec nonchilled 350  41.8  
## 12 Qn2   Quebec nonchilled 500  40.6  
## 13 Qn2   Quebec nonchilled 675  41.4  
## 14 Qn2   Quebec nonchilled 1000 44.3  
## 15 Qn3   Quebec nonchilled 95   16.2
```

```r
right_just(left_just(CO2[1:15,]))
```

```
##    Plant   Type  Treatment conc uptake
## 1    Qn1 Quebec nonchilled   95     16
## 2    Qn1 Quebec nonchilled  175   30.4
## 3    Qn1 Quebec nonchilled  250   34.8
## 4    Qn1 Quebec nonchilled  350   37.2
## 5    Qn1 Quebec nonchilled  500   35.3
## 6    Qn1 Quebec nonchilled  675   39.2
## 7    Qn1 Quebec nonchilled 1000   39.7
## 8    Qn2 Quebec nonchilled   95   13.6
## 9    Qn2 Quebec nonchilled  175   27.3
## 10   Qn2 Quebec nonchilled  250   37.1
## 11   Qn2 Quebec nonchilled  350   41.8
## 12   Qn2 Quebec nonchilled  500   40.6
## 13   Qn2 Quebec nonchilled  675   41.4
## 14   Qn2 Quebec nonchilled 1000   44.3
## 15   Qn3 Quebec nonchilled   95   16.2
```


<h4 id="search">Search Columns of a Dataframe</h4> 

A task of many analyses is to search a dataframe for a particular phrase and return those rows/observations that contain that term.  The researcher may optionally choose to specify a particular column to search (<font face="courier">column.name</font>) or search the entire dataframe.

<font size="5" color="orange">&diams;</font> **Search Dataframes** <font size="5" color="orange">&diams;</font>


```r
(SampDF <- data.frame("islands"=names(islands)[1:32],mtcars, row.names=NULL))
```

```
##            islands  mpg cyl  disp  hp drat    wt  qsec vs am gear carb
## 1           Africa 21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
## 2       Antarctica 21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
## 3             Asia 22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
## 4        Australia 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
## 5     Axel Heiberg 18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
## 6           Baffin 18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
## 7            Banks 14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
## 8           Borneo 24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
## 9          Britain 22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
## 10         Celebes 19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
## 11           Celon 17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
## 12            Cuba 16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
## 13           Devon 17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
## 14       Ellesmere 15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
## 15          Europe 10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
## 16       Greenland 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
## 17          Hainan 14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
## 18      Hispaniola 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
## 19        Hokkaido 30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
## 20          Honshu 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
## 21         Iceland 21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
## 22         Ireland 15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
## 23            Java 15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
## 24          Kyushu 13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
## 25           Luzon 19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
## 26      Madagascar 27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
## 27        Melville 26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
## 28        Mindanao 30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
## 29        Moluccas 15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
## 30     New Britain 19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
## 31      New Guinea 15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
## 32 New Zealand (N) 21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
```

```r
Search(SampDF, "Cuba", "islands")
```

```
##    islands  mpg cyl  disp  hp drat   wt qsec vs am gear carb
## 12    Cuba 16.4   8 275.8 180 3.07 4.07 17.4  0  0    3    3
```

```r
Search(SampDF, "New", "islands")
```

```
##            islands  mpg cyl  disp  hp drat   wt qsec vs am gear carb
## 8           Borneo 24.4   4 146.7  62 3.69 3.19 20.0  1  0    4    2
## 30     New Britain 19.7   6 145.0 175 3.62 2.77 15.5  0  1    5    6
## 31      New Guinea 15.0   8 301.0 335 3.54 3.57 14.6  0  1    5    8
## 32 New Zealand (N) 21.4   4 121.0 109 4.11 2.78 18.6  1  1    4    2
```

```r
Search(SampDF, "Ho")
```

```
##         islands  mpg cyl  disp  hp drat    wt  qsec vs am gear carb
## 5  Axel Heiberg 18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
## 8        Borneo 24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
## 11        Celon 17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
## 13        Devon 17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
## 15       Europe 10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
## 17       Hainan 14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
## 18   Hispaniola 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
## 19     Hokkaido 30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
## 20       Honshu 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
## 24       Kyushu 13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
## 25        Luzon 19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
## 28     Mindanao 30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
## 29     Moluccas 15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
```

```r
Search(SampDF, "Ho", max.distance = 0)
```

```
##     islands  mpg cyl disp hp drat    wt  qsec vs am gear carb
## 19 Hokkaido 30.4   4 75.7 52 4.93 1.615 18.52  1  1    4    2
## 20   Honshu 33.9   4 71.1 65 4.22 1.835 19.90  1  1    4    1
```

```r
Search(SampDF, "Axel Heiberg")
```

```
##        islands  mpg cyl disp  hp drat   wt  qsec vs am gear carb
## 5 Axel Heiberg 18.7   8  360 175 3.15 3.44 17.02  0  0    3    2
```

```r
Search(SampDF, 19) #too much tolerance in max.distance
```

```
##            islands  mpg cyl  disp  hp drat    wt  qsec vs am gear carb
## 1           Africa 21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
## 2       Antarctica 21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
## 3             Asia 22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
## 4        Australia 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
## 5     Axel Heiberg 18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
## 6           Baffin 18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
## 7            Banks 14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
## 8           Borneo 24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
## 9          Britain 22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
## 10         Celebes 19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
## 11           Celon 17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
## 12            Cuba 16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
## 13           Devon 17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
## 14       Ellesmere 15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
## 15          Europe 10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
## 16       Greenland 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
## 17          Hainan 14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
## 18      Hispaniola 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
## 19        Hokkaido 30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
## 20          Honshu 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
## 21         Iceland 21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
## 22         Ireland 15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
## 23            Java 15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
## 24          Kyushu 13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
## 25           Luzon 19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
## 26      Madagascar 27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
## 27        Melville 26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
## 28        Mindanao 30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
## 29        Moluccas 15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
## 30     New Britain 19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
## 31      New Guinea 15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
## 32 New Zealand (N) 21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
```

```r
Search(SampDF, 19, max.distance = 0)
```

```
##        islands  mpg cyl  disp  hp drat    wt  qsec vs am gear carb
## 4    Australia 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
## 8       Borneo 24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
## 10     Celebes 19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
## 18  Hispaniola 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
## 20      Honshu 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
## 25       Luzon 19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
## 30 New Britain 19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
```

```r
Search(SampDF, 19, "qsec", max.distance = 0)
```

```
##       islands  mpg cyl  disp  hp drat    wt  qsec vs am gear carb
## 4   Australia 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
## 18 Hispaniola 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
## 20     Honshu 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
```



<h3 id="tools">Generic qdap Tools</h3>

This manual arranges functions into categories in the order a researcher is likely to use them.  The Generic qdap Tools section does not fit this convention, however, because these tools may be used throughout all stages of analysis it is important that the reader is familiar with them.  It is important to note that after reading in transcript data the researcher will likely that the next step is the need to parse the dataframe utilizing the techniques found in the <a href="#cleaning">Cleaning/Preparing the Data</a> section.

<div class="funs">
The following functions will be utilized in this section (click to view more):<br>    

<form class="form_left" action="http://trinker.github.io/qdap_dev/hms2sec.html" target="_blank">
    <input type="submit" value="hms2sec"> 
</form>

<form action="http://trinker.github.io/qdap_dev/sec2hms.html" target="_blank">
    <input type="submit" value="sec2hms"> - <a href="#time">Time Conversion</a> 
</form>

<form class="form_left" action="http://trinker.github.io/qdap_dev/lookup.html" target="_blank">
    <input type="submit" value="lookup"><input type="submit" value="%l%"> 
</form>

<form ction="http://trinker.github.io/qdap_dev/hash.html" target="_blank">
    <input type="submit" value="hash"><input type="submit" value="hash_look"><input type="submit" value="%ha%"> - <a href="#hash">Hash Table/Dictionary Lookup</a>
</form>


<form action="http://trinker.github.io/qdap_dev/qcv.html" target="_blank">
    <input type="submit" value="qcv"> - <a href="#qcv">Quick Character Vector</a>
</form>

<form action="http://trinker.github.io/qdap_dev/url_dl.html" target="_blank">
    <input type="submit" value="url_dl"> - <a href="#urldl">Download Instructional Documents</a>
</form>
</div>

<h4 id="qcv">Quick Character Vector</h4> 

Often it can be tedious to supply quotes to character vectors when dealing with large vectors.  <a href="http://trinker.github.io/qdap_dev/qcv.html" target="_blank"><code>qcv</code></a> replaces the typical <font face="courier">c("A", "B", "C", "...")</font> approach to creating character vectors.  Instead the user supplies <font face="courier">qcv(A, B, C, ...)</font>.  This format assumes single words separated by commas.  If your data/string does not fit this approach the combined `terms` and `split` argument can be utilized.

<font size="5" color="orange">&diams;</font> **Quick Character Vector** <font size="5" color="orange">&diams;</font>


```r
qcv(I, like, dogs)
```

```
## [1] "I"    "like" "dogs"
```

```r
qcv(terms = "I like, big dogs", split = ",")
```

```
## [1] "I like"   "big dogs"
```

```r
qcv(I, like, dogs, space.wrap = TRUE)
```

```
## [1] " I "    " like " " dogs "
```

```r
qcv(I, like, dogs, trailing = TRUE)
```

```
## [1] "I "    "like " "dogs "
```

```r
qcv(I, like, dogs, leading = TRUE)
```

```
## [1] " I"    " like" " dogs"
```

```r
qcv(terms = "mpg cyl  disp  hp drat    wt  qsec vs am gear carb")
```

```
##  [1] "mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"   "gear"
## [11] "carb"
```


<h4 id="hash">Dictionary/Lookup</h4>  

Often the researcher who deals with text data will have the need to lookup values quickly and return an accompanying value.  This is often called a dictionary, hash, or lookup.  This can be used to find corresponding values or recode variables etc.  The <a href="http://trinker.github.io/qdap_dev/lookup.html" target="_blank"><code>lookup</code></a> & <a href="%l%" target="_blank">%l%</a> functions provide a fast enviroment lookup for single usage. The <a href="http://trinker.github.io/qdap_dev/hash.html" target="_blank"><code>hash</code></a> & <a href="http://trinker.github.io/qdap_dev/hash.html" target="_blank">hash_lookup</a>/<a href="http://trinker.github.io/qdap_dev/hash.html" target="_blank">%ha%</a> functions provide a fast enviroment lookup for multiple uses of the same hash table.

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/lookup.html" target="_blank"><code>lookup</code></a>**- *Dictionary/Look Up Examples* <font size="5" color="orange">&diams;</font>


```r
lookup(1:5, data.frame(1:4, 11:14))
```

```
## [1] 11 12 13 14 NA
```

```r
lookup(LETTERS[1:5], data.frame(LETTERS[1:4], 11:14), missing = NULL)
```

```
## [1] "11" "12" "13" "14" "E"
```

```r
lookup(LETTERS[1:5], data.frame(LETTERS[1:5], 100:104))
```

```
## [1] 100 101 102 103 104
```


<pre><code class="r">## Fast with very large vectors
key <- data.frame(x=1:2, y=c("A", "B"))
set.seed(10)
big.vec <- sample(1:2, 3000000, T)
out <- lookup(big.vec, key)
out[1:20]</code></pre>


<pre><code>##  [1] "B" "A" "A" "B" "A" "A" "A" "A" "B" "A" "B" "B" "A"
## [14] "B" "A" "A" "A" "A" "A" "B"</code></pre>


```r
## Supply a named list of vectors to key.match

codes <- list(A=c(1, 2, 4),
    B = c(3, 5),
    C = 7,
    D = c(6, 8:10))

lookup(1:10, codes) #or
```

```
##  [1] "A" "A" "B" "A" "B" "D" "C" "D" "D" "D"
```

```r
1:10 %l% codes
```

```
##  [1] "A" "A" "B" "A" "B" "D" "C" "D" "D" "D"
```



```r
## Supply a single vector to key.match and key.assign

lookup(mtcars$carb, sort(unique(mtcars$carb)),
    c('one', 'two', 'three', 'four', 'six', 'eight'))
```

```
##  [1] "four"  "four"  "one"   "one"   "two"   "one"   "four"  "two"  
##  [9] "two"   "four"  "four"  "three" "three" "three" "four"  "four" 
## [17] "four"  "one"   "two"   "one"   "one"   "two"   "two"   "four" 
## [25] "two"   "one"   "two"   "two"   "four"  "six"   "eight" "two"
```

```r

lookup(mtcars$carb, sort(unique(mtcars$carb)),
    seq(10, 60, by=10))
```

```
##  [1] 40 40 10 10 20 10 40 20 20 40 40 30 30 30 40 40 40 10 20 10 10 20 20
## [24] 40 20 10 20 20 40 50 60 20
```


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/hash.html" target="_blank"><code>hash</code></a>/<a href="http://trinker.github.io/qdap_dev/hash_look.html" target="_blank"><code>hash_look</code></a>**- *Dictionary/Look Up Examples* <font size="5" color="orange">&diams;</font>


```r
## Create a fake data set of hash values
(DF <- aggregate(mpg~as.character(carb), mtcars, mean))
```

```
##   as.character(carb)   mpg
## 1                  1 25.34
## 2                  2 22.40
## 3                  3 16.30
## 4                  4 15.79
## 5                  6 19.70
## 6                  8 15.00
```

```r
## Use `hash` to create a lookup environment
hashTab <- hash(DF)  
## Create a vector to lookup
x <- sample(DF[, 1], 20, TRUE)
## Lookup x in the hash with `hash_look` or `%ha%`
hash_look(x, hashTab)
```

```
##  [1] 25.34 16.30 15.79 22.40 22.40 22.40 25.34 16.30 22.40 15.79 15.79
## [12] 25.34 25.34 25.34 25.34 15.79 16.30 25.34 19.70 16.30
```

```r
x %ha% hashTab
```

```
##  [1] 25.34 16.30 15.79 22.40 22.40 22.40 25.34 16.30 22.40 15.79 15.79
## [12] 25.34 25.34 25.34 25.34 15.79 16.30 25.34 19.70 16.30
```


<h4 id="time">Time Conversion</h4>  

Researchers dealing with transcripts may have the need to convert between traditional Hours:Minutes:Seconds format and seconds.  The <a href="http://trinker.github.io/qdap_dev/hms2sec.html" target="_blank"><code>hms2sec</code></a> and <a href="http://trinker.github.io/qdap_dev/sec2hms.html" target="_blank"><code>sec2hms</code></a> functions offer this type of time conversion.


<font size="5" color="orange">&diams;</font> **Time Conversion Examples** <font size="5" color="orange">&diams;</font>


```r
hms2sec(c("02:00:03", "04:03:01"))
```

```
## [1]  7203 14581
```

```r
hms2sec(sec2hms(c(222, 1234, 55)))
```

```
## [1]  222 1234   55
```

```r
sec2hms(c(256, 3456, 56565))
```

```
## [1] 00:04:16 00:57:36 15:42:45
```


<h4 id="urldl">Download Documents</h4>  
 
<a href="http://trinker.github.io/qdap_dev/url_dl.html" target="_blank"><code>url_dl</code></a> is a function used to provide qdap users with examples taken from the Internet.  It is useful for most document downloads from the Internet.

<font size="5" color="orange">&diams;</font> **url_dl Examples** <font size="5" color="orange">&diams;</font>

<pre><code class="r">## Example 1 (download from dropbox)
# download transcript of the debate to working directory
url_dl(pres.deb1.docx, pres.deb2.docx, pres.deb3.docx)

# load multiple files with read transcript and assign to working directory
dat1 <- read.transcript("pres.deb1.docx", c("person", "dialogue"))
dat2 <- read.transcript("pres.deb2.docx", c("person", "dialogue"))
dat3 <- read.transcript("pres.deb3.docx", c("person", "dialogue"))

docs <- qcv(pres.deb1.docx, pres.deb2.docx, pres.deb3.docx)
dir() %in% docs
delete(docs)    #remove the documents
dir() %in% docs

## Example 2 (quoted string urls)
url_dl("https://dl.dropboxusercontent.com/u/61803503/qdap.pdf",
   "http://www.cran.r-project.org/doc/manuals/R-intro.pdf")

## Clean up
delete(qcv(qdap.pdf, R-intro.pdf))</code></pre>


<h3 id="cleaning">Cleaning/Preparing the Data</h3>

<div class="funs">
The following functions will be utilized in this section (click to view more):    

<form action="http://trinker.github.io/qdap_dev/bracketX.html" target="_blank">
    <input type="submit" value="bracketX"><input type="submit" value="bracketXtract"><input type="submit" value="genX"><input type="submit" value="genXtract"> - <a href="#bracket">Bracket/General Chunk Extraction</a>     
</form>

<form action="http://trinker.github.io/qdap_dev/beg2char.html" target="_blank">
    <input type="submit" value="beg2char"><input type="submit" value="char2end"> - <a href="#grab">Grab Begin/End of String to Character</a> 
</form>

<form action="http://trinker.github.io/qdap_dev/capitalizer.html" target="_blank">
    <input type="submit" value="capitalizer"> - <a href="#caps">Capitalize Select Words</a> 
</form>

<form class="form_left" action="http://trinker.github.io/qdap_dev/clean.html" target="_blank">
    <input type="submit" value="clean">
</form>

<form class="form_left" action="http://trinker.github.io/qdap_dev/scrubber.html" target="_blank">
    <input type="submit" value="scrubber">
</form>

<form action="http://trinker.github.io/qdap_dev/Trim.html" target="_blank">
    <input type="submit" value="Trim">- <a href="#clean">Clean Imported Text: Remove Escaped Characters & Leading/Trailing White Space</a>
</form> 
 

<form action="http://trinker.github.io/qdap_dev/incomplete_replace.html" target="_blank">
    <input type="submit" value="incomplete_replace"><input type="submit" value="incomp"> - <a href="#inc">Denote Incomplete End Marks With "|"</a>
</form>

<form action="http://trinker.github.io/qdap_dev/multigsub.html" target="_blank">
    <input type="submit" value="multigsub"><input type="submit" value="mgsub"> - <a href="#mgsub">Multiple gsub</a>
</form>

<form action="http://trinker.github.io/qdap_dev/name2sex.html" target="_blank">
    <input type="submit" value="name2sex"> - <a href="#nms">Names to Gender Prediction</a>
</form>

<form action="http://trinker.github.io/qdap_dev/potential_NA.html" target="_blank">
    <input type="submit" value="potential_NA"> - <a href="#na">Search for Potential Missing Values</a>
</form>

<form action="http://trinker.github.io/qdap_dev/qprep.html" target="_blank">
    <input type="submit" value="qprep"> - <a href="#replace">Quick Preparation of Text</a>
</form>

<form action="http://trinker.github.io/qdap_dev/replace_abbreviation.html" target="_blank">
    <input type="submit" value="replace_abbreviation"> - <a href="#replace">Replace Abbreviations</a>
</form>

<form action="http://trinker.github.io/qdap_dev/replace_contraction.html" target="_blank">
    <input type="submit" value="replace_contraction"> - <a href="#replace">Replace Contractions</a>
</form>

<form action="http://trinker.github.io/qdap_dev/replace_number.html" target="_blank">
    <input type="submit" value="replace_number"> - <a href="#replace">Replace Numbers With Text Representation</a>
</form>

<form action="http://trinker.github.io/qdap_dev/replace_symbol.html" target="_blank">
    <input type="submit" value="replace_symbol"> - <a href="#replace">Replace Symbols With Word Equivalents</a>
</form>

<form action="http://trinker.github.io/qdap_dev/rm_row.html" target="_blank">
    <input type="submit" value="rm_row"><input type="submit" value="rm_empty_row"> - <a href="#mark">Remove Rows That Contain Markers</a>
</form>

<form action="http://trinker.github.io/qdap_dev/space_fill.html" target="_blank">
    <input type="submit" value="space_fill"> - <a href="#fill">Replace Spaces</a>
</form>

<form action="http://trinker.github.io/qdap_dev/stemmer.html" target="_blank">
    <input type="submit" value="stemmer"><input type="submit" value="stem_words"><input type="submit" value="stem2df"> - <a href="#stem">Stem Text</a>
</form>
 
</div>

<h4 id="bracket">Bracket/General Chunk Extraction <a href="http://youtu.be/B4lvZGo_6bA" target="_blank" style="text-decoration: none"><b><font size="5" color="#B22222">[YT]</font></b></a>
</h4>   

After reading in the data the researcher may want to remove all non-dialogue text from the transcript dataframe such as transcriber annotations.  This can be accomplished with the <a href="http://trinker.github.io/qdap_dev/bracketX.html" target="_blank"><code>bracketX</code></a> family of functions, which removes text found between two brackets (<font face="courier">( )</font>, <font face="courier">{ }</font>, <font face="courier">[ ]</font>, <font face="courier">< ></font>) or more generally using <a href="http://trinker.github.io/qdap_dev/genX.html" target="_blank"><code>genX</code></a> and <a href="http://trinker.github.io/qdap_dev/genXtract.html" target="_blank"><code>genXtract</code></a> to remove text between two character reference points. 

If the bracketed text is useful to analysis it is recommended that the researcher assigns the un-bracketed text to a new column.


<font size="5" color="orange">&diams;</font> **Extracting Chunks 1**- *bracketX/bracketXtract* <font size="5" color="orange">&diams;</font>


```r
## A fake data set
examp <- structure(list(person = structure(c(1L, 2L, 1L, 3L),
    .Label = c("bob", "greg", "sue"), class = "factor"), text =
    c("I love chicken [unintelligible]!",
    "Me too! (laughter) It's so good.[interrupting]",
    "Yep it's awesome {reading}.", "Agreed. {is so much fun}")), .Names =
    c("person", "text"), row.names = c(NA, -4L), class = "data.frame")
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
## [1] "I love chicken !"                 "Me too! (laughter) It's so good."
## [3] "Yep it's awesome {reading} ."     "Agreed. {is so much fun}"
```

```r
bracketX(examp$text, "curly")
```

```
## [1] "I love chicken [unintelligible] !"              
## [2] "Me too! (laughter) It's so good. [interrupting]"
## [3] "Yep it's awesome ."                             
## [4] "Agreed."
```

```r
bracketX(examp$text, c("square", "round"))
```

```
## [1] "I love chicken !"             "Me too! It's so good."       
## [3] "Yep it's awesome {reading} ." "Agreed. {is so much fun}"
```

```r
bracketX(examp$text)
```

```
## [1] "I love chicken !"      "Me too! It's so good." "Yep it's awesome ."   
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


Often a researcher will want to extract some text from the transcript and put it back together.  One example is the reconstructing of material read from a book, poem, play or other text.  This information is generally dispersed throughout the dialogue (within classroom/teaching procedures).   If this text is denoted with a particular identifying bracket such as curly braces this text can be extracted and then pasted back together.

<font size="5" color="orange">&diams;</font> **Extracting Chunks 2**- *Recombining Chunks* <font size="5" color="orange">&diams;</font>


```r
paste2(bracketXtract(examp$text, "curly"), " ")
```

```
## [1] "reading is so much fun"
```


The researcher may need a more general extraction method that allows for any left/right boundaries to be specified.  This is useful in that many qualitative transciption/coding programs have specific syntax for various dialogue markup for events that must be parsed from the data set.  The <a href="http://trinker.github.io/qdap_dev/genX.html" target="_blank"><code>genX</code></a> and <a href="http://trinker.github.io/qdap_dev/genXtract.html" target="_blank"><code>genXtract</code></a> functions have such capabilities.

<font size="5" color="orange">&diams;</font> **Extracting Chunks 3**- *genX/genXtract* <font size="5" color="orange">&diams;</font>

```r
DATA$state  
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
## Look at the difference in number 1 and 10 from above
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
x <- c("Where is the /big dog#?",
    "I think he's @arunning@b with /little cat#.")
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
x2 <- c("Where is the L1big dogL2?",
    "I think he's 98running99 with L1little catL2.")
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


<h4 id="na">Search for Potential Missing Values</h4>

After reading in data, removing non-dialogue (via <a href="http://trinker.github.io/qdap_dev/bracketX.html" target="_blank"><code>bracketX</code></a>), and viewing it the researcher will want to find text rows that do not contain proper punctuation and or that contain punctuation and no text.  This is accomplished with the <a href="http://trinker.github.io/qdap_dev/htruncdf.html" target="_blank"><code>_truncdf</code></a> family of functions and <a href="http://trinker.github.io/qdap_dev/potential_NA.html" target="_blank"><code>potential_NA</code></a> functions as the researcher manually parses the original transcripts, makes alterations and re-reads the data back into qdap.  This important procedure is not an automatic process, requiring that the researcher give attention to detail in comparing the R dataframe with the original transcript.

<font size="5" color="orange">&diams;</font> **Identifying and Coding Missing Values** <font size="5" color="orange">&diams;</font>

```r
## Create a data set with punctuation and no text
DATA$state[c(3, 7, 10)] <- c(".", ".", NA)
DATA
```

```
##        person sex adult                                 state code
## 1         sam   m     0         Computer is fun. Not too fun.   K1
## 2        greg   m     0               No it's not, it's dumb.   K2
## 3     teacher   m     1                                     .   K3
## 4         sam   m     0                  You liar, it stinks!   K4
## 5        greg   m     0               I am telling the truth!   K5
## 6       sally   f     0                How can we be certain?   K6
## 7        greg   m     0                                     .   K7
## 8         sam   m     0                       I distrust you.   K8
## 9       sally   f     0           What are you talking about?   K9
## 10 researcher   f     1                                  <NA>  K10
## 11       greg   m     0 I'm hungry.  Let's eat.  You already?  K11
```

```r
potential_NA(DATA$state, 20)
```

```
##   row            text
## 1   3               .
## 2   7               .
## 3   8 I distrust you.
```

```r
potential_NA(DATA$state)
```

```
##   row text
## 1   3    .
## 2   7    .
```

```r
# USE TO SELCTIVELY REPLACE CELLS WITH MISSING VALUES
DATA$state[potential_NA(DATA$state, 20)$row[-c(3)]] <- NA
DATA
```

```
##        person sex adult                                 state code
## 1         sam   m     0         Computer is fun. Not too fun.   K1
## 2        greg   m     0               No it's not, it's dumb.   K2
## 3     teacher   m     1                                  <NA>   K3
## 4         sam   m     0                  You liar, it stinks!   K4
## 5        greg   m     0               I am telling the truth!   K5
## 6       sally   f     0                How can we be certain?   K6
## 7        greg   m     0                                  <NA>   K7
## 8         sam   m     0                       I distrust you.   K8
## 9       sally   f     0           What are you talking about?   K9
## 10 researcher   f     1                                  <NA>  K10
## 11       greg   m     0 I'm hungry.  Let's eat.  You already?  K11
```

```r
## Reset DATA
DATA <- qdap::DATA
```


<h4 id="mark">Remove Rows That Contain Markers</h4>

The researcher may wish to remove empty rows (using <a href="http://trinker.github.io/qdap_dev/rm_empty_row.html" target="_blank"><code>rm_empty_row</code></a>) and/or rows that contain certain markers (using <a href="http://trinker.github.io/qdap_dev/rm_row.html" target="_blank"><code>rm_row</code></a>).  Sometimes empty rows are read into the dataframe from the transcript.  These rows should be completely removed from the data set rather than denoting with `NA`.  The <a href="http://trinker.github.io/qdap_dev/rm_empty_row.html" target="_blank"><code>rm_empty_row</code></a> removes completely empty rows (those rows with only 1 or more blank spaces) from the dataframe.

<font size="5" color="orange">&diams;</font> **Remove Empty Rows**<font size="5" color="orange">&diams;</font>

```r
(dat <- rbind.data.frame(DATA[, c(1, 4)], matrix(rep(" ", 4),
   ncol =2, dimnames=list(12:13, colnames(DATA)[c(1, 4)]))))
```

```
##        person                                 state
## 1         sam         Computer is fun. Not too fun.
## 2        greg               No it's not, it's dumb.
## 3     teacher                    What should we do?
## 4         sam                  You liar, it stinks!
## 5        greg               I am telling the truth!
## 6       sally                How can we be certain?
## 7        greg                      There is no way.
## 8         sam                       I distrust you.
## 9       sally           What are you talking about?
## 10 researcher         Shall we move on?  Good then.
## 11       greg I'm hungry.  Let's eat.  You already?
## 12                                                 
## 13
```

```r
rm_empty_row(dat)
```

```
##        person                                 state
## 1         sam         Computer is fun. Not too fun.
## 2        greg               No it's not, it's dumb.
## 3     teacher                    What should we do?
## 4         sam                  You liar, it stinks!
## 5        greg               I am telling the truth!
## 6       sally                How can we be certain?
## 7        greg                      There is no way.
## 8         sam                       I distrust you.
## 9       sally           What are you talking about?
## 10 researcher         Shall we move on?  Good then.
## 11       greg I'm hungry.  Let's eat.  You already?
```


Other times the researcher may wish to use <a href="http://trinker.github.io/qdap_dev/rm_row.html" target="_blank"><code>rm_row</code></a> to remove rows from the dataframe/analysis based on transcription conventions or to remove demographic characteristics.  For example, in the example below the transcript is read in with <b>[Cross Talk 3</b>.  This is a transcription convention and we would want to parse these rows from the transcript.  A second example shows the removal of people from the dataframe.

<font size="5" color="orange">&diams;</font> **Remove Selected Rows**<font size="5" color="orange">&diams;</font>


```r
## Read in transcript
dat2 <- read.transcript(system.file("extdata/transcripts/trans1.docx", 
    package = "qdap"))
truncdf(dat2, 40)
```

```
##                  X1                                       X2
## 1      Researcher 2                         October 7, 1892.
## 2         Teacher 4 Students it's time to learn. [Student di
## 3 Multiple Students        Yes teacher we're ready to learn.
## 4     [Cross Talk 3                                      00]
## 5         Teacher 4 Let's read this terrific book together.
```

```r
## Use column names to remove rows
truncdf(rm_row(dat2, "X1", "[C"), 40)
```

```
##                  X1                                       X2
## 1      Researcher 2                         October 7, 1892.
## 2         Teacher 4 Students it's time to learn. [Student di
## 3 Multiple Students        Yes teacher we're ready to learn.
## 4         Teacher 4 Let's read this terrific book together.
```

```r
## Use column numbers to remove rows
truncdf(rm_row(dat2, 2, "[C"), 40)
```

```
##                  X1                                       X2
## 1      Researcher 2                         October 7, 1892.
## 2         Teacher 4 Students it's time to learn. [Student di
## 3 Multiple Students        Yes teacher we're ready to learn.
## 4     [Cross Talk 3                                      00]
## 5         Teacher 4 Let's read this terrific book together.
```

```r
## Also remove people ect. from the analysis
rm_row(DATA, 1, c("sam", "greg"))
```

```
##       person sex adult                         state code
## 1    teacher   m     1            What should we do?   K3
## 2      sally   f     0        How can we be certain?   K6
## 3      sally   f     0   What are you talking about?   K9
## 4 researcher   f     1 Shall we move on?  Good then.  K10
```


<h4 id="clean">Remove Extra Spaces and Escaped Characters</h4> 

An important step in the cleaning process is the removal of extra white spaces (use <a href="http://trinker.github.io/qdap_dev/Trim.html" target="_blank"><code>Trim</code></a>) and <a href="http://stat.ethz.ch/R-manual/R-devel/library/base/html/Quotes.html" target="_blank">escaped characters</a> (use <a href="http://trinker.github.io/qdap_dev/clean.html" target="_blank"><code>clean</code></a>).  The <a href="http://trinker.github.io/qdap_dev/scrubber.html" target="_blank"><code>scrubber</code></a> function wraps both <a href="http://trinker.github.io/qdap_dev/Trim.html" target="_blank"><code>Trim</code></a> and <a href="http://trinker.github.io/qdap_dev/clean.html" target="_blank"><code>clean</code></a> and adds in the functionality of some of the <font face="courier">replace_</font> family of functions.

<font size="5" color="orange">&diams;</font> **Remove Extra Spaces and Escaped Characters**<font size="5" color="orange">&diams;</font>

```r
x1 <- "I go \r
    to the \tnext line"
x1
```

```
## [1] "I go \r\n    to the \tnext line"
```

```r
clean(x1)
```

```
## [1] "I go to the next line"
```

```r
x2 <- c("  talkstats.com ", "   really? ", " yeah")
x2
```

```
## [1] "  talkstats.com " "   really? "      " yeah"
```

```r
Trim(x2)
```

```
## [1] "talkstats.com" "really?"       "yeah"
```

```r
x3 <- c("I like 456 dogs\t  , don't you?\"")
x3
```

```
## [1] "I like 456 dogs\t  , don't you?\""
```

```r
scrubber(x3)
```

```
## [1] "I like 456 dogs, don't you?"
```

```r
scrubber(x3, TRUE)
```

```
## [1] "I like four hundred fifty six dogs, don't you?"
```


<h4 id="replace">Replacement Functions</h4>

The replacement family of functions replace various text elements within the transcripts with alphabetic versions that are more suited to analysis.  These alterations may affect word counts and other alphabetic dependent forms of analysis.

The <a href="http://trinker.github.io/qdap_dev/replace_abbreviation.html" target="_blank"><code>replace_abbreviation</code></a> replaces standard abbreviations that utilize periods with forms that do not rely on periods.  This is necessary in that many sentence specific functions (e.g., <a href="http://trinker.github.io/qdap_dev/sentSplit.html" target="_blank"><code>sentSplit</code></a> and <a href="http://trinker.github.io/qdap_dev/word_stats.html" target="_blank"><code>word_stats</code></a>) rely on period usage acting as sentence endmarks.  The researcher may augment the standard <a href="http://trinker.github.io/qdapDictionaries/abbreviations.html" target="_blank"><code>abbreviations</code></a> dictionary from qdapDictionaries with field specific abbreviations.

<font size="5" color="orange">&diams;</font> **Replace Abbreviations**<font size="5" color="orange">&diams;</font>

```r
## Use the standard contractions dictionary
x <- c("Mr. Jones is here at 7:30 p.m.",
    "Check it out at www.github.com/trinker/qdap",
    "i.e. He's a sr. dr.; the best in 2012 A.D.",
    "the robot at t.s. is 10ft. 3in.")
x
```

```
## [1] "Mr. Jones is here at 7:30 p.m."             
## [2] "Check it out at www.github.com/trinker/qdap"
## [3] "i.e. He's a sr. dr.; the best in 2012 A.D." 
## [4] "the robot at t.s. is 10ft. 3in."
```

```r
replace_abbreviation(x)
```

```
## [1] "Mister Jones is here at 7:30 PM."                    
## [2] "Check it out at www dot github dot com /trinker/qdap"
## [3] "ie He's a Senior Doctor ; the best in 2012 AD."      
## [4] "the robot at t.s. is 10ft. 3in."
```

```r
## Augment the standard dictionary with replacement vectors
abv <- c("in.", "ft.", "t.s.")
repl <- c("inch", "feet", "talkstats")
replace_abbreviation(x, abv, repl)
```

```
## [1] "Mr. Jones is here at 7:30 p.m."             
## [2] "Check it out at www.github.com/trinker/qdap"
## [3] "i.e. He's a sr. dr.; the best in 2012 A.D." 
## [4] "the robot at talkstats is 10 feet 3 inch."
```

```r
## Augment the standard dictionary with a replacement dataframe
(KEY <- rbind(abbreviations, data.frame(abv = abv, rep = repl)))
```

```
##       abv       rep
## 1     Mr.    Mister
## 2    Mrs.    Misses
## 3     Ms.      Miss
## 4    .com   dot com
## 5    www.   www dot
## 6    i.e.        ie
## 7    A.D.        AD
## 8    B.C.        BC
## 9    A.M.        AM
## 10   P.M.        PM
## 11 et al.     et al
## 12    Jr.    Junior
## 13    Dr.    Doctor
## 14    Sr.    Senior
## 15    in.      inch
## 16    ft.      feet
## 17   t.s. talkstats
```

```r
replace_abbreviation(x, KEY)
```

```
## [1] "Mister Jones is here at 7:30 PM."                    
## [2] "Check it out at www dot github dot com /trinker/qdap"
## [3] "ie He's a Senior Doctor ; the best in 2012 AD."      
## [4] "the robot at talkstats is 10 feet 3 inch."
```


The <a href="http://trinker.github.io/qdap_dev/replace_contraction.html" target="_blank"><code>replace_contraction</code></a> replaces contractions with equivalent mult word forms.  This is useful for some word/sentence statistics.  The researcher may augment the <a href="http://trinker.github.io/qdapDictionaries/contractions.html" target="_blank"><code>contractions</code></a> dictionary supplied by qdapDictionaries, however, the word list is exhaustive.

<font size="5" color="orange">&diams;</font> **Replace Contractions**<font size="5" color="orange">&diams;</font>

```r
x <- c("Mr. Jones isn't going.",
    "Check it out what's going on.",
    "He's here but didn't go.",
    "the robot at t.s. wasn't nice",
    "he'd like it if i'd go away")
x
```

```
## [1] "Mr. Jones isn't going."        "Check it out what's going on."
## [3] "He's here but didn't go."      "the robot at t.s. wasn't nice"
## [5] "he'd like it if i'd go away"
```

```r
replace_contraction(x)
```

```
## [1] "Mr. Jones is not going."            
## [2] "Check it out what is going on."     
## [3] "He is here but did not go."         
## [4] "The robot at t.s. was not nice"     
## [5] "He would like it if I would go away"
```


The <a href="http://trinker.github.io/qdap_dev/replace_number.html" target="_blank"><code>replace_number</code></a> function utilizes The work of John <a href="">Fox (2005)</a> to turn numeric representations of numbers into their textual equivalents.  This is useful for word statistics that require the text version of dialogue.

<font size="5" color="orange">&diams;</font> **Replace Numbers**-*Numeral Representation*<font size="5" color="orange">&diams;</font>

```r
x <- c("I like 346457 ice cream cones.", "They are 99 percent good")
replace_number(x)
```

```
## [1] "I like three hundred forty six thousand four hundred fifty seven ice cream cones."
## [2] "They are ninety nine percent good"
```

```r
## Replace numbers that contain commas as well
y <- c("I like 346,457 ice cream cones.", "They are 99 percent good")
replace_number(y)
```

```
## [1] "I like three hundred forty six thousand four hundred fifty seven ice cream cones."
## [2] "They are ninety nine percent good"
```

```r
## Combine numbers as one word/string
replace_number(x, "combine")
```

```
## [1] "I like threehundredfortysixthousandfourhundredfiftyseven ice cream cones."
## [2] "They are ninetynine percent good"
```



The <a href="http://trinker.github.io/qdap_dev/replace_symbol.html" target="_blank"><code>replace_symbol</code></a> converts ($) to "dollar", (%) to "percent", (#) to "number", (@) to "at", (&) to "and", (w/) to "with".  Additional substitutions can be undertaken with the <a href="http://trinker.github.io/qdap_dev/multigsub.html" target="_blank"><code>multigsub</code></a> function. 

<font size="5" color="orange">&diams;</font> **Replace Symbols**<font size="5" color="orange">&diams;</font>

```r
x <- c("I am @ Jon's & Jim's w/ Marry",
    "I owe $41 for food",
    "two is 10% of a #")
x
```

```
## [1] "I am @ Jon's & Jim's w/ Marry" "I owe $41 for food"           
## [3] "two is 10% of a #"
```

```r
replace_symbol(x)
```

```
## [1] "I am at Jon's and Jim's with Marry"
## [2] "I owe dollar 41 for food"          
## [3] "two is 10 percent of a number"
```

```r
replace_number(replace_symbol(x))
```

```
## [1] "I am at Jon's and Jim's with Marry"
## [2] "I owe dollar forty one for food"   
## [3] "two is ten percent of a number"
```


The <a href="http://trinker.github.io/qdap_dev/qprep.html" target="_blank"><code>qprep</code></a> function is a wrapper for several other replcement family function that allows for more speedy cleaning of the text.  This approach, while speedy, reduces the flexiblity and care that is undertaken by the researcher when the individual replacment functions are utilized.  The function is intended for analysis that requires less care.

<font size="5" color="orange">&diams;</font> **General Replacement (Quick Preparation)**<font size="5" color="orange">&diams;</font>

```r
x <- "I like 60 (laughter) #d-bot and $6 @ the store w/o 8p.m."
x
```

```
## [1] "I like 60 (laughter) #d-bot and $6 @ the store w/o 8p.m."
```

```r
qprep(x)
```

```
## [1] "I like sixty number d bot and dollar six at the store without eight PM."
```



<h4 id="fill">Replace Spaces</h4>

Many qdap functions break sentences up into words based on the spaces between words.  Often the researcher will want to keep a group of words as a single unit.  The <a href="http://trinker.github.io/qdap_dev/space_fill.html" target="_blank"><code>space_fill</code></a> allows the researcher to replace spaces between selected phrases with <b><font color="blue" face="courier">&#126;&#126;</font></b>.  By defualt <b><font color="blue" face="courier">&#126;&#126;</font></b> is recognized by many qdap functions as a space separator.

<font size="5" color="orange">&diams;</font> **Space Fill Examples**<font size="5" color="orange">&diams;</font>

```r
## Fake Data
x <- c("I want to hear the Dr. Martin Luther King Jr. speech.",
    "I also want to go to the white House to see President Obama speak.")
x
```

```
## [1] "I want to hear the Dr. Martin Luther King Jr. speech."             
## [2] "I also want to go to the white House to see President Obama speak."
```

```r
## Words to keep as a single unit
keeps <- c("Dr. Martin Luther King Jr.", "The White House", "President Obama")
text <- space_fill(x, keeps)
text
```

```
## [1] "I want to hear the Dr.~~Martin~~Luther~~King~~Jr. speech."            
## [2] "I also want to go to The~~White~~House to see President~~Obama speak."
```

```r
## strip Example
strip(text, lower=FALSE)
```

```
## [1] "I want to hear the Dr~~Martin~~Luther~~King~~Jr speech"              
## [2] "I also want to go to The~~White~~House to see President~~Obama speak"
```

```r
## bag_o_words Example
bag_o_words(text, lower=FALSE)
```

```
##  [1] "I"                            "want"                        
##  [3] "to"                           "hear"                        
##  [5] "the"                          "Dr~~Martin~~Luther~~King~~Jr"
##  [7] "speech"                       "I"                           
##  [9] "also"                         "want"                        
## [11] "to"                           "go"                          
## [13] "to"                           "The~~White~~House"           
## [15] "to"                           "see"                         
## [17] "President~~Obama"             "speak"
```

```r
## wfm Example
wfm(text, c("greg", "bob"))
```

```
##                          bob greg
## also                       1    0
## dr martin luther king jr   0    1
## go                         1    0
## hear                       0    1
## i                          1    1
## president obama            1    0
## see                        1    0
## speak                      1    0
## speech                     0    1
## the                        0    1
## the white house            1    0
## to                         3    1
## want                       1    1
```

```r
## trans_cloud Example
obs <- strip(space_fill(keeps, keeps), lower=FALSE)
trans_cloud(text, c("greg", "bob"), target.words=list(obs), caps.list=obs, 
    cloud.colors=qcv(red, gray65), expand.target = FALSE, title.padj = .7,
    legend = c("space_filled", "other"), title.cex = 2, title.color = "blue", 
    max.word.size = 3)
```

![plot of chunk unnamed-chunk-26](figure/unnamed-chunk-261.png) ![plot of chunk unnamed-chunk-26](figure/unnamed-chunk-262.png) 


<h4 id="mgsub">Multiple gsub</h4>

The researcher may have the need to make multiple substitutions in a text.  An example of when this is needed is when a transcript is marked up with transctiption coding convention specific to a particular transcription method.  These codes, while useful in some contexts, may lead to inaccurate word statisitcs.  The base R function <a href="http://stat.ethz.ch/R-manual/R-devel/library/base/html/grep.html" target="_blank">gsub</a> makes a single replacement of these types of coding conventions. The <a href="http://trinker.github.io/qdap_dev/multigsub.html" target="_blank"><code>multigsub</code></a> (alias <a href="http://trinker.github.io/qdap_dev/mgsub.html" target="_blank"><code>mgsub</code></a>) takes a vector of patterns to search for as well as a vector of replacments.  Note that the replacements occur sequentially rather than all at once. This means a previous (first in pattern string) sub could alter or be altered by a later sub.  <a href="http://trinker.github.io/qdap_dev/mgsub.html" target="_blank"><code>mgsub</code></a> is useful throughout multiple stages of the research process.

<font size="5" color="orange">&diams;</font> **Multiple Substitutions**<font size="5" color="orange">&diams;</font>

```r
left_just(DATA[, c(1, 4)])
```

```
##    person     state                                
## 1  sam        Computer is fun. Not too fun.        
## 2  greg       No it's not, it's dumb.              
## 3  teacher    What should we do?                   
## 4  sam        You liar, it stinks!                 
## 5  greg       I am telling the truth!              
## 6  sally      How can we be certain?               
## 7  greg       There is no way.                     
## 8  sam        I distrust you.                      
## 9  sally      What are you talking about?          
## 10 researcher Shall we move on?  Good then.        
## 11 greg       I'm hungry.  Let's eat.  You already?
```

```r
multigsub(c("it's", "I'm"), c("it is", "I am"), DATA$state)
```

```
##  [1] "Computer is fun. Not too fun."       
##  [2] "No it is not, it is dumb."           
##  [3] "What should we do?"                  
##  [4] "You liar, it stinks!"                
##  [5] "I am telling the truth!"             
##  [6] "How can we be certain?"              
##  [7] "There is no way."                    
##  [8] "I distrust you."                     
##  [9] "What are you talking about?"         
## [10] "Shall we move on? Good then."        
## [11] "I am hungry. Let's eat. You already?"
```

```r
mgsub(c("it's", "I'm"), c("it is", "I am"), DATA$state)
```

```
##  [1] "Computer is fun. Not too fun."       
##  [2] "No it is not, it is dumb."           
##  [3] "What should we do?"                  
##  [4] "You liar, it stinks!"                
##  [5] "I am telling the truth!"             
##  [6] "How can we be certain?"              
##  [7] "There is no way."                    
##  [8] "I distrust you."                     
##  [9] "What are you talking about?"         
## [10] "Shall we move on? Good then."        
## [11] "I am hungry. Let's eat. You already?"
```

```r
mgsub(c("it's", "I'm"), "SINGLE REPLACEMENT", DATA$state)
```

```
##  [1] "Computer is fun. Not too fun."                      
##  [2] "No SINGLE REPLACEMENT not, SINGLE REPLACEMENT dumb."
##  [3] "What should we do?"                                 
##  [4] "You liar, it stinks!"                               
##  [5] "I am telling the truth!"                            
##  [6] "How can we be certain?"                             
##  [7] "There is no way."                                   
##  [8] "I distrust you."                                    
##  [9] "What are you talking about?"                        
## [10] "Shall we move on? Good then."                       
## [11] "SINGLE REPLACEMENT hungry. Let's eat. You already?"
```

```r
mgsub("[[:punct:]]", "PUNC", DATA$state, fixed = FALSE)
```

```
##  [1] "Computer is funPUNC Not too funPUNC"               
##  [2] "No itPUNCs notPUNC itPUNCs dumbPUNC"               
##  [3] "What should we doPUNC"                             
##  [4] "You liarPUNC it stinksPUNC"                        
##  [5] "I am telling the truthPUNC"                        
##  [6] "How can we be certainPUNC"                         
##  [7] "There is no wayPUNC"                               
##  [8] "I distrust youPUNC"                                
##  [9] "What are you talking aboutPUNC"                    
## [10] "Shall we move onPUNC Good thenPUNC"                
## [11] "IPUNCm hungryPUNC LetPUNCs eatPUNC You alreadyPUNC"
```

```r
## Iterative "I'm" converts to "I am" which converts to "INTERATIVE"
mgsub(c("it's", "I'm", "I am"), c("it is", "I am", "ITERATIVE"), DATA$state)
```

```
##  [1] "Computer is fun. Not too fun."            
##  [2] "No it is not, it is dumb."                
##  [3] "What should we do?"                       
##  [4] "You liar, it stinks!"                     
##  [5] "ITERATIVE telling the truth!"             
##  [6] "How can we be certain?"                   
##  [7] "There is no way."                         
##  [8] "I distrust you."                          
##  [9] "What are you talking about?"              
## [10] "Shall we move on? Good then."             
## [11] "ITERATIVE hungry. Let's eat. You already?"
```



<h4 id="nms">Names to Gender Prediction</h4>

A researcher may face a list of names and be uncertain about gender of the participants.  The <a href="http://trinker.github.io/qdap_dev/name2sex.html" target="_blank"><code>name2sex</code></a> function utilizes the <a href="http://trinker.github.io/qdapDictionaries/NAMES_LIST.html" target="_blank"><code>NAMES_LIST</code></a> dictionary based on the 1990 U.S. census data.  For gender neutral names the gender with the higher assignment rate is assumed if <font face="courier">pred.sex</font> is set to <font face="courier">TRUE</font>, otherwise a <font color="blue">B</font> is assigned to indicate "both" genders.  For names not matching the <a href="http://trinker.github.io/qdapDictionaries/NAMES_LIST.html" target="_blank"><code>NAMES_LIST</code></a> optional fuzzy matching can be utilized via the <font face="courier">fuzzy.match</font> argument based on the use of <a href="http://stat.ethz.ch/R-manual/R-devel/library/base/html/agrep.html" target="_blank">agrep</a>.  Both of these argument increase accuracy but act at the cost of speed.  The use of <font face="courier">fuzzy.match = TRUE</font> is particularly computationaly costly.

<font size="5" color="orange">&diams;</font> **Name to Gender Prediction**<font size="5" color="orange">&diams;</font>

```r
name2sex(qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA, tyler, jamie, JAMES, 
    tyrone, cheryl, drew), pred.sex = TRUE, fuzzy.match = TRUE)
```

```
##  [1] F F F M M F M F M M F M
## Levels: F M
```

```r
name2sex(qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA, tyler, jamie, JAMES, 
    tyrone, cheryl, drew), pred.sex = FALSE, fuzzy.match = FALSE)
```

```
##  [1] B    <NA> F    B    B    F    B    B    B    M    F    B   
## Levels: B F M
```

```r
name2sex(qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA, tyler, jamie, JAMES, 
    tyrone, cheryl, drew), pred.sex = FALSE, fuzzy.match = TRUE)
```

```
##  [1] B F F B B F B B B M F B
## Levels: B F M
```

```r
name2sex(qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA, tyler, jamie, JAMES, 
    tyrone, cheryl, drew), pred.sex = TRUE, fuzzy.match = FALSE)
```

```
##  [1] F    <NA> F    M    M    F    M    F    M    M    F    M   
## Levels: F M
```

```r
## Get rank percent frequency ratio of being a gender
library(qdapDictionaries)
orig_nms <- qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA,
    tyler, jamie, JAMES, tyrone, cheryl, drew)

sex <- name2sex(orig_nms, FALSE, TRUE)

names(sex) <- rep("", length(sex))
names(sex)[sex == "B"] <- sapply(toupper(orig_nms[sex == "B"]), function(x) {
        y <- NAMES[NAMES[, 1] %in% x, ]
        round(log(Reduce("/", y[ order(y[, "gender"]), "per.freq"])), 2)
    })

## The log ratio of being a female name
data.frame(name = orig_nms, sex = sex, `ratio_F:M` = names(sex),
    check.names=FALSE)
```

```
##       name sex ratio_F:M
## 1     mary   B      5.68
## 2     jenn   F          
## 3    linda   F          
## 4     JAME   B     -2.08
## 5  GABRIEL   B     -3.19
## 6    OLIVA   F          
## 7    tyler   B      -3.8
## 8    jamie   B      0.84
## 9    JAMES   B      -5.8
## 10  tyrone   M          
## 11  cheryl   F          
## 12    drew   B     -3.18
```


<h4 id="stem">Stem Text</h4>

During the initial cleaning stage of analysis the researcher may chose to create a stemmed verion of the dialogue, that is words are reduced to their root words.  The <a href="http://trinker.github.io/qdap_dev/stemmer.html" target="_blank"><code>stemmer</code></a> family of functions allow the researcher to create stemmed text.  The <a href="http://trinker.github.io/qdap_dev/stem2df.html" target="_blank"><code>stem2df</code></a> function wraps <a href="http://trinker.github.io/qdap_dev/stemmer.html" target="_blank"><code>stemmer</code></a> to quickly create a dataframe with the stemmed column added.

<font size="5" color="orange">&diams;</font> **Stemming**<font size="5" color="orange">&diams;</font>

```r
## stem2df EXAMPLE:
stem2df(DATA, "state", "new")
```

```
##        person sex adult                                 state code
## 1         sam   m     0         Computer is fun. Not too fun.   K1
## 2        greg   m     0               No it's not, it's dumb.   K2
## 3     teacher   m     1                    What should we do?   K3
## 4         sam   m     0                  You liar, it stinks!   K4
## 5        greg   m     0               I am telling the truth!   K5
## 6       sally   f     0                How can we be certain?   K6
## 7        greg   m     0                      There is no way.   K7
## 8         sam   m     0                       I distrust you.   K8
## 9       sally   f     0           What are you talking about?   K9
## 10 researcher   f     1         Shall we move on?  Good then.  K10
## 11       greg   m     0 I'm hungry.  Let's eat.  You already?  K11
##                                new
## 1       Comput is fun not too fun.
## 2               No it not it dumb.
## 3               What should we do?
## 4               You liar it stink!
## 5             I am tell the truth!
## 6           How can we be certain?
## 7                 There is no way.
## 8                  I distrust you.
## 9         What are you talk about?
## 10     Shall we move on good then.
## 11 I'm hungri let eat you alreadi?
```

```r
with(stem2df(DATA, "state", "new"), trans_cloud(new, sex, title.cex = 2.5, 
    title.color = "blue", max.word.size = 5, title.padj = .7))
```

![plot of chunk unnamed-chunk-29](figure/unnamed-chunk-291.png) ![plot of chunk unnamed-chunk-29](figure/unnamed-chunk-292.png) 

```r
## stemmer EXAMPLE:
stemmer(DATA$state)
```

```
##  [1] "Comput is fun not too fun."      "No it not it dumb."             
##  [3] "What should we do?"              "You liar it stink!"             
##  [5] "I am tell the truth!"            "How can we be certain?"         
##  [7] "There is no way."                "I distrust you."                
##  [9] "What are you talk about?"        "Shall we move on good then."    
## [11] "I'm hungri let eat you alreadi?"
```

```r
## stem_words EXAMPLE:
stem_words(doggies, jumping, swims)
```

```
## [1] "doggi" "jump"  "swim"
```



<h4 id="grab">Grab Begin/End of String to Character</h4>

At times it is handy to be able to grab from the begining or enf of a string to a specific character.  The <a href="http://trinker.github.io/qdap_dev/beg2char.html" target="_blank"><code>beg2char</code></a> function allows you to grab from the begining of a string to the n<sup>th</sup> occurenece of a character.  The counterpart function, <a href="http://trinker.github.io/qdap_dev/char2end.html" target="_blank"><code>char2end</code></a>, grab from the n<sup>th</sup> occurenece of a character to the end of a string to. This behavior is useful if the transcript contains annotations at the begining or end of a line that should beliminated.

<font size="5" color="orange">&diams;</font> **Grab From Character to Beginning/End of String**<font size="5" color="orange">&diams;</font>

```r
x <- c("a_b_c_d", "1_2_3_4", "<_?_._:")
beg2char(x, "_")
```

```
## [1] "a" "1" "<"
```

```r
beg2char(x, "_", 4)
```

```
## [1] "a_b_c_d" "1_2_3_4" "<_?_._:"
```

```r
char2end(x, "_")
```

```
## [1] "b_c_d" "2_3_4" "?_._:"
```

```r
char2end(x, "_", 2)
```

```
## [1] "c_d" "3_4" "._:"
```

```r
char2end(x, "_", 3, include=TRUE)
```

```
## [1] "_d" "_4" "_:"
```

```r
(x2 <- gsub("_", " ", x))
```

```
## [1] "a b c d" "1 2 3 4" "< ? . :"
```

```r
beg2char(x2, " ", 2)
```

```
## [1] "a b" "1 2" "< ?"
```

```r
(x3 <- gsub("_", "\\^", x))
```

```
## [1] "a^b^c^d" "1^2^3^4" "<^?^.^:"
```

```r
char2end(x3, "^", 2)
```

```
## [1] "c^d" "3^4" ".^:"
```


<h4 id="inc">Denote Incomplete End Marks With "|"</h4> 

Often incomplete sentences have a different function than complete sentences.  The researcher may want to denote incomplete sentences for consideration in later analysis.  Traditionally, incomplete sentence are denoted with the following end marks (.., ..., .?, ..?, en & em).  The <a href="http://trinker.github.io/qdap_dev/incomplete_replace.html" target="_blank"><code>incomplete_replace</code></a> can identify and replace the traditional endmarks with a standard form <font color="blue">"|"</font>.

<font size="5" color="orange">&diams;</font> **Incomplete Sentence Identification**<font size="5" color="orange">&diams;</font>

```r
x <- c("the...",  "I.?", "you.", "threw..", "we?")
incomplete_replace(x)
```

```
## [1] "the|"   "I|"     "you."   "threw|" "we?"
```

```r
incomp(x)
```

```
## [1] "the|"   "I|"     "you."   "threw|" "we?"
```

```r
incomp(x, scan.mode = TRUE)
```

```
##   row.num text   
## 1       1 the... 
## 2       2 I.?    
## 3       4 threw..
```



<h4 id="caps">Capitalize Select Words</h4>

The <a href="http://trinker.github.io/qdap_dev/capitalizer.html" target="_blank"><code>capitalizer</code></a> functions allows the researcher to specify words within a vector to be capitalized.  By defualt <font color="blue">I</font>, and contractions containing <font color="blue">I</font>, are capitalized.  Additional words can be specified through the <font face="courier">caps.list</font> argument.  To capitalize words within strings the <a href="http://trinker.github.io/qdap_dev/mgsub.html" target="_blank"><code>mgsub</code></a> can be used.

<font size="5" color="orange">&diams;</font> **Word Capitalization**<font size="5" color="orange">&diams;</font>

```r
capitalizer(bag_o_words("i like it but i'm not certain"), "like")
```

```
## [1] "I"       "Like"    "it"      "but"     "I'm"     "not"     "certain"
```

```r
capitalizer(bag_o_words("i like it but i'm not certain"), "like", FALSE)
```

```
## [1] "i"       "Like"    "it"      "but"     "i'm"     "not"     "certain"
```


<h3 id="reshaping">Reshaping the Data</h3>

<div class="funs">
The following functions will be utilized in this section (click to view more):    

<form action="http://trinker.github.io/qdap_dev/adjacency_matrix.html" target="_blank">
    <input type="submit" value="adjacency_matrix"><input type="submit" value="adjmat"> - <a href="#adj">Create Adjacency Matrix</a>
</form>


<form class="form_left" action="http://trinker.github.io/qdap_dev/gantt.html" target="_blank">
    <input type="submit" value="gantt">
</form>
<form action="http://trinker.github.io/qdap_dev/gantt_rep.html" target="_blank">    
    <input type="submit" value="gantt_rep">- <a href="#ganttspan">Generate Unit Spans</a>
</form>


<form action="http://trinker.github.io/qdap_dev/key_merge.html" target="_blank">
    <input type="submit" value="key_merge"> - <a href="#merge">Merge Demographic Information with Person/Text Transcript</a>
</form>


<form class="form_left" action="http://trinker.github.io/qdap_dev/paste2.html" target="_blank">
    <input type="submit" value="paste2"/>
</form>
<form class="form_left" action="http://trinker.github.io/qdap_dev/paste2.html" target="_blank">
    <input type="submit" value="colpaste2df"/>
</form>
<form class="form_left" action="http://trinker.github.io/qdap_dev/colSplit.html" target="_blank">
    <input type="submit" value="colSplit"/>
</form>
<form class="form_left" action="http://trinker.github.io/qdap_dev/colsplit2df.html" target="_blank">
    <input type="submit" value="colsplit2df"/>
</form>
<form action="http://trinker.github.io/qdap_dev/colsplit2df.html" target="_blank">
    <input type="submit" value="lcolsplit2df"/>- <a href="#paste2">Paste and Separate Columns</a>
</form>


<form class="form_left" action="http://trinker.github.io/qdap_dev/sentSplit.html" target="_blank">
    <input type="submit" value="sentSplit"><input type="submit" value="sentCombine"><input type="submit" value="TOT">
</form>

<form action="http://trinker.github.io/qdap_dev/speakerSplit.html" target="_blank">
    <input type="submit" value="speakerSplit"> - <a href="#sentsplit">Sentence Splitting/Combining</a>
</form>

</div>

<h4 id="sentsplit">Sentence Splitting/Combining</h4>

Many functions in the qdap package require that the dialogue is broken apart into individual sentences, failure to do so may invalidate many of the outputs from the analysis and will lead to lead to warnings.  After reading in and cleaning the data the next step should be to split the text variable into individual sentences.  The <a href="http://trinker.github.io/qdap_dev/sentSplit.html" target="_blank"><code>sentSplit</code></a> function outputs a dataframe with the text variable split into individual sentences and repeats the demographic variables as necessary.  Additionally, a turn of talk (<font color="red">tot column</font>) variable is added that keeps track of the original turn of talk (row number) and the sentence number per turn of talk.  The researcher may also want to create a second text column that has benn stemmed for future analysis by setting <font face="courier">stem.col = TRUE</font>, though this is more time intensive.

<font size="5" color="orange">&diams;</font> **Sentence Splitting**<font size="5" color="orange">&diams;</font>


```r
sentSplit(DATA, "state")
```

```
##        person  tot sex adult code                       state
## 1         sam  1.1   m     0   K1            Computer is fun.
## 2         sam  1.2   m     0   K1                Not too fun.
## 3        greg  2.1   m     0   K2     No it's not, it's dumb.
## 4     teacher  3.1   m     1   K3          What should we do?
## 5         sam  4.1   m     0   K4        You liar, it stinks!
## 6        greg  5.1   m     0   K5     I am telling the truth!
## 7       sally  6.1   f     0   K6      How can we be certain?
## 8        greg  7.1   m     0   K7            There is no way.
## 9         sam  8.1   m     0   K8             I distrust you.
## 10      sally  9.1   f     0   K9 What are you talking about?
## 11 researcher 10.1   f     1  K10           Shall we move on?
## 12 researcher 10.2   f     1  K10                  Good then.
## 13       greg 11.1   m     0  K11                 I'm hungry.
## 14       greg 11.2   m     0  K11                  Let's eat.
## 15       greg 11.3   m     0  K11                You already?
```

```r
sentSplit(DATA, "state", stem.col = TRUE)
```

```
##        person  tot sex adult code                       state
## 1         sam  1.1   m     0   K1            Computer is fun.
## 2         sam  1.2   m     0   K1                Not too fun.
## 3        greg  2.1   m     0   K2     No it's not, it's dumb.
## 4     teacher  3.1   m     1   K3          What should we do?
## 5         sam  4.1   m     0   K4        You liar, it stinks!
## 6        greg  5.1   m     0   K5     I am telling the truth!
## 7       sally  6.1   f     0   K6      How can we be certain?
## 8        greg  7.1   m     0   K7            There is no way.
## 9         sam  8.1   m     0   K8             I distrust you.
## 10      sally  9.1   f     0   K9 What are you talking about?
## 11 researcher 10.1   f     1  K10           Shall we move on?
## 12 researcher 10.2   f     1  K10                  Good then.
## 13       greg 11.1   m     0  K11                 I'm hungry.
## 14       greg 11.2   m     0  K11                  Let's eat.
## 15       greg 11.3   m     0  K11                You already?
##                   stem.text
## 1            Comput is fun.
## 2              Not too fun.
## 3        No it not it dumb.
## 4        What should we do?
## 5        You liar it stink!
## 6      I am tell the truth!
## 7    How can we be certain?
## 8          There is no way.
## 9           I distrust you.
## 10 What are you talk about?
## 11        Shall we move on?
## 12               Good then.
## 13              I'm hungri.
## 14                 Let eat.
## 15             You alreadi?
```

```r
sentSplit(raj, "dialogue")[1:11, ]
```

```
##     person tot act                                               dialogue
## 1  Sampson 1.1   1             Gregory, o my word, we'll not carry coals.
## 2  Gregory 2.1   1                    No, for then we should be colliers.
## 3  Sampson 3.1   1                I mean, an we be in choler, we'll draw.
## 4  Gregory 4.1   1   Ay, while you live, draw your neck out o the collar.
## 5  Sampson 5.1   1                         I strike quickly, being moved.
## 6  Gregory 6.1   1              But thou art not quickly moved to strike.
## 7  Sampson 7.1   1               A dog of the house of Montague moves me.
## 8  Gregory 8.1   1     To move is to stir; and to be valiant is to stand.
## 9  Gregory 8.2   1       therefore, if thou art moved, thou runn'st away.
## 10 Sampson 9.1   1            A dog of that house shall move me to stand.
## 11 Sampson 9.2   1 I will take the wall of any man or maid of Montague's.
```

```r
## Convert tot column wirh sub sentences to turns of talk
dat <- sentSplit(DATA, "state")
TOT(dat$tot)
```

```
##  1.1  1.2  2.1  3.1  4.1  5.1  6.1  7.1  8.1  9.1 10.1 10.2 11.1 11.2 11.3 
##    1    1    2    3    4    5    6    7    8    9   10   10   11   11   11
```


Within dialogue (particularly classroom dialogue) several speakers may say the same speech at the same.  The transcripts may lump this speech together in the form of: 

<TABLE>
    <TR> <TD align="right"><b>Person</b></TD> <TD align="right"><b>Dialogue</b></TD> </TR>
    <TR> <TD align="right"> John, Josh & Imani &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</TD> <TD align="right"> Yes Mrs. Smith. &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</TD> </TR>
</TABLE>

The <a href="http://trinker.github.io/qdap_dev/speakerSplit.html" target="_blank"><code>speakerSplit</code></a> function attributes this text to each of the people as separate entries.  The default behavior is the search for the person separators of <font face="courier">sep = c("and", "&", ",")</font>, though other separators may be specified.

<font size="5" color="orange">&diams;</font> **Break and Stretch if Multiple Persons per Cell**<font size="5" color="orange">&diams;</font>


```r
## Create data set with multiple speakers per turn of talk
DATA$person <- as.character(DATA$person)
DATA$person[c(1, 4, 6)] <- c("greg, sally, & sam",
    "greg, sally", "sam and sally")
speakerSplit(DATA)
```

```
##        person sex adult                                 state code
## 1        greg   m     0         Computer is fun. Not too fun.   K1
## 2       sally   m     0         Computer is fun. Not too fun.   K1
## 3         sam   m     0         Computer is fun. Not too fun.   K1
## 4        greg   m     0               No it's not, it's dumb.   K2
## 5     teacher   m     1                    What should we do?   K3
## 6        greg   m     0                  You liar, it stinks!   K4
## 7       sally   m     0                  You liar, it stinks!   K4
## 8        greg   m     0               I am telling the truth!   K5
## 9         sam   f     0                How can we be certain?   K6
## 10      sally   f     0                How can we be certain?   K6
## 11       greg   m     0                      There is no way.   K7
## 12        sam   m     0                       I distrust you.   K8
## 13      sally   f     0           What are you talking about?   K9
## 14 researcher   f     1         Shall we move on?  Good then.  K10
## 15       greg   m     0 I'm hungry.  Let's eat.  You already?  K11
```

```r
## Change the separator
DATA$person[c(1, 4, 6)] <- c("greg_sally_sam",
    "greg.sally", "sam; sally")
speakerSplit(DATA, sep = c(".", "_", ";"))
```

```
##        person sex adult                                 state code
## 1        greg   m     0         Computer is fun. Not too fun.   K1
## 2       sally   m     0         Computer is fun. Not too fun.   K1
## 3         sam   m     0         Computer is fun. Not too fun.   K1
## 4        greg   m     0               No it's not, it's dumb.   K2
## 5     teacher   m     1                    What should we do?   K3
## 6        greg   m     0                  You liar, it stinks!   K4
## 7       sally   m     0                  You liar, it stinks!   K4
## 8        greg   m     0               I am telling the truth!   K5
## 9         sam   f     0                How can we be certain?   K6
## 10      sally   f     0                How can we be certain?   K6
## 11       greg   m     0                      There is no way.   K7
## 12        sam   m     0                       I distrust you.   K8
## 13      sally   f     0           What are you talking about?   K9
## 14 researcher   f     1         Shall we move on?  Good then.  K10
## 15       greg   m     0 I'm hungry.  Let's eat.  You already?  K11
```

```r
## Reset DATA
DATA <- qdap::DATA  
```


The <a href="http://trinker.github.io/qdap_dev/sentCombine.html" target="_blank"><code>sentCombine</code></a> function is the opposite of the <a href="http://trinker.github.io/qdap_dev/sentSplit.html" target="_blank"><code>sentSplit</code></a>, combining sentences into a single turn of talk per grouping variable.

<font size="5" color="orange">&diams;</font> **Sentence Combining**<font size="5" color="orange">&diams;</font>


```r
dat <- sentSplit(DATA, "state")
## Combine by person
sentCombine(dat$state, dat$person)
```

```
##        person                            text.var
## 1         sam       Computer is fun. Not too fun.
## 2        greg             No it's not, it's dumb.
## 3     teacher                  What should we do?
## 4         sam                You liar, it stinks!
## 5        greg             I am telling the truth!
## 6       sally              How can we be certain?
## 7        greg                    There is no way.
## 8         sam                     I distrust you.
## 9       sally         What are you talking about?
## 10 researcher        Shall we move on? Good then.
## 11       greg I'm hungry. Let's eat. You already?
```

```r
## Combine by sex
truncdf(sentCombine(dat$state, dat$sex), 65)
```

```
##   sex                                                          text.var
## 1   m Computer is fun. Not too fun. No it's not, it's dumb. What should
## 2   f                                            How can we be certain?
## 3   m                                  There is no way. I distrust you.
## 4   f          What are you talking about? Shall we move on? Good then.
## 5   m                               I'm hungry. Let's eat. You already?
```


<h4 id="merge">Merge Demographic Information with Person/Text Transcript</h4>

It is more efficient to maintain a dialogue dataframe (consisting of a column for people and a column for dialogue) and a separate demographics dataframe (a person column and demographic column(s)) and then merge the two during analysis.  The <a href="http://trinker.github.io/qdap_dev/key_merge.html" target="_blank"><code>key_merge</code></a> function is a wrapper for the <a href="http://stat.ethz.ch/R-manual/R-devel/library/base/html/merge.html" target="_blank">merge</a> function from R's base install that merges the dialogue and demographics dataframe. <a href="http://trinker.github.io/qdap_dev/key_merge.html" target="_blank"><code>key_merge</code></a> attempts to guess the person column and outputs a qdap friendly dataframe.

<font size="5" color="orange">&diams;</font> **Merging Demographic Information**<font size="5" color="orange">&diams;</font>


```r
## A dialogue dataframe and a demographocs dataframe
ltruncdf(list(dialogue=raj, demographics=raj.demographics), 10, 50)
```

```
## $dialogue
##     person                                           dialogue act
## 1  Sampson         Gregory, o my word, we'll not carry coals.   1
## 2  Gregory                No, for then we should be colliers.   1
## 3  Sampson            I mean, an we be in choler, we'll draw.   1
## 4  Gregory Ay, while you live, draw your neck out o the colla   1
## 5  Sampson                     I strike quickly, being moved.   1
## 6  Gregory          But thou art not quickly moved to strike.   1
## 7  Sampson           A dog of the house of Montague moves me.   1
## 8  Gregory To move is to stir; and to be valiant is to stand.   1
## 9  Sampson A dog of that house shall move me to stand. I will   1
## 10 Gregory That shows thee a weak slave; for the weakest goes   1
## 
## $demographics
##            person  sex fam.aff  died
## 1         Abraham    m    mont FALSE
## 2      Apothecary    m    none FALSE
## 3       Balthasar    m    mont FALSE
## 4        Benvolio    m    mont FALSE
## 5         Capulet    f     cap FALSE
## 6          Chorus none    none FALSE
## 7   First Citizen none    none FALSE
## 8  First Musician    m    none FALSE
## 9   First Servant    m    none FALSE
## 10 First Watchman    m    none FALSE
```

```r
## Merge the two
merged.raj <- key_merge(raj, raj.demographics)
htruncdf(merged.raj, 10, 40)
```

```
##     person act sex fam.aff  died                                 dialogue
## 1  Sampson   1   m     cap FALSE Gregory, o my word, we'll not carry coal
## 2  Gregory   1   m     cap FALSE      No, for then we should be colliers.
## 3  Sampson   1   m     cap FALSE  I mean, an we be in choler, we'll draw.
## 4  Gregory   1   m     cap FALSE Ay, while you live, draw your neck out o
## 5  Sampson   1   m     cap FALSE           I strike quickly, being moved.
## 6  Gregory   1   m     cap FALSE But thou art not quickly moved to strike
## 7  Sampson   1   m     cap FALSE A dog of the house of Montague moves me.
## 8  Gregory   1   m     cap FALSE To move is to stir; and to be valiant is
## 9  Sampson   1   m     cap FALSE A dog of that house shall move me to sta
## 10 Gregory   1   m     cap FALSE That shows thee a weak slave; for the we
```


<h4 id="paste2">Paste and Split Columns</h4>

Many functions in qdap utilize the <a href="http://trinker.github.io/qdap_dev/paste2.html" target="_blank"><code>paste2</code></a> function, which pastes multiple columns/lists of vectors.  <a href="http://trinker.github.io/qdap_dev/paste2.html" target="_blank"><code>paste2</code></a> differs from base R's <a href="http://127.0.0.1:16084/library/base/html/paste.html" target="_blank">paste</a> function in that <a href="http://trinker.github.io/qdap_dev/paste2.html" target="_blank"><code>paste2</code></a> can paste unspecified columns or a list of vectors together.  The <a href="http://trinker.github.io/qdap_dev/paste2.html" target="_blank"><code>colpaste2df</code></a> function, a wrapper for <a href="http://trinker.github.io/qdap_dev/paste2.html" target="_blank"><code>paste2</code></a>, pastes multiple columns together and outputs an appropriately named dataframe.  The <a href="http://trinker.github.io/qdap_dev/colsplit2df.html" target="_blank"><code>colsplit2df</code></a> and <a href="http://trinker.github.io/qdap_dev/colsplit2df.html" target="_blank"><code>lcolsplit2df</code></a> are useful because they can split the output from qdap functions that contain dataframes with pasted columns.

<font size="5" color="orange">&diams;</font> **Using <a href="http://trinker.github.io/qdap_dev/paste2.html" target="_blank"><code>paste2</code></a> and <a href="http://trinker.github.io/qdap_dev/colSplit.html" target="_blank"><code>colSplit</code></a>**: *Pasting & Splitting Vectors and Dataframes*<font size="5" color="orange">&diams;</font>


```r
## Pasting a list of vectors
paste2(rep(list(state.abb[1:8],  month.abb[1:8]) , 2), sep = "|_|")
```

```
## [1] "AL|_|Jan|_|AL|_|Jan" "AK|_|Feb|_|AK|_|Feb" "AZ|_|Mar|_|AZ|_|Mar"
## [4] "AR|_|Apr|_|AR|_|Apr" "CA|_|May|_|CA|_|May" "CO|_|Jun|_|CO|_|Jun"
## [7] "CT|_|Jul|_|CT|_|Jul" "DE|_|Aug|_|DE|_|Aug"
```

```r
## Pasting a dataframe
foo1 <- paste2(CO2[, 1:3])
head(foo1, 12)
```

```
##  [1] "Qn1.Quebec.nonchilled" "Qn1.Quebec.nonchilled"
##  [3] "Qn1.Quebec.nonchilled" "Qn1.Quebec.nonchilled"
##  [5] "Qn1.Quebec.nonchilled" "Qn1.Quebec.nonchilled"
##  [7] "Qn1.Quebec.nonchilled" "Qn2.Quebec.nonchilled"
##  [9] "Qn2.Quebec.nonchilled" "Qn2.Quebec.nonchilled"
## [11] "Qn2.Quebec.nonchilled" "Qn2.Quebec.nonchilled"
```

```r
## Splitting a pasted column
bar1 <- colSplit(foo1)
head(bar1, 10)
```

```
##     X1     X2         X3
## 1  Qn1 Quebec nonchilled
## 2  Qn1 Quebec nonchilled
## 3  Qn1 Quebec nonchilled
## 4  Qn1 Quebec nonchilled
## 5  Qn1 Quebec nonchilled
## 6  Qn1 Quebec nonchilled
## 7  Qn1 Quebec nonchilled
## 8  Qn2 Quebec nonchilled
## 9  Qn2 Quebec nonchilled
## 10 Qn2 Quebec nonchilled
```


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/colpaste2df.html" target="_blank"><code>colpaste2df</code></a> & <a href="http://trinker.github.io/qdap_dev/colsplit2df.html" target="_blank"><code>colsplit2df</code></a>**: *Splitting Columns in Dataframes*<font size="5" color="orange">&diams;</font>


```r
## Create a dataset with a pasted column
(dat <- colpaste2df(head(CO2), 1:3, keep.orig = FALSE)[, c(3, 1:2)])
```

```
##    Plant&Type&Treatment conc uptake
## 1 Qn1.Quebec.nonchilled   95   16.0
## 2 Qn1.Quebec.nonchilled  175   30.4
## 3 Qn1.Quebec.nonchilled  250   34.8
## 4 Qn1.Quebec.nonchilled  350   37.2
## 5 Qn1.Quebec.nonchilled  500   35.3
## 6 Qn1.Quebec.nonchilled  675   39.2
```

```r
## Split column
colsplit2df(dat)
```

```
##   Plant   Type  Treatment conc uptake
## 1   Qn1 Quebec nonchilled   95   16.0
## 2   Qn1 Quebec nonchilled  175   30.4
## 3   Qn1 Quebec nonchilled  250   34.8
## 4   Qn1 Quebec nonchilled  350   37.2
## 5   Qn1 Quebec nonchilled  500   35.3
## 6   Qn1 Quebec nonchilled  675   39.2
```

```r
## Specify names
colsplit2df(dat, new.names = qcv(A, B, C))
```

```
##     A      B          C conc uptake
## 1 Qn1 Quebec nonchilled   95   16.0
## 2 Qn1 Quebec nonchilled  175   30.4
## 3 Qn1 Quebec nonchilled  250   34.8
## 4 Qn1 Quebec nonchilled  350   37.2
## 5 Qn1 Quebec nonchilled  500   35.3
## 6 Qn1 Quebec nonchilled  675   39.2
```

```r
## Keep the original pasted column
colsplit2df(dat, new.names = qcv(A, B, C), keep.orig = TRUE)
```

```
##    Plant&Type&Treatment   A      B          C conc uptake
## 1 Qn1.Quebec.nonchilled Qn1 Quebec nonchilled   95   16.0
## 2 Qn1.Quebec.nonchilled Qn1 Quebec nonchilled  175   30.4
## 3 Qn1.Quebec.nonchilled Qn1 Quebec nonchilled  250   34.8
## 4 Qn1.Quebec.nonchilled Qn1 Quebec nonchilled  350   37.2
## 5 Qn1.Quebec.nonchilled Qn1 Quebec nonchilled  500   35.3
## 6 Qn1.Quebec.nonchilled Qn1 Quebec nonchilled  675   39.2
```

```r
## Pasting columns and output a dataframe
colpaste2df(head(mtcars)[, 1:5], qcv(mpg, cyl, disp), sep ="_", name.sep = "|")
```

```
##                    mpg cyl disp  hp drat mpg|cyl|disp
## Mazda RX4         21.0   6  160 110 3.90     21_6_160
## Mazda RX4 Wag     21.0   6  160 110 3.90     21_6_160
## Datsun 710        22.8   4  108  93 3.85   22.8_4_108
## Hornet 4 Drive    21.4   6  258 110 3.08   21.4_6_258
## Hornet Sportabout 18.7   8  360 175 3.15   18.7_8_360
## Valiant           18.1   6  225 105 2.76   18.1_6_225
```

```r
colpaste2df(head(CO2)[, -3], list(1:2, qcv("conc", "uptake")))
```

```
##   Plant   Type conc uptake Plant&Type conc&uptake
## 1   Qn1 Quebec   95   16.0 Qn1.Quebec       95.16
## 2   Qn1 Quebec  175   30.4 Qn1.Quebec    175.30.4
## 3   Qn1 Quebec  250   34.8 Qn1.Quebec    250.34.8
## 4   Qn1 Quebec  350   37.2 Qn1.Quebec    350.37.2
## 5   Qn1 Quebec  500   35.3 Qn1.Quebec    500.35.3
## 6   Qn1 Quebec  675   39.2 Qn1.Quebec    675.39.2
```


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/lcolsplit2df.html" target="_blank"><code>lcolsplit2df</code></a>**: *Splitting Columns in Lists of Dataframes*<font size="5" color="orange">&diams;</font>


```r
## A list with dataframes that contain pasted columns
x <- question_type(DATA.SPLIT$state, list(DATA.SPLIT$sex, DATA.SPLIT$adult))
ltruncdf(x[1:4])
```

```
## $raw
##   sex&adult   raw.text n.row endmark strip.text     q.type
## 1       m.1 What shoul     4       ?  what shou       what
## 2       f.0 How can we     7       ?  how can w        how
## 3       f.0 What are y    10       ?  what are        what
## 4       f.1 Shall we m    11       ?  shall we       shall
## 5       m.0 You alread    15       ?  you alrea implied_do
## 
## $count
##   sex&adult tot.quest what how shall implied_do
## 1       f.0         2    1   1     0          0
## 2       f.1         1    0   0     1          0
## 3       m.0         1    0   0     0          1
## 4       m.1         1    1   0     0          0
## 
## $prop
##   sex&adult tot.quest what how shall implied_do
## 1       f.0         2   50  50     0          0
## 2       f.1         1    0   0   100          0
## 3       m.0         1    0   0     0        100
## 4       m.1         1  100   0     0          0
## 
## $rnp
##   sex&adult tot.quest    what    how   shall implied_do
## 1       f.0         2  1(50%) 1(50%)       0          0
## 2       f.1         1       0      0 1(100%)          0
## 3       m.0         1       0      0       0    1(100%)
## 4       m.1         1 1(100%)      0       0          0
```

```r
z <- lcolsplit2df(x)
ltruncdf(z[1:4])
```

```
## $raw
##   sex adult   raw.text n.row endmark strip.text     q.type
## 1   m     1 What shoul     4       ?  what shou       what
## 2   f     0 How can we     7       ?  how can w        how
## 3   f     0 What are y    10       ?  what are        what
## 4   f     1 Shall we m    11       ?  shall we       shall
## 5   m     0 You alread    15       ?  you alrea implied_do
## 
## $count
##   sex adult tot.quest what how shall implied_do
## 1   f     0         2    1   1     0          0
## 2   f     1         1    0   0     1          0
## 3   m     0         1    0   0     0          1
## 4   m     1         1    1   0     0          0
## 
## $prop
##   sex adult tot.quest what how shall implied_do
## 1   f     0         2   50  50     0          0
## 2   f     1         1    0   0   100          0
## 3   m     0         1    0   0     0        100
## 4   m     1         1  100   0     0          0
## 
## $rnp
##   sex adult tot.quest    what    how   shall implied_do
## 1   f     0         2  1(50%) 1(50%)       0          0
## 2   f     1         1       0      0 1(100%)          0
## 3   m     0         1       0      0       0    1(100%)
## 4   m     1         1 1(100%)      0       0          0
```


<h4 id="ganttspan">Generate Unit Spans</h4>

Often a researcher will want to view the patterns of the discourse by grouping variables over time.  This requires the data to have start and end times based on units (sentence, turn of talk, or word).  The <a href="http://trinker.github.io/qdap_dev/gantt.html" target="_blank"><code>gantt</code></a> function provides the user with unit spans (start and end times) with the <a href="http://trinker.github.io/qdap_dev/gantt_rep.html" target="_blank"><code>gantt_rep</code></a> extending this capability to repeated measures.  The <a href="http://trinker.github.io/qdap_dev/gantt.html" target="_blank"><code>gantt</code></a> function has a basic plotting method to allow visualization of the unit span data, however, the <a href="http://trinker.github.io/qdap_dev/gantt_wrap.html" target="_blank"><code>gantt_wrap</code></a> function extends the <a href="http://trinker.github.io/qdap_dev/gantt.html" target="_blank"><code>gantt</code></a> and <a href="http://trinker.github.io/qdap_dev/gantt_rep.html" target="_blank"><code>gantt_rep</code></a> functions to plot precise depictions (Gantt plots) of the unit span data.  Note that if the researcher is only interested in the plotting the data as a Gantt plot, the <a href="http://trinker.github.io/qdap_dev/gantt_plot.html" target="_blank"><code>gantt_plot</code></a> function combines the <a href="http://trinker.github.io/qdap_dev/gantt.html" target="_blank"><code>gantt</code></a>/<a href="http://trinker.github.io/qdap_dev/gantt_rep.html" target="_blank"><code>gantt_rep</code></a> functions with the <a href="http://trinker.github.io/qdap_dev/gantt.html" target="_blank"><code>gantt</code></a> function  

<font size="5" color="orange">&diams;</font> **Unit Spans**<font size="5" color="orange">&diams;</font>


```r
## Unit Span Dataframe
dat <- gantt(mraja1$dialogue, mraja1$person) 
head(dat, 12)
```

```
##     person  n start end
## 1  Sampson  8     0   8
## 2  Gregory  7     8  15
## 3  Sampson  9    15  24
## 4  Gregory 11    24  35
## 5  Sampson  5    35  40
## 6  Gregory  8    40  48
## 7  Sampson  9    48  57
## 8  Gregory 20    57  77
## 9  Sampson 22    77  99
## 10 Gregory 13    99 112
## 11 Sampson 30   112 142
## 12 Gregory 10   142 152
```

```r
plot(dat)
```

![plot of chunk unnamed-chunk-40](figure/unnamed-chunk-401.png) 

```r
plot(dat, base = TRUE)
```

![plot of chunk unnamed-chunk-40](figure/unnamed-chunk-402.png) 


<font size="5" color="orange">&diams;</font> **Repeated Measures Unit Spans**<font size="5" color="orange">&diams;</font>


```r
## Repeated Measures Unit Span Dataframe
dat2 <- with(rajSPLIT, gantt_rep(act, dialogue, list(fam.aff, sex)))
head(dat2, 12)
```

```
##    act fam.aff_sex   n start end
## 1    1       cap_m 327     0 327
## 2    1      mont_m   8   327 335
## 3    1       cap_m   6   335 341
## 4    1      mont_m   8   341 349
## 5    1       cap_m  32   349 381
## 6    1      mont_m   4   381 385
## 7    1       cap_m  16   385 401
## 8    1      mont_m   2   401 403
## 9    1       cap_m  14   403 417
## 10   1      mont_m   2   417 419
## 11   1       cap_m  10   419 429
## 12   1      mont_m  12   429 441
```

```r
## Plotting Repeated Measures Unit Span Dataframe
plot(dat2)
```

![plot of chunk unnamed-chunk-41](figure/unnamed-chunk-411.png) 

```r
gantt_wrap(dat2, "fam.aff_sex", facet.vars = "act",
    title = "Repeated Measures Gantt Plot")
```

![plot of chunk unnamed-chunk-41](figure/unnamed-chunk-412.png) 


<h4 id="adj">Create Adjacency Matrix</h4>

It is useful to convert data to an adjaceny matrix for examing relationships between grouping variables in word usage.  The <a href="http://trinker.github.io/qdap_dev/adjaceny_matrix.html" target="_blank"><code>adjaceny_matrix</code></a> (aka: <a href="http://trinker.github.io/qdap_dev/adjmat.html" target="_blank"><code>adjmat</code></a>) provide this capability, interacting with a <a href="http://trinker.github.io/qdap_dev/termco.html" target="_blank"><code>termco</code></a> or <a href="http://trinker.github.io/qdap_dev/wfm.html" target="_blank"><code>wfm</code></a> object.  In the first example below Sam and Greg share 4 words in common, whereas, the Teacher and Greg share no words.  The adjaceny matrix can be passed to a network graphing package such as the <a href="http://igraph.sourceforge.net/" target="_blank">igraph</a> package for visulaization of the data structure as seen in Example 3.


<font size="5" color="orange">&diams;</font> **Adjaceny Matrix**: *Example 1*<font size="5" color="orange">&diams;</font>


```r
adjacency_matrix(wfm(DATA$state, DATA$person))
```


<pre><code>## Adjacency Matrix:
## 
##            greg researcher sally sam
## researcher    0                     
## sally         1          1          
## sam           4          0     1    
## teacher       0          1     2   0
## 
## 
## Summed occurrences:
## 
##       greg researcher      sally        sam    teacher 
##         18          6         10         11          4 
</code></pre>

<font size="5" color="orange">&diams;</font> **Adjaceny Matrix**: *Example 2*<font size="5" color="orange">&diams;</font>


```r
words <- c(" education", " war ", " econom", " job", "governor ")
(terms <- with(pres_debates2012, termco(dialogue, person, words)))
adjmat(terms)
```


<pre><code>## Adjacency Matrix:
## 
##           OBAMA ROMNEY CROWLEY LEHRER QUESTION
## ROMNEY        5                               
## CROWLEY       2      2                        
## LEHRER        4      4       2                
## QUESTION      4      4       2      4         
## SCHIEFFER     2      2       1      1        1
## 
## 
## Summed occurrences:
## 
##     OBAMA    ROMNEY   CROWLEY    LEHRER  QUESTION SCHIEFFER 
##         5         5         2         4         4         2 
</code></pre>

<font size="5" color="orange">&diams;</font> **Plotting an Adjaceny Matrix**: *Example 3*<font size="5" color="orange">&diams;</font>



```r
library(igraph)
dat <- adjacency_matrix(wfm(DATA$state, DATA$person, stopword = Top25Words))
g <- graph.adjacency(dat$adjacency, weighted=TRUE, mode ="undirected")
g <- simplify(g)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
plot(g, layout=layout.auto(g))
```

![plot of chunk unnamed-chunk-44](figure/unnamed-chunk-44.png) 




<h3 id="word">Extract Words</h3>

<div class="funs">
The following functions will be utilized in this section (click to view more):    

<form action="http://trinker.github.io/qdap_dev/all_words.html" target="_blank">
    <input type="submit" value="all_words"> - <a href="#all_words">Searches Text Column for Words</a>
</form>

<form action="http://trinker.github.io/qdap_dev/bag_o_words.html" target="_blank">
    <input type="submit" value="bag_o_words"><input type="submit" value="breaker"><input type="submit" value="word.split"> - <a href="#bag">Bag of Words</a>
</form>

<form action="http://trinker.github.io/qdap_dev/common.html" target="_blank">
    <input type="submit" value="common"> - <a href="#common">Find Common Words Between Groups</a>
</form>

<form action="http://trinker.github.io/qdap_dev/exclude.html" target="_blank">
    <input type="submit" value="exclude"> - <a href="#exclude">Exclude Elements From a Vector</a>
</form>

<form action="http://trinker.github.io/qdap_dev/ngrams.html" target="_blank">
    <input type="submit" value="ngrams"> - <a href="#ngram">Generate ngrams</a>
</form>

<form action="http://trinker.github.io/qdap_dev/stopwords.html" target="_blank">
    <input type="submit" value="stopwords"> - <a href="#stopwords">Remove Stopwords</a>
</form>

<form action="http://trinker.github.io/qdap_dev/strip.html" target="_blank">
    <input type="submit" value="strip"> - <a href="#strip">Strip Text of Unwanted Characters/Capitalization</a>
</form>

<form action="http://trinker.github.io/qdap_dev/synonyms.html" target="_blank">
    <input type="submit" value="synonyms"><input type="submit" value="syn"> - <a href="#syn">Search For Synonyms</a>
</form>

<form action="http://trinker.github.io/qdap_dev/word_associate.html" target="_blank">
    <input type="submit" value="word_associate"> - <a href="#assoc">Find Associated Words</a>
</form>

<form action="http://trinker.github.io/qdap_dev/word_diff_list.html" target="_blank">
    <input type="submit" value="word_diff_list"> - <a href="#diffs">Differences In Word Use Between Groups</a>
</form>

<form action="http://trinker.github.io/qdap_dev/word_list.html" target="_blank">
    <input type="submit" value="word_list"> - <a href="#word_list">Raw Word Lists/Frequency Counts</a>
</form>
</div>

This section overviews functions that can extract words and word lists from dialogue text.  The subsections describing function use are in alphabetical order as there is no set chronology for use.

<h4 id="all_words">Searches Text Column for Words</h4>

The <a href="http://trinker.github.io/qdap_dev/all_words.html" target="_blank"><code>all_words</code></a> breaks the dialogue into a bag of words and searches based on the criteria arguments <font face="courier">begins.with</font> and <font face="courier">contains</font>.  The resulting word list can be useful for analysis or to pass to qdap functions that deal with <a href="#counts">Word Counts and Descriptive Statistics</a>.

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/all_words.html" target="_blank"><code>all_words</code></a>**<font size="5" color="orange">&diams;</font>



```r
## Words starting with `re`
x1 <- all_words(raj$dialogue, begins.with="re")
head(x1, 10)
```

```
##    WORD       FREQ
## 1  re            2
## 2  reach         1
## 3  read          6
## 4  ready         5
## 5  rearward      1
## 6  reason        5
## 7  reason's      1
## 8  rebeck        1
## 9  rebellious    1
## 10 receipt       1
```

```r
## Words containing with `conc`
all_words(raj$dialogue, contains = "conc")
```

```
##   WORD      FREQ
## 1 conceal'd    1
## 2 conceit      2
## 3 conceive     1
## 4 concludes    1
## 5 reconcile    1
```

```r
## All words ordered by frequency
x2 <- all_words(raj$dialogue, alphabetical = FALSE)
head(x2, 10)
```

```
##    WORD FREQ
## 1  and   666
## 2  the   656
## 3  i     573
## 4  to    517
## 5  a     445
## 6  of    378
## 7  my    358
## 8  is    344
## 9  that  344
## 10 in    312
```


<h4 id="bag">Word Splitting (Bag of Words)</h4>

The qdap package utilizes the following functions to turn text into a bag of words (word order is perserved):


<TABLE>
    <TR> <TD align="right"><font face="courier"><b><a href="http://trinker.github.io/qdap_dev/bag_o_words.html">bag_o_words</a> </b></font></TD> <TD align="right">Reduces a text column to a <b>single</b> vector bag of words.</TD> </TR>
    <TR> <TD align="right"><font face="courier"><b><a href="http://trinker.github.io/qdap_dev/bag_o_words.html">breaker</a></b></font></TD> <TD align="right"> Reduces a text column to a <b>single</b> vector bag of words and qdap recognized end marks.</TD> </TR>
    <TR> <TD align="right"><font face="courier"><b><a href="http://trinker.github.io/qdap_dev/bag_o_words.html">word.split</a></b></font></TD> <TD align="right"> Reduces a text column to a <b>list</b> of vectors of bag of words and qdap recognized end marks (i.e., ".", "!", "?", "*", "-").</TD> </TR>
</TABLE>

Bag of words can be useful for any number of reasons within the scope of analyzing discourse.  Many other qdap functions employ or mention these three functions as seen in the following counts for the three word splitting functions functions.





<TABLE border=1>
 <TR> <TD align="right">  </TD> <TD><b>Function</b> </TD> <TD> <b>bag_o_words</b> </TD> <TD> <b>breaker</b></TD> <TD> <b>word.split</b></TD> </TR>
 
 
  <TR> <TD align="right"> 1 </TD> <TD> all_words.R                   </TD> <TD> 1 </TD> <TD> - </TD> <TD> - </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD> automated_readability_index.R </TD> <TD> - </TD> <TD> - </TD> <TD> 2 </TD> </TR>
  <TR> <TD align="right"> 3 </TD> <TD> bag_o_words.R                 </TD> <TD> 10 </TD> <TD> 6 </TD> <TD> 3 </TD> </TR>
  <TR> <TD align="right"> 4 </TD> <TD> capitalizer.R                 </TD> <TD> 3 </TD> <TD> 1 </TD> <TD> - </TD> </TR>
  <TR> <TD align="right"> 5 </TD> <TD> imperative.R                  </TD> <TD> - </TD> <TD> 3 </TD> <TD> - </TD> </TR>
  <TR> <TD align="right"> 6 </TD> <TD> ngrams.R                      </TD> <TD> 1 </TD> <TD> - </TD> <TD> - </TD> </TR>
  <TR> <TD align="right"> 7 </TD> <TD> polarity.R                    </TD> <TD> 2 </TD> <TD> - </TD> <TD> - </TD> </TR>
  <TR> <TD align="right"> 8 </TD> <TD> stopwords.R                   </TD> <TD> 1 </TD> <TD> 3 </TD> <TD> - </TD> </TR>
  <TR> <TD align="right"> 9 </TD> <TD> textLISTER.R                  </TD> <TD> - </TD> <TD> - </TD> <TD> 2 </TD> </TR>
  <TR> <TD align="right"> 10 </TD> <TD> trans_cloud.R                 </TD> <TD> 1 </TD> <TD> 1 </TD> <TD> - </TD> </TR>
  <TR> <TD align="right"> 11 </TD> <TD> wfm.R                         </TD> <TD> 1 </TD> <TD> - </TD> <TD> - </TD> </TR>
   </TABLE>
<br>

<font size="5" color="orange">&diams;</font> **Word Splitting Examples**<font size="5" color="orange">&diams;</font>


```r
bag_o_words("I'm going home!")
```

```
## [1] "i'm"   "going" "home"
```

```r
bag_o_words("I'm going home!", apostrophe.remove = TRUE)
```

```
## [1] "im"    "going" "home"
```

```r
bag_o_words(DATA$state)
```

```
##  [1] "computer" "is"       "fun"      "not"      "too"      "fun"     
##  [7] "no"       "it's"     "not"      "it's"     "dumb"     "what"    
## [13] "should"   "we"       "do"       "you"      "liar"     "it"      
## [19] "stinks"   "i"        "am"       "telling"  "the"      "truth"   
## [25] "how"      "can"      "we"       "be"       "certain"  "there"   
## [31] "is"       "no"       "way"      "i"        "distrust" "you"     
## [37] "what"     "are"      "you"      "talking"  "about"    "shall"   
## [43] "we"       "move"     "on"       "good"     "then"     "i'm"     
## [49] "hungry"   "let's"    "eat"      "you"      "already"
```

```r
by(DATA$state, DATA$person, bag_o_words)
```

```
## DATA$person: greg
##  [1] "no"      "it's"    "not"     "it's"    "dumb"    "i"       "am"     
##  [8] "telling" "the"     "truth"   "there"   "is"      "no"      "way"    
## [15] "i'm"     "hungry"  "let's"   "eat"     "you"     "already"
## -------------------------------------------------------- 
## DATA$person: researcher
## [1] "shall" "we"    "move"  "on"    "good"  "then" 
## -------------------------------------------------------- 
## DATA$person: sally
##  [1] "how"     "can"     "we"      "be"      "certain" "what"    "are"    
##  [8] "you"     "talking" "about"  
## -------------------------------------------------------- 
## DATA$person: sam
##  [1] "computer" "is"       "fun"      "not"      "too"      "fun"     
##  [7] "you"      "liar"     "it"       "stinks"   "i"        "distrust"
## [13] "you"     
## -------------------------------------------------------- 
## DATA$person: teacher
## [1] "what"   "should" "we"     "do"
```

```r
lapply(DATA$state,  bag_o_words)
```

```
## [[1]]
## [1] "computer" "is"       "fun"      "not"      "too"      "fun"     
## 
## [[2]]
## [1] "no"   "it's" "not"  "it's" "dumb"
## 
## [[3]]
## [1] "what"   "should" "we"     "do"    
## 
## [[4]]
## [1] "you"    "liar"   "it"     "stinks"
## 
## [[5]]
## [1] "i"       "am"      "telling" "the"     "truth"  
## 
## [[6]]
## [1] "how"     "can"     "we"      "be"      "certain"
## 
## [[7]]
## [1] "there" "is"    "no"    "way"  
## 
## [[8]]
## [1] "i"        "distrust" "you"     
## 
## [[9]]
## [1] "what"    "are"     "you"     "talking" "about"  
## 
## [[10]]
## [1] "shall" "we"    "move"  "on"    "good"  "then" 
## 
## [[11]]
## [1] "i'm"     "hungry"  "let's"   "eat"     "you"     "already"
```

```r
breaker(DATA$state)
```

```
##  [1] "Computer" "is"       "fun"      "."        "Not"      "too"     
##  [7] "fun"      "."        "No"       "it's"     "not,"     "it's"    
## [13] "dumb"     "."        "What"     "should"   "we"       "do"      
## [19] "?"        "You"      "liar,"    "it"       "stinks"   "!"       
## [25] "I"        "am"       "telling"  "the"      "truth"    "!"       
## [31] "How"      "can"      "we"       "be"       "certain"  "?"       
## [37] "There"    "is"       "no"       "way"      "."        "I"       
## [43] "distrust" "you"      "."        "What"     "are"      "you"     
## [49] "talking"  "about"    "?"        "Shall"    "we"       "move"    
## [55] "on"       "?"        "Good"     "then"     "."        "I'm"     
## [61] "hungry"   "."        "Let's"    "eat"      "."        "You"     
## [67] "already"  "?"
```

```r
by(DATA$state, DATA$person, breaker)
```

```
## DATA$person: greg
##  [1] "No"      "it's"    "not,"    "it's"    "dumb"    "."       "I"      
##  [8] "am"      "telling" "the"     "truth"   "!"       "There"   "is"     
## [15] "no"      "way"     "."       "I'm"     "hungry"  "."       "Let's"  
## [22] "eat"     "."       "You"     "already" "?"      
## -------------------------------------------------------- 
## DATA$person: researcher
## [1] "Shall" "we"    "move"  "on"    "?"     "Good"  "then"  "."    
## -------------------------------------------------------- 
## DATA$person: sally
##  [1] "How"     "can"     "we"      "be"      "certain" "?"       "What"   
##  [8] "are"     "you"     "talking" "about"   "?"      
## -------------------------------------------------------- 
## DATA$person: sam
##  [1] "Computer" "is"       "fun"      "."        "Not"      "too"     
##  [7] "fun"      "."        "You"      "liar,"    "it"       "stinks"  
## [13] "!"        "I"        "distrust" "you"      "."       
## -------------------------------------------------------- 
## DATA$person: teacher
## [1] "What"   "should" "we"     "do"     "?"
```

```r
lapply(DATA$state,  breaker)
```

```
## [[1]]
## [1] "Computer" "is"       "fun"      "."        "Not"      "too"     
## [7] "fun"      "."       
## 
## [[2]]
## [1] "No"   "it's" "not," "it's" "dumb" "."   
## 
## [[3]]
## [1] "What"   "should" "we"     "do"     "?"     
## 
## [[4]]
## [1] "You"    "liar,"  "it"     "stinks" "!"     
## 
## [[5]]
## [1] "I"       "am"      "telling" "the"     "truth"   "!"      
## 
## [[6]]
## [1] "How"     "can"     "we"      "be"      "certain" "?"      
## 
## [[7]]
## [1] "There" "is"    "no"    "way"   "."    
## 
## [[8]]
## [1] "I"        "distrust" "you"      "."       
## 
## [[9]]
## [1] "What"    "are"     "you"     "talking" "about"   "?"      
## 
## [[10]]
## [1] "Shall" "we"    "move"  "on"    "?"     "Good"  "then"  "."    
## 
## [[11]]
## [1] "I'm"     "hungry"  "."       "Let's"   "eat"     "."       "You"    
## [8] "already" "?"
```

```r
word_split(c(NA, DATA$state))
```

```
## $<NA>
## [1] NA
## 
## $`Computer is fun. Not too fun.`
## [1] "Computer" "is"       "fun"      "."        "Not"      "too"     
## [7] "fun"      "."       
## 
## $`No it's not, it's dumb.`
## [1] "No"   "it's" "not," "it's" "dumb" "."   
## 
## $`What should we do?`
## [1] "What"   "should" "we"     "do"     "?"     
## 
## $`You liar, it stinks!`
## [1] "You"    "liar,"  "it"     "stinks" "!"     
## 
## $`I am telling the truth!`
## [1] "I"       "am"      "telling" "the"     "truth"   "!"      
## 
## $`How can we be certain?`
## [1] "How"     "can"     "we"      "be"      "certain" "?"      
## 
## $`There is no way.`
## [1] "There" "is"    "no"    "way"   "."    
## 
## $`I distrust you.`
## [1] "I"        "distrust" "you"      "."       
## 
## $`What are you talking about?`
## [1] "What"    "are"     "you"     "talking" "about"   "?"      
## 
## $`Shall we move on? Good then.`
## [1] "Shall" "we"    "move"  "on"    "?"     "Good"  "then"  "."    
## 
## $`I'm hungry. Let's eat. You already?`
## [1] "I'm"     "hungry"  "."       "Let's"   "eat"     "."       "You"    
## [8] "already" "?"
```



<h4 id="common">Find Common Words Between Groups</h4>

The <a href="http://trinker.github.io/qdap_dev/common.html" target="_blank"><code>common</code></a> function finds items that are common between n vectors 
(i.e., subjects or grouping variables).  This is useful for determining common language choices shared across participants in a conversation.

<font size="5" color="orange">&diams;</font> **Words in Common Examples**<font size="5" color="orange">&diams;</font>


```r
## Create vectors of words
a <- c("a", "cat", "dog", "the", "the")
b <- c("corn", "a", "chicken", "the")
d <- c("house", "feed", "a", "the", "chicken")

## Supply individual vectors
common(a, b, d, overlap=2)
```

```
##      word freq
## 1       a    3
## 2     the    3
## 3 chicken    2
```

```r
common(a, b, d, overlap=3)
```

```
##   word freq
## 1    a    3
## 2  the    3
```

```r
## Supply a lsit of vectors
common(list(a, b, d))
```

```
##   word freq
## 1    a    3
## 2  the    3
```

```r
## Using to find common words between subjects
common(word_list(DATA$state, DATA$person)$cwl, overlap = 2)
```

```
##   word freq
## 1   we    3
## 2  you    3
## 3    I    2
## 4   is    2
## 5  not    2
## 6 what    2
```



<h4 id="exclude">Exclude Elements From a Vector</h4>

It is often useful and more efficient to start with a preset vector of words and eliminate or <a href="http://trinker.github.io/qdap_dev/exclude.html" target="_blank"><code>exclude</code></a> the words you do not wish to include.  Examples could range from excluding an individual(s) from a column of participant names or excluding a few select word(s) from a pre defined qdap word list.  THis is particlarly useful for passsing terms or stopwords to word counting functions like <a href="http://trinker.github.io/qdap_dev/termco.html" target="_blank"><code>termco</code></a> or <a href="http://trinker.github.io/qdap_dev/trans_cloud.html" target="_blank"><code>trans_cloud</code></a>.

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/exclude.html" target="_blank"><code>exclude</code></a> Examples**<font size="5" color="orange">&diams;</font>

```r
exclude(1:10, 3, 4)
```

```
## [1]  1  2  5  6  7  8  9 10
```

```r
exclude(Top25Words, qcv(the, of, and))
```

```
##  [1] "a"    "to"   "in"   "is"   "you"  "that" "it"   "he"   "was"  "for" 
## [11] "on"   "are"  "as"   "with" "his"  "they" "I"    "at"   "be"   "this"
## [21] "have" "from"
```

```r
exclude(Top25Words, "the", "of", "an")
```

```
##  [1] "and"  "a"    "to"   "in"   "is"   "you"  "that" "it"   "he"   "was" 
## [11] "for"  "on"   "are"  "as"   "with" "his"  "they" "I"    "at"   "be"  
## [21] "this" "have" "from"
```

```r
#Using with `term_match` and `termco`
MTCH.LST <- exclude(term_match(DATA$state, qcv(th, i)), qcv(truth, stinks))
termco(DATA$state, DATA$person, MTCH.LST)
```

```
##       person word.count        th          i
## 1       greg         20 3(15.00%) 13(65.00%)
## 2 researcher          6 2(33.33%)          0
## 3      sally         10         0  4(40.00%)
## 4        sam         13         0 11(84.62%)
## 5    teacher          4         0          0
```


<h4 id="ngramn">Generate ngrams</h4>

Utilizing <a href="http://en.wikipedia.org/wiki/N-gram" target="_blank">ngrams</a> can be useful for gaining a sense of what terms are used in conjunction with other terms.  This is particularly useful in the analysis of dialogue when the combination of a particular vocabulary is meaningful.  The <a href="http://trinker.github.io/qdap_dev/ngrams.html" target="_blank"><code>ngrams</code></a> function provides a list of ngram related output that can be utilize in various analyses.

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/ngrams.html" target="_blank"><code>ngrams</code></a> Example** *note that the out put is only partial*<font size="5" color="orange">&diams;</font>


```r
out <- ngrams(DATA$state, DATA$person, 2)
lapply(out[["all_n"]], function(x) sapply(x, paste, collapse = " "))
```

```
## $n_1
##  [1] "about"    "already"  "am"       "are"      "be"       "can"     
##  [7] "certain"  "computer" "distrust" "do"       "dumb"     "eat"     
## [13] "fun"      "fun"      "good"     "how"      "hungry"   "i"       
## [19] "i"        "i'm"      "is"       "is"       "it"       "it's"    
## [25] "it's"     "let's"    "liar"     "move"     "no"       "no"      
## [31] "not"      "not"      "on"       "shall"    "should"   "stinks"  
## [37] "talking"  "telling"  "the"      "then"     "there"    "too"     
## [43] "truth"    "way"      "we"       "we"       "we"       "what"    
## [49] "what"     "you"      "you"      "you"      "you"     
## 
## $n_2
##  [1] "am telling"    "are you"       "be certain"    "can we"       
##  [5] "computer is"   "distrust you"  "eat you"       "fun not"      
##  [9] "good then"     "how can"       "hungry let's"  "i'm hungry"   
## [13] "i am"          "i distrust"    "is fun"        "is no"        
## [17] "it's dumb"     "it's not"      "it stinks"     "let's eat"    
## [21] "liar it"       "move on"       "no it's"       "no way"       
## [25] "not it's"      "not too"       "on good"       "shall we"     
## [29] "should we"     "talking about" "telling the"   "the truth"    
## [33] "there is"      "too fun"       "we be"         "we do"        
## [37] "we move"       "what are"      "what should"   "you already"  
## [41] "you liar"      "you talking"
```


<h4 id="stopwords">Remove Stopwords</h4>

In analyzing discourse it may be helpful to remove certain words from the analysis as the words may not be meaningful or may overshadow the impact of other words.  The <a href="http://trinker.github.io/qdap_dev/stopwords.html" target="_blank"><code>stopwords</code></a> function can be utilized to remove <a href="http://nlp.stanford.edu/IR-book/html/htmledition/dropping-common-terms-stop-words-1.html" target="_blank">stopwords</a> from the dialogue before passing to further analysis.  It should be noted that many functions have a stopwords argument that allows for the remval of the stopwords within the function environment rather than altering the text in the primary discourse dataframe.  Careful researcher consideration must be given as to the functional impact of removing words from an analysis.

<font size="5" color="orange">&diams;</font> **Stopword Removal Examples**<font size="5" color="orange">&diams;</font>


```r
## The data
DATA$state
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
stopwords(DATA$state, Top200Words)
```

```
## [[1]]
## [1] "computer" "fun"      "."        "fun"      "."       
## 
## [[2]]
## [1] "it's" ","    "it's" "dumb" "."   
## 
## [[3]]
## [1] "?"
## 
## [[4]]
## [1] "liar"   ","      "stinks" "!"     
## 
## [[5]]
## [1] "am"      "telling" "truth"   "!"      
## 
## [[6]]
## [1] "certain" "?"      
## 
## [[7]]
## [1] "."
## 
## [[8]]
## [1] "distrust" "."       
## 
## [[9]]
## [1] "talking" "?"      
## 
## [[10]]
## [1] "shall" "?"     "."    
## 
## [[11]]
## [1] "i'm"     "hungry"  "."       "let's"   "eat"     "."       "already"
## [8] "?"
```

```r
stopwords(DATA$state, Top200Words, strip = TRUE)
```

```
## [[1]]
## [1] "computer" "fun"      "fun"     
## 
## [[2]]
## [1] "it's" "it's" "dumb"
## 
## [[3]]
## character(0)
## 
## [[4]]
## [1] "liar"   "stinks"
## 
## [[5]]
## [1] "am"      "telling" "truth"  
## 
## [[6]]
## [1] "certain"
## 
## [[7]]
## character(0)
## 
## [[8]]
## [1] "distrust"
## 
## [[9]]
## [1] "talking"
## 
## [[10]]
## [1] "shall"
## 
## [[11]]
## [1] "i'm"     "hungry"  "let's"   "eat"     "already"
```

```r
stopwords(DATA$state, Top200Words, separate = FALSE)
```

```
##  [1] "computer fun. fun."              "it's, it's dumb."               
##  [3] "?"                               "liar, stinks!"                  
##  [5] "am telling truth!"               "certain?"                       
##  [7] "."                               "distrust."                      
##  [9] "talking?"                        "shall?."                        
## [11] "i'm hungry. let's eat. already?"
```

```r
stopwords(DATA$state, Top200Words, unlist = TRUE, unique = TRUE)
```

```
##  [1] "computer" "fun"      "."        "it's"     ","        "dumb"    
##  [7] "?"        "liar"     "stinks"   "!"        "am"       "telling" 
## [13] "truth"    "certain"  "distrust" "talking"  "shall"    "i'm"     
## [19] "hungry"   "let's"    "eat"      "already"
```


<h4 id="strip">Strip Text of Unwanted Characters/Capitalization</h4>

It is often useful to remove capitalization and puntuation from the dialogue in order to standardize the text.  R is case sensitive.  By removing capital letters and extra punctuation with the <a href="http://trinker.github.io/qdap_dev/strip.html" target="_blank"><code>strip</code></a> function the text is more comparable.  In the following output we can see, through the <a href="http://stat.ethz.ch/R-manual/R-devel/library/base/html/Comparison.html" target="_blank">==</a> comparison operator and <a href="http://stat.ethz.ch/R-manual/R-devel/library/base/html/Comparison.html" target="_blank">outer</a> function that the use of <a href="http://trinker.github.io/qdap_dev/strip.html" target="_blank"><code>strip</code></a> makes the differnet forms of <font color="blue">Dan</font> comparable.


```r
x <- c("Dan", "dan", "dan.", "DAN")
y <- outer(x, x, "==")
dimnames(y) <- list(x, x); y
```

```
##        Dan   dan  dan.   DAN
## Dan   TRUE FALSE FALSE FALSE
## dan  FALSE  TRUE FALSE FALSE
## dan. FALSE FALSE  TRUE FALSE
## DAN  FALSE FALSE FALSE  TRUE
```

```r
x <- strip(c("Dan", "dan", "dan.", "DAN"))
y <- outer(x, x, "==")
dimnames(y) <- list(x, x); y
```

```
##      dan  dan  dan  dan
## dan TRUE TRUE TRUE TRUE
## dan TRUE TRUE TRUE TRUE
## dan TRUE TRUE TRUE TRUE
## dan TRUE TRUE TRUE TRUE
```


As seen in the examples below, <a href="http://trinker.github.io/qdap_dev/strip.html" target="_blank"><code>strip</code></a> comes with multiple arguments to adjust the flexiblity of the degree of text standardization.

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/strip.html" target="_blank"><code>strip</code></a> Examples**<font size="5" color="orange">&diams;</font>


```r
## Demonstrating the standardization of 
## The data
DATA$state
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
strip(DATA$state)
```

```
##  [1] "computer is fun not too fun"    "no its not its dumb"           
##  [3] "what should we do"              "you liar it stinks"            
##  [5] "i am telling the truth"         "how can we be certain"         
##  [7] "there is no way"                "i distrust you"                
##  [9] "what are you talking about"     "shall we move on good then"    
## [11] "im hungry lets eat you already"
```

```r
strip(DATA$state, apostrophe.remove=FALSE)
```

```
##  [1] "computer is fun not too fun"      "no it's not it's dumb"           
##  [3] "what should we do"                "you liar it stinks"              
##  [5] "i am telling the truth"           "how can we be certain"           
##  [7] "there is no way"                  "i distrust you"                  
##  [9] "what are you talking about"       "shall we move on good then"      
## [11] "i'm hungry let's eat you already"
```

```r
strip(DATA$state, char.keep = c("?", "."))
```

```
##  [1] "computer is fun. not too fun."    
##  [2] "no its not its dumb."             
##  [3] "what should we do?"               
##  [4] "you liar it stinks"               
##  [5] "i am telling the truth"           
##  [6] "how can we be certain?"           
##  [7] "there is no way."                 
##  [8] "i distrust you."                  
##  [9] "what are you talking about?"      
## [10] "shall we move on? good then."     
## [11] "im hungry. lets eat. you already?"
```


<h4 id="syn">Search For Synonyms</h4>

It is useful in discourse analysis to analyze vocabularly use.  This may mean searching for words similar to your initial word list.  The <a href="http://trinker.github.io/qdap_dev/synonyms.html" target="_blank"><code>synonyms</code></a> (aka <a href="http://trinker.github.io/qdap_dev/syn.html" target="_blank"><code>syn</code></a>) function generates synonyms from the <a href="http://trinker.github.io/qdapDictionaries/" target="_blank">qdapDictionaries'</a> <a href="http://trinker.github.io/qdapDictionaries/SYNONYM.html" target="_blank">SYNONYM</a> dictionary.  These synonyms can be returned as a list or a vector that can then be passed to other qdap functions.

<font size="5" color="orange">&diams;</font> **Synonyms Examples**<font size="5" color="orange">&diams;</font>


```r
synonyms(c("the", "cat", "teach"))
```

```
## no match for the following:
## 
## the ========================
```

```
## $cat.def_1
## [1] "feline"    "gib"       "grimalkin" "kitty"     "malkin"   
## 
## $cat.def_2
## [1] "moggy"
## 
## $cat.def_3
## [1] "mouser" "puss"  
## 
## $cat.def_4
## [1] "pussy"
## 
## $cat.def_5
## [1] "tabby"
## 
## $teach.def_1
##  [1] "advise"          "coach"           "demonstrate"    
##  [4] "direct"          "discipline"      "drill"          
##  [7] "edify"           "educate"         "enlighten"      
## [10] "give lessons in" "guide"           "impart"         
## [13] "implant"         "inculcate"       "inform"         
## [16] "instil"          "instruct"        "school"         
## [19] "show"            "train"           "tutor"
```

```r
syn(c("the", "cat", "teach"), return.list = FALSE)
```

```
## no match for the following:
## 
## the ========================
```

```
##  [1] "feline"          "gib"             "grimalkin"      
##  [4] "kitty"           "malkin"          "moggy"          
##  [7] "mouser"          "puss"            "pussy"          
## [10] "tabby"           "advise"          "coach"          
## [13] "demonstrate"     "direct"          "discipline"     
## [16] "drill"           "edify"           "educate"        
## [19] "enlighten"       "give lessons in" "guide"          
## [22] "impart"          "implant"         "inculcate"      
## [25] "inform"          "instil"          "instruct"       
## [28] "school"          "show"            "train"          
## [31] "tutor"
```

```r
syn(c("the", "cat", "teach"), multiwords = FALSE)
```

```
## no match for the following:
## 
## the ========================
```

```
## $cat.def_1
## [1] "feline"    "gib"       "grimalkin" "kitty"     "malkin"   
## 
## $cat.def_2
## [1] "moggy"
## 
## $cat.def_3
## [1] "mouser" "puss"  
## 
## $cat.def_4
## [1] "pussy"
## 
## $cat.def_5
## [1] "tabby"
## 
## $teach.def_1
##  [1] "advise"      "coach"       "demonstrate" "direct"      "discipline" 
##  [6] "drill"       "edify"       "educate"     "enlighten"   "guide"      
## [11] "impart"      "implant"     "inculcate"   "inform"      "instil"     
## [16] "instruct"    "school"      "show"        "train"       "tutor"
```


<h4 id="assoc">Find Associated Words</h4>

<font size="5" color="orange">&diams;</font> **Word Association Examples**<font size="5" color="orange">&diams;</font>


```r
ms <- c(" I ", "you")
et <- c(" it", " tell", "tru")
word_associate(DATA2$state, DATA2$person, match.string = ms,
    wordcloud = TRUE,  proportional = TRUE,
    network.plot = TRUE,  nw.label.proportional = TRUE, extra.terms = et,
    cloud.legend =c("A", "B", "C"),
    title.color = "blue", cloud.colors = c("red", "purple", "gray70"))
```

![plot of chunk unnamed-chunk-55](figure/unnamed-chunk-551.png) ![plot of chunk unnamed-chunk-55](figure/unnamed-chunk-552.png) ![plot of chunk unnamed-chunk-55](figure/unnamed-chunk-553.png) ![plot of chunk unnamed-chunk-55](figure/unnamed-chunk-554.png) ![plot of chunk unnamed-chunk-55](figure/unnamed-chunk-555.png) ![plot of chunk unnamed-chunk-55](figure/unnamed-chunk-556.png) 

```
##    row group unit text                             
## 1    4   sam    4 You liar, it stinks!             
## 2    5  greg    5 I am telling the truth!          
## 3    8   sam    8 I distrust you.                  
## 4    9 sally    9 What are you talking about?      
## 5   11  greg   11 Im hungry. Lets eat. You already?
## 6   12   sam   12 I distrust you.                  
## 7   15  greg   15 I am telling the truth!          
## 8   18  greg   18 Im hungry. Lets eat. You already?
## 9   19 sally   19 What are you talking about?      
## 10  20   sam   20 You liar, it stinks!             
## 11  21  greg   21 I am telling the truth!          
## 12  22   sam   22 You liar, it stinks!             
## 13  24  greg   24 Im hungry. Lets eat. You already?
## 14  25  greg   25 I am telling the truth!          
## 15  30   sam   30 I distrust you.                  
## 16  31  greg   31 Im hungry. Lets eat. You already?
## 17  33   sam   33 I distrust you.                  
## 18  36   sam   36 You liar, it stinks!             
## 19  40  greg   40 I am telling the truth!          
## 20  41   sam   41 You liar, it stinks!             
## 21  42  greg   42 I am telling the truth!          
## 22  44   sam   44 You liar, it stinks!             
## 23  47   sam   47 I distrust you.                  
## 24  49   sam   49 You liar, it stinks!             
## 25  52 sally   52 What are you talking about?      
## 26  53 sally   53 What are you talking about?      
## 27  54  greg   54 I am telling the truth!          
## 28  55   sam   55 I distrust you.                  
## 29  56  greg   56 Im hungry. Lets eat. You already?
## 30  57  greg   57 I am telling the truth!          
## 31  58  greg   58 I am telling the truth!          
## 32  59  greg   59 Im hungry. Lets eat. You already?
## 33  62   sam   62 You liar, it stinks!             
## 34  63 sally   63 What are you talking about?      
## 35  65   sam   65 I distrust you.                  
## 36  67 sally   67 What are you talking about?      
## 37  68   sam   68 I distrust you.
```

```
## Match Terms ===========
## 
## List 1: i, you
```


<h4 id="diffs">Differences In Word Use Between Groups</h4>

<font size="5" color="orange">&diams;</font> **Word Difference Examples**<font size="5" color="orange">&diams;</font>


```r
out <- with(DATA, word_diff_list(text.var = state,
    grouping.var = list(sex, adult)))

ltruncdf(unlist(out, recursive = FALSE), n=4)
```

```
## $f.0_vs_f.1.unique_to_f.0
##    word freq       prop
## 1 about    1        0.1
## 2   are    1       0.25
## 3    be    1        0.1
## 4   can    1 0.16666666
## 
## $f.0_vs_f.1.unique_to_f.1
##    word freq       prop
## 1  good    1 0.03030303
## 2  move    1        0.1
## 3    on    1       0.25
## 4 shall    1        0.1
## 
## $f.0_vs_m.0.unique_to_f.0
##    word freq       prop
## 1 about    1        0.1
## 2   are    1       0.25
## 3    be    1        0.1
## 4   can    1 0.16666666
## 
## $f.0_vs_m.0.unique_to_m.0
##   word freq       prop
## 1  fun    2 0.06060606
## 2    i    2 0.06060606
## 3   is    2        0.2
## 4 it's    2 0.06060606
## 
## $f.1_vs_m.0.unique_to_f.1
##    word freq       prop
## 1  good    1 0.03030303
## 2  move    1        0.1
## 3    on    1       0.25
## 4 shall    1        0.1
## 
## $f.1_vs_m.0.unique_to_m.0
##   word freq       prop
## 1  you    3 0.09090909
## 2  fun    2 0.06060606
## 3    i    2 0.06060606
## 4   is    2        0.2
## 
## $f.0_vs_m.1.unique_to_f.0
##    word freq       prop
## 1 about    1        0.1
## 2   are    1       0.25
## 3    be    1        0.1
## 4   can    1 0.16666666
## 
## $f.0_vs_m.1.unique_to_m.1
##     word freq prop
## 1     do    1  0.1
## 2 should    1 0.25
## 
## $f.1_vs_m.1.unique_to_f.1
##    word freq       prop
## 1  good    1 0.03030303
## 2  move    1        0.1
## 3    on    1       0.25
## 4 shall    1        0.1
## 
## $f.1_vs_m.1.unique_to_m.1
##     word freq       prop
## 1     do    1        0.1
## 2 should    1       0.25
## 3   what    1 0.03030303
## 
## $m.0_vs_m.1.unique_to_m.0
##   word freq       prop
## 1  you    3 0.09090909
## 2  fun    2 0.06060606
## 3    i    2 0.06060606
## 4   is    2        0.2
## 
## $m.0_vs_m.1.unique_to_m.1
##     word freq       prop
## 1     do    1        0.1
## 2 should    1       0.25
## 3     we    1 0.16666666
## 4   what    1 0.03030303
```


<h4 id="word_list">Raw Word Lists/Frequency Counts</h4>

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/word_list.html" target="_blank"><code>word_list</code></a> Examples**<font size="5" color="orange">&diams;</font>


```r
with(DATA, word_list(state, person))
```

```
## $greg
##       WORD FREQ
## 1     it's    2
## 2       no    2
## 3  already    1
## 4       am    1
## 5     dumb    1
## 6      eat    1
## 7   hungry    1
## 8        I    1
## 9      I'm    1
## 10      is    1
## 11   let's    1
## 12     not    1
## 13 telling    1
## 14     the    1
## 15   there    1
## 16   truth    1
## 17     way    1
## 18     you    1
## 
## $researcher
##    WORD FREQ
## 1  good    1
## 2  move    1
## 3    on    1
## 4 shall    1
## 5  then    1
## 6    we    1
## 
## $sally
##       WORD FREQ
## 1    about    1
## 2      are    1
## 3       be    1
## 4      can    1
## 5  certain    1
## 6      how    1
## 7  talking    1
## 8       we    1
## 9     what    1
## 10     you    1
## 
## $sam
##        WORD FREQ
## 1       fun    2
## 2       you    2
## 3  computer    1
## 4  distrust    1
## 5         I    1
## 6        is    1
## 7        it    1
## 8      liar    1
## 9       not    1
## 10   stinks    1
## 11      too    1
## 
## $teacher
##     WORD FREQ
## 1     do    1
## 2 should    1
## 3     we    1
## 4   what    1
```

```r
with(DATA, word_list(state, person, stopwords = Top25Words))
```

```
## $greg
##       WORD FREQ
## 1     it's    2
## 2       no    2
## 3  already    1
## 4       am    1
## 5     dumb    1
## 6      eat    1
## 7   hungry    1
## 8      I'm    1
## 9    let's    1
## 10     not    1
## 11 telling    1
## 12   there    1
## 13   truth    1
## 14     way    1
## 
## $researcher
##    WORD FREQ
## 1  good    1
## 2  move    1
## 3 shall    1
## 4  then    1
## 5    we    1
## 
## $sally
##      WORD FREQ
## 1   about    1
## 2     can    1
## 3 certain    1
## 4     how    1
## 5 talking    1
## 6      we    1
## 7    what    1
## 
## $sam
##       WORD FREQ
## 1      fun    2
## 2 computer    1
## 3 distrust    1
## 4     liar    1
## 5      not    1
## 6   stinks    1
## 7      too    1
## 
## $teacher
##     WORD FREQ
## 1     do    1
## 2 should    1
## 3     we    1
## 4   what    1
```

```r
with(DATA, word_list(state, person, cap = FALSE, cap.list=c("do", "we")))
```

```
## $greg
##       WORD FREQ
## 1     it's    2
## 2       no    2
## 3  already    1
## 4       am    1
## 5     dumb    1
## 6      eat    1
## 7   hungry    1
## 8        I    1
## 9      I'm    1
## 10      is    1
## 11   let's    1
## 12     not    1
## 13 telling    1
## 14     the    1
## 15   there    1
## 16   truth    1
## 17     way    1
## 18     you    1
## 
## $researcher
##    WORD FREQ
## 1  good    1
## 2  move    1
## 3    on    1
## 4 shall    1
## 5  then    1
## 6    We    1
## 
## $sally
##       WORD FREQ
## 1    about    1
## 2      are    1
## 3       be    1
## 4      can    1
## 5  certain    1
## 6      how    1
## 7  talking    1
## 8       We    1
## 9     what    1
## 10     you    1
## 
## $sam
##        WORD FREQ
## 1       fun    2
## 2       you    2
## 3  computer    1
## 4  distrust    1
## 5         I    1
## 6        is    1
## 7        it    1
## 8      liar    1
## 9       not    1
## 10   stinks    1
## 11      too    1
## 
## $teacher
##     WORD FREQ
## 1     Do    1
## 2 should    1
## 3     We    1
## 4   what    1
```

   
<h3 id="coding">Qualitative Coding System</h3>

<div class="funs">
The following functions will be utilized in this section (click to view more): <br>   

<form class="form_left" action="http://trinker.github.io/qdap_dev/cm_code.blank.html" target="_blank">
    <input type="submit" value="cm_code.blank"> 
</form>

<form class="form_left" action="http://trinker.github.io/qdap_dev/cm_code.combine.html" target="_blank">
    <input type="submit" value="cm_code.combine"> 
</form>

<form class="form_left" action="http://trinker.github.io/qdap_dev/cm_code.exclude.html" target="_blank">
    <input type="submit" value="cm_code.exclude"> 
</form>

<form class="form_left" action="http://trinker.github.io/qdap_dev/cm_code.overlap.html" target="_blank">
    <input type="submit" value="cm_code.overlap"> 
</form>

<form action="http://trinker.github.io/qdap_dev/cm_code.transform.html" target="_blank">
    <input type="submit" value="cm_code.transform"> - <a href="#reshape">Combine, Exclude, and Overlap Codes</a>
</form>

<form class="form_left" action="http://trinker.github.io/qdap_dev/cm_df.temp.html" target="_blank">
    <input type="submit" value="cm_df.temp"> 
</form>

<form action="http://trinker.github.io/qdap_dev/cm_2long.html" target="_blank">
    <input type="submit" value="cm_long"> - <a href="#wordcsv">Coding Words:  .csv Approach</a>
</form>



<form class="form_left" action="http://trinker.github.io/qdap_dev/cm_range.temp.html" target="_blank">
    <input type="submit" value="cm_range.temp">
</form>
<form class="form_left" action="http://trinker.github.io/qdap_dev/cm_df.transcript.html" target="_blank">
    <input type="submit" value="cm_df.transcript"> 
</form>
<form action="http://trinker.github.io/qdap_dev/cm_2long.html" target="_blank">
    <input type="submit" value="cm_2long">  - <a href="#wordtrans">Coding Words: Transcript & List Approach</a>
</form>



<form class="form_left" action="http://trinker.github.io/qdap_dev/cm_time.temp.html" target="_blank">
    <input type="submit" value="cm_time.temp">
</form>

<form action="http://trinker.github.io/qdap_dev/cm_2long.html" target="_blank">
    <input type="submit" value="cm_2long"> - <a href="#timespan">Coding Words: Time Spans Approach</a> 
</form>


<form action="http://trinker.github.io/qdap_dev/cm_distance.html" target="_blank">
    <input type="submit" value="cm_distance"> - <a href="#cmdist">Distance Matrix Between Codes</a>
</form>


</div>

A major task in qualitative work is coding either time or words with selected coding structures.  For example a researcher may code the teacher's dialogue as related to the resulting behavior of a student in a classroom as "high", "medium" or "low" engagement. The researcher may choose to apply the coding to:

- The dialogue
- The time spans

The coding process in qdap starts with decison of whether to code the dialogue and/or the time spans.  After that the researcher may follow the sequential subsections in the <a href="#coding">Qualitative Coding System</a> section outlined in these steps:

1. Making a template for coding dialogue/time spans
2. The actual coding  dialogue/time spans
3. Reading in the dialogue/time spans
4. Transforming codes (finding overlap and/or differences between word span/time span of codes)
5. Initial analysis

If you choose the route of coding words qdap gives two approaches.  Each has distinct benefits and disadvantages dependant upon the situation.  If you chose the coding of time spans qdap provides one option. 

If you chose the coding of words you may chose to code a csv file or to code the transcript directly (perhaps with markers or other forms of markup), record the ranges in a text list and then read in the data.  Both approaches can result in the same data being read back into qdap.  The csv approach may allow for extended capabilties (beyond the scope of this vignette) while the transcript/list approach is generally more efficient and takes the approach many qualitative researchers typically utilize in qualitative coding (it also has the added benefit of producing a hard copy).

The next three subsections will walk the reader through how to make a template, code in the template, and read the data back into R/qdap.  Subsections 4-5 will cover reshaping and initial analysis after the data has been read in (this approach is generally the same for all three coded data types).

1. <a href="#wordcsv">Coding Words - The .csv Approach</a> - How to template, code, read in and reshape the data
2. <a href="#wordtrans">Coding Words - The Transcript/List Approach</a> - How to template, code, read in  and reshape the data
3. <a href="#timespan">Coding Time Spans</a> - How to template, code, read in and reshape the data
4. <a href="#reshape">Transforming Codes</a>
5. <a href="#analysis">Initial Coding Analysis</a>

Before getting started with subsections 1-3 the reader will want to know the naming scheme of the code matrix (<font color="red">cm&#95;</font>) functions used.  The initial <font color="red">cm&#95;</font> is utilized for any code matrix family of functions.  The functions containing <font color="red">cm&#95;temp</font> are template functions.  The <font color="red">df</font>, <font color="red">range</font>, or <font color="red">time</font> determine whether the csv (<font color="red">df</font>), Transcript/List (<font color="red">range</font>), or Time Span (<font color="red">time</font>) approach is being utilized.  <font color="red">cm&#95;</font> functions that bear <font color="red">2long</font> transform a read in list to a usable long format.

<h4 id="wordcsv">Coding Words - The .csv Approach <a href="http://www.youtube.com/watch?v=tH242SIESIs" target="_blank" style="text-decoration: none"><b><font size="5" color="#B22222">[YT]</font></b></a>
</h4>

The csv approach utilizes <a href="http://trinker.github.io/qdap_dev/cm_df.temp.html" target="_blank"><code>cm_df.temp</code></a> and <a href="http://trinker.github.io/qdap_dev/cm_2long.html" target="_blank"><code>cm_2long</code></a> functions.  To utilize the csv template approach simply supply the dataframe, specify the text variable and provide a list of anticipated codes.  

<font size="5" color="orange">&diams;</font> **Coding Words (csv approach)**: The Template <font size="5" color="orange">&diams;</font>

<pre><code class="r">## Codes
codes <- qcv(dc, sf, wes, pol, rejk, lk, azx, mmm)

## The csv template
X <- cm_df.temp(DATA, text.var = "state", codes = codes, file = "DATA.csv")
qview(X)
</code></pre>

<pre><code>========================================================================
nrow =  56           ncol =  14             X
========================================================================
   person sex adult code     text word.num dc sf wes pol rejk lk azx mmm
1     sam   m     0   K1 Computer        1  0  0   0   0    0  0   0   0
2     sam   m     0   K1       is        2  0  0   0   0    0  0   0   0
3     sam   m     0   K1     fun.        3  0  0   0   0    0  0   0   0
4     sam   m     0   K1      Not        4  0  0   0   0    0  0   0   0
5     sam   m     0   K1      too        5  0  0   0   0    0  0   0   0
6     sam   m     0   K1     fun.        6  0  0   0   0    0  0   0   0
7    greg   m     0   K2       No        7  0  0   0   0    0  0   0   0
8    greg   m     0   K2     it's        8  0  0   0   0    0  0   0   0
9    greg   m     0   K2     not,        9  0  0   0   0    0  0   0   0
10   greg   m     0   K2     it's       10  0  0   0   0    0  0   0   0
</code></pre>

After coding the data (see the <a href="http://www.youtube.com/watch?v=tH242SIESIs" target="_blank">YouTube video</a>) the data can be read back in with <a href="http://stat.ethz.ch/R-manual/R-devel/library/utils/html/read.table.html" target="_blank">read.csv</a>.


<font size="5" color="orange">&diams;</font> **Coding Words (csv approach)**: Read In and Reshape <font size="5" color="orange">&diams;</font>

<pre><code class="r">## Read in the data
dat <- read.csv("DATA.csv")

## Reshape to long format with word durations
cm_2long(dat)
</code></pre>

<pre><code>    code     person sex adult code.1     text word.num start end variable
1     dc        sam   m     0     K1 Computer        1     0   1      dat
2    wes        sam   m     0     K1 Computer        1     0   1      dat
3   rejk        sam   m     0     K1 Computer        1     0   1      dat
4    mmm        sam   m     0     K1 Computer        1     0   1      dat
5     lk        sam   m     0     K1       is        2     1   2      dat
6    azx        sam   m     0     K1       is        2     1   2      dat
.
.
.
198  wes       greg   m     0    K11 already?       56    55  56      dat
199 rejk       greg   m     0    K11 already?       56    55  56      dat
200   lk       greg   m     0    K11 already?       56    55  56      dat
201  azx       greg   m     0    K11 already?       56    55  56      dat
202  mmm       greg   m     0    K11 already?       56    55  56      dat
</code></pre>


<h4 id="wordtrans">Coding Words - The Transcript/List Approach <a href="http://www.youtube.com/watch?v=cxcD-j0iI2U" target="_blank" style="text-decoration: none"><b><font size="5" color="#B22222">[YT]</font></b></a>
</h4>

The Transcript/List approach utilizes <a href="http://trinker.github.io/qdap_dev/cm_df.transcript.html" target="_blank"><code>cm_df.transcript</code></a>,  <a href="http://trinker.github.io/qdap_dev/cm_range.temp.html" target="_blank"><code>cm_range.temp</code></a> and <a href="http://trinker.github.io/qdap_dev/cm_2long.html" target="_blank"><code>cm_2long</code></a> functions.  To use the transcript template simply supply the dataframe, specify the text variable and provide a list of anticipated codes.  

<font size="5" color="orange">&diams;</font> **Coding Words (Transcript/List approach)**: Transcript Template <font size="5" color="orange">&diams;</font>

<pre><code class="r">## Codes
codes <- qcv(AA, BB, CC)

## Transcript template
X <- cm_df.transcript(DATA$state, DATA$person, file="DATA.txt")
</code></pre>

<pre><code>sam:

                                  
     1        2  3    4   5   6   
     Computer is fun. Not too fun.

greg:

                            
     7  8    9    10   11   
     No it's not, it's dumb.

teacher:

                       
     12   13     14 15 
     What should we do?

sam:

                         
     16  17    18 19     
     You liar, it stinks!
</code></pre>

<font size="5" color="orange">&diams;</font> **Coding Words (Transcript/List approach)**: List Template 1<font size="5" color="orange">&diams;</font>

<pre><code class="r">### List template
cm_range.temp(codes, file = "foo1.txt")
</code></pre>

<pre><code>list(
    AA = qcv(terms=''),
    BB = qcv(terms=''),
    CC = qcv(terms='')
)
</code></pre>

This list below contains demographic variables.  If the researcher has demographic variables it is recomended to supply them at this point.  The demographic variables will be generated with durations automatically.

<font size="5" color="orange">&diams;</font> **Coding Words (Transcript/List approach)**: List Template 2<font size="5" color="orange">&diams;</font>

<pre><code class="r">### List template with demographic variables
with(DATA, cm_range.temp(codes = codes, text.var = state, 
    grouping.var = list(person, adult), file = "foo2.txt"))
</code></pre>

<pre><code>list(
    person_greg = qcv(terms='7:11, 20:24, 30:33, 49:56'),
    person_researcher = qcv(terms='42:48'),
    person_sally = qcv(terms='25:29, 37:41'),
    person_sam = qcv(terms='1:6, 16:19, 34:36'),
    person_teacher = qcv(terms='12:15'),
    adult_0 = qcv(terms='1:11, 16:41, 49:56'),
    adult_1 = qcv(terms='12:15, 42:48'),
    AA = qcv(terms=''),
    BB = qcv(terms=''),
    CC = qcv(terms='')
)
</code></pre>

After coding the data (see the <a href="http://www.youtube.com/watch?v=cxcD-j0iI2U" target="_blank">YouTube video</a>) the data can be read back in with <a href="http://stat.ethz.ch/R-manual/R-devel/library/base/html/source.html" target="_blank">source</a>.  Be sure to assign list to an object (e.g., `dat <- list()`).

<font size="5" color="orange">&diams;</font> **Coding Words (Transcript/List approach)**: Read in the data<font size="5" color="orange">&diams;</font>

<pre><code class="r">## Read it in
source("foo1.txt")

### View it
Time1
</code></pre>

<pre><code>$AA
[1] "1"

$BB
[1] "1:2,"  "3:10," "19"   

$CC
[1] "1:9,"    "100:150"
</code></pre>

This format is not particularly useful.  The data can be reshaped to long format with durations via <a href="http://trinker.github.io/qdap_dev/cm_2long.html" target="_blank"><code>cm_2long</code></a>:

<font size="5" color="orange">&diams;</font> **Coding Words (Transcript/List approach)**: Long format<font size="5" color="orange">&diams;</font>

<pre><code class="r">## Long format with durations
datL <- cm_2long(Time1)
datL
</code></pre>

<pre><code>  code start end variable
1   AA     0   1    Time1
2   BB     0   2    Time1
3   BB     2  10    Time1
4   BB    18  19    Time1
5   CC     0   9    Time1
6   CC    99 150    Time1
</code></pre>

<h4 id="timespan">Coding Time Spans <a href="http://youtu.be/XC-RXeY63bM" target="_blank" style="text-decoration: none"><b><font size="5" color="#B22222">[YT]</font></b></a>
</h4>

The Time Span approach utilizes the <a href="http://trinker.github.io/qdap_dev/cm_time.temp.html" target="_blank"><code>cm_time.temp</code></a> and <a href="http://trinker.github.io/qdap_dev/cm_2long.html" target="_blank"><code>cm_2long</code></a> functions.  To generate the timespan template approach simply supply the list of anticipated codes and a start/end time.  


<font size="5" color="orange">&diams;</font> **Coding Times Spans**: Time Span Template <font size="5" color="orange">&diams;</font>

<pre><code class="r">## Codes
## Time span template
X <- cm_time.temp(start = ":14", end = "7:40", file="timespans.txt")
X <- cm_time.temp(start = ":14", end = "7:40", file="timespans.doc")
</code></pre>


<pre><code>[0]                                14 15 16 ... 51 52 53 54 55 56 57 58 59
[1]0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 ... 51 52 53 54 55 56 57 58 59
[2]0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 ... 51 52 53 54 55 56 57 58 59
[3]0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 ... 51 52 53 54 55 56 57 58 59
[4]0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 ... 51 52 53 54 55 56 57 58 59
[5]0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 ... 51 52 53 54 55 56 57 58 59
[6]0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 ... 51 52 53 54 55 56 57 58 59
[7]0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 ... 51 52 53                                                
</code></pre>


<font size="5" color="orange">&diams;</font> **Coding Times Spans**: List Template 1<font size="5" color="orange">&diams;</font>

<pre><code class="r">### List template
codes <- qcv(AA, BB, CC)
cm_time.temp(codes, file = "codelist.txt")
</code></pre>

<pre><code> list(                                                 
     transcript_time_span = qcv(terms="00:00 - 00:00"),
     AA = qcv(terms=""),                               
     BB = qcv(terms=""),                               
     CC = qcv(terms="")                                
 )  
</code></pre>

This list below contains demographic variables.  If the researcher has demographic variables it is recomended to supply them at this point.  

<font size="5" color="orange">&diams;</font> **Coding Times Spans**: List Template 2<font size="5" color="orange">&diams;</font>

<pre><code class="r">### List template with demographic variables
with(DATA, cm_time.temp(codes, list(person, adult), file = "codelist.txt"))
</code></pre>

<pre><code>list(
    transcript_time_span = qcv(terms="00:00 - 00:00"),
    person_sam = qcv(terms=""),
    person_greg = qcv(terms=""),
    person_teacher = qcv(terms=""),
    person_sally = qcv(terms=""),
    person_researcher = qcv(terms=""),
    adult_0 = qcv(terms=""),
    adult_1 = qcv(terms=""),
    AA = qcv(terms=""),
    BB = qcv(terms=""),
    CC = qcv(terms="")
)
</code></pre>

After coding the data (see the <a href="http://www.youtube.com/watch?v=XC-RXeY63bM&feature=youtu.be" target="_blank">YouTube video</a>) the data can be read back in with <a href="http://stat.ethz.ch/R-manual/R-devel/library/base/html/source.html" target="_blank">source</a>.  Be sure to assign list to an object (e.g., `dat <- list()`).  

<font size="5" color="orange">&diams;</font> **Coding Times Spans**: Read in the data<font size="5" color="orange">&diams;</font>


<pre><code class="r">## Read it in
source("codelist.txt")

### View it
Time1
</code></pre>

<pre><code>$transcript_time_span
[1] "00:00"   "-"       "1:12:00"

$A
[1] "2.40:3.00," "5.01,"      "6.52:7.00," "9.00"      

$B
[1] "2.40,"      "3.01:3.40," "5.01,"      "6.52:7.00," "9.00"      

$C
[1] "2.40:4.00,"  "5.01,"       "6.52:7.00,"  "9.00,"       "13.00:17.01"
</code></pre>

This format is not particularly useful.  The data can be reshaped to long format with durations via <a href="http://trinker.github.io/qdap_dev/cm_2long.html" target="_blank"><code>cm_2long</code></a>:

<font size="5" color="orange">&diams;</font> **Coding Times Spans**: Long format<font size="5" color="orange">&diams;</font>

<pre><code class="r">## Long format with durations
datL <- cm_2long(Time1, v.name = "time")
datL
</code></pre>

<pre><code>   code start  end    Start      End variable
1     A   159  180 00:02:39 00:03:00    Time1
2     A   300  301 00:05:00 00:05:01    Time1
3     A   411  420 00:06:51 00:07:00    Time1
4     A   539  540 00:08:59 00:09:00    Time1
5     B   159  160 00:02:39 00:02:40    Time1
6     B   180  220 00:03:00 00:03:40    Time1
7     B   300  301 00:05:00 00:05:01    Time1
8     B   411  420 00:06:51 00:07:00    Time1
9     B   539  540 00:08:59 00:09:00    Time1
10    C   159  240 00:02:39 00:04:00    Time1
11    C   300  301 00:05:00 00:05:01    Time1
12    C   411  420 00:06:51 00:07:00    Time1
13    C   539  540 00:08:59 00:09:00    Time1
14    C   779 1021 00:12:59 00:17:01    Time1
</code></pre>


<h4 id="reshape">Transforming Codes</h4>

The researcher may want to determine where codes do and do not overlap with one other.  The <font color="red">cm_</font> family of functions bearing (<font color="red">cm_code.</font>) perform various transformative functions (Boolean search).  <a href="http://trinker.github.io/qdap_dev/cm_code.combine.html" target="_blank"><code>cm_code.combine</code></a> will merge the spans (time or word) for given codes.  <a href="http://trinker.github.io/qdap_dev/cm_code.exclude.html" target="_blank"><code>cm_code.exclude</code></a> will give provide spans that exclude given codes.  <a href="http://trinker.github.io/qdap_dev/cm_code.overlap.html" target="_blank"><code>cm_code.overlap</code></a> will yield the spans where all of the given codes co-occur.  <a href="http://trinker.github.io/qdap_dev/cm_code.transform.html" target="_blank"><code>cm_code.transform</code></a> is a wrapper for the previous three functions that produces one dataframe in a single call.  Lastly, <a href="http://trinker.github.io/qdap_dev/cm_code.blank.html" target="_blank"><code>cm_code.blank</code></a> proveds a more flexible framework that allows for the introduction of multiple lofical operators between codes.  Most tasks can be handled with the <a href="http://trinker.github.io/qdap_dev/cm_code.transform.html" target="_blank"><code>cm_code.transform</code></a> function.

For Examples of each click the links below:    
1. <a href="#cm_code.combine">cm_code.combine Examples</a>     
2. <a href="#cm_code.exclude">cm_code.exclude Examples</a>  
3. <a href="#cm_code.overlap">cm_code.overlap Examples</a>       
4. <a href="#cm_code.transform">cm_code.transform Examples</a>    
5. <a href="#cm_code.blank">cm_code.blank Examples</a>    

For the sake of simplicity the uses of these functions will be demonstrated via a gantt plot for a visual comparison of the data sets.

The reader should note that all of the above functions utilize two helper functions (<a href="http://trinker.github.io/qdap_dev/cm_long2dummy.html" target="_blank"><code>cm_long2dummy</code></a> and <a href="http://trinker.github.io/qdap_dev/cm_dummy2long.html" target="_blank"><code>cm_dummy2long</code></a>) to stretch the spans into single units of measure (word or second) perform a calculation and then condense back to spans.  More advanced needs may require the explicit use of these functions, though they are beyond the scope of this vignette.  

The following data sets will be utilized through out the demonstrations of the <font color="red">cm_code.</font> family of functions:

<font size="5" color="orange">&diams;</font> **Common Data Sets** - Word Approach<font size="5" color="orange">&diams;</font>


```r
foo <- list(
    AA = qcv(terms="1:10"),
    BB = qcv(terms="1:2, 3:10, 19"),
    CC = qcv(terms="1:3, 5:6")
)

foo2  <- list(
    AA = qcv(terms="4:8"),
    BB = qcv(terms="1:4, 10:12"),
    CC = qcv(terms="1, 11, 15:20"),
    DD = qcv(terms="")
)
```



```r
## Single time, long word approach
(x <- cm_2long(foo))
```

```
##   code start end variable
## 1   AA     0  10      foo
## 2   BB     0   2      foo
## 3   BB     2  10      foo
## 4   BB    18  19      foo
## 5   CC     0   3      foo
## 6   CC     4   6      foo
```


![plot of chunk unnamed-chunk-60](figure/unnamed-chunk-60.png) 



```r
## Repeated measures, long word approach
(z <- cm_2long(foo, foo2, v.name="time"))
```

```
##    code start end time
## 1    AA     0  10  foo
## 2    BB     0   2  foo
## 3    BB     2  10  foo
## 4    BB    18  19  foo
## 5    CC     0   3  foo
## 6    CC     4   6  foo
## 7    AA     3   8 foo2
## 8    BB     0   4 foo2
## 9    BB     9  12 foo2
## 10   CC     0   1 foo2
## 11   CC    10  11 foo2
## 12   CC    14  20 foo2
```


![plot of chunk unnamed-chunk-62](figure/unnamed-chunk-62.png) 



<font size="5" color="orange">&diams;</font> **Common Data Sets** - Time Span Approach<font size="5" color="orange">&diams;</font>


```r
bar1 <- list(
    transcript_time_span = qcv(00:00 - 1:12:00),
    A = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00"),
    B = qcv(terms = "2.40, 3.01:3.02, 5.01, 6.02:7.00, 9.00,
        1.12.00:1.19.01"),
    C = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00, 16.25:17.01")
)

bar2 <- list(
    transcript_time_span = qcv(00:00 - 1:12:00),
    A = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00"),
    B = qcv(terms = "2.40, 3.01:3.02, 5.01, 6.02:7.00, 9.00,
        1.12.00:1.19.01"),
    C = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00, 17.01")
)
```



```r
## Single time, long time approach
(dat <- cm_2long(bar1))
```

```
##    code start  end    Start      End variable
## 1     A   159  180 00:02:39 00:03:00     bar1
## 2     A   300  301 00:05:00 00:05:01     bar1
## 3     A   361  420 00:06:01 00:07:00     bar1
## 4     A   539  540 00:08:59 00:09:00     bar1
## 5     B   159  160 00:02:39 00:02:40     bar1
## 6     B   180  182 00:03:00 00:03:02     bar1
## 7     B   300  301 00:05:00 00:05:01     bar1
## 8     B   361  420 00:06:01 00:07:00     bar1
## 9     B   539  540 00:08:59 00:09:00     bar1
## 10    B  4319 4741 01:11:59 01:19:01     bar1
## 11    C   159  180 00:02:39 00:03:00     bar1
## 12    C   300  301 00:05:00 00:05:01     bar1
## 13    C   361  420 00:06:01 00:07:00     bar1
## 14    C   539  540 00:08:59 00:09:00     bar1
## 15    C   984 1021 00:16:24 00:17:01     bar1
```


![plot of chunk unnamed-chunk-65](figure/unnamed-chunk-65.png) 



```r
## Repeated measures, long time approach
(dats <- cm_2long(bar1, bar2, v.name = "time"))
```

```
##    code start  end    Start      End time
## 1     A   159  180 00:02:39 00:03:00 bar1
## 2     A   300  301 00:05:00 00:05:01 bar1
## 3     A   361  420 00:06:01 00:07:00 bar1
## 4     A   539  540 00:08:59 00:09:00 bar1
## 5     B   159  160 00:02:39 00:02:40 bar1
## 6     B   180  182 00:03:00 00:03:02 bar1
## 7     B   300  301 00:05:00 00:05:01 bar1
## 8     B   361  420 00:06:01 00:07:00 bar1
## 9     B   539  540 00:08:59 00:09:00 bar1
## 10    B  4319 4741 01:11:59 01:19:01 bar1
## 11    C   159  180 00:02:39 00:03:00 bar1
## 12    C   300  301 00:05:00 00:05:01 bar1
## 13    C   361  420 00:06:01 00:07:00 bar1
## 14    C   539  540 00:08:59 00:09:00 bar1
## 15    C   984 1021 00:16:24 00:17:01 bar1
## 16    A   159  180 00:02:39 00:03:00 bar2
## 17    A   300  301 00:05:00 00:05:01 bar2
## 18    A   361  420 00:06:01 00:07:00 bar2
## 19    A   539  540 00:08:59 00:09:00 bar2
## 20    B   159  160 00:02:39 00:02:40 bar2
## 21    B   180  182 00:03:00 00:03:02 bar2
## 22    B   300  301 00:05:00 00:05:01 bar2
## 23    B   361  420 00:06:01 00:07:00 bar2
## 24    B   539  540 00:08:59 00:09:00 bar2
## 25    B  4319 4741 01:11:59 01:19:01 bar2
## 26    C   159  180 00:02:39 00:03:00 bar2
## 27    C   300  301 00:05:00 00:05:01 bar2
## 28    C   361  420 00:06:01 00:07:00 bar2
## 29    C   539  540 00:08:59 00:09:00 bar2
## 30    C  1020 1021 00:17:00 00:17:01 bar2
```


![plot of chunk unnamed-chunk-67](figure/unnamed-chunk-67.png) 


<h5 id="cm_code.combine"><font color="green">cm_code.combine Examples</font></h5>

<a href="http://trinker.github.io/qdap_dev/cm_code.combine.html" target="_blank"><code>cm_code.combine</code></a> provides all the spans (time/words) that are occupied by one or more of the combined codes.  For example, if we utilized <a href="http://trinker.github.io/qdap_dev/cm_code.combine.html" target="_blank"><code>cm_code.combine</code></a> on code list X and Y the result would be any span where X or Y is located. This is the OR of the Boolean search.  Note that `combine.code.list` must be supplied as a list of named character vectors.

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/cm_code.combine.html" target="_blank"><code>cm_code.combine</code></a> Single Time** *Word Example*<font size="5" color="orange">&diams;</font>


```r
(cc1 <- cm_code.combine(x, list(ALL=qcv(AA, BB, CC))))
```

```
##   code start end
## 1   AA     0  10
## 2   BB     0  10
## 3   BB    18  19
## 4   CC     0   3
## 5   CC     4   6
## 6  ALL     0  10
## 7  ALL    18  19
```


![plot of chunk unnamed-chunk-69](figure/unnamed-chunk-69.png) 


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/cm_code.combine.html" target="_blank"><code>cm_code.combine</code></a> Repeated Measures** *Word Example*<font size="5" color="orange">&diams;</font>


```r
combines <- list(AB=qcv(AA, BB), ABC=qcv(AA, BB, CC))
(cc2 <- cm_code.combine(z, combines, rm.var = "time"))
```

```
##    code start end time
## 1    AA     0  10  foo
## 2    BB     0  10  foo
## 3    BB    18  19  foo
## 4    CC     0   3  foo
## 5    CC     4   6  foo
## 6    AB     0  10  foo
## 7    AB    18  19  foo
## 8   ABC     0  10  foo
## 9   ABC    18  19  foo
## 10   AA     3   8 foo2
## 11   BB     0   4 foo2
## 12   BB     9  12 foo2
## 13   CC     0   1 foo2
## 14   CC    10  11 foo2
## 15   CC    14  20 foo2
## 16   AB     0   8 foo2
## 17   AB     9  12 foo2
## 18  ABC     0   8 foo2
## 19  ABC     9  12 foo2
## 20  ABC    14  20 foo2
```


![plot of chunk unnamed-chunk-71](figure/unnamed-chunk-71.png) 


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/cm_code.combine.html" target="_blank"><code>cm_code.combine</code></a> Single Time** *Time Span Example*<font size="5" color="orange">&diams;</font>


```r
combines2 <- list(AB=qcv(A, B), BC=qcv(B, C), ABC=qcv(A, B, C))
(cc3 <- cm_code.combine(dat, combines2))
```

```
##    code start  end    Start      End
## 1     A   159  180 00:02:39 00:03:00
## 2     A   300  301 00:05:00 00:05:01
## 3     A   361  420 00:06:01 00:07:00
## 4     A   539  540 00:08:59 00:09:00
## 5     B   159  160 00:02:39 00:02:40
## 6     B   180  182 00:03:00 00:03:02
## 7     B   300  301 00:05:00 00:05:01
## 8     B   361  420 00:06:01 00:07:00
## 9     B   539  540 00:08:59 00:09:00
## 10    B  4319 4741 01:11:59 01:19:01
## 11    C   159  180 00:02:39 00:03:00
## 12    C   300  301 00:05:00 00:05:01
## 13    C   361  420 00:06:01 00:07:00
## 14    C   539  540 00:08:59 00:09:00
## 15    C   984 1021 00:16:24 00:17:01
## 16   AB   159  182 00:02:39 00:03:02
## 17   AB   300  301 00:05:00 00:05:01
## 18   AB   361  420 00:06:01 00:07:00
## 19   AB   539  540 00:08:59 00:09:00
## 20   AB  4319 4741 01:11:59 01:19:01
## 21   BC   159  182 00:02:39 00:03:02
## 22   BC   300  301 00:05:00 00:05:01
## 23   BC   361  420 00:06:01 00:07:00
## 24   BC   539  540 00:08:59 00:09:00
## 25   BC   984 1021 00:16:24 00:17:01
## 26   BC  4319 4741 01:11:59 01:19:01
## 27  ABC   159  182 00:02:39 00:03:02
## 28  ABC   300  301 00:05:00 00:05:01
## 29  ABC   361  420 00:06:01 00:07:00
## 30  ABC   539  540 00:08:59 00:09:00
## 31  ABC   984 1021 00:16:24 00:17:01
## 32  ABC  4319 4741 01:11:59 01:19:01
```


![plot of chunk unnamed-chunk-73](figure/unnamed-chunk-73.png) 


<h5 id="cm_code.exclude"><font color="green">cm_code.exclude Examples</font></h5>

<a href="http://trinker.github.io/qdap_dev/cm_code.exclude.html" target="_blank"><code>cm_code.exclude</code></a> provides all the spans (time/words) that are occupied by one or more of the combined codes with the exclusion of another code.  For example, if we utilized <a href="http://trinker.github.io/qdap_dev/cm_code.combine.html" target="_blank"><code>cm_code.combine</code></a> on code list X and Y the result would be any span where X is located but Y is not. This is the NOT of the Boolean search.  The last term supplied to exclude.code.list is the excluded term.  All other terms are combined and the final code term is partitioned out.  Note that `exclude.code.list` must be supplied as a list of named character vectors.


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/cm_code.exclude.html" target="_blank"><code>cm_code.exclude</code></a> Single Time** *Word Example*<font size="5" color="orange">&diams;</font>


```r
(ce1 <- cm_code.exclude(x, list(BnoC=qcv(BB, CC))))
```

```
##   code start end
## 1   AA     0  10
## 2   BB     0  10
## 3   BB    18  19
## 4   CC     0   3
## 5   CC     4   6
## 6 BnoC     3   4
## 7 BnoC     6  10
## 8 BnoC    18  19
```


![plot of chunk unnamed-chunk-75](figure/unnamed-chunk-75.png) 


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/cm_code.exclude.html" target="_blank"><code>cm_code.exclude</code></a> Repeated Measures** *Word Example*<font size="5" color="orange">&diams;</font>


```r
exlist <- list(AnoB=qcv(AA, BB), ABnoC=qcv(AA, BB, CC))
(ce2 <- cm_code.exclude(z, exlist, rm.var = "time"))
```

```
##     code start end time
## 1     AA     0  10  foo
## 2     BB     0  10  foo
## 3     BB    18  19  foo
## 4     CC     0   3  foo
## 5     CC     4   6  foo
## 6  ABnoC     3   4  foo
## 7  ABnoC     6  10  foo
## 8  ABnoC    18  19  foo
## 9     AA     3   8 foo2
## 10    BB     0   4 foo2
## 11    BB     9  12 foo2
## 12    CC     0   1 foo2
## 13    CC    10  11 foo2
## 14    CC    14  20 foo2
## 15  AnoB     4   8 foo2
## 16 ABnoC     1   8 foo2
## 17 ABnoC     9  10 foo2
## 18 ABnoC    11  12 foo2
```


![plot of chunk unnamed-chunk-77](figure/unnamed-chunk-77.png) 


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/cm_code.exclude.html" target="_blank"><code>cm_code.exclude</code></a> Repeated Measures** *Time Span Example*<font size="5" color="orange">&diams;</font>


```r
exlist2 <- list(AnoB=qcv(A, B), BnoC=qcv(B, C), ABnoC=qcv(A, B, C))
(ce3 <- cm_code.exclude(dats, exlist2, "time"))
```

```
##     code start  end    Start      End time
## 1      A   159  180 00:02:39 00:03:00 bar1
## 2      A   300  301 00:05:00 00:05:01 bar1
## 3      A   361  420 00:06:01 00:07:00 bar1
## 4      A   539  540 00:08:59 00:09:00 bar1
## 5      B   159  160 00:02:39 00:02:40 bar1
## 6      B   180  182 00:03:00 00:03:02 bar1
## 7      B   300  301 00:05:00 00:05:01 bar1
## 8      B   361  420 00:06:01 00:07:00 bar1
## 9      B   539  540 00:08:59 00:09:00 bar1
## 10     B  4319 4741 01:11:59 01:19:01 bar1
## 11     C   159  180 00:02:39 00:03:00 bar1
## 12     C   300  301 00:05:00 00:05:01 bar1
## 13     C   361  420 00:06:01 00:07:00 bar1
## 14     C   539  540 00:08:59 00:09:00 bar1
## 15     C   984 1021 00:16:24 00:17:01 bar1
## 16  AnoB   160  180 00:02:40 00:03:00 bar1
## 17  BnoC   180  182 00:03:00 00:03:02 bar1
## 18  BnoC  4319 4741 01:11:59 01:19:01 bar1
## 19 ABnoC   180  182 00:03:00 00:03:02 bar1
## 20 ABnoC  4319 4741 01:11:59 01:19:01 bar1
## 21     A   159  180 00:02:39 00:03:00 bar2
## 22     A   300  301 00:05:00 00:05:01 bar2
## 23     A   361  420 00:06:01 00:07:00 bar2
## 24     A   539  540 00:08:59 00:09:00 bar2
## 25     B   159  160 00:02:39 00:02:40 bar2
## 26     B   180  182 00:03:00 00:03:02 bar2
## 27     B   300  301 00:05:00 00:05:01 bar2
## 28     B   361  420 00:06:01 00:07:00 bar2
## 29     B   539  540 00:08:59 00:09:00 bar2
## 30     B  4319 4741 01:11:59 01:19:01 bar2
## 31     C   159  180 00:02:39 00:03:00 bar2
## 32     C   300  301 00:05:00 00:05:01 bar2
## 33     C   361  420 00:06:01 00:07:00 bar2
## 34     C   539  540 00:08:59 00:09:00 bar2
## 35     C  1020 1021 00:17:00 00:17:01 bar2
## 36  AnoB   160  180 00:02:40 00:03:00 bar2
## 37  BnoC   180  182 00:03:00 00:03:02 bar2
## 38  BnoC  4319 4741 01:11:59 01:19:01 bar2
## 39 ABnoC   180  182 00:03:00 00:03:02 bar2
## 40 ABnoC  4319 4741 01:11:59 01:19:01 bar2
```


![plot of chunk unnamed-chunk-79](figure/unnamed-chunk-79.png) 


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/cm_code.exclude.html" target="_blank"><code>cm_code.exclude</code></a> Single TIme** *Time Span Combined Exclude Example*<font size="5" color="orange">&diams;</font>


```r
(ce4.1 <- cm_code.combine(dat, list(AB = qcv(A, B))))
```

```
##    code start  end    Start      End
## 1     A   159  180 00:02:39 00:03:00
## 2     A   300  301 00:05:00 00:05:01
## 3     A   361  420 00:06:01 00:07:00
## 4     A   539  540 00:08:59 00:09:00
## 5     B   159  160 00:02:39 00:02:40
## 6     B   180  182 00:03:00 00:03:02
## 7     B   300  301 00:05:00 00:05:01
## 8     B   361  420 00:06:01 00:07:00
## 9     B   539  540 00:08:59 00:09:00
## 10    B  4319 4741 01:11:59 01:19:01
## 11    C   159  180 00:02:39 00:03:00
## 12    C   300  301 00:05:00 00:05:01
## 13    C   361  420 00:06:01 00:07:00
## 14    C   539  540 00:08:59 00:09:00
## 15    C   984 1021 00:16:24 00:17:01
## 16   AB   159  182 00:02:39 00:03:02
## 17   AB   300  301 00:05:00 00:05:01
## 18   AB   361  420 00:06:01 00:07:00
## 19   AB   539  540 00:08:59 00:09:00
## 20   AB  4319 4741 01:11:59 01:19:01
```

```r
(ce4.2 <- cm_code.exclude(ce4.1, list(CnoAB = qcv(C, AB))))
```

```
##     code start  end    Start      End
## 1      A   159  180 00:02:39 00:03:00
## 2      A   300  301 00:05:00 00:05:01
## 3      A   361  420 00:06:01 00:07:00
## 4      A   539  540 00:08:59 00:09:00
## 5      B   159  160 00:02:39 00:02:40
## 6      B   180  182 00:03:00 00:03:02
## 7      B   300  301 00:05:00 00:05:01
## 8      B   361  420 00:06:01 00:07:00
## 9      B   539  540 00:08:59 00:09:00
## 10     B  4319 4741 01:11:59 01:19:01
## 11     C   159  180 00:02:39 00:03:00
## 12     C   300  301 00:05:00 00:05:01
## 13     C   361  420 00:06:01 00:07:00
## 14     C   539  540 00:08:59 00:09:00
## 15     C   984 1021 00:16:24 00:17:01
## 16    AB   159  182 00:02:39 00:03:02
## 17    AB   300  301 00:05:00 00:05:01
## 18    AB   361  420 00:06:01 00:07:00
## 19    AB   539  540 00:08:59 00:09:00
## 20    AB  4319 4741 01:11:59 01:19:01
## 21 CnoAB   984 1021 00:16:24 00:17:01
```


![plot of chunk unnamed-chunk-81](figure/unnamed-chunk-81.png) 


<h5 id="cm_code.overlap"><font color="green">cm_code.overlap Examples</font></h5>

<a href="http://trinker.github.io/qdap_dev/cm_code.overlap.html" target="_blank"><code>cm_code.overlap</code></a> provides all the spans (time/words) that are occupied by all of the given codes.  For example, if we utilized <a href="http://trinker.github.io/qdap_dev/cm_code.overlap.html" target="_blank"><code>cm_code.overlap</code></a> on code list X and Y the result would be any span where X and Y are both located. This is the AND of the Boolean search.  Note that `overlap.code.list` must be supplied as a list of named character vectors.

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/cm_code.overlap.html" target="_blank"><code>cm_code.overlap</code></a> Single Time** *Word Example*<font size="5" color="orange">&diams;</font>


```r
(co1 <- cm_code.overlap(x, list(BC=qcv(BB, CC))))
```

```
##   code start end
## 1   AA     0  10
## 2   BB     0  10
## 3   BB    18  19
## 4   CC     0   3
## 5   CC     4   6
## 6   BC     0   3
## 7   BC     4   6
```


![plot of chunk unnamed-chunk-83](figure/unnamed-chunk-83.png) 


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/cm_code.overlap.html" target="_blank"><code>cm_code.overlap</code></a> Repeated Measures** *Word Example*<font size="5" color="orange">&diams;</font>


```r
overlist <- list(AB=qcv(AA, BB), ABC=qcv(AA, BB, CC))
(co2 <- cm_code.overlap(z, overlist, rm.var = "time"))
```

```
##    code start end time
## 1    AA     0  10  foo
## 2    BB     0  10  foo
## 3    BB    18  19  foo
## 4    CC     0   3  foo
## 5    CC     4   6  foo
## 6    AB     0  10  foo
## 7   ABC     0   3  foo
## 8   ABC     4   6  foo
## 9    AA     3   8 foo2
## 10   BB     0   4 foo2
## 11   BB     9  12 foo2
## 12   CC     0   1 foo2
## 13   CC    10  11 foo2
## 14   CC    14  20 foo2
## 15   AB     3   4 foo2
```


![plot of chunk unnamed-chunk-85](figure/unnamed-chunk-85.png) 


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/cm_code.overlap.html" target="_blank"><code>cm_code.overlap</code></a> Repeated Measures** *Time Span Example*<font size="5" color="orange">&diams;</font>


```r
overlist2 <- list(AB=qcv(A, B), BC=qcv(B, C), ABC=qcv(A, B, C))
(co3 <- cm_code.overlap(dats, overlist2, "time"))
```

```
##    code start  end    Start      End time
## 1     A   159  180 00:02:39 00:03:00 bar1
## 2     A   300  301 00:05:00 00:05:01 bar1
## 3     A   361  420 00:06:01 00:07:00 bar1
## 4     A   539  540 00:08:59 00:09:00 bar1
## 5     B   159  160 00:02:39 00:02:40 bar1
## 6     B   180  182 00:03:00 00:03:02 bar1
## 7     B   300  301 00:05:00 00:05:01 bar1
## 8     B   361  420 00:06:01 00:07:00 bar1
## 9     B   539  540 00:08:59 00:09:00 bar1
## 10    B  4319 4741 01:11:59 01:19:01 bar1
## 11    C   159  180 00:02:39 00:03:00 bar1
## 12    C   300  301 00:05:00 00:05:01 bar1
## 13    C   361  420 00:06:01 00:07:00 bar1
## 14    C   539  540 00:08:59 00:09:00 bar1
## 15    C   984 1021 00:16:24 00:17:01 bar1
## 16   AB   159  160 00:02:39 00:02:40 bar1
## 17   AB   300  301 00:05:00 00:05:01 bar1
## 18   AB   361  420 00:06:01 00:07:00 bar1
## 19   AB   539  540 00:08:59 00:09:00 bar1
## 20   BC   159  160 00:02:39 00:02:40 bar1
## 21   BC   300  301 00:05:00 00:05:01 bar1
## 22   BC   361  420 00:06:01 00:07:00 bar1
## 23   BC   539  540 00:08:59 00:09:00 bar1
## 24  ABC   159  160 00:02:39 00:02:40 bar1
## 25  ABC   300  301 00:05:00 00:05:01 bar1
## 26  ABC   361  420 00:06:01 00:07:00 bar1
## 27  ABC   539  540 00:08:59 00:09:00 bar1
## 28    A   159  180 00:02:39 00:03:00 bar2
## 29    A   300  301 00:05:00 00:05:01 bar2
## 30    A   361  420 00:06:01 00:07:00 bar2
## 31    A   539  540 00:08:59 00:09:00 bar2
## 32    B   159  160 00:02:39 00:02:40 bar2
## 33    B   180  182 00:03:00 00:03:02 bar2
## 34    B   300  301 00:05:00 00:05:01 bar2
## 35    B   361  420 00:06:01 00:07:00 bar2
## 36    B   539  540 00:08:59 00:09:00 bar2
## 37    B  4319 4741 01:11:59 01:19:01 bar2
## 38    C   159  180 00:02:39 00:03:00 bar2
## 39    C   300  301 00:05:00 00:05:01 bar2
## 40    C   361  420 00:06:01 00:07:00 bar2
## 41    C   539  540 00:08:59 00:09:00 bar2
## 42    C  1020 1021 00:17:00 00:17:01 bar2
## 43   AB   159  160 00:02:39 00:02:40 bar2
## 44   AB   300  301 00:05:00 00:05:01 bar2
## 45   AB   361  420 00:06:01 00:07:00 bar2
## 46   AB   539  540 00:08:59 00:09:00 bar2
## 47   BC   159  160 00:02:39 00:02:40 bar2
## 48   BC   300  301 00:05:00 00:05:01 bar2
## 49   BC   361  420 00:06:01 00:07:00 bar2
## 50   BC   539  540 00:08:59 00:09:00 bar2
## 51  ABC   159  160 00:02:39 00:02:40 bar2
## 52  ABC   300  301 00:05:00 00:05:01 bar2
## 53  ABC   361  420 00:06:01 00:07:00 bar2
## 54  ABC   539  540 00:08:59 00:09:00 bar2
```


![plot of chunk unnamed-chunk-87](figure/unnamed-chunk-87.png) 


<h5 id="cm_code.transform"><font color="green"><a href="http://trinker.github.io/qdap_dev/cm_code.transform.html" target="_blank"><code>cm_code.transform</code></a> Examples</font></h5>

<a href="http://trinker.github.io/qdap_dev/cm_code.transform.html" target="_blank"><code>cm_code.transform</code></a> is merely a wrapper for <a href="http://trinker.github.io/qdap_dev/cm_code.combine.html" target="_blank"><code>cm_code.combine</code></a>, <a href="http://trinker.github.io/qdap_dev/cm_code.exclude.html" target="_blank"><code>cm_code.exclude</code></a>, and <a href="http://trinker.github.io/qdap_dev/cm_code.overlap.html" target="_blank"><code>cm_code.overlap</code></a>.


<font size="5" color="orange">&diams;</font> <b><a href="http://trinker.github.io/qdap_dev/cm_code.transform.html" target="_blank"><code>cm_code.transform</code></a></b> - Example 1<font size="5" color="orange">&diams;</font>


```r
ct1 <- cm_code.transform(x, 
    overlap.code.list = list(oABC=qcv(AA, BB, CC)),
    combine.code.list = list(ABC=qcv(AA, BB, CC)), 
    exclude.code.list = list(ABnoC=qcv(AA, BB, CC))
)
ct1
```

```
##     code start end
## 1     AA     0  10
## 2     BB     0  10
## 3     BB    18  19
## 4     CC     0   3
## 5     CC     4   6
## 6   oABC     0   3
## 7   oABC     4   6
## 8    ABC     0  10
## 9    ABC    18  19
## 10 ABnoC     3   4
## 11 ABnoC     6  10
## 12 ABnoC    18  19
```


![plot of chunk unnamed-chunk-89](figure/unnamed-chunk-89.png) 


<font size="5" color="orange">&diams;</font> <b><a href="http://trinker.github.io/qdap_dev/cm_code.transform.html" target="_blank"><code>cm_code.transform</code></a></b> - Example 2<font size="5" color="orange">&diams;</font>


```r
ct2 <-cm_code.transform(z, 
    overlap.code.list = list(oABC=qcv(AA, BB, CC)),
    combine.code.list = list(ABC=qcv(AA, BB, CC)), 
    exclude.code.list = list(ABnoC=qcv(AA, BB, CC)), "time"
)
ct2
```

```
##     code start end time
## 1     AA     0  10  foo
## 2     BB     0  10  foo
## 3     BB    18  19  foo
## 4     CC     0   3  foo
## 5     CC     4   6  foo
## 6   oABC     0   3  foo
## 7   oABC     4   6  foo
## 14   ABC     0  10  foo
## 15   ABC    18  19  foo
## 19 ABnoC     3   4  foo
## 20 ABnoC     6  10  foo
## 21 ABnoC    18  19  foo
## 8     AA     3   8 foo2
## 9     BB     0   4 foo2
## 10    BB     9  12 foo2
## 11    CC     0   1 foo2
## 12    CC    10  11 foo2
## 13    CC    14  20 foo2
## 16   ABC     0   8 foo2
## 17   ABC     9  12 foo2
## 18   ABC    14  20 foo2
## 22 ABnoC     1   8 foo2
## 23 ABnoC     9  10 foo2
## 24 ABnoC    11  12 foo2
```


![plot of chunk unnamed-chunk-91](figure/unnamed-chunk-91.png) 


<font size="5" color="orange">&diams;</font> <b><a href="http://trinker.github.io/qdap_dev/cm_code.transform.html" target="_blank"><code>cm_code.transform</code></a></b> - Example 3<font size="5" color="orange">&diams;</font>


```r
ct3 <-cm_code.transform(dat, 
    overlap.code.list = list(oABC=qcv(A, B, C)),
    combine.code.list = list(ABC=qcv(A, B, C)), 
    exclude.code.list = list(ABnoC=qcv(A, B, C))
)
ct3
```

```
##     code start  end    Start      End
## 1      A   159  180 00:02:39 00:03:00
## 2      A   300  301 00:05:00 00:05:01
## 3      A   361  420 00:06:01 00:07:00
## 4      A   539  540 00:08:59 00:09:00
## 5      B   159  160 00:02:39 00:02:40
## 6      B   180  182 00:03:00 00:03:02
## 7      B   300  301 00:05:00 00:05:01
## 8      B   361  420 00:06:01 00:07:00
## 9      B   539  540 00:08:59 00:09:00
## 10     B  4319 4741 01:11:59 01:19:01
## 11     C   159  180 00:02:39 00:03:00
## 12     C   300  301 00:05:00 00:05:01
## 13     C   361  420 00:06:01 00:07:00
## 14     C   539  540 00:08:59 00:09:00
## 15     C   984 1021 00:16:24 00:17:01
## 16  oABC   159  160 00:02:39 00:02:40
## 17  oABC   300  301 00:05:00 00:05:01
## 18  oABC   361  420 00:06:01 00:07:00
## 19  oABC   539  540 00:08:59 00:09:00
## 20   ABC   159  182 00:02:39 00:03:02
## 21   ABC   300  301 00:05:00 00:05:01
## 22   ABC   361  420 00:06:01 00:07:00
## 23   ABC   539  540 00:08:59 00:09:00
## 24   ABC   984 1021 00:16:24 00:17:01
## 25   ABC  4319 4741 01:11:59 01:19:01
## 26 ABnoC   180  182 00:03:00 00:03:02
## 27 ABnoC  4319 4741 01:11:59 01:19:01
```


![plot of chunk unnamed-chunk-93](figure/unnamed-chunk-93.png) 



<h5 id="cm_code.blank"><font color="green">cm_code.blank Examples</font></h5>

<a href="http://trinker.github.io/qdap_dev/cm_code.blank.html" target="_blank"><code>cm_code.blank</code></a> provides flexible Boolean comparisons between word.time spans.  The `overlap` argument takes a logical value, an integer or a character string of binary operator couple with an integer.  It is important to understand how the function operates.  This initial step calls <a href="http://trinker.github.io/qdap_dev/cm_long2dummy.html" target="_blank"><code>cm_long2dummy</code></a> as seen below (stretching the spans to dummy coded columns), the comparison is conduted between columns, and then the columns are reverted back to spans via the <a href="http://trinker.github.io/qdap_dev/cm)dummy2long.html" target="_blank"><code>cm)dummy2long</code></a>.  This first example illustates the stretching to dummy and reverting back to spans.

<font size="5" color="orange">&diams;</font> **Long to dummy and dummy to long** <font size="5" color="orange">&diams;</font>


```r
long2dummy <- cm_long2dummy(x, "variable")
list(original =x,
    long_2_dummy_format = long2dummy[[1]],
    dummy_back_2_long = cm_dummy2long(long2dummy, "variable")
)
```

```
$original
  code start end variable
1   AA     0  10      foo
2   BB     0   2      foo
3   BB     2  10      foo
4   BB    18  19      foo
5   CC     0   3      foo
6   CC     4   6      foo

$long_2_dummy_format
   AA BB CC
0   1  1  1
1   1  1  1
2   1  1  1
3   1  1  0
4   1  1  1
5   1  1  1
6   1  1  0
7   1  1  0
8   1  1  0
9   1  1  0
10  0  0  0
11  0  0  0
12  0  0  0
13  0  0  0
14  0  0  0
15  0  0  0
16  0  0  0
17  0  0  0
18  0  1  0
19  0  0  0

$dummy_back_2_long
  code start end variable
1   AA     0  10      foo
2   BB     0  10      foo
3   BB    18  19      foo
4   CC     0   3      foo
5   CC     4   6      foo
```


Now let's examine a few uses of <a href="http://trinker.github.io/qdap_dev/cm_code.blank.html" target="_blank"><code>cm_code.blank</code></a>.  The first is to set `overlap = TRUE` (the default behavior).  This defualt behavior is identical to <a href="http://trinker.github.io/qdap_dev/cm_code.overlap.html" target="_blank"><code>cm_code.overlap</code></a> as seen below.

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/cm_code.blank.html" target="_blank"><code>cm_code.blank</code></a>** - `overlap = TRUE` <font size="5" color="orange">&diams;</font>


```r
(cb1 <- cm_code.blank(x, list(ABC=qcv(AA, BB, CC))))
```

```
##   code start end
## 1   AA     0  10
## 2   BB     0  10
## 3   BB    18  19
## 4   CC     0   3
## 5   CC     4   6
## 6  ABC     0   3
## 7  ABC     4   6
```


![plot of chunk unnamed-chunk-96](figure/unnamed-chunk-96.png) 


Next we'll set `overlap = FALSE` and see that it is identical to <a href="http://trinker.github.io/qdap_dev/cm_code.combine.html" target="_blank"><code>cm_code.combine</code></a>.

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/cm_code.blank.html" target="_blank"><code>cm_code.blank</code></a>** - `overlap = FALSE` <font size="5" color="orange">&diams;</font>


```r
(cb2 <- cm_code.blank(x, list(ABC=qcv(AA, BB, CC)), overlap = FALSE))
```

```
##   code start end
## 1   AA     0  10
## 2   BB     0  10
## 3   BB    18  19
## 4   CC     0   3
## 5   CC     4   6
## 6  ABC     0  10
## 7  ABC    18  19
```


![plot of chunk unnamed-chunk-98](figure/unnamed-chunk-98.png) 



By first combining all codes (see `cb2` above) and then excluding the final code by setting
`overlap = 1` the behavior of <a href="http://trinker.github.io/qdap_dev/cm_code.exclude.html" target="_blank"><code>cm_code.exclude</code></a> can be mimicked. 

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/cm_code.blank.html" target="_blank"><code>cm_code.blank</code></a>** - *mimicking <a href="http://trinker.github.io/qdap_dev/cm_code.exclude.html" target="_blank"><code>cm_code.exclude</code></a>* <font size="5" color="orange">&diams;</font>


```r
## Using the output from `cb2` above.
(cb3 <- cm_code.blank(cb2, list(ABnoC=qcv(ABC, CC)), overlap = 1))
```

```
##     code start end
## 1     AA     0  10
## 2     BB     0  10
## 3     BB    18  19
## 4     CC     0   3
## 5     CC     4   6
## 6    ABC     0  10
## 7    ABC    18  19
## 8  ABnoC     3   4
## 9  ABnoC     6  10
## 10 ABnoC    18  19
```


![plot of chunk unnamed-chunk-100](figure/unnamed-chunk-100.png) 


Next we shall find when at least two codes overlap by setting `overlap = ">1"`.

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/cm_code.blank.html" target="_blank"><code>cm_code.blank</code></a>** - *At least 2 codes overlap* <font size="5" color="orange">&diams;</font>



```r
blanklist <- list(AB=qcv(AA, BB), ABC=qcv(AA, BB, CC))
(cb4 <- cm_code.blank(z, blanklist, rm.var = "time", overlap = ">1"))
```

```
##    code start end time
## 1    AA     0  10  foo
## 2    BB     0  10  foo
## 3    BB    18  19  foo
## 4    CC     0   3  foo
## 5    CC     4   6  foo
## 6    AB     0  10  foo
## 7   ABC     0  10  foo
## 8    AA     3   8 foo2
## 9    BB     0   4 foo2
## 10   BB     9  12 foo2
## 11   CC     0   1 foo2
## 12   CC    10  11 foo2
## 13   CC    14  20 foo2
## 14   AB     3   4 foo2
## 15  ABC     0   1 foo2
## 16  ABC     3   4 foo2
## 17  ABC    10  11 foo2
```


![plot of chunk unnamed-chunk-102](figure/unnamed-chunk-102.png) 


Last, we will find spans where not one of the codes occurred by setting `overlap = "==0"`.

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/cm_code.blank.html" target="_blank"><code>cm_code.blank</code></a>** - *Spans where no code occurs* <font size="5" color="orange">&diams;</font>


```r
blanklist2 <- list(noAB=qcv(AA, BB), noABC=qcv(AA, BB, CC))
(cb5 <- cm_code.blank(z, blanklist2, rm.var = "time", overlap = "==0"))
```

```
##     code start end time
## 1     AA     0  10  foo
## 2     BB     0  10  foo
## 3     BB    18  19  foo
## 4     CC     0   3  foo
## 5     CC     4   6  foo
## 6   noAB    10  18  foo
## 7   noAB    19  20  foo
## 8  noABC    10  18  foo
## 9  noABC    19  20  foo
## 10    AA     3   8 foo2
## 11    BB     0   4 foo2
## 12    BB     9  12 foo2
## 13    CC     0   1 foo2
## 14    CC    10  11 foo2
## 15    CC    14  20 foo2
## 16  noAB     8   9 foo2
## 17  noAB    12  21 foo2
## 18 noABC     8   9 foo2
## 19 noABC    12  14 foo2
## 20 noABC    20  21 foo2
```


![plot of chunk unnamed-chunk-104](figure/unnamed-chunk-104.png) 


<h4 id="analysis">Initial Coding Analysis</h4>

The <font color="red">cm_</font> family of functions has three approaches to intial analysis of codes.  The researcher may want to summarize, visualize or determine the proximaty of codes to one another.  The following functions accomplish these tasks:

1. <a href="#cmsum">Summary</a>    
2. <a href="#cmplot">Plotting</a>    
2. <a href="#cmdist">Distance Measures</a>    

<h5 id="cmsum"><font color="green">Summary</font></h5>

Most of the <font color="red">cm_</font> family of functions have a <a href="http://trinker.github.io/qdap_dev/summary.cmspans.html" target="_blank"><code>summary</code></a> method to allows for summaries of codes by group.  Note that these summaries can be wrapped with <a href="http://trinker.github.io/qdap_dev/plot.sum_cmspans.html" target="_blank"><code>plot</code></a> to print a heat map of the table of summaries.

<font size="5" color="orange">&diams;</font> **Example 1: Summarizing Transcript/List Approach** <font size="5" color="orange">&diams;</font>


```r
## Two transcript lists
A <- list(
    person_greg = qcv(terms='7:11, 20:24, 30:33, 49:56'),
    person_researcher = qcv(terms='42:48'),
    person_sally = qcv(terms='25:29, 37:41'),
    person_sam = qcv(terms='1:6, 16:19, 34:36'),
    person_teacher = qcv(terms='12:15'),
    adult_0 = qcv(terms='1:11, 16:41, 49:56'),
    adult_1 = qcv(terms='12:15, 42:48'),
    AA = qcv(terms="1"),
    BB = qcv(terms="1:2, 3:10, 19"),
    CC = qcv(terms="1:9, 100:150")
)

B  <- list(
    person_greg = qcv(terms='7:11, 20:24, 30:33, 49:56'),
    person_researcher = qcv(terms='42:48'),
    person_sally = qcv(terms='25:29, 37:41'),
    person_sam = qcv(terms='1:6, 16:19, 34:36'),
    person_teacher = qcv(terms='12:15'),
    adult_0 = qcv(terms='1:11, 16:41, 49:56'),
    adult_1 = qcv(terms='12:15, 42:48'),
    AA = qcv(terms="40"),
    BB = qcv(terms="50:90"),
    CC = qcv(terms="60:90, 100:120, 150"),
    DD = qcv(terms="")
)

## Long format for transcript/list approach
v <- cm_2long(A, B, v.name = "time")
head(v)
```

```
##                code start end time
## 1       person_greg     6  11    A
## 2       person_greg    19  24    A
## 3       person_greg    29  33    A
## 4       person_greg    48  56    A
## 5 person_researcher    41  48    A
## 6      person_sally    24  29    A
```



```r
## Summary of the data and plotting the summary
summary(v)
```


<pre><code>time              code total percent_total n percent_n  ave min max   mean(sd)
1  a       person_greg    22         12.0% 4     18.2%  5.5   4   8   5.5(1.7)
2  a person_researcher     7          3.8% 1      4.5%  7.0   7   7     7.0(0)
3  a      person_sally    10          5.4% 2      9.1%  5.0   5   5     5.0(0)
4  a        person_sam    13          7.1% 3     13.6%  4.3   3   6   4.3(1.5)
5  a    person_teacher     4          2.2% 1      4.5%  4.0   4   4     4.0(0)
6  a           adult_0    45         24.5% 3     13.6% 15.0   8  26  15.0(9.6)
7  a           adult_1    11          6.0% 2      9.1%  5.5   4   7   5.5(2.1)
8  a                AA     1           .5% 1      4.5%  1.0   1   1     1.0(0)
9  a                BB    11          6.0% 3     13.6%  3.7   1   8   3.7(3.8)
10 a                CC    60         32.6% 2      9.1% 30.0   9  51 30.0(29.7)
11 b       person_greg    22         10.6% 4     19.0%  5.5   4   8   5.5(1.7)
12 b person_researcher     7          3.4% 1      4.8%  7.0   7   7     7.0(0)
13 b      person_sally    10          4.8% 2      9.5%  5.0   5   5     5.0(0)
14 b        person_sam    13          6.3% 3     14.3%  4.3   3   6   4.3(1.5)
15 b    person_teacher     4          1.9% 1      4.8%  4.0   4   4     4.0(0)
16 b           adult_0    45         21.7% 3     14.3% 15.0   8  26  15.0(9.6)
17 b           adult_1    11          5.3% 2      9.5%  5.5   4   7   5.5(2.1)
18 b                AA     1           .5% 1      4.8%  1.0   1   1     1.0(0)
19 b                BB    41         19.8% 1      4.8% 41.0  41  41    41.0(0)
20 b                CC    53         25.6% 3     14.3% 17.7   1  31 17.7(15.3)
============================
Unit of measure: words
</code></pre>



```r
plot(summary(v))
```

![plot of chunk unnamed-chunk-107](figure/unnamed-chunk-1071.png) 

```r
plot(summary(v), facet.vars = "time")
```

![plot of chunk unnamed-chunk-107](figure/unnamed-chunk-1072.png) 



<font size="5" color="orange">&diams;</font> **Example 2: Summarizing Time Spans Approach** <font size="5" color="orange">&diams;</font>


```r
## Single time list
x <- list(
    transcript_time_span = qcv(00:00 - 1:12:00),
    A = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00"),
    B = qcv(terms = "2.40, 3.01:3.02, 5.01, 6.02:7.00,
        9.00, 1.12.00:1.19.01"),
    C = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00, 17.01")
)

## Long format for time span approach
z <-cm_2long(x)
head(z)
```

```
##   code start end    Start      End variable
## 1    A   159 180 00:02:39 00:03:00        x
## 2    A   300 301 00:05:00 00:05:01        x
## 3    A   361 420 00:06:01 00:07:00        x
## 4    A   539 540 00:08:59 00:09:00        x
## 5    B   159 160 00:02:39 00:02:40        x
## 6    B   180 182 00:03:00 00:03:02        x
```



```r
## Summary of the data and plotting the summary
summary(z)
```


<pre><code>  code total percent_total n percent_n  ave min max    mean(sd)
1    A 01:22         12.6% 4     26.7% 20.5   1  59  20.5(27.3)
2    B 08:06         74.7% 6     40.0% 81.0   1 422 81.0(168.6)
3    C 01:23         12.7% 5     33.3% 16.6   1  59  16.6(25.2)
============================
Unit of measure: time
Columns measured in seconds unless in the form hh:mm:ss
</code></pre>


```r
plot(summary(z))
```

![plot of chunk unnamed-chunk-110](figure/unnamed-chunk-110.png) 


<font size="5" color="orange">&diams;</font> **Trouble Shooting Summary: Suppress Measurement Units** <font size="5" color="orange">&diams;</font>


```r
## suppress printing measurement units
suppressMessages(print(summary(z)))
```


<pre><code>  code total percent_total n percent_n  ave min max    mean(sd)
1    A 01:22         12.6% 4     26.7% 20.5   1  59  20.5(27.3)
2    B 08:06         74.7% 6     40.0% 81.0   1 422 81.0(168.6)
3    C 01:23         12.7% 5     33.3% 16.6   1  59  16.6(25.2)
</code></pre>


<font size="5" color="orange">&diams;</font> **Trouble Shooting Summary: Print as Dataframe** <font size="5" color="orange">&diams;</font>


```r
## remove print method
class(z) <- "data.frame"
z
```

```
##    code start  end    Start      End variable
## 1     A   159  180 00:02:39 00:03:00        x
## 2     A   300  301 00:05:00 00:05:01        x
## 3     A   361  420 00:06:01 00:07:00        x
## 4     A   539  540 00:08:59 00:09:00        x
## 5     B   159  160 00:02:39 00:02:40        x
## 6     B   180  182 00:03:00 00:03:02        x
## 7     B   300  301 00:05:00 00:05:01        x
## 8     B   361  420 00:06:01 00:07:00        x
## 9     B   539  540 00:08:59 00:09:00        x
## 10    B  4319 4741 01:11:59 01:19:01        x
## 11    C   159  180 00:02:39 00:03:00        x
## 12    C   300  301 00:05:00 00:05:01        x
## 13    C   361  420 00:06:01 00:07:00        x
## 14    C   539  540 00:08:59 00:09:00        x
## 15    C  1020 1021 00:17:00 00:17:01        x
```



<h5 id="cmplot"><font color="green">Plotting</font></h5>

Like <a href="http://trinker.github.io/qdap_dev/summary.cmspans.html" target="_blank"><code>summary</code></a>, most of the <font color="red">cm_</font> family of functions have a <a href="http://trinker.github.io/qdap_dev/plot.cmspans.html" target="_blank"><code>plot</code></a> method as well that allows a Gantt plot visualization of codes by group.

<font size="5" color="orange">&diams;</font> **Gantt Plot of Transcript/List or Time Spans Data** <font size="5" color="orange">&diams;</font>



```r
## Two transcript lists
A <- list(
    person_greg = qcv(terms='7:11, 20:24, 30:33, 49:56'),
    person_researcher = qcv(terms='42:48'),
    person_sally = qcv(terms='25:29, 37:41'),
    person_sam = qcv(terms='1:6, 16:19, 34:36'),
    person_teacher = qcv(terms='12:15'),
    adult_0 = qcv(terms='1:11, 16:41, 49:56'),
    adult_1 = qcv(terms='12:15, 42:48'),
    AA = qcv(terms="1"),
    BB = qcv(terms="1:2, 3:10, 19"),
    CC = qcv(terms="1:9, 100:150")
)

B  <- list(
    person_greg = qcv(terms='7:11, 20:24, 30:33, 49:56'),
    person_researcher = qcv(terms='42:48'),
    person_sally = qcv(terms='25:29, 37:41'),
    person_sam = qcv(terms='1:6, 16:19, 34:36'),
    person_teacher = qcv(terms='12:15'),
    adult_0 = qcv(terms='1:11, 16:41, 49:56'),
    adult_1 = qcv(terms='12:15, 42:48'),
    AA = qcv(terms="40"),
    BB = qcv(terms="50:90"),
    CC = qcv(terms="60:90, 100:120, 150"),
    DD = qcv(terms="")
)

## Long format
x <- cm_2long(A, v.name = "time")
y <- cm_2long(A, B, v.name = "time")

## cm_code family
combs <- list(sam_n_sally = qcv(person_sam, person_sally))
z <- cm_code.combine(v, combs, "time")
```



```r
plot(x, title = "Single")
```

![plot of chunk unnamed-chunk-114](figure/unnamed-chunk-114.png) 



```r
plot(y, title = "Repeated Measure")
```

![plot of chunk unnamed-chunk-115](figure/unnamed-chunk-1151.png) 

```r
plot(z, title = "Combined Codes")
```

![plot of chunk unnamed-chunk-115](figure/unnamed-chunk-1152.png) 


<h5 id="cmdist"><font color="green">Distance Measures</font></h5>

Often a research will want to know which codes are clustering closer to other codes (regardless of whether the codes represent word or time spans).  <a href="http://trinker.github.io/qdap_dev/cm_distance.html" target="_blank"><code>cm_distance</code></a> allows the research to find the distances between codes and standardize the mean of the differences to allow for comparisons similar to a correlation.  The matrix output from <a href="http://trinker.github.io/qdap_dev/cm_distance.html" target="_blank"><code>cm_distance</code></a> is arrived at by taking the means and standard deviations of the differences between codes and scaling them (without centering) and then multiplying the two together.  This results in a standarized distance measure that is non-negative, with values closer to zero indicating a codes that are found in closer proximaty.  

The researcher may also access the means, standard deviations and number of codes by indexing the list output for each transcript.  This distance measure compliments the Gantt plot.  

Note that the argument <b><font color="green" face="courier new">causal = FALSE</font></b> (the defualt) does not assume Code A comes before Code B whereas <b><font color="green" face="courier new">causal = TRUE</font></b> assumes the first code precedes the second code.  Generally, setting <b><font color="green" face="courier new">causal = FALSE</font></b> wil result in larger mean of differences and accompanying standardized values.  Also note that rownames are the first code and column names are the second comparison code.  The values for Code A compared to Code B will not be the same as Code B compared to Code A.  This is because, unlike a true distance measure, <a href="http://trinker.github.io/qdap_dev/cm_distance.html" target="_blank"><code>cm_distance</code></a>'s matrix is assymetrical.  <a href="http://trinker.github.io/qdap_dev/cm_distance.html" target="_blank"><code>cm_distance</code></a>computes the distance by taking each span (start and end) for Code A and comparing it to the nearest start or end for Code B.  So for example there may be 6 Code A spans and thus six differences between A and B, whereas Code B may only have 3 spans and thus three differences between B and A.  This fact alone will lead to differences in A compared to B versus B compared to A.  


<font size="5" color="orange">&diams;</font> <a href="http://trinker.github.io/qdap_dev/cm_distance.html" target="_blank"><code>cm_distance</code></a> - *Initial Data Setup* <font size="5" color="orange">&diams;</font>



```r
x <- list(
    transcript_time_span = qcv(00:00 - 1:12:00),
    A = qcv(terms = "2.40:3.00, 6.32:7.00, 9.00,
        10.00:11.00, 33.23:40.00, 59.56"),
    B = qcv(terms = "3.01:3.02, 5.01,  19.00, 1.12.00:1.19.01"),
    C = qcv(terms = "2.40:3.00, 5.01, 6.32:7.00, 9.00, 17.01, 38.09:40.00")
)
y <- list(
    transcript_time_span = qcv(00:00 - 1:12:00),
    A = qcv(terms = "2.40:3.00, 6.32:7.00, 9.00,
        10.00:11.00, 23.44:25.00, 59.56"),
    B = qcv(terms = "3.01:3.02, 5.01, 7.05:8.00 19.30, 1.12.00:1.19.01"),
    C = qcv(terms = "2.40:3.00, 5.01, 6.32:7.30, 9.00, 17.01, 25.09:27.00")
)

## Long format
dat <- cm_2long(x, y)
```


![plot of chunk unnamed-chunk-117](figure/unnamed-chunk-117.png) 


<font size="5" color="orange">&diams;</font> <a href="http://trinker.github.io/qdap_dev/cm_distance.html" target="_blank"><code>cm_distance</code></a> - *Non-Causal Distance* <font size="5" color="orange">&diams;</font>


```r
## a cm_distance output
(out1 <- cm_distance(dat, time.var = "variable"))
```


<pre><code>x

standardized:
     A    B    C
A 0.00 1.04 0.82
B 0.88 0.00 3.89
C 0.09 0.95 0.00


y

standardized:
     A    B    C
A 0.00 0.38 1.97
B 0.47 0.00 4.94
C 0.08 0.09 0.00
</code></pre>


```r
## The elements available from the output
names(out1)
```


<pre><code>[1] "x" "y"
</code></pre>


```r
## A list containing means, standard deviations and other 
## descriptive statistics for for the differences between codes
out1$x
```


<pre><code>$mean
       A      B      C
A   0.00 367.67 208.67
B 322.50   0.00 509.00
C  74.67 265.00   0.00

$sd
       A      B      C
A   0.00 347.51 483.27
B 337.47   0.00 940.94
C 143.77 440.92   0.00

$n
  A B C
A 6 6 6
B 4 4 4
C 6 6 6

$combined
  A                B                 C                
A n=6              367.67(347.51)n=6 208.67(483.27)n=6
B 322.5(337.47)n=4 n=4               509(940.94)n=4   
C 74.67(143.77)n=6 265(440.92)n=6    n=6              

$standardized
     A    B    C
A 0.00 1.04 0.82
B 0.88 0.00 3.89
C 0.09 0.95 0.00
</code></pre>

<font size="5" color="orange">&diams;</font> <a href="http://trinker.github.io/qdap_dev/cm_distance.html" target="_blank"><code>cm_distance</code></a> - *Causal Distance* <font size="5" color="orange">&diams;</font>


```r
## a cm_distance output `causal = TRUE`
cm_distance(dat, time.var = "variable", causal = TRUE)
```


<pre><code>x

standardized:
     A    B    C
A 0.66 0.84 0.08
B 0.29 3.96 0.49
C 0.40 0.86 0.37


y

standardized:
     A    B    C
A 1.11 1.63 0.08
B 0.03 2.95 0.04
C 0.70 1.27 0.11
</code></pre>

<h3 id="counts">Word Counts and Descriptive Statistics</h3>


<div class="funs">
The following functions will be utilized in this section (click to view more):    

<form action="http://trinker.github.io/qdap_dev/dist_tab.html" target="_blank">
    <input type="submit" value="dist_tab"> - <a href="#freqtab">SPSS Style Frequency Tables</a>
</form>

<form action="http://trinker.github.io/qdap_dev/pos.html" target="_blank">
    <input type="submit" value="pos"><input type="submit" value="pos_by"><input type="submit" value="pos_tags"> - <a href="#pos">Parts of Speech Tagging & Counts</a>
</form>

<form action="http://trinker.github.io/qdap_dev/question_type.html" target="_blank">
    <input type="submit" value="question_type"> - <a href="#quest">Question Type Counts</a>
</form>

<form action="http://trinker.github.io/qdap_dev/syllable_sum.html" target="_blank">
    <input type="submit" value="syllable_sum"><input type="submit" value="combo_syllable_sum"><input type="submit" value="polysyllable_sum"><input type="submit" value="syllable_count"> - <a href="#syll">Syllabication and Counts</a>
</form>

<form action="http://trinker.github.io/qdap_dev/tdm.html" target="_blank">
    <input type="submit" value="tdm"><input type="submit" value="dtm"> - <a href="#tdm">Convert/Generate Term Document Matrix or Document Term Matrix</a>
</form>

<form class="form_left" action="http://trinker.github.io/qdap_dev/termco.html" target="_blank">
    <input type="submit" value="termco"><input type="submit" value="term_match"><input type="submit" value="termco_d"><input type="submit" value="termco2mat">
</form>

<form class="form_left" action="http://trinker.github.io/qdap_dev/termco_c.html" target="_blank">
    <input type="submit" value="termco_c"> 
</form>

<form action="http://trinker.github.io/qdap_dev/spaste.html" target="_blank">
    <input type="submit" value="spaste"> - <a href="#termco">Search For and Count Terms</a>
</form>

<form action="http://trinker.github.io/qdap_dev/wfm.html" target="_blank">
    <input type="submit" value="wfm"><input type="submit" value="wfdf"><input type="submit" value="wf_combine"><input type="submit" value="wfm_expanded"> - <a href="#wfm">Word Frequency Matrix</a>
</form>

<form class="form_left" action="http://trinker.github.io/qdap_dev/word_count.html" target="_blank">
    <input type="submit" value="word_count"> 
</form>

<form class="form_left" action="http://trinker.github.io/qdap_dev/word_count.html" target="_blank">
    <input type="submit" value="character_count"><input type="submit" value="character_table"> 
</form>

<form action="http://trinker.github.io/qdap_dev/word_list.html" target="_blank">
    <input type="submit" value="word_list">  - <a href="#wordcount">Word & Character Counts</a>
</form>

<form action="http://trinker.github.io/qdap_dev/word_stats.html" target="_blank">
    <input type="submit" value="word_stats"> - <a href="#wordstats">Descriptive Word Statistics</a>
</form>

</div>

A researcher often needs to quickly gather frequency counts for various words/word types.  qdap offers multiple functions designed to efficiently generate descriptive word statistics by any combination of grouping variables.  Many of the functions also offer proportional usage to more fairly compare between groups.  Additionally, many functions also have plotting methods to better visualize the data that is transformed.

<h4 id="wordstats">Descriptive Word Statistics</h4>

Often a researcher may want to get a general sense of how words are functioning for different grouping variables.  The <a href="http://trinker.github.io/qdap_dev/word_stats.html" target="_blank"><code>word_stats</code></a> function enables a quick picture of what is occuring within the data.  The displayed (printed) output is a dataframe, however, the output from word_stats is actually a list.  Use <a href="http://trinker.github.io/qdap_dev/word_stats.html" target="_blank"><code>?word_stats</code></a> to learn more.

The displayed output is a wide dataframe, hence the abbreviated column names.  The following column names and meanings will provide guidance in understanding the output:

<h5 id="wordstats"><a href="http://trinker.github.io/qdap_dev/word_stats.html" target="_blank"><code>word_stats</code></a> Column Names</h5>

- n.tot - number of turns of talk   
- n.sent - number of sentences   
- n.words - number of words   
- n.char - number of characters   
- n.syl - number of syllables   
- n.poly - number of polysyllables   
- sptot - syllables per turn of talk   
- wptot - words per turn of talk   
- wps - words per sentence   
- cps - characters per sentence   
- sps - syllables per sentence   
- psps - poly-syllables per sentence   
- cpw - characters per word   
- spw - syllables per word   
- n.state - number of statements   
- n.quest - number of questions   
- n.exclm - number of exclamations   
- n.incom - number of incomplete statements   
- p.state - proportion of statements   
- p.quest - proportion of questions   
- p.exclm - proportion of exclamations   
- p.incom - proportion of incomplete statements   
- n.hapax - number of hapax legomenon   
- n.dis - number of dis legomenon   
- grow.rate - proportion of hapax legomenon to words   
- prop.dis - proportion of dis legomenon to words

<div class="middleDiv">
<b><font size="4" color="red">It is assumed you have run <font face="courier">sentSplit</font> on the data.<br>If this is not the case the counts will not be accurate.</font></b>
</div>


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/word_stats.html" target="_blank"><code>word_stats</code></a> Example** <font size="5" color="orange">&diams;</font>

Note that the initial output is broken into three dataframe outputs because of the width of printed output from <a href="http://trinker.github.io/qdap_dev/word_stats.html" target="_blank"><code>word_stats</code></a> being so large.  The user will see that these three dataframes are actually one wide dataframe in the R output.


```r
(desc_wrds <- with(mraja1spl, word_stats(dialogue, person, tot = tot)))
```






```
           person n.tot n.sent n.words n.char n.syl n.poly sptot wptot
1           Romeo    49    113    1163   4757  1441     48   2.3  23.7
2        Benvolio    34     51     621   2563   780     25   1.5  18.3
3           Nurse    20     59     599   2274   724     20   3.0  29.9
4         Sampson    20     28     259    912   294      7   1.4  12.9
5          Juliet    16     24     206    789   238      5   1.5  12.9
6         Gregory    15     20     149    553   166      1   1.3   9.9
7         Capulet    14     72     736   2900   902     35   5.1  52.6
8    Lady Capulet    12     27     288   1205   370     10   2.2  24.0
9        Mercutio    11     24     549   2355   704     29   2.2  49.9
10        Servant    10     19     184    725   226      5   1.9  18.4
11         Tybalt     8     17     160    660   207      9   2.1  20.0
12       Montague     6     13     217    919   284     13   2.2  36.2
13        Abraham     5      6      24     79    26      0   1.2   4.8
14  First Servant     3      7      69    294    87      2   2.3  23.0
15 Second Servant     3      4      41    160    49      0   1.3  13.7
16  Lady Montague     2      4      28     88    30      0   2.0  14.0
17          Paris     2      3      32    124    41      2   1.5  16.0
18 Second Capulet     2      2      17     64    21      0   1.0   8.5
19         Prince     1      9     167    780   228     17   9.0 167.0
20  First Citizen     1      5      16     79    22      3   5.0  16.0
```



```
           person  wps  cps  sps psps cpw spw pspw n.state n.quest n.exclm
1           Romeo 10.3 42.1 12.8  0.4 4.1 1.2  0.0      69      22      22
2        Benvolio 12.2 50.3 15.3  0.5 4.1 1.3  0.0      39       8       4
3           Nurse 10.2 38.5 12.3  0.3 3.8 1.2  0.0      37       9      13
4         Sampson  9.2 32.6 10.5  0.2 3.5 1.1  0.0      27       1       0
5          Juliet  8.6 32.9  9.9  0.2 3.8 1.2  0.0      16       5       3
6         Gregory  7.5 27.6  8.3  0.0 3.7 1.1  0.0      14       3       3
7         Capulet 10.2 40.3 12.5  0.5 3.9 1.2  0.0      40      10      22
8    Lady Capulet 10.7 44.6 13.7  0.4 4.2 1.3  0.0      20       6       1
9        Mercutio 22.9 98.1 29.3  1.2 4.3 1.3  0.1      20       2       2
10        Servant  9.7 38.2 11.9  0.3 3.9 1.2  0.0      14       2       3
11         Tybalt  9.4 38.8 12.2  0.5 4.1 1.3  0.1      13       2       2
12       Montague 16.7 70.7 21.8  1.0 4.2 1.3  0.1      11       2       0
13        Abraham  4.0 13.2  4.3  0.0 3.3 1.1  0.0       3       2       1
14  First Servant  9.9 42.0 12.4  0.3 4.3 1.3  0.0       3       2       2
15 Second Servant 10.2 40.0 12.2  0.0 3.9 1.2  0.0       4       0       0
16  Lady Montague  7.0 22.0  7.5  0.0 3.1 1.1  0.0       2       2       0
17          Paris 10.7 41.3 13.7  0.7 3.9 1.3  0.1       2       1       0
18 Second Capulet  8.5 32.0 10.5  0.0 3.8 1.2  0.0       2       0       0
19         Prince 18.6 86.7 25.3  1.9 4.7 1.4  0.1       7       1       1
20  First Citizen  3.2 15.8  4.4  0.6 4.9 1.4  0.2       0       0       5
```



```
           person p.state p.quest p.exclm n.hapax n.dis grow.rate prop.dis
1           Romeo     0.6     0.2     0.2     365    84       0.3      0.1
2        Benvolio     0.8     0.2     0.1     252    43       0.4      0.1
3           Nurse     0.6     0.2     0.2     147    48       0.2      0.1
4         Sampson     1.0     0.0     0.0      81    22       0.3      0.1
5          Juliet     0.7     0.2     0.1      94    22       0.5      0.1
6         Gregory     0.7     0.2     0.2      72    17       0.5      0.1
7         Capulet     0.6     0.1     0.3     232    46       0.3      0.1
8    Lady Capulet     0.7     0.2     0.0     135    28       0.5      0.1
9        Mercutio     0.8     0.1     0.1     253    28       0.5      0.1
10        Servant     0.7     0.1     0.2      71    19       0.4      0.1
11         Tybalt     0.8     0.1     0.1      79    17       0.5      0.1
12       Montague     0.8     0.2     0.0     117    21       0.5      0.1
13        Abraham     0.5     0.3     0.2       3     7       0.1      0.3
14  First Servant     0.4     0.3     0.3      33     8       0.5      0.1
15 Second Servant     1.0     0.0     0.0      32     3       0.8      0.1
16  Lady Montague     0.5     0.5     0.0      24     2       0.9      0.1
17          Paris     0.7     0.3     0.0      25     2       0.8      0.1
18 Second Capulet     1.0     0.0     0.0       7     5       0.4      0.3
19         Prince     0.8     0.1     0.1      83    15       0.5      0.1
20  First Citizen     0.0     0.0     1.0       9     2       0.6      0.1
```



```r
## The following shows all the available elements in the `word_stats` output
names(desc_wrds)
```

```
## [1] "ts"        "gts"       "mpun"      "word.elem" "sent.elem" "omit"     
## [7] "digits"
```


<a href="http://trinker.github.io/qdap_dev/word_stats.html" target="_blank"><code>word_stats</code></a> has a plot method that plots the output as a heat map.  This can be useful for finding high/low elements in the data set.

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/word_stats.html" target="_blank"><code>word_stats</code></a> Plot** <font size="5" color="orange">&diams;</font>



```r
plot(desc_wrds)
```

![plot of chunk unnamed-chunk-128](figure/unnamed-chunk-128.png) 



```r
plot(desc_wrds, label=TRUE, lab.digits = 1)
```

![plot of chunk unnamed-chunk-129](figure/unnamed-chunk-129.png) 


It takes considerable time to run <a href="http://trinker.github.io/qdap_dev/word_stats.html" target="_blank"><code>word_stats</code></a> because it is calculating syllable counts.  The user may re-use the object output from one run and bass this as the text variable (`text.var`) in a subsequent run with different grouping variables (`grouping.vars`) as long as the text variable has not changed.  The example below demonstrates how to re-use the output from one <a href="http://trinker.github.io/qdap_dev/word_stats.html" target="_blank"><code>word_stats</code></a> run in another run.

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/word_stats.html" target="_blank"><code>word_stats</code></a> Re-use** <font size="5" color="orange">&diams;</font>



```r
with(mraja1spl, word_stats(desc_wrds, list(sex, fam.aff, died), tot = tot))
```



<h4 id="wfm">Word Frequency Matrix</h4>

Many analyses with words involve a matrix based on the words.  qdap uses a *word frequency matrix* (<a href="http://trinker.github.io/qdap_dev/wfm.html" target="_blank"><code>wfm</code></a>) or the less maliable dataframe version, *word frequency dataframe* (<a href="http://trinker.github.io/qdap_dev/wfdf.html" target="_blank"><code>wfdf</code></a>).  The <a href="http://trinker.github.io/qdap_dev/wfm.html" target="_blank"><code>wfm</code></a> is a count of word usages per grouping variable(s).  This is a similar concept to the <a href="http://cran.r-project.org/web/packages/tm/index.html">tm packae's</a> Term Document Matrix, though instead of documents we are interested in the grouping variable's usage of terms.  <a href="http://trinker.github.io/qdap_dev/wfm.html" target="_blank"><code>wfm</code></a> is the general function that should be used, however, the <a href="http://trinker.github.io/qdap_dev/wfdf.html" target="_blank"><code>wfdf</code></a> function does provide options for margin sums (row and column).  Also note that the <a href="http://trinker.github.io/qdap_dev/wfm_expanded.html" target="_blank"><code>wfm_expanded</code></a> and <a href="http://trinker.github.io/qdap_dev/wfm_combine.html" target="_blank"><code>wfm_combine</code></a> can expand or combine terms within a word frequency matrix.

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/wfm.html" target="_blank"><code>wfm</code></a> Examples** <font size="5" color="orange">&diams;</font>


```r
## By a single grouping variable
with(DATA, wfm(state, person))[1:15, ]
```

```
##          greg researcher sally sam teacher
## about       0          0     1   0       0
## already     1          0     0   0       0
## am          1          0     0   0       0
## are         0          0     1   0       0
## be          0          0     1   0       0
## can         0          0     1   0       0
## certain     0          0     1   0       0
## computer    0          0     0   1       0
## distrust    0          0     0   1       0
## do          0          0     0   0       1
## dumb        1          0     0   0       0
## eat         1          0     0   0       0
## fun         0          0     0   2       0
## good        0          1     0   0       0
## how         0          0     1   0       0
```

```r
## By two grouping variables
with(DATA, wfm(state, list(sex, adult)))[1:15, ]
```

```
##          f.0 f.1 m.0 m.1
## about      1   0   0   0
## already    0   0   1   0
## am         0   0   1   0
## are        1   0   0   0
## be         1   0   0   0
## can        1   0   0   0
## certain    1   0   0   0
## computer   0   0   1   0
## distrust   0   0   1   0
## do         0   0   0   1
## dumb       0   0   1   0
## eat        0   0   1   0
## fun        0   0   2   0
## good       0   1   0   0
## how        1   0   0   0
```


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/wfm.html" target="_blank"><code>wfm</code></a>: Keep Two Word Phrase as a Single Term** <font size="5" color="orange">&diams;</font>


```r
## insert double tilde ("~~") to keep phrases(e. g., first last name)
space_keeps <- c(" fun", "I ")
state2 <- space_fill(DATA$state, space_keeps, rm.extra = FALSE)
with(DATA, wfm(state2, list(sex, adult)))[1:18, ]
```

```
##            f.0 f.1 m.0 m.1
## about        1   0   0   0
## already      0   0   1   0
## are          1   0   0   0
## be           1   0   0   0
## can          1   0   0   0
## certain      1   0   0   0
## computer     0   0   1   0
## do           0   0   0   1
## dumb         0   0   1   0
## eat          0   0   1   0
## good         0   1   0   0
## how          1   0   0   0
## hungry       0   0   1   0
## i'm          0   0   1   0
## i am         0   0   1   0
## i distrust   0   0   1   0
## is           0   0   1   0
## is fun       0   0   1   0
```


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/wfdf.html" target="_blank"><code>wfdf</code></a> Examples**: *Add Margins* <font size="5" color="orange">&diams;</font>


```r
with(DATA, wfdf(state, person, margins = TRUE))[c(1:15, 41:42), ]
```

```
##             Words greg researcher sally sam teacher TOTAL.USES
## 1           about    0          0     1   0       0          1
## 2         already    1          0     0   0       0          1
## 3              am    1          0     0   0       0          1
## 4             are    0          0     1   0       0          1
## 5              be    0          0     1   0       0          1
## 6             can    0          0     1   0       0          1
## 7         certain    0          0     1   0       0          1
## 8        computer    0          0     0   1       0          1
## 9        distrust    0          0     0   1       0          1
## 10             do    0          0     0   0       1          1
## 11           dumb    1          0     0   0       0          1
## 12            eat    1          0     0   0       0          1
## 13            fun    0          0     0   2       0          2
## 14           good    0          1     0   0       0          1
## 15            how    0          0     1   0       0          1
## 41            you    1          0     1   2       0          4
## 42 TOTAL.WORDS ->   20          6    10  13       4         53
```

```r
with(DATA, wfdf(state, list(sex, adult), margins = TRUE))[c(1:15, 41:42), ]
```

```
##             Words f.0 f.1 m.0 m.1 TOTAL.USES
## 1           about   1   0   0   0          1
## 2         already   0   0   1   0          1
## 3              am   0   0   1   0          1
## 4             are   1   0   0   0          1
## 5              be   1   0   0   0          1
## 6             can   1   0   0   0          1
## 7         certain   1   0   0   0          1
## 8        computer   0   0   1   0          1
## 9        distrust   0   0   1   0          1
## 10             do   0   0   0   1          1
## 11           dumb   0   0   1   0          1
## 12            eat   0   0   1   0          1
## 13            fun   0   0   2   0          2
## 14           good   0   1   0   0          1
## 15            how   1   0   0   0          1
## 41            you   1   0   3   0          4
## 42 TOTAL.WORDS ->  10   6  33   4         53
```


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/wfm_expanded.html" target="_blank"><code>wfm_expanded</code></a>: Expand the wfm** <font size="5" color="orange">&diams;</font>


```r
## Start with a word frequency matrix
z <- wfm(DATA$state, DATA$person)
## Note a single `you`
z[30:41, ]
```

```
##         greg researcher sally sam teacher
## stinks     0          0     0   1       0
## talking    0          0     1   0       0
## telling    1          0     0   0       0
## the        1          0     0   0       0
## then       0          1     0   0       0
## there      1          0     0   0       0
## too        0          0     0   1       0
## truth      1          0     0   0       0
## way        1          0     0   0       0
## we         0          1     1   0       1
## what       0          0     1   0       1
## you        1          0     1   2       0
```

```r
## Note that there are two `you`s in the expanded version
wfm_expanded(z)[33:45, ] 
```

```
##         greg researcher sally sam teacher
## stinks     0          0     0   1       0
## talking    0          0     1   0       0
## telling    1          0     0   0       0
## the        1          0     0   0       0
## then       0          1     0   0       0
## there      1          0     0   0       0
## too        0          0     0   1       0
## truth      1          0     0   0       0
## way        1          0     0   0       0
## we         0          1     1   0       1
## what       0          0     1   0       1
## you        1          0     1   1       0
## you        0          0     0   1       0
```


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/wfm_combine.html" target="_blank"><code>wfm_combine</code></a>: Combine Terms in the wfm** <font size="5" color="orange">&diams;</font>


```r
## Start with a word frequency matrix
x <- wfm(DATA$state, DATA$person)

## The terms to exclude
WL <- list(
    random = c("the", "fun", "i"), 
    yous = c("you", "your", "you're")
)

## Combine the terms
(out <- wfm_combine(x, WL))
```

```
##            greg researcher sally sam teacher
## random        2          0     0   3       0
## yous          1          0     1   2       0
## else.words   17          6     9   8       4
```

```r
## Pass the combined version to Chi Squared Test
chisq.test(out)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  out
## X-squared = 7.661, df = 8, p-value = 0.4673
```


<h5 id="tdm"><font color="green">Convert/Generate Term Document Matrix or Document Term Matrix</font></h5>

Some packages that could further the analysis of qdap expect a Document Term or Term Docuement Matrix.  qdap's <a href="http://trinker.github.io/qdap_dev/wfm.html" target="_blank"><code>wfm</code></a> is similar to the <a href="http://cran.r-project.org/web/packages/tm/index.html">tm packae's</a> <a href="http://www.inside-r.org/packages/cran/tm/docs/DocumentTermMatrix">TermDocumentMatrix</a>     and <a href="http://www.inside-r.org/packages/cran/tm/docs/DocumentTermMatrix">DocumentTermMatrix</a>.  qdap does not try to replicate the extensive work of the<a href="http://cran.r-project.org/web/packages/tm/index.html">tm</a> package, however, the <a href="http://trinker.github.io/qdap_dev/tdm.html" target="_blank"><code>tdm</code></a> and <a href="http://trinker.github.io/qdap_dev/tdm.html" target="_blank"><code>dtm</code></a> do attempt to extend the work the researcher conducts in qdap to be utilized in other R packages.

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/tdm.html" target="_blank"><code>tdm</code></a> Use** <font size="5" color="orange">&diams;</font>



```r
x <- wfm(DATA$state, DATA$person)
## Term Document Matrix
tdm(x)
```

```
##           Docs
## Terms      greg researcher sally sam teacher
##   about       0          0     1   0       0
##   already     1          0     0   0       0
##   am          1          0     0   0       0
##   are         0          0     1   0       0
##   be          0          0     1   0       0
##   can         0          0     1   0       0
##   certain     0          0     1   0       0
##   computer    0          0     0   1       0
##   distrust    0          0     0   1       0
##   do          0          0     0   0       1
##   dumb        1          0     0   0       0
##   eat         1          0     0   0       0
##   fun         0          0     0   2       0
##   good        0          1     0   0       0
##   how         0          0     1   0       0
##   hungry      1          0     0   0       0
##   i           1          0     0   1       0
##   i'm         1          0     0   0       0
##   is          1          0     0   1       0
##   it          0          0     0   1       0
##   it's        2          0     0   0       0
##   let's       1          0     0   0       0
##   liar        0          0     0   1       0
##   move        0          1     0   0       0
##   no          2          0     0   0       0
##   not         1          0     0   1       0
##   on          0          1     0   0       0
##   shall       0          1     0   0       0
##   should      0          0     0   0       1
##   stinks      0          0     0   1       0
##   talking     0          0     1   0       0
##   telling     1          0     0   0       0
##   the         1          0     0   0       0
##   then        0          1     0   0       0
##   there       1          0     0   0       0
##   too         0          0     0   1       0
##   truth       1          0     0   0       0
##   way         1          0     0   0       0
##   we          0          1     1   0       1
##   what        0          0     1   0       1
##   you         1          0     1   2       0
```

```r
## Document Term Matrix
dtm(x)
```

```
##             Terms
## Docs         about already am are be can certain computer distrust do dumb
##   greg           0       1  1   0  0   0       0        0        0  0    1
##   researcher     0       0  0   0  0   0       0        0        0  0    0
##   sally          1       0  0   1  1   1       1        0        0  0    0
##   sam            0       0  0   0  0   0       0        1        1  0    0
##   teacher        0       0  0   0  0   0       0        0        0  1    0
##             Terms
## Docs         eat fun good how hungry i i'm is it it's let's liar move no
##   greg         1   0    0   0      1 1   1  1  0    2     1    0    0  2
##   researcher   0   0    1   0      0 0   0  0  0    0     0    0    1  0
##   sally        0   0    0   1      0 0   0  0  0    0     0    0    0  0
##   sam          0   2    0   0      0 1   0  1  1    0     0    1    0  0
##   teacher      0   0    0   0      0 0   0  0  0    0     0    0    0  0
##             Terms
## Docs         not on shall should stinks talking telling the then there too
##   greg         1  0     0      0      0       0       1   1    0     1   0
##   researcher   0  1     1      0      0       0       0   0    1     0   0
##   sally        0  0     0      0      0       1       0   0    0     0   0
##   sam          1  0     0      0      1       0       0   0    0     0   1
##   teacher      0  0     0      1      0       0       0   0    0     0   0
##             Terms
## Docs         truth way we what you
##   greg           1   1  0    0   1
##   researcher     0   0  1    0   0
##   sally          0   0  1    1   1
##   sam            0   0  0    0   2
##   teacher        0   0  1    1   0
```



```r
## Run Latant Semantic Analysis
library(lsa)
lsa(tdm(x), dims=dimcalc_share())
```



<pre><code>$tk
                 [,1]         [,2]
about    -0.021153126  0.072269368
already  -0.169239530 -0.124825133
am       -0.169239530 -0.124825133
are      -0.021153126  0.072269368
be       -0.021153126  0.072269368
can      -0.021153126  0.072269368
certain  -0.021153126  0.072269368
computer -0.090637878  0.215786300
distrust -0.090637878  0.215786300
do       -0.001903917  0.014326564
dumb     -0.169239530 -0.124825133
eat      -0.169239530 -0.124825133
fun      -0.181275756  0.431572601
good     -0.001108363  0.009865681
how      -0.021153126  0.072269368
hungry   -0.169239530 -0.124825133
i        -0.259877408  0.090961168
i'm      -0.169239530 -0.124825133
is       -0.259877408  0.090961168
it       -0.090637878  0.215786300
it's     -0.338479060 -0.249650265
let's    -0.169239530 -0.124825133
liar     -0.090637878  0.215786300
move     -0.001108363  0.009865681
no       -0.338479060 -0.249650265
not      -0.259877408  0.090961168
on       -0.001108363  0.009865681
shall    -0.001108363  0.009865681
should   -0.001903917  0.014326564
stinks   -0.090637878  0.215786300
talking  -0.021153126  0.072269368
telling  -0.169239530 -0.124825133
the      -0.169239530 -0.124825133
then     -0.001108363  0.009865681
there    -0.169239530 -0.124825133
too      -0.090637878  0.215786300
truth    -0.169239530 -0.124825133
way      -0.169239530 -0.124825133
we       -0.024165406  0.096461613
what     -0.023057043  0.086595932
you      -0.371668412  0.379016836

$dk
                   [,1]        [,2]
greg       -0.876176894 -0.47984657
researcher -0.005738152  0.03792516
sally      -0.109512712  0.27781431
sam        -0.469245067  0.82951496
teacher    -0.009856846  0.05507346

$sk
[1] 5.177141 3.844150

attr(,"class")
[1] "LSAspace"
</code></pre>


<h4 id="termco">Search For and Count Terms</h4>

The <a href="http://trinker.github.io/qdap_dev/termco.html" target="_blank"><code>termco</code></a> family of functions are some of the most useful qdap functions for quantative discourse analysis.  <a href="http://trinker.github.io/qdap_dev/termco.html" target="_blank"><code>termco</code></a> searches for (an optionally groups) terms and outputs a raw count, percent, and combined (raw/percent) matrix of term counts by grouping variable.  The <a href="http://trinker.github.io/qdap_dev/term_match.html" target="_blank"><code>term_match</code></a> <a href="http://trinker.github.io/qdap_dev/all_words.html" target="_blank"><code>all_words</code></a> <a href="http://trinker.github.io/qdap_dev/synonyms.html" target="_blank"><code>syn</code></a>, <a href="http://trinker.github.io/qdap_dev/exclude.html" target="_blank"><code>exclude</code></a>, and <a href="http://trinker.github.io/qdap_dev/spaste.html" target="_blank"><code>spaste</code></a> are complementary  functions that are useful in developing word lists to provide to the <b><font color="green" face="courier new">match.list</font></b>.  

The <b><font color="green" face="courier new">match.list</font></b> acts to search for similarly grouped <em>themes</em>.  For example <font color="green" face="courier new">c(" read ", " reads", " reading", " reader")</font> may be a search for words associated with reading.  It is good practice to name the vectors of words that are stored in the <b><font color="green" face="courier new">match.list</font></b> .  This is the general form for how to set up a <b><font color="green" face="courier new">match.list</font></b>:


```r
themes <- list(
    theme_1 = c(),
    theme_2 = c(),
    theme_n = c()
)
```


It is important to understand how the <b><font color="green" face="courier new">match.list</font></b> is handled by <a href="http://trinker.github.io/qdap_dev/termco.html" target="_blank"><code>termco</code></a>.  The <b><font color="green" face="courier new">match.list</font></b> is (optionally) case and character sensitive. Spacing is an important way to grab specific words and requires careful thought. For example using <font color="green">"read"</font> will find the words <font color="green">"bread"</font>, <font color="green">"read"</font>, <font color="green">"reading"</font>, and <font color="green">"ready"</font>. If you want to search for just the word <font color="purple">"read"</font> supply a vector of <font color="green" face="courier new">c(" read ", " reads", " reading", " reader")</font>. Notice the leading and trailing spaces.  A space acts as a boundary where as starting/ending with a nonspace allows for greedy matching that will find words that contain this term.  A leading, trailing or both may be used to control how <a href="http://trinker.github.io/qdap_dev/termco.html" target="_blank"><code>termco</code></a> searches for the supplied terms.  So the reader may ask why not supply one string spaced as <font color="green">" read"</font>?  Keep in mind that <a href="http://trinker.github.io/qdap_dev/termco.html" target="_blank"><code>termco</code></a> would also find the word <font color="purple">"ready"</font>

This section's examples will first view the complementary  functions that augment the *themes* supplied to <b><font color="green" face="courier new">match.list</font></b> and then main <a href="http://trinker.github.io/qdap_dev/termco.html" target="_blank"><code>termco</code></a> function will be explored.


<a href="http://trinker.github.io/qdap_dev/term_match.html" target="_blank"><code>term_match</code></a> looks through a text variable (usually the text found in the transcript) and finds/returns a vector of words containing a term(s).

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/term_match.html" target="_blank"><code>term_match</code></a> and  <a href="http://trinker.github.io/qdap_dev/exclude.html" target="_blank"><code>exclude</code></a> Examples**<font size="5" color="orange">&diams;</font>


```r
term_match(text.var = DATA$state, terms = qcv(the, trust), return.list = FALSE)
```

```
## [1] "distrust" "the"      "then"     "there"
```

```r
term_match(DATA$state, "i", FALSE)
```

```
##  [1] "certain"  "distrust" "i"        "i'm"      "is"       "it"      
##  [7] "it's"     "liar"     "stinks"   "talking"  "telling"
```

```r
exclude(term_match(DATA$state, "i", FALSE), talking, telling)
```

```
## [1] "certain"  "distrust" "i"        "i'm"      "is"       "it"      
## [7] "it's"     "liar"     "stinks"
```


<a href="http://trinker.github.io/qdap_dev/all_words.html" target="_blank"><code>all_words</code></a> is similar to <a href="http://trinker.github.io/qdap_dev/term_match.html" target="_blank"><code>term_match</code></a>, however, the function looks at all the words found in a text variable (usually the transcript text) and returns words that begin with or contain the term(s).  The output can be arrange alphabetically or by frequency.  The output is a dataframe which helps the researcher to make decisions with regard to frequency of word use. 

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/all_words.html" target="_blank"><code>all_words</code></a> Examples**<font size="5" color="orange">&diams;</font>


```r
x1 <- all_words(raj$dialogue, begins.with="re")
head(x1, 10)
```

```
##    WORD       FREQ
## 1  re            2
## 2  reach         1
## 3  read          6
## 4  ready         5
## 5  rearward      1
## 6  reason        5
## 7  reason's      1
## 8  rebeck        1
## 9  rebellious    1
## 10 receipt       1
```

```r
all_words(raj$dialogue, begins.with="q")
```

```
##    WORD        FREQ
## 1  qualities      1
## 2  quarrel       11
## 3  quarrelled     1
## 4  quarrelling    2
## 5  quarrels       1
## 6  quarter        1
## 7  queen          1
## 8  quench         2
## 9  question       2
## 10 quick          2
## 11 quickly        5
## 12 quiet          4
## 13 quinces        1
## 14 quit           1
## 15 quite          2
## 16 quivering      1
## 17 quivers        1
## 18 quote          1
## 19 quoth          5
```

```r
all_words(raj$dialogue, contains="conc")
```

```
##   WORD      FREQ
## 1 conceal'd    1
## 2 conceit      2
## 3 conceive     1
## 4 concludes    1
## 5 reconcile    1
```

```r
x2 <- all_words(raj$dialogue)
head(x2, 10)
```

```
##    WORD      FREQ
## 1  'tis         9
## 2  a          445
## 3  a'           1
## 4  abate        1
## 5  abbey        1
## 6  abed         1
## 7  abhorred     1
## 8  abhors       1
## 9  able         2
## 10 ableeding    1
```

```r
x3 <- all_words(raj$dialogue, alphabetical = FALSE)
head(x3, 10)
```

```
##    WORD FREQ
## 1  and   666
## 2  the   656
## 3  i     573
## 4  to    517
## 5  a     445
## 6  of    378
## 7  my    358
## 8  is    344
## 9  that  344
## 10 in    312
```


The <a href="http://trinker.github.io/qdap_dev/synonyms.html" target="_blank"><code>synonyms</code></a> (short hand: <a href="http://trinker.github.io/qdap_dev/synonyms.html" target="_blank"><code>syn</code></a>) function finds words that are synonyms of a given set of terms and returns either a list of vector that can be passed to <a href="http://trinker.github.io/qdap_dev/termco.html" target="_blank"><code>termco</code></a>'s <b><font color="green" face="courier new">match.list</font></b>.

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/synonyms.html" target="_blank"><code>synonyms</code></a> Examples**<font size="5" color="orange">&diams;</font>


```r
synonyms(c("the", "cat", "job", "environment", "read", "teach"))
```

```
## $cat.def_1
## [1] "feline"    "gib"       "grimalkin" "kitty"     "malkin"   
## 
## $cat.def_2
## [1] "moggy"
## 
## $cat.def_3
## [1] "mouser" "puss"  
## 
## $cat.def_4
## [1] "pussy"
## 
## $cat.def_5
## [1] "tabby"
## 
## $job.def_1
##  [1] "affair"         "assignment"     "charge"         "chore"         
##  [5] "concern"        "contribution"   "duty"           "enterprise"    
##  [9] "errand"         "function"       "pursuit"        "responsibility"
## [13] "role"           "stint"          "task"           "undertaking"   
## [17] "venture"        "work"          
## 
## $job.def_2
##  [1] "business"   "calling"    "capacity"   "career"     "craft"     
##  [6] "employment" "function"   "livelihood" "mtier"      "occupation"
## [11] "office"     "position"   "post"       "profession" "situation" 
## [16] "trade"      "vocation"  
## 
## $job.def_3
##  [1] "allotment"   "assignment"  "batch"       "commission"  "consignment"
##  [6] "contract"    "lot"         "output"      "piece"       "portion"    
## [11] "product"     "share"      
## 
## $environment.def_1
##  [1] "atmosphere"   "background"   "conditions"   "context"     
##  [5] "domain"       "element"      "habitat"      "locale"      
##  [9] "medium"       "milieu"       "scene"        "setting"     
## [13] "situation"    "surroundings" "territory"   
## 
## $environment.def_2
## [1] "The environment is the natural world of land"
## [2] "sea"                                         
## [3] "air"                                         
## [4] "plants"                                      
## [5] "and animals."                                
## 
## $read.def_1
## [1] "glance at"          "look at"            "peruse"            
## [4] "pore over"          "refer to"           "run one's eye over"
## [7] "scan"               "study"             
## 
## $read.def_2
## [1] "announce" "declaim"  "deliver"  "recite"   "speak"    "utter"   
## 
## $read.def_3
## [1] "comprehend"              "construe"               
## [3] "decipher"                "discover"               
## [5] "interpret"               "perceive the meaning of"
## [7] "see"                     "understand"             
## 
## $read.def_4
## [1] "display"  "indicate" "record"   "register" "show"    
## 
## $teach.def_1
##  [1] "advise"          "coach"           "demonstrate"    
##  [4] "direct"          "discipline"      "drill"          
##  [7] "edify"           "educate"         "enlighten"      
## [10] "give lessons in" "guide"           "impart"         
## [13] "implant"         "inculcate"       "inform"         
## [16] "instil"          "instruct"        "school"         
## [19] "show"            "train"           "tutor"
```

```r
head(syn(c("the", "cat", "job", "environment", "read", "teach"),
    return.list = FALSE), 30)
```

```
##  [1] "feline"         "gib"            "grimalkin"      "kitty"         
##  [5] "malkin"         "moggy"          "mouser"         "puss"          
##  [9] "pussy"          "tabby"          "affair"         "assignment"    
## [13] "charge"         "chore"          "concern"        "contribution"  
## [17] "duty"           "enterprise"     "errand"         "function"      
## [21] "pursuit"        "responsibility" "role"           "stint"         
## [25] "task"           "undertaking"    "venture"        "work"          
## [29] "business"       "calling"
```

```r
syn(c("the", "cat", "job", "environment", "read", "teach"), multiwords = FALSE)
```

```
## $cat.def_1
## [1] "feline"    "gib"       "grimalkin" "kitty"     "malkin"   
## 
## $cat.def_2
## [1] "moggy"
## 
## $cat.def_3
## [1] "mouser" "puss"  
## 
## $cat.def_4
## [1] "pussy"
## 
## $cat.def_5
## [1] "tabby"
## 
## $job.def_1
##  [1] "affair"         "assignment"     "charge"         "chore"         
##  [5] "concern"        "contribution"   "duty"           "enterprise"    
##  [9] "errand"         "function"       "pursuit"        "responsibility"
## [13] "role"           "stint"          "task"           "undertaking"   
## [17] "venture"        "work"          
## 
## $job.def_2
##  [1] "business"   "calling"    "capacity"   "career"     "craft"     
##  [6] "employment" "function"   "livelihood" "mtier"      "occupation"
## [11] "office"     "position"   "post"       "profession" "situation" 
## [16] "trade"      "vocation"  
## 
## $job.def_3
##  [1] "allotment"   "assignment"  "batch"       "commission"  "consignment"
##  [6] "contract"    "lot"         "output"      "piece"       "portion"    
## [11] "product"     "share"      
## 
## $environment.def_1
##  [1] "atmosphere"   "background"   "conditions"   "context"     
##  [5] "domain"       "element"      "habitat"      "locale"      
##  [9] "medium"       "milieu"       "scene"        "setting"     
## [13] "situation"    "surroundings" "territory"   
## 
## $environment.def_2
## [1] "sea"    "air"    "plants"
## 
## $read.def_1
## [1] "peruse" "scan"   "study" 
## 
## $read.def_2
## [1] "announce" "declaim"  "deliver"  "recite"   "speak"    "utter"   
## 
## $read.def_3
## [1] "comprehend" "construe"   "decipher"   "discover"   "interpret" 
## [6] "see"        "understand"
## 
## $read.def_4
## [1] "display"  "indicate" "record"   "register" "show"    
## 
## $teach.def_1
##  [1] "advise"      "coach"       "demonstrate" "direct"      "discipline" 
##  [6] "drill"       "edify"       "educate"     "enlighten"   "guide"      
## [11] "impart"      "implant"     "inculcate"   "inform"      "instil"     
## [16] "instruct"    "school"      "show"        "train"       "tutor"
```


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/termco.html" target="_blank"><code>termco</code></a> - Simple Example**<font size="5" color="orange">&diams;</font>


```r
## Make a small dialogue data set
(dat2 <- data.frame(dialogue=c("@bryan is bryan good @br",
    "indeed", "@ brian"), person=qcv(A, B, A)))
```

```
##                   dialogue person
## 1 @bryan is bryan good @br      A
## 2                   indeed      B
## 3                  @ brian      A
```

```r

## The word list to search for
ml <- list(
    wrds=c("bryan", "indeed"), 
    "@", 
    bryan=c("bryan", "@ br", "@br")
)

## Search by person
with(dat2, termco(dialogue, person, match.list=ml))
```

```
##   person word.count       wrds         @     bryan
## 1      A          6  2(33.33%) 3(50.00%) 5(83.33%)
## 2      B          1 1(100.00%)         0         0
```

```r
## Search by person peopoerion output
with(dat2, termco(dialogue, person, match.list=ml, percent = FALSE))
```

```
##   person word.count    wrds      @  bryan
## 1      A          6  2(.33) 3(.50) 5(.83)
## 2      B          1 1(1.00)      0      0
```


<p id = "rajex"><font size="5" color="orange">&diams;</font> <b><a href="http://trinker.github.io/qdap_dev/termco.html" target="_blank"><code>termco</code></a> - Romeo and Juliet Act 1 Example</b><font size="5" color="orange">&diams;</font></p>


```r
## Word list to search for
## Note: In the last vector using "the" will actually 
## include the other 3 versions
ml2 <- list(
    theme_1 = c(" the ", " a ", " an "),
    theme_2 = c(" I'" ),
    "good",
    the_words = c("the", " the ", " the", "the ")
)

(out <- with(raj.act.1,  termco(dialogue, person, ml2)))
```

```
##            person word.count   theme_1 theme_2     good   the_words
## 1         Abraham         24         0       0        0           0
## 2        Benvolio        621 32(5.15%) 2(.32%)  2(.32%) 123(19.81%)
## 3         Capulet        736 39(5.30%) 3(.41%)  3(.41%)  93(12.64%)
## 4   First Citizen         16 2(12.50%)       0        0  10(62.50%)
## 5   First Servant         69 8(11.59%)       0 1(1.45%)  20(28.99%)
## 6         Gregory        149  9(6.04%)       0        0  48(32.21%)
## 7          Juliet        206  5(2.43%) 1(.49%)  1(.49%)   20(9.71%)
## 8    Lady Capulet        286 20(6.99%)       0        0  63(22.03%)
## 9   Lady Montague         28  2(7.14%)       0        0           0
## 10       Mercutio        552 49(8.88%)       0  2(.36%) 146(26.45%)
## 11       Montague        217 12(5.53%)       0  1(.46%)  41(18.89%)
## 12          Nurse        598 44(7.36%) 1(.17%)  2(.33%) 103(17.22%)
## 13          Paris         32         0       0        0    1(3.12%)
## 14         Prince        167  8(4.79%)       0        0  35(20.96%)
## 15          Romeo       1164 56(4.81%) 3(.26%)  3(.26%) 142(12.20%)
## 16        Sampson        259 19(7.34%)       0  1(.39%)  70(27.03%)
## 17 Second Capulet         17         0       0        0           0
## 18 Second Servant         41  2(4.88%)       0 1(2.44%)   8(19.51%)
## 19        Servant        183 12(6.56%) 1(.55%)  1(.55%)  46(25.14%)
## 20         Tybalt        160 11(6.88%) 1(.62%)        0  24(15.00%)
```

```r
## Available elements in the termco output (use dat$...)
names(out)
```

```
## [1] "raw"          "prop"         "rnp"          "zero.replace"
## [5] "percent"      "digits"
```

```r
## Raw and proportion - useful for presenting in tables
out$rnp  
```

```
##            person word.count   theme_1 theme_2     good   the_words
## 1         Abraham         24         0       0        0           0
## 2        Benvolio        621 32(5.15%) 2(.32%)  2(.32%) 123(19.81%)
## 3         Capulet        736 39(5.30%) 3(.41%)  3(.41%)  93(12.64%)
## 4   First Citizen         16 2(12.50%)       0        0  10(62.50%)
## 5   First Servant         69 8(11.59%)       0 1(1.45%)  20(28.99%)
## 6         Gregory        149  9(6.04%)       0        0  48(32.21%)
## 7          Juliet        206  5(2.43%) 1(.49%)  1(.49%)   20(9.71%)
## 8    Lady Capulet        286 20(6.99%)       0        0  63(22.03%)
## 9   Lady Montague         28  2(7.14%)       0        0           0
## 10       Mercutio        552 49(8.88%)       0  2(.36%) 146(26.45%)
## 11       Montague        217 12(5.53%)       0  1(.46%)  41(18.89%)
## 12          Nurse        598 44(7.36%) 1(.17%)  2(.33%) 103(17.22%)
## 13          Paris         32         0       0        0    1(3.12%)
## 14         Prince        167  8(4.79%)       0        0  35(20.96%)
## 15          Romeo       1164 56(4.81%) 3(.26%)  3(.26%) 142(12.20%)
## 16        Sampson        259 19(7.34%)       0  1(.39%)  70(27.03%)
## 17 Second Capulet         17         0       0        0           0
## 18 Second Servant         41  2(4.88%)       0 1(2.44%)   8(19.51%)
## 19        Servant        183 12(6.56%) 1(.55%)  1(.55%)  46(25.14%)
## 20         Tybalt        160 11(6.88%) 1(.62%)        0  24(15.00%)
```

```r
## Raw - useful for performing calculations
out$raw 
```

```
##            person word.count theme_1 theme_2 good the_words
## 1         Abraham         24       0       0    0         0
## 2        Benvolio        621      32       2    2       123
## 3         Capulet        736      39       3    3        93
## 4   First Citizen         16       2       0    0        10
## 5   First Servant         69       8       0    1        20
## 6         Gregory        149       9       0    0        48
## 7          Juliet        206       5       1    1        20
## 8    Lady Capulet        286      20       0    0        63
## 9   Lady Montague         28       2       0    0         0
## 10       Mercutio        552      49       0    2       146
## 11       Montague        217      12       0    1        41
## 12          Nurse        598      44       1    2       103
## 13          Paris         32       0       0    0         1
## 14         Prince        167       8       0    0        35
## 15          Romeo       1164      56       3    3       142
## 16        Sampson        259      19       0    1        70
## 17 Second Capulet         17       0       0    0         0
## 18 Second Servant         41       2       0    1         8
## 19        Servant        183      12       1    1        46
## 20         Tybalt        160      11       1    0        24
```

```r
## Proportion - useful for performing calculations
out$prop
```

```
##            person word.count theme_1 theme_2   good the_words
## 1         Abraham         24   0.000  0.0000 0.0000     0.000
## 2        Benvolio        621   5.153  0.3221 0.3221    19.807
## 3         Capulet        736   5.299  0.4076 0.4076    12.636
## 4   First Citizen         16  12.500  0.0000 0.0000    62.500
## 5   First Servant         69  11.594  0.0000 1.4493    28.986
## 6         Gregory        149   6.040  0.0000 0.0000    32.215
## 7          Juliet        206   2.427  0.4854 0.4854     9.709
## 8    Lady Capulet        286   6.993  0.0000 0.0000    22.028
## 9   Lady Montague         28   7.143  0.0000 0.0000     0.000
## 10       Mercutio        552   8.877  0.0000 0.3623    26.449
## 11       Montague        217   5.530  0.0000 0.4608    18.894
## 12          Nurse        598   7.358  0.1672 0.3344    17.224
## 13          Paris         32   0.000  0.0000 0.0000     3.125
## 14         Prince        167   4.790  0.0000 0.0000    20.958
## 15          Romeo       1164   4.811  0.2577 0.2577    12.199
## 16        Sampson        259   7.336  0.0000 0.3861    27.027
## 17 Second Capulet         17   0.000  0.0000 0.0000     0.000
## 18 Second Servant         41   4.878  0.0000 2.4390    19.512
## 19        Servant        183   6.557  0.5464 0.5464    25.137
## 20         Tybalt        160   6.875  0.6250 0.0000    15.000
```


<font size="5" color="orange">&diams;</font> **Using <a href="http://trinker.github.io/qdap_dev/termco.html" target="_blank"><code>termco</code></a> with <a href="http://trinker.github.io/qdap_dev/term_match.html" target="_blank"><code>term_match</code></a> and <a href="http://trinker.github.io/qdap_dev/exclude.html" target="_blank"><code>exclude</code></a>**<font size="5" color="orange">&diams;</font>


```r
## Example 1
termco(DATA$state, DATA$person, exclude(term_match(DATA$state, qcv(th),
    FALSE), "truth"))
```

```
##       person word.count       the      then    there
## 1       greg         20 2(10.00%)         0 1(5.00%)
## 2 researcher          6 1(16.67%) 1(16.67%)        0
## 3      sally         10         0         0        0
## 4        sam         13         0         0        0
## 5    teacher          4         0         0        0
```

```r
## Example 2
MTCH.LST <- exclude(term_match(DATA$state, qcv(th, i)), qcv(truth, stinks))
termco(DATA$state, DATA$person, MTCH.LST)
```

```
##       person word.count        th          i
## 1       greg         20 3(15.00%) 13(65.00%)
## 2 researcher          6 2(33.33%)          0
## 3      sally         10         0  4(40.00%)
## 4        sam         13         0 11(84.62%)
## 5    teacher          4         0          0
```


<font size="5" color="orange">&diams;</font> **Using <a href="http://trinker.github.io/qdap_dev/termco.html" target="_blank"><code>termco</code></a> with <a href="http://trinker.github.io/qdap_dev/syn.html" target="_blank"><code>syn</code></a>**<font size="5" color="orange">&diams;</font>


```r
syns <- synonyms("doubt")
syns[1]
```

```
## $doubt.def_1
## [1] "discredit"          "distrust"           "fear"              
## [4] "lack confidence in" "misgive"            "mistrust"          
## [7] "query"              "question"           "suspect"
```



```r
termco(DATA$state, DATA$person, unlist(syns[1]))
```



```
##       person word.count discredit distrust fear query question
## 1       greg         20         0        0    0     0        0
## 2 researcher          6         0        0    0     0        0
## 3      sally         10         0        0    0     0        0
## 4        sam         13         0 1(7.69%)    0     0        0
## 5    teacher          4         0        0    0     0        0
```



```r
synonyms("doubt", FALSE)
```

```
##  [1] "discredit"          "distrust"           "fear"              
##  [4] "lack confidence in" "misgive"            "mistrust"          
##  [7] "query"              "question"           "suspect"           
## [10] "apprehension"       "disquiet"           "incredulity"       
## [13] "lack of faith"      "misgiving"          "qualm"             
## [16] "scepticism"         "suspicion"          "be dubious"        
## [19] "be uncertain"       "demur"              "fluctuate"         
## [22] "hesitate"           "scruple"            "vacillate"         
## [25] "waver"              "dubiety"            "hesitancy"         
## [28] "hesitation"         "indecision"         "irresolution"      
## [31] "lack of conviction" "suspense"           "uncertainty"       
## [34] "vacillation"        "confusion"          "difficulty"        
## [37] "dilemma"            "perplexity"         "problem"           
## [40] "quandary"           "admittedly"         "assuredly"         
## [43] "certainly"          "doubtless"          "doubtlessly"       
## [46] "probably"           "surely"             "accept"            
## [49] "believe"            "buy"                "belief"            
## [52] "certainty"          "confidence"         "conviction"        
## [55] "trust"
```

```r
termco(DATA$state, DATA$person, list(doubt = synonyms("doubt", FALSE)))
```

```
##       person word.count     doubt
## 1       greg         20         0
## 2 researcher          6         0
## 3      sally         10         0
## 4        sam         13 2(15.38%)
## 5    teacher          4         0
```



```r
termco(DATA$state, DATA$person, syns)
```



```
##       person word.count doubt.def_1 doubt.def_2 doubt.def_7 doubt.def_8
## 1       greg         20           0           0           0           0
## 2 researcher          6           0           0           0           0
## 3      sally         10           0           0           0           0
## 4        sam         13    1(7.69%)    1(7.69%)           0    1(7.69%)
## 5    teacher          4           0           0           0           0
```


<a href="http://trinker.github.io/qdap_dev/termco.html" target="_blank"><code>termco</code></a> also has a plot method that plots a heatmap of the <a href="http://trinker.github.io/qdap_dev/termco.html" target="_blank"><code>termco</code></a> output based on the percent usage by grouping variable.  This allows for rapid visualizations of patterns and anables fast spotting of extreme values.  Here are some plots from the <a href="#rajex">Romeo and Juliet Act 1 Example</a> above.

<font size="5" color="orange">&diams;</font> **Using <a href="http://trinker.github.io/qdap_dev/termco.html" target="_blank"><code>termco</code></a> Plotting**<font size="5" color="orange">&diams;</font>


```r
plot(out)
```

![plot of chunk unnamed-chunk-151](figure/unnamed-chunk-151.png) 



```r
plot(out, label = TRUE)
```

![plot of chunk unnamed-chunk-152](figure/unnamed-chunk-152.png) 


<h4 id="quest">Question Type Counts</h4>

A researcher may be interested in classifying and investigating the types of questions used within dialogue.  
<a href="http://trinker.github.io/qdap_dev/question_type.html" target="_blank"><code>question_type</code></a> provides question classification.  The algorithm searches for the following interrogative words (and optionally, their negative contraction form as well):


```
are*           had*           must*          what           why                   
can*           has            ok             when           will*                 
correct        have*          right          where          would*                
could          how            shall          which          implied do/does/did   
did*           is             should         who                                  
do*            may            was*           whom                                 
does*          might*         were*          whose                                
```



The interrogative word that is found first (with the exception of "ok", "right" and "correct") in the question determines the sentence type. "ok", "right" and "correct" sentence types are determined if the sentence is a question with no other interrogative words found and "ok", "right" or "correct" is the last word of the sentence. Those interrogative sentences beginning with the word "you", "wanna", or "want" are categorized as implying do/does/did question type, though the use of do/does is not explicit. Those sentence beginning with "you" followed by a select interrogative word (and or their negative counter parts) above (marked with *) or 1-2 amplifier(s) followed by the select interrogative word are categorized by the select word rather than an implied do/does/did question type.  A sentence that is marked "ok" over rides an implied do/does/did label.  Those with undetermined sentence type are labeled unknown.

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/question_type.html" target="_blank"><code>question_type</code></a> - Basic Example**<font size="5" color="orange">&diams;</font>


```r
## Basic Example
(x <- question_type(DATA.SPLIT$state, DATA.SPLIT$person))
```

```
##       person tot.quest    what    how   shall implied_do/does/did
## 1       greg         1       0      0       0             1(100%)
## 2 researcher         1       0      0 1(100%)                   0
## 3      sally         2  1(50%) 1(50%)       0                   0
## 4    teacher         1 1(100%)      0       0                   0
## 5        sam         0       0      0       0                   0
```

```r
## Available elements from output
names(x)
```

```
## [1] "raw"          "count"        "prop"         "rnp"         
## [5] "inds"         "missing"      "percent"      "zero.replace"
```

```r
## Table of counts useful for additional analysis
x$count
```

```
##       person tot.quest what how shall implied_do/does/did
## 1       greg         1    0   0     0                   1
## 2 researcher         1    0   0     1                   0
## 3      sally         2    1   1     0                   0
## 4    teacher         1    1   0     0                   0
## 5        sam         0    0   0     0                   0
```

```r
## The raw output with question types
truncdf(x$raw, 15)
```

```
##       person        raw.text n.row endmark      strip.text          q.type
## 1    teacher What should we      4       ?  what should we            what
## 2      sally How can we be c     7       ?  how can we be              how
## 3      sally What are you ta    10       ?  what are you t            what
## 4 researcher Shall we move o    11       ?  shall we move            shall
## 5       greg    You already?    15       ?    you already  implied_do/does
```


<a href="http://trinker.github.io/qdap_dev/question_type.html" target="_blank"><code>question_type</code></a> also has a plot method that plots a heatmap of the output.  This allows for rapid visualizations of patterns and anables fast spotting of extreme values.

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/question_type.html" target="_blank"><code>question_type</code></a> - Plotting Method**<font size="5" color="orange">&diams;</font>


```r
plot(x)
```

![plot of chunk unnamed-chunk-155](figure/unnamed-chunk-155.png) 



```r
plot(x, label = TRUE, high = "red", low = "yellow", grid = NULL)
```

![plot of chunk unnamed-chunk-156](figure/unnamed-chunk-156.png) 


Negative forms of questions such as <font color="green">Don't you want the robots to leave?</font> are, by defualt, grouped with with their equivalent positive <font color="green">Do</font> forms, such as <font color="green">Do you want the robots to leave?</font>.  The researcher may choose to keep the two forms separate using the argument <b><font color="green">neg.cont = TRUE</font>
</b>

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/question_type.html" target="_blank"><code>question_type</code></a> - Include Negative Questions**<font size="5" color="orange">&diams;</font>


```r
## Create a Dataframe with Do and Don't
(DATA.SPLIT2 <- rbind(DATA.SPLIT,
    c("sam", "1.1", "1", "m", "0", "K1", "Don't you think so?", "x"),
    c("sam", "1.1", "1", "m", "0", "K1", "Do you think so?", "x")
))[, c(1, 7)]
```

```
##          person                       state
## 1.1         sam            Computer is fun.
## 1.2         sam                Not too fun.
## 2.1        greg     No it's not, it's dumb.
## 3.1     teacher          What should we do?
## 4.1         sam        You liar, it stinks!
## 5.1        greg     I am telling the truth!
## 6.1       sally      How can we be certain?
## 7.1        greg            There is no way.
## 8.1         sam             I distrust you.
## 9.1       sally What are you talking about?
## 10.1 researcher           Shall we move on?
## 10.2 researcher                  Good then.
## 11.1       greg                 I'm hungry.
## 11.2       greg                  Let's eat.
## 11.3       greg                You already?
## 16          sam         Don't you think so?
## 17          sam            Do you think so?
```

```r
## Do and Don't Grouped Together
question_type(DATA.SPLIT2$state, DATA.SPLIT2$person)
```

```
##       person tot.quest    what      do    how   shall implied_do/does/did
## 1       greg         1       0       0      0       0             1(100%)
## 2 researcher         1       0       0      0 1(100%)                   0
## 3      sally         2  1(50%)       0 1(50%)       0                   0
## 4        sam         2       0 2(100%)      0       0                   0
## 5    teacher         1 1(100%)       0      0       0                   0
```



```r
## Do and Don't Grouped Separately
question_type(DATA.SPLIT2$state, DATA.SPLIT2$person, neg.cont = TRUE)
```


<pre><code>      person tot.quest    what  don't     do    how   shall implied_do/does/did
1       greg         1       0      0      0      0       0             1(100%)
2 researcher         1       0      0      0      0 1(100%)                   0
3      sally         2  1(50%)      0      0 1(50%)       0                   0
4        sam         2       0 1(50%) 1(50%)      0       0                   0
5    teacher         1 1(100%)      0      0      0       0                   0
</code></pre>

It may be helpful to take the indices of the question types int the **x[["inds"]]** output or access **x[["raw"]][, "n.row"]**

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/question_type.html" target="_blank"><code>question_type</code></a> - Passing to <a href="http://trinker.github.io/qdap_dev/trans_context.html" target="_blank"><code>trans_context</code></a>**<font size="5" color="orange">&diams;</font>


```r
## The indices of all questions
x <- question_type(DATA.SPLIT$state, DATA.SPLIT$person)
(inds1 <- x[["inds"]])
```

```
## [1]  4  7 10 11 15
```



```r
with(DATA.SPLIT, trans_context(state, person, inds = inds1, n.before = 2))
```


<pre><code>===================================
Event 1: [lines 2-6]

    sam:        Computer is fun. Not too fun.

    greg:       No it's not, it's dumb.

 ** teacher:    What should we do?

    sam:        You liar, it stinks!

    greg:       I am telling the truth! 

===================================
Event 2: [lines 5-9]

    sam:        You liar, it stinks!

    greg:       I am telling the truth!

 ** sally:      How can we be certain?

    greg:       There is no way.

    sam:        I distrust you. 

===================================
Event 3: [lines 8-12]

    greg:       There is no way.

    sam:        I distrust you.

 ** sally:      What are you talking about?

    researcher: Shall we move on? Good then.

    greg:       I'm hungry. Let's eat. You already? 

===================================
Event 4: [lines 9-13]

    sam:        I distrust you.

    sally:      What are you talking about?

 ** researcher: Shall we move on? Good then.

    greg:       I'm hungry. Let's eat. You already? 

===================================
Event 5: [lines 13-15]

    sally:      What are you talking about?

    researcher: Shall we move on? Good then.

 ** greg:       I'm hungry. Let's eat. You already? </code></pre>
 

```r
## Find what and how questions
inds2 <- x[["raw"]][x[["raw"]]$q.type %in% c("what", "how"), "n.row"]
with(DATA.SPLIT, trans_context(state, person, inds = inds2, n.before = 2))
```


<pre><code>===================================
Event 1: [lines 2-6]

    sam:        Computer is fun. Not too fun.

    greg:       No it's not, it's dumb.

 ** teacher:    What should we do?

    sam:        You liar, it stinks!

    greg:       I am telling the truth! 

===================================
Event 2: [lines 5-9]

    sam:        You liar, it stinks!

    greg:       I am telling the truth!

 ** sally:      How can we be certain?

    greg:       There is no way.

    sam:        I distrust you. 

===================================
Event 3: [lines 8-12]

    greg:       There is no way.

    sam:        I distrust you.

 ** sally:      What are you talking about?

    researcher: Shall we move on? Good then.

    greg:       I'm hungry. Let's eat. You already? </code></pre>


<h4 id="wordcount">Word & Character Counts</h4>

A research may have the need to view simple word or character counts for the sake of comparisons between grouping variables.  <a href="http://trinker.github.io/qdap_dev/word_count.html" target="_blank"><code>word_count</code></a> (<a href="http://trinker.github.io/qdap_dev/word_count.html" target="_blank"><code>wc</code></a>), <a href="http://trinker.github.io/qdap_dev/word_list.html" target="_blank"><code>word_list</code></a>, <a href="http://trinker.github.io/qdap_dev/word_count.html" target="_blank"><code>character_count</code></a>, <a href="http://trinker.github.io/qdap_dev/word_count.html" target="_blank"><code>character_table</code></a> (<a href="http://trinker.github.io/qdap_dev/word_count.html" target="_blank"><code>char_table</code></a>) serve the purposes of counting words and characters with <a href="http://trinker.github.io/qdap_dev/word_list.html" target="_blank"><code>word_list</code></a> producing a lists of words usage by grouping variable and <a href="http://trinker.github.io/qdap_dev/word_count.html" target="_blank"><code>character_table</code></a> producing a count table of characters.  The following examples demonstrate the uses of these functions.  

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/word_count.html" target="_blank"><code>word_count</code></a> Examples**<font size="5" color="orange">&diams;</font>


```r
word_count(DATA$state)
```

```
##  [1] 6 5 4 4 5 5 4 3 5 6 6
```

```r
## `wc a shortened version of `word_count`
wc(DATA$state)
```

```
##  [1] 6 5 4 4 5 5 4 3 5 6 6
```

```r
## Retain the text
wc(DATA$state, names = TRUE)
```

```
##         Computer is fun. Not too fun. 
##                                     6 
##               No it's not, it's dumb. 
##                                     5 
##                    What should we do? 
##                                     4 
##                  You liar, it stinks! 
##                                     4 
##               I am telling the truth! 
##                                     5 
##                How can we be certain? 
##                                     5 
##                      There is no way. 
##                                     4 
##                       I distrust you. 
##                                     3 
##           What are you talking about? 
##                                     5 
##         Shall we move on?  Good then. 
##                                     6 
## I'm hungry.  Let's eat.  You already? 
##                                     6
```

```r
## Setting `byrow=FALSE` gives a total for the text variable
word_count(DATA$state, byrow=FALSE, names = TRUE)
```

```
## [1] 53
```

```r
## identical to `byrow=FALSE` above
sum(word_count(DATA$state))
```

```
## [1] 53
```

```r
## By grouping variable
tapply(DATA$state, DATA$person, wc, byrow=FALSE)
```

```
##       greg researcher      sally        sam    teacher 
##         20          6         10         13          4
```


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/word_count.html" target="_blank"><code>character_count</code></a> Examples**<font size="5" color="orange">&diams;</font>


```r
character_count(DATA$state)
```

```
##  [1] 22 17 14 15 18 17 12 12 22 21 27
```

```r
## Setting `byrow=FALSE` gives a total for the text variable
character_count(DATA$state, byrow=FALSE)
```

```
## [1] 197
```

```r
## identical to `byrow=FALSE` above
sum(character_count(DATA$state))
```

```
## [1] 197
```

```r
## By grouping variable
tapply(DATA$state, DATA$person, character_count, byrow=FALSE)
```

```
##       greg researcher      sally        sam    teacher 
##         74         21         39         49         14
```


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/word_count.html" target="_blank"><code>character_table</code></a> Example**<font size="5" color="orange">&diams;</font>


```r
x <- character_table(DATA$state, DATA$person)
names(x)
```

```
## [1] "raw"          "prop"         "rnp"          "percent"     
## [5] "zero.replace"
```

```r
x$raw
```

```
##       person '    ! , . ? a b c d e f g h i k l m n o p r s  t u v w y
## 1       greg 4 16 1 1 7 1 5 1 0 2 7 0 2 4 6 0 4 3 5 4 0 4 4 10 4 0 1 4
## 2 researcher 0  5 0 0 1 1 1 0 0 1 3 0 1 2 0 0 2 1 2 4 0 0 1  1 0 1 1 0
## 3      sally 0  8 0 0 1 2 6 2 2 0 4 0 1 2 2 1 1 0 3 3 0 2 0  4 2 0 3 1
## 4        sam 0 10 1 1 5 0 1 0 1 1 1 2 0 0 6 1 1 1 4 6 1 3 5  7 6 0 0 2
## 5    teacher 0  3 0 0 0 1 1 0 0 2 1 0 0 2 0 0 1 0 0 2 0 0 1  1 1 0 2 0
```

```r
x$prop[, 1:10]
```

```
##       person '           !     ,     .     ?      a b     c
## 1       greg 4 16.00 1.000 1.000 7.000 1.000  5.000 1 0.000
## 2 researcher 0 17.86 0.000 0.000 3.571 3.571  3.571 0 0.000
## 3      sally 0 16.00 0.000 0.000 2.000 4.000 12.000 4 4.000
## 4        sam 0 15.15 1.515 1.515 7.576 0.000  1.515 0 1.515
## 5    teacher 0 16.67 0.000 0.000 0.000 5.556  5.556 0 0.000
```

```r
x$rnp[, 1:7]
```

```
##       person     '                   !        ,        .        ?
## 1       greg 4(4%) 16(16.00%) 1(1.00%) 1(1.00%) 7(7.00%) 1(1.00%)
## 2 researcher     0  5(17.86%)        0        0 1(3.57%) 1(3.57%)
## 3      sally     0  8(16.00%)        0        0 1(2.00%) 2(4.00%)
## 4        sam     0 10(15.15%) 1(1.52%) 1(1.52%) 5(7.58%)        0
## 5    teacher     0  3(16.67%)        0        0        0 1(5.56%)
```

```r
## Combine Columns
vowels <- c("a", "e", "i", "o", "u")
cons <- letters[!letters %in% c(vowels, qcv(j, q, x, z))]
colcomb2class(x, list(vowels = vowels, consonants = cons, other = 2:7))
```

```
##       person     vowels consonants      other
## 1       greg 26(26.00%) 44(44.00%) 30(30.00%)
## 2 researcher  8(28.57%) 13(46.43%)  7(25.00%)
## 3      sally 17(34.00%) 22(44.00%) 11(22.00%)
## 4        sam 20(30.30%) 29(43.94%) 17(25.76%)
## 5    teacher  5(27.78%)  9(50.00%)  4(22.22%)
```


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/word_count.html" target="_blank"><code>character_table</code></a> Plot Method**<font size="5" color="orange">&diams;</font>


```r
plot(x)
```

![plot of chunk unnamed-chunk-165](figure/unnamed-chunk-165.png) 



```r
plot(x, label = TRUE, high = "red", lab.digits = 1, zero.replace = "")
```

![plot of chunk unnamed-chunk-166](figure/unnamed-chunk-166.png) 


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/word_count.html" target="_blank"><code>character_table</code></a> Additional Plotting**<font size="5" color="orange">&diams;</font>


```r
library(ggplot2);library(reshape2)
dat <- char_table(DATA$state, list(DATA$sex, DATA$adult))
dat2 <- colsplit2df(melt(dat$raw), keep.orig = TRUE)
dat2$adult2 <- lookup(dat2$adult, c(0, 1), c("child", "adult"))
head(dat2, 15)
```

```
##    sex&adult sex adult variable value adult2
## 1        f.0   f     0        '     0  child
## 2        f.1   f     1        '     0  adult
## 3        m.0   m     0        '     4  child
## 4        m.1   m     1        '     0  adult
## 5        f.0   f     0              8  child
## 6        f.1   f     1              5  adult
## 7        m.0   m     0             26  child
## 8        m.1   m     1              3  adult
## 9        f.0   f     0        !     0  child
## 10       f.1   f     1        !     0  adult
## 11       m.0   m     0        !     2  child
## 12       m.1   m     1        !     0  adult
## 13       f.0   f     0        ,     0  child
## 14       f.1   f     1        ,     0  adult
## 15       m.0   m     0        ,     2  child
```



```r
ggplot(data = dat2, aes(y = variable, x = value, colour=sex)) +
    facet_grid(adult2~.) +
    geom_line(size=1, aes(group =variable), colour = "black") +
    geom_point()
```

![plot of chunk unnamed-chunk-168](figure/unnamed-chunk-168.png) 



```r
ggplot(data = dat2, aes(x = variable, y = value)) +
    geom_bar(aes(fill = variable), stat = "identity") +
    facet_grid(sex ~ adult2, margins = TRUE) +
    theme(legend.position="none")
```

![plot of chunk unnamed-chunk-169](figure/unnamed-chunk-169.png) 


<h4 id="freqtab">SPSS Style Frequency Tables</h4>

It is helpful to view the frequency distributions for a vector, matrix or dataframe.   The <a href="http://trinker.github.io/qdap_dev/dist_tab.html" target="_blank"><code>dist_tab</code></a> function allows the researcher to quickly generate frequency distributions.


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/dist_tab.html" target="_blank"><code>dist_tab</code></a> Examples**<font size="5" color="orange">&diams;</font>


```r
dist_tab(rnorm(10000), 10)
```

```
##          interval Freq cum.Freq percent cum.percent
## 1   (-3.79,-2.98]   20       20    0.20        0.20
## 2   (-2.98,-2.17]  132      152    1.32        1.52
## 3   (-2.17,-1.37]  697      849    6.97        8.49
## 4  (-1.37,-0.561] 1956     2805   19.56       28.05
## 5  (-0.561,0.245] 3150     5955   31.50       59.55
## 6    (0.245,1.05] 2582     8537   25.82       85.37
## 7     (1.05,1.86] 1165     9702   11.65       97.02
## 8     (1.86,2.66]  265     9967    2.65       99.67
## 9     (2.66,3.47]   31     9998    0.31       99.98
## 10    (3.47,4.28]    2    10000    0.02      100.00
```

```r
dist_tab(sample(c("red", "blue", "gray"), 100, T), right = FALSE)
```

```
##   interval Freq cum.Freq percent cum.percent
## 1     blue   23       23      23          23
## 2     gray   39       62      39          62
## 3      red   38      100      38         100
```

```r
dist_tab(CO2, 4)
```

```
## $Plant
##    interval Freq cum.Freq percent cum.percent
## 1       Qn1    7        7    8.33        8.33
## 2       Qn2    7       14    8.33       16.67
## 3       Qn3    7       21    8.33       25.00
## 4       Qc1    7       28    8.33       33.33
## 5       Qc3    7       35    8.33       41.67
## 6       Qc2    7       42    8.33       50.00
## 7       Mn3    7       49    8.33       58.33
## 8       Mn2    7       56    8.33       66.67
## 9       Mn1    7       63    8.33       75.00
## 10      Mc2    7       70    8.33       83.33
## 11      Mc3    7       77    8.33       91.67
## 12      Mc1    7       84    8.33      100.00
## 
## $Type
##      interval Freq cum.Freq percent cum.percent
## 1      Quebec   42       42      50          50
## 2 Mississippi   42       84      50         100
## 
## $Treatment
##     interval Freq cum.Freq percent cum.percent
## 1 nonchilled   42       42      50          50
## 2    chilled   42       84      50         100
## 
## $conc
##      interval Freq cum.Freq percent cum.percent
## 1  (94.1,321]   36       36   42.86       42.86
## 2   (321,548]   24       60   28.57       71.43
## 3   (548,774]   12       72   14.29       85.71
## 4 (774,1e+03]   12       84   14.29      100.00
## 
## $uptake
##      interval Freq cum.Freq percent cum.percent
## 1 (7.66,17.1]   19       19   22.62       22.62
## 2 (17.1,26.6]   18       37   21.43       44.05
## 3 (26.6,36.1]   25       62   29.76       73.81
## 4 (36.1,45.5]   22       84   26.19      100.00
```



```r
wdst <- with(mraja1spl, word_stats(dialogue, list(sex, fam.aff, died)))
dist_tab(wdst$gts[1:4], 5)
```


<pre><code>$`sex&fam.aff&died`
          interval Freq cum.Freq percent cum.percent
1      f.cap.FALSE    1        1    9.09        9.09
2       f.cap.TRUE    1        2    9.09       18.18
3      f.mont.TRUE    1        3    9.09       27.27
4      m.cap.FALSE    1        4    9.09       36.36
5       m.cap.TRUE    1        5    9.09       45.45
6    m.escal.FALSE    1        6    9.09       54.55
7     m.escal.TRUE    1        7    9.09       63.64
8     m.mont.FALSE    1        8    9.09       72.73
9      m.mont.TRUE    1        9    9.09       81.82
10    m.none.FALSE    1       10    9.09       90.91
11 none.none.FALSE    1       11    9.09      100.00

$n.sent
     interval Freq cum.Freq percent cum.percent
1 (3.85,34.7]    7        7   63.64       63.64
2 (34.7,65.6]    0        7    0.00       63.64
3 (65.6,96.4]    2        9   18.18       81.82
4  (96.4,127]    1       10    9.09       90.91
5   (127,158]    1       11    9.09      100.00

$n.words
            interval Freq cum.Freq percent cum.percent
1         (14.4,336]    6        6   54.55       54.55
2          (336,658]    2        8   18.18       72.73
3          (658,981]    1        9    9.09       81.82
4      (981,1.3e+03]    1       10    9.09       90.91
5 (1.3e+03,1.62e+03]    1       11    9.09      100.00

$n.char
             interval Freq cum.Freq percent cum.percent
1     (72.7,1.34e+03]    6        6   54.55       54.55
2  (1.34e+03,2.6e+03]    2        8   18.18       72.73
3  (2.6e+03,3.86e+03]    1        9    9.09       81.82
4 (3.86e+03,5.12e+03]    1       10    9.09       90.91
5 (5.12e+03,6.39e+03]    1       11    9.09      100.00
</code></pre>


<h4 id="pos">Parts of Speech Tagging & Counts</h4>

In some analysis of text the research may wish to gather information about parts of speech (POS).  The function <a href="http://trinker.github.io/qdap_dev/pos.html" target="_blank"><code>pos</code></a> and it's grouping variable counterpart, <a href="http://trinker.github.io/qdap_dev/pos.html" target="_blank"><code>pos_by</code></a>, can provide this functionality.  The <a href="http://trinker.github.io/qdap_dev/pos.html" target="_blank"><code>pos</code></a> functions are wrappers for POS related functions from the <a href="http://cran.r-project.org/web/packages/openNLP/index.html">openNLP</a> package.  The <a href="http://trinker.github.io/qdap_dev/pos.html" target="_blank"><code>pos_tags</code></a> function provides a quick reference to what the POS tags utilized by <a href="http://cran.r-project.org/web/packages/openNLP/index.html">openNLP</a> mean.  For more information on the POS tags see the <a href="http://www.cis.upenn.edu/~treebank/">Penn Treebank Project</a>.

The following examples utilize the <a href="http://trinker.github.io/qdap_dev/pos.html" target="_blank"><code>pos_by</code></a> function as the <a href="http://trinker.github.io/qdap_dev/pos.html" target="_blank"><code>pos</code></a> function is used identically, except without specifying a `grouping.var`.  It is important to realize that POS tagging is a very slow process.  The speed can be increased by using the <b><font  color="green" face="courier">parallel = TRUE</font></b> argument. Additionally, the user can recycle the output from one run of <a href="http://trinker.github.io/qdap_dev/pos.html" target="_blank"><code>pos</code></a>, <a href="http://trinker.github.io/qdap_dev/pos.html" target="_blank"><code>pos_by</code></a> or <a href="http://trinker.github.io/qdap_dev/formality.html" target="_blank"><code>formality</code></a> and use it interchangably between the <a href="http://trinker.github.io/qdap_dev/pos.html" target="_blank"><code>pos_by</code></a> and <a href="http://trinker.github.io/qdap_dev/formality.html" target="_blank"><code>formality</code></a> functions.  This reuses the POS tagging which is the time intensive part (and can be extracted via <b><font  color="green" face="courier">YOUR_OUTPUT_HERE[[&quot;POStagged&quot;]]</font></b> from any of the above objects).

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/pos.html" target="_blank"><code>pos_tags</code></a>** - *Interpreting POS Tags*<font size="5" color="orange">&diams;</font>


```r
pos_tags()
```

```
##    Tag  Description                             
## 1  CC   Coordinating conjunction                
## 2  CD   Cardinal number                         
## 3  DT   Determiner                              
## 4  EX   Existential there                       
## 5  FW   Foreign word                            
## 6  IN   Preposition or subordinating conjunction
## 7  JJ   Adjective                               
## 8  JJR  Adjective, comparative                  
## 9  JJS  Adjective, superlative                  
## 10 LS   List item marker                        
## 11 MD   Modal                                   
## 12 NN   Noun, singular or mass                  
## 13 NNS  Noun, plural                            
## 14 NNP  Proper noun, singular                   
## 15 NNPS Proper noun, plural                     
## 16 PDT  Predeterminer                           
## 17 POS  Possessive ending                       
## 18 PRP  Personal pronoun                        
## 19 PRP$ Possessive pronoun                      
## 20 RB   Adverb                                  
## 21 RBR  Adverb, comparative                     
## 22 RBS  Adverb, superlative                     
## 23 RP   Particle                                
## 24 SYM  Symbol                                  
## 25 TO   to                                      
## 26 UH   Interjection                            
## 27 VB   Verb, base form                         
## 28 VBD  Verb, past tense                        
## 29 VBG  Verb, gerund or present participle      
## 30 VBN  Verb, past participle                   
## 31 VBP  Verb, non-3rd person singular present   
## 32 VBZ  Verb, 3rd person singular present       
## 33 WDT  Wh-determiner                           
## 34 WP   Wh-pronoun                              
## 35 WP$  Possessive wh-pronoun                   
## 36 WRB  Wh-adverb
```






<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/pos.html" target="_blank"><code>pos_by</code></a>** - *POS by Group(s)*<font size="5" color="orange">&diams;</font>


```r
posbydat <- with(DATA, pos_by(state, list(adult, sex)))
## Available elements
names(posbydat)
```

```
 [1] "text"         "POStagged"    "POSprop"      "POSfreq"     
 [5] "POSrnp"       "percent"      "zero.replace" "pos.by.freq" 
 [9] "pos.by.prop"  "pos.by.rnp"  
```




```r
## Inspecting the truncated output
lview(posbydat)
```


<pre><code>$text
[1] "computer is fun not too fun"     "no its not its dumb"           
[3] "what should we do"               "you liar it stinks"            
[5] "i am telling the truth"          "how can we be certain"         
[7] "there is no way"                 "i distrust you"                
[9] "what are you talking about"      "shall we move on good then"    
[11] "im hungry lets eat you already"


$POStagged
                               POStagged                   POStags word.count
1 computer/NN is/VBZ fun/NN ...RB fun/NN   NN, VBZ, NN, RB, RB, NN          6
2 no/DT its/PRP$ not/RB its/PRP$ dumb/JJ    DT, PRP$, RB, PRP$, JJ          5
3         what/WP should/MD we/PRP do/VB           WP, MD, PRP, VB          4
4      you/PRP liar/VBP it/PRP stinks/VB         PRP, VBP, PRP, VB          4
5 i/PRP am/VBP telling/VBG...DT truth/NN     PRP, VBP, VBG, DT, NN          5
6 how/WRB can/MD we/PRP ...VB certain/JJ      WRB, MD, PRP, VB, JJ          5
7           there/EX is/VBZ no/DT way/NN           EX, VBZ, DT, NN          4
8               i/FW distrust/NN you/PRP               FW, NN, PRP          3
9  what/WP are/VBP you/PR...VBG about/IN     WP, VBP, PRP, VBG, IN          5
10 shall/MD we/PRP move/VB ...JJ then/RB   MD, PRP, VB, IN, JJ, RB          6
11 im/PRP hungry/JJ let...PRP already/RB PRP, JJ, VBZ, VB, PRP, RB          6


$POSprop
   wrd.cnt propDT propEX   propFW   propIN   propJJ   propMD ... propWRB
1        6      0      0  0.00000  0.00000  0.00000  0.00000           0
2        5     20      0  0.00000  0.00000 20.00000  0.00000           0
3        4      0      0  0.00000  0.00000  0.00000 25.00000           0
4        4      0      0  0.00000  0.00000  0.00000  0.00000           0
5        5     20      0  0.00000  0.00000  0.00000  0.00000           0
6        5      0      0  0.00000  0.00000 20.00000 20.00000          20
7        4     25     25  0.00000  0.00000  0.00000  0.00000           0
8        3      0      0 33.33333  0.00000  0.00000  0.00000           0
9        5      0      0  0.00000 20.00000  0.00000  0.00000           0
10       6      0      0  0.00000 16.66667 16.66667 16.66667           0
11       6      0      0  0.00000  0.00000 16.66667  0.00000           0

$POSfreq
   wrd.cnt DT EX FW IN JJ MD NN PRP PRP$ RB VB VBG VBP VBZ WP WRB
1        6  0  0  0  0  0  0  3   0    0  2  0   0   0   1  0   0
2        5  1  0  0  0  1  0  0   0    2  1  0   0   0   0  0   0
3        4  0  0  0  0  0  1  0   1    0  0  1   0   0   0  1   0
4        4  0  0  0  0  0  0  0   2    0  0  1   0   1   0  0   0
5        5  1  0  0  0  0  0  1   1    0  0  0   1   1   0  0   0
6        5  0  0  0  0  1  1  0   1    0  0  1   0   0   0  0   1
7        4  1  1  0  0  0  0  1   0    0  0  0   0   0   1  0   0
8        3  0  0  1  0  0  0  1   1    0  0  0   0   0   0  0   0
9        5  0  0  0  1  0  0  0   1    0  0  0   1   1   0  1   0
10       6  0  0  0  1  1  1  0   1    0  1  1   0   0   0  0   0
11       6  0  0  0  0  1  0  0   2    0  1  1   0   0   1  0   0

$POSrnp
   wrd.cnt       DT       EX       FW       IN       JJ ...      WRB
1        6        0        0        0        0        0            0
2        5 1(20.0%)        0        0        0 1(20.0%)            0
3        4        0        0        0        0        0            0
4        4        0        0        0        0        0            0
5        5 1(20.0%)        0        0        0        0            0
6        5        0        0        0        0 1(20.0%)     1(20.0%)
7        4 1(25.0%) 1(25.0%)        0        0        0            0
8        3        0        0 1(33.3%)        0        0            0
9        5        0        0        0 1(20.0%)        0            0
10       6        0        0        0 1(16.7%) 1(16.7%)            0
11       6        0        0        0        0 1(16.7%)            0

$percent
[1] TRUE

$zero.replace
[1] 0

$pos.by.freq
  adult&sex wrd.cnt DT EX FW IN JJ MD NN PRP PRP$ RB VB VBG VBP VBZ WP WRB
1       0.f      10  0  0  0  1  1  1  0   2    0  0  1   1   1   0  1   1
2       0.m      33  3  1  1  0  2  0  6   6    2  4  2   1   2   3  0   0
3       1.f       6  0  0  0  1  1  1  0   1    0  1  1   0   0   0  0   0
4       1.m       4  0  0  0  0  0  1  0   1    0  0  1   0   0   0  1   0


$pos.by.prop
  adult&sex wrd.cnt       DT       EX       FW       IN        JJ ... WP
1       0.f      10 0.000000 0.000000 0.000000 10.00000 10.000000     10
2       0.m      33 9.090909 3.030303 3.030303  0.00000  6.060606      0
3       1.f       6 0.000000 0.000000 0.000000 16.66667 16.666667      0
4       1.m       4 0.000000 0.000000 0.000000  0.00000  0.000000     25


$pos.by.rnp
  adult&sex wrd.cnt      DT      EX      FW       IN       JJ ...       WP
1       0.f      10       0       0       0 1(10.0%) 1(10.0%)     1(10.0%)
2       0.m      33 3(9.1%) 1(3.0%) 1(3.0%)        0  2(6.1%)            0
3       1.f       6       0       0       0 1(16.7%) 1(16.7%)            0
4       1.m       4       0       0       0        0        0     1(25.0%)
</code></pre>


<font size="5" color="orange">&diams;</font> **Plot Method**<font size="5" color="orange">&diams;</font>


```r
plot(posbydat, values = TRUE, digits = 2)
```

![plot of chunk unnamed-chunk-176](figure/unnamed-chunk-176.png) 



<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/pos.html" target="_blank"><code>pos_by</code></a>** - *Recycling Saves Time*<font size="5" color="orange">&diams;</font>


```r
posbydat2 <- with(DATA, pos_by(posbydat, list(person, sex)))
system.time(with(DATA, pos_by(posbydat, list(person, sex))))
```


<pre><code>    user  system elapsed 
    0.07    0.00    0.07
</code></pre>


```r
## `pos_by` output Recycled for `formality`
with(DATA, formality(posbydat, list(person, sex)))
```

```
##     person&sex word.count formality
## 1       greg.m         20     35.00
## 2        sam.m         13     34.62
## 3 researcher.f          6     33.33
## 4      sally.f         10     20.00
## 5    teacher.m          4      0.00
```



<h4 id="syll">Syllabication and Counts</h4>

Examining syllable counts can be a useful source of information in associating with education level, age, SES, gender, etc.  Several readability scores rely on syllable and polysyllable word counts.  qdap defines a *polysyllable* word as a word with 3 or more syllables, though some in the linguistics/literacy fields may include two syllable words.  <a href="http://trinker.github.io/qdap_dev/syllabication.html" target="_blank"><code>syllable_count</code></a> is the base function for <a href="http://trinker.github.io/qdap_dev/syllabication.html" target="_blank"><code>syllable_sum</code></a>, <a href="http://trinker.github.io/qdap_dev/syllabication.html" target="_blank"><code>polysyllable_sum</code></a>, and <a href="http://trinker.github.io/qdap_dev/syllabication.html" target="_blank"><code>combo_syllable_sum</code></a>, though is generally not of direct use to the researcher conducting discourse analysis.  <a href="http://trinker.github.io/qdap_dev/syllabication.html" target="_blank"><code>syllable_count</code></a> uses a dictionary lookup method augmented with a syllable algorithm for words not found in the dictionary.  Words not found in the dictionary are denoted with a <b>NF</b> in the  <b>in.dictionary</b> column of the output.


Here is a list of qdap <a href="http://trinker.github.io/qdap_dev/syllabication.html" target="_blank"><code>syllabication</code></a> functions and their descriptions:<br>

<TABLE BORDER=2 CELLPADDING=2 CELLSPACING=2 WIDTH="90%">
  <TR> <TD> <b>syllable_count</b> </TD> <TD> Count the number of syllables in a single text string. </TD> </TR>
  <TR> <TD> <b>syllable_sum</b> </TD> <TD> Count the number of syllables per row of text. </TD> </TR>
  <TR> <TD> <b>polysyllable_sum</b> </TD> <TD> Count the number of polysyllables per row of text. </TD> </TR>
  <TR> <TD> <b>combo_syllable_sum</b> </TD> <TD> Count the number of both syllables and polysyllables per row of text. </TD> </TR>
   </TABLE>


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/syllabication.html" target="_blank"><code>syllabication</code></a> Examples**<font size="5" color="orange">&diams;</font>



```r
syllable_count("Robots like Dason lie.")
```

```
   words syllables in.dictionary
1 robots         2             -
2   like         1             -
3  dason         2            NF
4    lie         1             -
```

```r
## The text variable for reference
DATA$state
```

```
 [1] "Computer is fun. Not too fun."        
 [2] "No it's not, it's dumb."              
 [3] "What should we do?"                   
 [4] "You liar, it stinks!"                 
 [5] "I am telling the truth!"              
 [6] "How can we be certain?"               
 [7] "There is no way."                     
 [8] "I distrust you."                      
 [9] "What are you talking about?"          
[10] "Shall we move on?  Good then."        
[11] "I'm hungry.  Let's eat.  You already?"
```

```r
syllable_sum(DATA$state)
```

```
 [1] 8 5 4 5 6 6 4 4 7 6 9
```

```r
polysyllable_sum(DATA$state)
```

```
 [1] 1 0 0 0 0 0 0 0 0 0 1
```

```r
combo_syllable_sum(DATA$state)
```

```
   syllable.count polysyllable.count
1               8                  1
2               5                  0
3               4                  0
4               5                  0
5               6                  0
6               6                  0
7               4                  0
8               4                  0
9               7                  0
10              6                  0
11              9                  1
```




<h3 id="measures">Word Measures and Scoring</h3>

<div class="funs">
The following functions will be utilized in this section (click to view more):    

<form action="http://trinker.github.io/qdap_dev/automated_readability_index.html" target="_blank">
    <input type="submit" value="automated_readability_index"><input type="submit" value="coleman_liau"><input type="submit" value="flesch_kincaid"><input type="submit" value="fry"><input type="submit" value="linsear_write"><input type="submit" value="SMOG"> - <a href="#readability">Readability Measures</a>
</form>

<form action="http://trinker.github.io/qdap_dev/dissimilarity.html" target="_blank">
    <input type="submit" value="dissimilarity"> - <a href="#dissimilarity">Dissimilarity</a>
</form>

<form action="http://trinker.github.io/qdap_dev/diversity.html" target="_blank">
    <input type="submit" value="diversity"> - <a href="#diversity">Diversity Statistics</a>
</form>

<form action="http://trinker.github.io/qdap_dev/formality.html" target="_blank">
    <input type="submit" value="formality"> - <a href="#formality">Formality Score</a>
</form>

<form action="http://trinker.github.io/qdap_dev/kullback_leibler.html" target="_blank">
    <input type="submit" value="kullback_leibler"> - <a href="#kullback">Kullback-Leibler divergence </a>
</form>

<form action="http://trinker.github.io/qdap_dev/polarity.html" target="_blank">
    <input type="submit" value="polarity"> - <a href="#polarity">Polarity Score (Sentiment Analysis)</a>
</form>
</div>

qdap offers a number of word statistics and and scores applied by grouping variable.  Some functions are original to qdap, while others are taken from academic papers.  Complete references for statistics/scores based on others' work are provided in the <a href="http://cran.r-project.org/web/packages/qdap/qdap.pdf">help manual</a> where appropriate.  It is assumed that the reader is familiar, or can become acquainted, with the theory and methods for qdap functions based on the work of others.  For qdap functions that are original to qdap a more narrow description of the use and theory is provided.

<h4 id="readability">Readability Scores</h4>

Readability scores were originally designed to measure the defficulty of text.  Scores are generally based on, number of words, syllables, polly-syllables amnd word length.  While these scores are not specifically designed for, or tested on, speech, they can be useful indicators of speech complexity.  The following score examples demonstrate the use of the following readability scores:

1. <a href="#ari">Automated Readability Index</a>    
2. <a href="#coleman">Coleman Liau</a>    
3. <a href="#smog">SMOG</a>    
4. <a href="#flesch">Flesch Kincaid</a>     
5. <a href="#fry">Fry</a>    
6.  <a href="#linwr">Linsear Write</a>    


<p id="ari"><font size="5" color="orange">&diams;</font> **Automated Readability Index**<font size="5" color="orange">&diams;</font></p >


```r
with(rajSPLIT, automated_readability_index(dialogue, list(sex, fam.aff)))
```


<pre><code>  sex&fam.aff word.count sentence.count character.count Aut._Read._Index
1       f.cap       9458            929           37474              2.3
2      f.mont         28              4              88             -3.1
3       m.cap       1204            133            4615              1.2
4     m.escal       3292            262           13406              4.0
5      m.mont       6356            555           26025              3.6
6      m.none       3233            250           13527              4.7
7   none.none        156             12             665              5.1
</code></pre>

<p id="coleman"><font size="5" color="orange">&diams;</font> **Coleman Liau**<font size="5" color="orange">&diams;</font></p >


```r
with(rajSPLIT, coleman_liau(dialogue, list(fam.aff, act)))
```


<pre><code>  fam.aff&act word.count sentence.count character.count Coleman_Liau
1       cap.1       2636            272           10228          4.0
2       cap.2       2113            193            8223          4.4
3       cap.3       3540            339           14183          4.9
4       cap.4       2159            232            8620          4.5
5       cap.5        214             26             835          3.5
6     escal.1        748             36            3259          8.4
</code></pre>

<p id="smog"><font size="5" color="orange">&diams;</font> **SMOG**<font size="5" color="orange">&diams;</font></p >


```r
with(rajSPLIT, SMOG(dialogue, list(person, act)))
```


<pre><code>         person&act word.count sentence.count polysyllable.count SMOG
1        Benvolio.1        621             51                 25  7.1
2         Capulet.1        736             72                 35  7.1
3         Capulet.3        749             69                 28  6.8
4         Capulet.4        569             73                 25  6.5
5  Friar Laurence.2        699             42                 36  8.4
6  Friar Laurence.3        675             61                 32  7.3
7  Friar Laurence.4        656             42                 25  7.5
8  Friar Laurence.5        696             54                 32  7.5
9          Juliet.2       1289            113                 48  6.9
10         Juliet.3       1722            152                 64  6.8
11         Juliet.4        932             61                 37  7.6
12   Lady Capulet.3        393             39                 15  6.7
13       Mercutio.2        964             82                 43  7.3
14       Mercutio.3        578             54                 19  6.5
15          Nurse.1        599             59                 20  6.5
16          Nurse.2        779             76                 24  6.3
17          Nurse.3        579             68                 14  5.7
18          Nurse.4        250             50                  9  5.6
19          Romeo.1       1158            113                 48  6.9
20          Romeo.2       1289            109                 46  6.8
21          Romeo.3        969             87                 48  7.4
22          Romeo.5       1216            103                 52  7.2
</code></pre>

<p id="flesch"><font size="5" color="orange">&diams;</font> **Flesch Kincaid**<font size="5" color="orange">&diams;</font></p >


```r
with(rajSPLIT, flesch_kincaid(dialogue, list(sex, fam.aff)))
```


<pre><code>  sex&fam.aff word.count sentence.count syllable.count FK_grd.lvl FK_read.ease
1       f.cap       9458            929          11641        2.9       92.375
2      f.mont         28              4             30       -0.2      109.087
3       m.cap       1204            133           1452        2.2       95.621
4     m.escal       3292            262           4139        4.1       87.715
5      m.mont       6356            555           7965        3.7       89.195
6      m.none       3233            250           4097        4.4       86.500
7   none.none        156             12            195        4.2       87.890
</code></pre>

Note that the Fry score is a graphical display, rather than text as the other readability scores are.  This is in keeping with the original procedures outlined by Fry.

<p id="fry"><font size="5" color="orange">&diams;</font> **Fry**<font size="5" color="orange">&diams;</font></p >


```r
with(rajSPLIT, fry(dialogue, list(sex, fam.aff)))
```



![plot of chunk unnamed-chunk-185](figure/unnamed-chunk-185.png) 



<p id="linwr"><font size="5" color="orange">&diams;</font> **Linsear Write**<font size="5" color="orange">&diams;</font></p >


```r
with(rajSPLIT, linsear_write(dialogue, person))
```


<pre><code>           person sent.per.100 hard_easy_sum Linsear_Write
1       Balthasar        9.556           110          4.76
2        Benvolio        4.143           108         12.03
3         Capulet       11.469           115          4.01
4          Chorus        3.071           104         15.93
5  First Watchman       14.222           114          3.01
6  Friar Laurence        4.263           108         11.67
7         Gregory       11.000           100          3.55
8          Juliet        3.446           110         14.96
9    Lady Capulet        7.267           110          6.57
10       Mercutio        5.625           102          8.07
11       Montague        6.000           114          8.50
12          Nurse       12.098           102          3.22
13          Paris        9.091           110          5.05
14          Peter       10.357           110          4.31
15         Prince       10.842           110          4.07
16          Romeo        9.250           114          5.16
17        Sampson        9.421           107          4.68
18        Servant        9.667           104          4.38
19         Tybalt        9.591           112          4.84
</code></pre>


<h4 id="dissimilarity">Dissimilarity</h4>

*Dissimilarity* is another term for distance that is often used in text analysis to measure the pairwise proximity of grouping variables.  The qdap <a href="http://trinker.github.io/qdap_dev/dissimilarity.html" target="_blank"><code>dissimilarity</code></a> function is a wrapper for the R stats package's <a href="http://stat.ethz.ch/R-manual/R-devel/library/stats/html/dist.html">dist</a> function designed to handle text.  <a href="http://trinker.github.io/qdap_dev/dissimilarity.html" target="_blank"><code>dissimilarity</code></a> takes all the same <b><font color="green" face="courier new">method</font></b> types as <a href="http://stat.ethz.ch/R-manual/R-devel/library/stats/html/dist.html">dist</a> but also includes the defualt <b><font color="green" face="courier new">method = "prop"</font></b> (1 - "binary") that is focused on the similarity between grouping variables.

<p id="linwr"><font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/dissimilarity.html" target="_blank"><code>dissimilarity</code></a> Examples**<font size="5" color="orange">&diams;</font></p >


```r
with(DATA, dissimilarity(state, list(sex, adult)))
```

```
      f.0   f.1   m.0
f.1 0.067            
m.0 0.029 0.000      
m.1 0.167 0.111 0.000
```

```r
with(DATA, dissimilarity(state, person))
```

```
            greg researcher sally   sam
researcher 0.000                       
sally      0.037      0.067            
sam        0.160      0.000 0.050      
teacher    0.000      0.111 0.167 0.000
```

```r
with(DATA, dissimilarity(state, person, method = "minkowski"))
```

```
            greg researcher sally   sam
researcher 5.477                       
sally      5.657      3.742            
sam        5.568      4.796 4.796      
teacher    5.292      2.828 3.162 4.583
```



```r
dat <- pres_debates2012[pres_debates2012$person %in% qcv(OBAMA, ROMNEY),]
with(dat, dissimilarity(dialogue, list(person, time)))
```


<pre><code>         OBAMA.1 OBAMA.2 OBAMA.3 ROMNEY.1 ROMNEY.2
OBAMA.2    0.340                                  
OBAMA.3    0.300   0.341                          
ROMNEY.1   0.340   0.287   0.258                  
ROMNEY.2   0.291   0.349   0.296    0.321         
ROMNEY.3   0.264   0.297   0.329    0.290    0.338
</code></pre>

<h4 id="kullback">Kullback-Leibler divergence </h4>

The Kullback Leibler is often used as a measure of distance, though the matrix is assymetrical.  qdap's 

```

Error in base::parse(text = code, srcfile = NULL) : 
  1:7: unexpected symbol
1: FUN(""kullback_leibler
          ^

```

 compares the differences between two probability distributions and often leads to results similar to those from <a href="http://trinker.github.io/qdap_dev/dissimilarity.html" target="_blank"><code>dissimilarity</code></a>.  Note that unlike many other qdap functions the user must either supply a word frequency matric (<a href="http://trinker.github.io/qdap_dev/wfm.html" target="_blank"><code>wfm</code></a>) to  <b><font color="green" face="courier new">x</font></b> some other matrix format.  This allows the function to be flexibly used with <a href="http://trinker.github.io/qdap_dev/termco.html" target="_blank"><code>termco</code></a> and other functions that produce count matrices.




<p id="linwr"><font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/kullback_leibler.html" target="_blank"><code>kullback_leibler</code></a> Example** - *Compare to <a href="http://trinker.github.io/qdap_dev/dissimilarity.html" target="_blank"><code>dissimilarity</code></a>*<font size="5" color="orange">&diams;</font></p >


```r
dat <- pres_debates2012[pres_debates2012$person %in% qcv(OBAMA, ROMNEY),]
(KL <- (kullback_leibler(with(dat, wfm(dialogue, list(person, time))))))
```


<pre><code>         OBAMA.1 OBAMA.2 OBAMA.3 ROMNEY.1 ROMNEY.2 ROMNEY.3
OBAMA.1    0.000   0.237   0.221    0.195    0.250    0.264
OBAMA.2    0.104   0.000   0.161    0.148    0.142    0.223
OBAMA.3    0.119   0.152   0.000    0.142    0.180    0.168
ROMNEY.1   0.207   0.297   0.279    0.000    0.216    0.224
ROMNEY.2   0.194   0.195   0.262    0.116    0.000    0.234
ROMNEY.3   0.160   0.182   0.141    0.101    0.140    0.000
</code></pre>


```r
plot(KL, high = "red", values = TRUE)
```

![plot of chunk unnamed-chunk-191](figure/unnamed-chunk-191.png) 



<h4 id="diversity">Diversity Statistics</h4>

Diversity, as applied to dialogue, is a measure of the richness of language being used.  Specifically, it measures how expansive the vocabulary is while taking into account the number of total words used and the different words being used.  qdap's <a href="http://trinker.github.io/qdap_dev/diversity.html" target="_blank"><code>diversity</code></a> function provdes output for the Simpson, Shannon, Collision, Berger Parker, and Brillouin measures.

<p id="linwr"><font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/diversity.html" target="_blank"><code>diversity</code></a> Example**<font size="5" color="orange">&diams;</font></p >

```r
(div.mod <- with(mraja1spl, diversity(dialogue, person)))
```

```
           person   wc simpson shannon collision berger_parker brillouin
1         Abraham   24   0.942   2.405     2.331         0.167     1.873
2        Benvolio  621   0.994   5.432     4.874         0.037     4.809
3         Capulet  736   0.993   5.358     4.805         0.027     4.813
4   First Citizen   16   0.958   2.393     2.287         0.188     1.718
5   First Servant   69   0.983   3.664     3.464         0.072     2.961
6         Gregory  149   0.991   4.405     4.141         0.054     3.686
7          Juliet  206   0.993   4.676     4.398         0.039     3.971
8    Lady Capulet  288   0.993   4.921     4.519         0.042     4.231
9   Lady Montague   28   0.995   3.233     3.199         0.071     2.375
10       Mercutio  549   0.991   5.302     4.564         0.051     4.663
11       Montague  217   0.993   4.805     4.496         0.041     4.063
12          Nurse  599   0.991   5.111     4.561         0.040     4.588
13          Paris   32   0.990   3.276     3.194         0.094     2.449
14         Prince  167   0.990   4.463     4.161         0.048     3.757
15          Romeo 1163   0.994   5.650     4.917         0.026     5.124
16        Sampson  259   0.987   4.531     4.106         0.066     3.947
17 Second Capulet   17   0.963   2.425     2.371         0.118     1.767
18 Second Servant   41   0.993   3.532     3.457         0.073     2.687
19        Servant  184   0.987   4.389     4.023         0.060     3.744
20         Tybalt  160   0.993   4.539     4.345         0.044     3.801
```


<p id="linwr"><font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/diversity.html" target="_blank"><code>diversity</code></a> Plot Method**<font size="5" color="orange">&diams;</font></p >


```r
plot(div.mod, low = "yellow", grid = FALSE, values = TRUE)
```

![plot of chunk unnamed-chunk-193](figure/unnamed-chunk-193.png) 


<h4 id="formality">Formality</h4>

<h4 id="polarity">Polarity Score (Sentiment Analysis)</h4>

Polarity assignment, a form of sentiment analysis, is using an algorithm to detmine the polarity of a sentence/statement.  While the use polarity scores is applied to many forms of written social dialogue (e.g., Twitter, Facebook, etc.) it has not been applied to spoken dialogue.  qdap offers a flexible function, <a href="http://trinker.github.io/qdap_dev/polarity.html" target="_blank"><code>polarity</code></a> to determine polarity at the sentence level as well as to assign an average polarity score to individual groups within the grouping variable(s).  The frame work for <a href="http://trinker.github.io/qdap_dev/polarity.html" target="_blank"><code>polarity</code></a> is flexible in that the user may supply a polarized dictionary and optional weights.  Many dictionaries used in sentiment analysis are designed for written, adult, online interaction.  

<div class="middleDiv">
<b><font size="4" color="red">It is assumed you have run <font face="courier">sentSplit</font> on the data.<br>If this is not the case the counts will not be accurate.</font></b>
</div>

The polarity score generated by <a href="http://trinker.github.io/qdap_dev/polarity.html" target="_blank"><code>polarity</code></a> is dependent upon the polarity dictionary used.  This function defaults to the word polarity dictionary used by <a href="http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html">Hu & Liu (2004)</a>, however, this may not be appropriate for the context of children in a classroom.  For instance the word <font color="green">"sick"</font> in a high school setting may mean that something is good, whereas <font color="green">"sick"</font> used by a typical adult indicates something is not right or negative connotation.  The user may (is encouraged) to provide/augment/alter the dictionary (see the <a href="http://trinker.github.io/qdap_dev/polarity.html" target="_blank"><code>polarity_frame</code></a> function).  Development of context specific dictionaries, that are better suited for spoken dialogue in a school setting is an exciting prospect that could lead to greater understanding of the emotional aspects of the spoken word on students.  The user may add a dictionary with optional weights as a dataframe within an environment.  The <a href="http://trinker.github.io/qdap_dev/polarity.html" target="_blank"><code>polarity_frame</code></a> function aides the user in creating the polarity environment. 





The equation used by the algorithm to assign value to polarity of each sentence fist utilizes the sentiment dictionary (Hu & Liu, 2004) to tag polarized words.  A context cluster of words is pulled from around this polarized word (default 4 words before and two words after) to be considered as valence shifters.  The words in this context cluster are tagged as neutral ($x_i^{0}$), negator ($x_i^{N}$), amplifier ($x_i^{a}$), or de-amplifier ($x_i^{d}$). Neutral words hold no value in the equation but do affect word count ($n$).  Each polarized word is then weighted $w$ based on the weights from the <b><font color="green" face="courier new">polarity.frame</font></b> argument and then further weighted by the number and position of the valence shifters directly surrounding the positive or negative word.  The researcher may provide a 
weight $c$ to be utilized with amplifiers/de-amplifiers (default is .8; deamplifier weight is constrained to -1 lower bound).  Last, these values are then summed and divided by the word count ($n$) yielding an unbounded polarity score ($\delta$).  Note that context clusters containing a comma before the polarized word will only consider words found after the comma.

$$
\delta=\frac{\sum{((1 + c(x_i^{A} - x_i^{D}))\cdot w(-1)^{\sum{x_i^{N}}})}}{n}
$$

Where:

$$
x_i^{A}=\sum{(w_{neg}\cdot x_i^{a})}
$$
<br>
$$
x_i^{D}=\sum{((1 - w_{neg})\cdot x_i^{a} + x_i^{d})}
$$
<br>
$$
w_{neg}=\left\{\begin{array}{cc}
1 & \sum{x_i^{N}}>0         \\\\ 
0 & \sum{x_i^{N}}=0
\end{array}\right.
$$

The following examples demonstrate how the <a href="http://trinker.github.io/qdap_dev/polarity.html" target="_blank"><code>polarity</code></a> and <a href="http://trinker.github.io/qdap_dev/polarity.html" target="_blank"><code>polarity_frame</code></a> functions operate.  Here the polarity for the <a href="http://trinker.github.io/qdap_dev/mraja1spl.html" target="_blank"><code>mraja1spl</code></a> data set (Act 1 of Romeo and Juliet).  The gender, family affiliation and binary died/didn't die are used as the grouping variables.

<p id="linwr"><font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/polarity.html" target="_blank"><code>polarity</code></a> Example**<font size="5" color="orange">&diams;</font></p >






```r
(poldat <- with(mraja1spl, polarity(state, list(sex, fam.aff, died))))
```


<pre><code>POLARITY BY GROUP
=================

  sex&fam.aff&died tot.sent tot.word ave.polarity sd.polarity sd.mean.polarity
1      f.cap.FALSE      158     1810        0.019       0.098            0.194
2       f.cap.TRUE       24      221        0.011       0.065            0.177
3      f.mont.TRUE        4       29        0.025       0.126            0.199
4      m.cap.FALSE       73      717        0.033       0.136            0.241
5       m.cap.TRUE       17      185       -0.047       0.095           -0.496
6    m.escal.FALSE        9      195       -0.034       0.070           -0.489
7     m.escal.TRUE       27      646       -0.013       0.076           -0.170
8     m.mont.FALSE       70      952       -0.015       0.186           -0.078
9      m.mont.TRUE      114     1273        0.004       0.133            0.027
10    m.none.FALSE        7       78        0.014       0.024            0.575
11 none.none.FALSE        5       18       -0.233       0.435           -0.537
</code></pre>


```r
names(poldat)
```

```
## [1] "all"    "group"  "digits"
```


<p id="linwr"><font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/polarity.html" target="_blank"><code>polarity</code></a>** - *Sentence Level Polarity Scores*<font size="5" color="orange">&diams;</font></p >


```r
htruncdf(poldat$all, 20, 10)
```

```
##    sex&fam.af wc   polarity pos.words  neg.words   text.var
## 1  m.cap.FALS 10       -0.1      well          - Gregory, o
## 2  m.cap.FALS  8          0         -          - No, for th
## 3  m.cap.FALS 11 0.09090909      well          - I mean, an
## 4  m.cap.FALS 13          0         -          - Ay, while 
## 5  m.cap.FALS  6 -0.1666666         -     strike I strike q
## 6  m.cap.FALS  8      0.125         -     strike But thou a
## 7  m.cap.FALS  9          0         -          - A dog of t
## 8  m.cap.FALS 12 0.08333333   valiant          - To move is
## 9  m.cap.FALS 10          0         -          - therefore,
## 10 m.cap.FALS 10          0         -          - A dog of t
## 11 m.cap.FALS 12          0         -          - I will tak
## 12 m.cap.FALS 13 -0.1538461         - c("weak",  That shows
## 13 m.cap.FALS 16    -0.0625         -     weaker True; and 
## 14 m.cap.FALS 17          0         -          - therefore 
## 15 m.cap.FALS 10          0   masters    quarrel The quarre
## 16 m.cap.FALS 10       -0.1         -     tyrant 'Tis all o
## 17 m.cap.FALS 21 -0.0476190         -      cruel when I hav
## 18 m.cap.FALS  5          0         -          - The heads 
## 19 m.cap.FALS 18 -0.0555555         -       wilt Ay, the he
## 20 m.cap.FALS  9          0         -          - They must
```


<p id="linwr"><font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/polarity.html" target="_blank"><code>polarity</code></a> Plot Method**<font size="5" color="orange">&diams;</font></p >


```r
plot(poldat)
```

![plot of chunk unnamed-chunk-199](figure/unnamed-chunk-199.png) 


<p id="linwr"><font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/polarity.html" target="_blank"><code>polarity</code></a> Plot Group Polarity as Heat Map**<font size="5" color="orange">&diams;</font></p >


```r
qheat(poldat[["group"]], high="blue", low="yellow", grid=NULL, order.b="ave.polarity")
```

![plot of chunk unnamed-chunk-200](figure/unnamed-chunk-200.png) 


<p id="linwr"><font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/polarity_frame.html" target="_blank"><code>polarity_frame</code></a>** - *Specify Your Own Polarity Environment*<font size="5" color="orange">&diams;</font></p >


```r
(POLENV <- polarity_frame(positive.words, negative.words))
```

```
<environment: 0x11586500>
```

```r
ls(POLENV)[1:20]
```

```
 [1] "a plus"        "abnormal"      "abolish"       "abominable"   
 [5] "abominably"    "abominate"     "abomination"   "abort"        
 [9] "aborted"       "aborts"        "abound"        "abounds"      
[13] "abrade"        "abrasive"      "abrupt"        "abruptly"     
[17] "abscond"       "absence"       "absent minded" "absentee"     
```



<h3 id="visualization">Visualizing Discourse Data</h3>

<div class="funs">
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

<form action="http://trinker.github.io/qdap_dev/trans_cloud.html" target="_blank">
    <input type="submit" value="trans_cloud"> - Word Clouds by Grouping Variable
</form>

<form action="http://trinker.github.io/qdap_dev/trans_venn.html" target="_blank">
    <input type="submit" value="trans_venn"> - Venn Diagram by Grouping Variable
</form>

<form action="http://trinker.github.io/qdap_dev/word_network_plot.html" target="_blank">
    <input type="submit" value="word_network_plot"> - Word Network Plot
</form>
</div>




<h3 id="id">ID Sentences</h3>

<div class="funs">
The following functions will be utilized in this section (click to view more):    

<form action="http://trinker.github.io/qdap_dev/end_inc.html" target="_blank">
    <input type="submit" value="end_inc"> - Test for Incomplete Sentences
</form>

<form action="http://trinker.github.io/qdap_dev/end_mark.html" target="_blank">
    <input type="submit" value="end_mark"> - Sentence End Marks
</form>

<form action="http://trinker.github.io/qdap_dev/imperative.html" target="_blank">
    <input type="submit" value="imperative"> - Detect and Remark Imperative Sentences
</form>

<form action="http://trinker.github.io/qdap_dev/NAer.html" target="_blank">
    <input type="submit" value="NAer"> - Replace Missing Values (NA)
</form>
</div>

<h3 id="data">Data Sets</h3>

<div class="textbox", style="background-color: #D6EFD6;"> 
The following data sets are included with qdap (click to view more)
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
</div>




<h3 id="dict">Dictionaries and Word Lists</h3>

<div class="textbox", style="background-color: #D6EFD6;"> 
The following dictionaries/word lists are utilized by qdap (click to view more)

<form action="http://trinker.github.io/qdapDictionaries//abbreviations.html" target="_blank" ">
    <input type="submit" value="abbreviations"> - Small Abbreviations Data Set
</form>

<form action="http://trinker.github.io/qdapDictionaries//action.verbs.html" target="_blank" ">
    <input type="submit" value="action.verbs"> - Action Word List
</form>

<form action="http://trinker.github.io/qdapDictionaries//adverb.html" target="_blank" ">
    <input type="submit" value="adverb"> - Adverb Word List
</form>

<form action="http://trinker.github.io/qdapDictionaries//BuckleySaltonSWL.html" target="_blank" ">
    <input type="submit" value="BuckleySaltonSWL"> - Buckley & Salton Stopword List
</form>

<form action="http://trinker.github.io/qdapDictionaries//contractions.html" target="_blank" ">
    <input type="submit" value="contractions"> - Contraction Conversions
</form>

<form action="http://trinker.github.io/qdapDictionaries//DICTIONARY.html" target="_blank" ">
    <input type="submit" value="DICTIONARY"> - Nettalk Corpus Syllable Data Set
</form>

<form action="http://trinker.github.io/qdapDictionaries//emoticon.html" target="_blank" ">
    <input type="submit" value="emoticon"> - Emoticons Data Set
</form>

<form action="http://trinker.github.io/qdapDictionaries//env.syl.html" target="_blank" ">
    <input type="submit" value="env.syl"> - Syllable Lookup Environment
</form>

<form action="http://trinker.github.io/qdapDictionaries//env.syn.html" target="_blank" ">
    <input type="submit" value="env.syn"> - Syllable Lookup Environment
</form>

<form action="http://trinker.github.io/qdapDictionaries//increase.amplification.words.html" target="_blank" ">
    <input type="submit" value="increase.amplification.words"> - Amplifying Words
</form>

<form action="http://trinker.github.io/qdapDictionaries//interjections.html" target="_blank" ">
    <input type="submit" value="interjections"> - Interjections
</form>

<form action="http://trinker.github.io/qdapDictionaries//labMT.html" target="_blank" ">
    <input type="submit" value="labMT"> - Language Assessment by Mechanical Turk (labMT) Sentiment Words
</form>

<form action="http://trinker.github.io/qdapDictionaries//NAMES.html" target="_blank" ">
    <input type="submit" value="NAMES"> - First Names and Gender (U.S.)
</form>

<form action="http://trinker.github.io/qdapDictionaries//NAMES_SEX.html" target="_blank" ">
    <input type="submit" value="NAMES_SEX"> - First Names and Predictive Gender (U.S.)
</form>

<form action="http://trinker.github.io/qdapDictionaries//NAMES_LIST.html" target="_blank" ">
    <input type="submit" value="NAMES_LIST"> - First Names and Predictive Gender (U.S.) List
</form>

<form action="http://trinker.github.io/qdapDictionaries//negation.words.html" target="_blank" ">
    <input type="submit" value="negation.words"> - Negating Words
</form>

<form action="http://trinker.github.io/qdapDictionaries//negative.words.html" target="_blank" ">
    <input type="submit" value="negative.words"> - Negative Words
</form>

<form action="http://trinker.github.io/qdapDictionaries//OnixTxtRetToolkitSWL1.html" target="_blank" ">
    <input type="submit" value="OnixTxtRetToolkitSWL1"> - Onix Text Retrieval Toolkit Stopword List 1
</form>

<form action="http://trinker.github.io/qdapDictionaries//positive.words.html" target="_blank" ">
    <input type="submit" value="positive.words"> - Positive Words
</form>

<form action="http://trinker.github.io/qdapDictionaries//preposition.html" target="_blank" ">
    <input type="submit" value="preposition"> - Preposition Words
</form>

<form action="http://trinker.github.io/qdapDictionaries//SYNONYM.html" target="_blank" ">
    <input type="submit" value="SYNONYM"> - Synonyms Data Set
</form>

<form action="http://trinker.github.io/qdapDictionaries//Top100Words.html" target="_blank" ">
    <input type="submit" value="Top100Words"> - Fry's  100 Most Commonly Used English Words
</form>

<form action="http://trinker.github.io/qdapDictionaries//Top200Words.html" target="_blank" ">
    <input type="submit" value="Top200Words"> - Fry's 200 Most Commonly Used English Words
</form>

<form action="http://trinker.github.io/qdapDictionaries//Top25Words.html" target="_blank" ">
    <input type="submit" value="Top25Words"> - Fry's 25 Most Commonly Used English Words
</form>
</div>

<h3 id="install">Installation Issues</h3>

<h4 id="java">Java Issues</h3>
  
<p>If there is a discrepancy between the <a href="https://dl.dropbox.com/u/61803503/rjava_warning.txt">R and Java architectures</a> you will have to <a href="http://www.java.com/en/download/manual.jsp">download</a> the appropriate version of Java compatible with the version of R you're using.    

For more see <a href="http://www.r-statistics.com/2012/08/how-to-load-the-rjava-package-after-the-error-java_home-cannot-be-determined-from-the-registry/" target="_blank">Tal Galili's blog post</a> regarding rJava issues.


<hr>
## Acknowledgements

The qdap package was my first R package and a learning process. Several people contributed immensely to my learning. I'd like to particularly thank <a href="https://github.com/Dasonk/" target="_blank">Dason Kurkiewicz</a> for his constant mentoring/assistance in learning the R language, GitHub and package development as well as collaboration on numerous qdap functions. Thank you to <a href="https://twitter.com/bryangoodrich" target="_blank">Bryan Goodrich</a> for his teaching, feedback and collaboration on serveral qdap functions. Thank you to <a href="https://github.com/hadley" target="_blank">Dr. Hadley Wickham</a> for roxygen2, ggplot2, devtools and GitHub repos which I referenced often. I'd also like to thank the many folks at <a href="http://www.talkstats.com/" target="_blank">talkstats.com</a> and <a href="http://stackoverflow.com/questions/tagged/r" target="_blank">stackoverflow.com</a> for their help in answering many R questions related to qdap.

<hr> 

## Improvements

If the reader spots an error in this Vignette or would like to suggest an improvement please contact me @ Tyler Rinker&lt;<a href="mailto:tyler.rinker@gmail.com" target="_blank">tyler.rinker@gmail.com</a>&gt;.  To submit bug reports and feature requests related to the qdap package please visit <a href="https://github.com/trinker/qdap/issues?state=open" target="_blank">qdap's GitHub issues page</a>.

<hr> 

*<em><font size="3">Vignette created with the reports package (<a href="http://github.com/trinker/reports">Rinker, 2013b</a>)</font><em>





## References

- J. Fox,   (2005) Programmer's niche: How do you spell that number?.  <em>R News</em>  <strong>5</strong>  (1)   51-55-NA
- Hu Minqing, Liu Bing,   (2004) Mining Opinion Features in Customer Reviews.  <a href="http://www.cs.uic.edu/~liub/publications/aaai04-featureExtract.pdf">http://www.cs.uic.edu/~liub/publications/aaai04-featureExtract.pdf</a>
- Tyler Rinker,   (2013) qdap: Quantitative Discourse Analysis Package.  <a href="http://github.com/trinker/qdap">http://github.com/trinker/qdap</a>
- Tyler Rinker,   (2013) reports: Package to asssist in report writing.  <a href="http://github.com/trinker/reports">http://github.com/trinker/reports</a>



