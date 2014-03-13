<!--
%\VignetteEngine{knitr}
%\VignetteIndexEntry{An Introduction to qdap}
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
<div>13.  <a href="#tm">tm Package Compatability</a>    </div> 
<div>14.  <a href="#data">Data Sets</a>    </div> 
<div>15.  <a href="#dict">Dictionaries and Word Lists</a>    </div>   
<div>16.  <a href="#install">Installation Issues</a>    </div>     
<div>17.  <a href="#connect">Recommended Packages (Extending qdap)</a>    </div>   

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

This subsection covers how to read in transcript data.  Generally the researcher will have data stored as a .docx (Microsoft Word or Open/Libre Office) or .xlsx/.csv (spreadsheet format).  It is of great importance that the researcher manually writes/parses their transcripts to avoid potential analysis problems later.  All sentences should contain appropriate qdap punctuation (declarative = ., interrogative = ?, exclamatory = !, interrupted = | or <a href="http://trinker.github.io/qdap_dev/imperative.html" target="_blank"><code>imperative</code></a> = *., *?, *!, *|).  Additionally, if a sentence contains an end mark/punctuation it should have accompanying text/dialogue.  Two functions are useful for reading in data, <a href="http://trinker.github.io/qdap_dev/read.transcript.html" target="_blank"><code>read.transcript</code></a> and <a href="http://trinker.github.io/qdap_dev/dir_map.html" target="_blank"><code>dir_map</code></a>.  <a href="http://trinker.github.io/qdap_dev/read.transcript.html" target="_blank"><code>read.transcript</code></a> detects file type (.docx/.csv/.xlsx) and reads in a single transcript whereas <a href="http://trinker.github.io/qdap_dev/dir_map.html" target="_blank"><code>dir_map</code></a> generates code that utilizes <a href="http://trinker.github.io/qdap_dev/read.transcript.html" target="_blank"><code>read.transcript</code></a> for each of the multiple transcripts in a single directory.  Note that <a href="http://trinker.github.io/qdap_dev/read.transcript.html" target="_blank"><code>read.transcript</code></a> expects a two column formatted transcript (usually with person on the left and dialogue on the right).

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

Often transcripts contain extraneous material at the top and the argument <font face="courier">skip = ?</font> must be used to skip these extra lines.  Some sort of unique separator must also be used to separate the person column from the text column.  By default <font face="courier">sep = ":"</font> is assumed.  If your transcripts do not contain a separator one must be inserted manually.  Also note that the researcher may want to prepare the transcripts with brackets to denote non spoken annotations as well dialogue that is read rather than spoken.  For more on bracket parsing see <a href="#bracket">Bracket/General Chunk Extraction</a>.

<div class="middleDiv">
<b><font size="4" color="red">Note: It is important that all sentences contain valid qdap punctuation (<font face="courier">.</font>, <font face="courier">?</font>, <font face="courier">!</font>, <font face="courier">|</font>) in your transcripts. Many qdap functions are dependent upon this assumption.</font></b>
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
## Incorrect Read; Wrong `sep` Provided (used default `:`)
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

The <a href="http://trinker.github.io/qdap_dev/dir_map.html" target="_blank"><code>dir_map</code></a> function enables the researcher to produce multiple lines of code, one line with <a href="http://trinker.github.io/qdap_dev/read.transcript.html" target="_blank"><code>read.transcript</code></a> for each file in a directory, which is then optionally copied to the clipboard for easy insertion into a script.  Note that setting the argument <font face="courier">use.path = FALSE</font> may allow the code to be more portable in that a static path is not supplied to the <a href="http://trinker.github.io/qdap_dev/read.transcript.html" target="_blank"><code>read.transcript</code></a> scripts.

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

The <a href="http://trinker.github.io/qdap_dev/mcsv_r.html" target="_blank"><code>mcsv_r</code></a> function reads multiple files at once and then assigns then dataframes to identically named objects (minus the file extension) in the global environment.  Additionally, all of the dataframes that are read in are also assigned to an inclusive list (name `L1` by default).

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

## The dataframe names and list of dataframe can be altered
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
##   wrd. prop prop prop prop prop prop prop prop prop prop prop prop prop prop prop prop prop prop prop prop prop prop prop prop prop prop prop prop prop prop prop prop prop prop prop prop
## 1    8    0    0    0    0    0    0    0 12.5    0    0    0    0   25    0    0 12.5    0    0    0 12.5   25    0    0    0    0    0 12.5    0    0    0    0    0    0    0    0    0
## 2    7    0    0    0    0 14.2    0    0 14.2    0    0    0 14.2    0    0    0 14.2    0    0 14.2    0 14.2    0    0    0    0    0 14.2    0    0    0    0    0    0    0    0    0
## 3    9    0    0    0    0 11.1    0    0 11.1    0    0    0    0 11.1    0    0    0    0    0 22.2    0 11.1    0    0    0    0    0 22.2    0    0    0 11.1    0    0    0    0    0
## 4   11    0    0    0    0 9.09    0    0 27.2    0    0    0    0 27.2    0    0    0    0    0 9.09 9.09    0    0    0    0    0    0 9.09    0    0    0 9.09    0    0    0    0    0
## 5    5    0    0    0    0    0    0    0    0    0    0    0    0   20    0    0    0    0    0    0    0   20    0    0    0    0    0    0    0   20   40    0    0    0    0    0    0
## 6    8    0    0 12.5    0 12.5    0    0    0    0    0    0    0 12.5    0    0    0    0    0    0    0   25    0    0    0 12.5    0 12.5 12.5    0    0    0    0    0    0    0    0
## 
## $POSfreq
##   wrd. , . CC CD DT EX FW IN JJ JJR JJS MD NN NNP NNPS NNS PDT POS PRP PRP$ RB RBR RBS RP TO UH VB VBD VBG VBN VBP VBZ WDT WP WP$ WRB
## 1    8 0 0  0  0  0  0  0  1  0   0   0  0  2   0    0   1   0   0   0    1  2   0   0  0  0  0  1   0   0   0   0   0   0  0   0   0
## 2    7 0 0  0  0  1  0  0  1  0   0   0  1  0   0    0   1   0   0   1    0  1   0   0  0  0  0  1   0   0   0   0   0   0  0   0   0
## 3    9 0 0  0  0  1  0  0  1  0   0   0  0  1   0    0   0   0   0   2    0  1   0   0  0  0  0  2   0   0   0   1   0   0  0   0   0
## 4   11 0 0  0  0  1  0  0  3  0   0   0  0  3   0    0   0   0   0   1    1  0   0   0  0  0  0  1   0   0   0   1   0   0  0   0   0
## 5    5 0 0  0  0  0  0  0  0  0   0   0  0  1   0    0   0   0   0   0    0  1   0   0  0  0  0  0   0   1   2   0   0   0  0   0   0
## 6    8 0 0  1  0  1  0  0  0  0   0   0  0  1   0    0   0   0   0   0    0  2   0   0  0  1  0  1   1   0   0   0   0   0  0   0   0
## 
## $POSrnp
##   wrd. , .   CC CD   DT EX FW   IN JJ JJR JJS   MD   NN NNP NNPS  NNS PDT POS  PRP PRP$   RB RBR RBS RP   TO UH   VB  VBD  VBG  VBN  VBP VBZ WDT WP WP$ WRB
## 1    8 0 0    0  0    0  0  0 1(12  0   0   0    0 2(25   0    0 1(12   0   0    0 1(12 2(25   0   0  0    0  0 1(12    0    0    0    0   0   0  0   0   0
## 2    7 0 0    0  0 1(14  0  0 1(14  0   0   0 1(14    0   0    0 1(14   0   0 1(14    0 1(14   0   0  0    0  0 1(14    0    0    0    0   0   0  0   0   0
## 3    9 0 0    0  0 1(11  0  0 1(11  0   0   0    0 1(11   0    0    0   0   0 2(22    0 1(11   0   0  0    0  0 2(22    0    0    0 1(11   0   0  0   0   0
## 4   11 0 0    0  0 1(9.  0  0 3(27  0   0   0    0 3(27   0    0    0   0   0 1(9. 1(9.    0   0   0  0    0  0 1(9.    0    0    0 1(9.   0   0  0   0   0
## 5    5 0 0    0  0    0  0  0    0  0   0   0    0 1(20   0    0    0   0   0    0    0 1(20   0   0  0    0  0    0    0 1(20 2(40    0   0   0  0   0   0
## 6    8 0 0 1(12  0 1(12  0  0    0  0   0   0    0 1(12   0    0    0   0   0    0    0 2(25   0   0  0 1(12  0 1(12 1(12    0    0    0   0   0  0   0   0
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
## 
## $digits
## [1] 2
```


<h4 id="just">Text Justification</h4> 

By default text data (character vectors) are displayed as right justified in R.  This can be difficult and unnatural to read, particularly as the length of the sentences increase.  The <a href="http://trinker.github.io/qdap_dev/left_just.html" target="_blank"><code>left_just</code></a> function creates a more natural left justification of text.  Note that <a href="http://trinker.github.io/qdap_dev/left_just.html" target="_blank"><code>left_just</code></a> inserts spaces to achieve the justification. This could interfere with analysis and therefore the output from <a href="http://trinker.github.io/qdap_dev/left_just.html" target="_blank"><code>left_just</code></a> should only be used for visualization purposes, not analysis.

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
## left just to the rescue
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

<form action="http://trinker.github.io/qdap_dev/hash.html" target="_blank">
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

Often the researcher who deals with text data will have the need to lookup values quickly and return an accompanying value.  This is often called a dictionary, hash, or lookup.  This can be used to find corresponding values or recode variables etc.  The <a href="http://trinker.github.io/qdap_dev/lookup.html" target="_blank"><code>lookup</code></a> & <a href="%l%" target="_blank">%l%</a> functions provide a fast environment lookup for single usage. The <a href="http://trinker.github.io/qdap_dev/hash.html" target="_blank"><code>hash</code></a> & <a href="http://trinker.github.io/qdap_dev/hash.html" target="_blank">hash_lookup</a>/<a href="http://trinker.github.io/qdap_dev/hash.html" target="_blank">%ha%</a> functions provide a fast environment lookup for multiple uses of the same hash table.

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
    D = c(6, 8:10)
)

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
##  [1] 25.34 15.79 16.30 16.30 15.79 25.34 19.70 15.00 19.70 16.30 16.30
## [12] 19.70 15.79 15.79 16.30 19.70 15.00 15.79 15.79 15.79
```

```r
x %ha% hashTab
```

```
##  [1] 25.34 15.79 16.30 16.30 15.79 25.34 19.70 15.00 19.70 16.30 16.30
## [12] 19.70 15.79 15.79 16.30 19.70 15.00 15.79 15.79 15.79
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


The researcher may need a more general extraction method that allows for any left/right boundaries to be specified.  This is useful in that many qualitative transcription/coding programs have specific syntax for various dialogue markup for events that must be parsed from the data set.  The <a href="http://trinker.github.io/qdap_dev/genX.html" target="_blank"><code>genX</code></a> and <a href="http://trinker.github.io/qdap_dev/genXtract.html" target="_blank"><code>genXtract</code></a> functions have such capabilities.

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
## [10] "Shall ? Good then."                 
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
## Create A Data Set With Punctuation and No Text
(DATA$state[c(3, 7, 10)] <- c(".", ".", NA))
```

```
## [1] "." "." NA
```

```r
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
## Use To Selctively Replace Cells With Missing Values
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


<pre><code class="r">## Read in transcript
dat2 <- read.transcript(system.file("extdata/transcripts/trans1.docx", 
    package = "qdap"))
truncdf(dat2, 40)</code></pre>

<pre><code>##                  X1                                       X2
## 1      Researcher 2                         October 7, 1892.
## 2         Teacher 4 Students it&#39;s time to learn. [Student di
## 3 Multiple Students        Yes teacher we&#39;re ready to learn.
## 4     [Cross Talk 3                                      00]
## 5         Teacher 4 Let&#39;s read this terrific book together.
</code></pre>

<pre><code class="r">## Use column names to remove rows
truncdf(rm_row(dat2, "X1", "[C"), 40)</code></pre>

<pre><code>##                  X1                                       X2
## 1      Researcher 2                         October 7, 1892.
## 2         Teacher 4 Students it&#39;s time to learn. [Student di
## 3 Multiple Students        Yes teacher we&#39;re ready to learn.
## 4         Teacher 4 Let&#39;s read this terrific book together.
</code></pre>

<pre><code class="r">## Use column numbers to remove rows
truncdf(rm_row(dat2, 2, "[C"), 40)</code></pre>

<pre><code>##                  X1                                       X2
## 1      Researcher 2                         October 7, 1892.
## 2         Teacher 4 Students it&#39;s time to learn. [Student di
## 3 Multiple Students        Yes teacher we&#39;re ready to learn.
## 4     [Cross Talk 3                                      00]
## 5         Teacher 4 Let&#39;s read this terrific book together.
</code></pre>

<pre><code class="r">## Also remove people etc. from the analysis
rm_row(DATA, 1, c("sam", "greg"))</code></pre>

<pre><code>##       person sex adult                         state code
## 1    teacher   m     1            What should we do?   K3
## 2      sally   f     0        How can we be certain?   K6
## 3      sally   f     0   What are you talking about?   K9
## 4 researcher   f     1 Shall we move on?  Good then.  K10
</code></pre>

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
## [1] "I like 456 dogs, don't you?"
```


<h4 id="replace">Replacement Functions</h4>

The replacement family of functions replace various text elements within the transcripts with alphabetic versions that are more suited to analysis.  These alterations may affect word counts and other alphabetic dependent forms of analysis.

The <a href="http://trinker.github.io/qdap_dev/replace_abbreviation.html" target="_blank"><code>replace_abbreviation</code></a> replaces standard abbreviations that utilize periods with forms that do not rely on periods.  This is necessary in that many sentence specific functions (e.g., <a href="http://trinker.github.io/qdap_dev/sentSplit.html" target="_blank"><code>sentSplit</code></a> and <a href="http://trinker.github.io/qdap_dev/word_stats.html" target="_blank"><code>word_stats</code></a>) rely on period usage acting as sentence end marks.  The researcher may augment the standard <a href="http://trinker.github.io/qdapDictionaries/abbreviations.html" target="_blank"><code>abbreviations</code></a> dictionary from qdapDictionaries with field specific abbreviations.

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


The <a href="http://trinker.github.io/qdap_dev/replace_contraction.html" target="_blank"><code>replace_contraction</code></a> replaces contractions with equivalent multi-word forms.  This is useful for some word/sentence statistics.  The researcher may augment the <a href="http://trinker.github.io/qdapDictionaries/contractions.html" target="_blank"><code>contractions</code></a> dictionary supplied by qdapDictionaries, however, the word list is exhaustive.

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
replace_number(x, FALSE)
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


The <a href="http://trinker.github.io/qdap_dev/qprep.html" target="_blank"><code>qprep</code></a> function is a wrapper for several other replacement family function that allows for more speedy cleaning of the text.  This approach, while speedy, reduces the flexibility and care that is undertaken by the researcher when the individual replacement functions are utilized.  The function is intended for analysis that requires less care.

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

Many qdap functions break sentences up into words based on the spaces between words.  Often the researcher will want to keep a group of words as a single unit.  The <a href="http://trinker.github.io/qdap_dev/space_fill.html" target="_blank"><code>space_fill</code></a> allows the researcher to replace spaces between selected phrases with <b><font color="blue" face="courier">&#126;&#126;</font></b>.  By default <b><font color="blue" face="courier">&#126;&#126;</font></b> is recognized by many qdap functions as a space separator.

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

![plot of chunk unnamed-chunk-25](figure/unnamed-chunk-251.png) ![plot of chunk unnamed-chunk-25](figure/unnamed-chunk-252.png) 


<h4 id="mgsub">Multiple gsub</h4>

The researcher may have the need to make multiple substitutions in a text.  An example of when this is needed is when a transcript is marked up with transcription coding convention specific to a particular transcription method.  These codes, while useful in some contexts, may lead to inaccurate word statistics.  The base R function <a href="http://stat.ethz.ch/R-manual/R-devel/library/base/html/grep.html" target="_blank">gsub</a> makes a single replacement of these types of coding conventions. The <a href="http://trinker.github.io/qdap_dev/multigsub.html" target="_blank"><code>multigsub</code></a> (alias <a href="http://trinker.github.io/qdap_dev/mgsub.html" target="_blank"><code>mgsub</code></a>) takes a vector of patterns to search for as well as a vector of replacements.  Note that the replacements occur sequentially rather than all at once. This means a previous (first in pattern string) sub could alter or be altered by a later sub.  <a href="http://trinker.github.io/qdap_dev/mgsub.html" target="_blank"><code>mgsub</code></a> is useful throughout multiple stages of the research process.

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

A researcher may face a list of names and be uncertain about gender of the participants.  The <a href="http://trinker.github.io/qdap_dev/name2sex.html" target="_blank"><code>name2sex</code></a> function utilizes the <a href="http://trinker.github.io/qdapDictionaries/NAMES_LIST.html" target="_blank"><code>NAMES_LIST</code></a> dictionary based on the 1990 U.S. census data.  For gender neutral names the gender with the higher assignment rate is assumed if <font face="courier">pred.sex</font> is set to <font face="courier">TRUE</font>, otherwise a <font color="blue">B</font> is assigned to indicate "both" genders.  For names not matching the <a href="http://trinker.github.io/qdapDictionaries/NAMES_LIST.html" target="_blank"><code>NAMES_LIST</code></a> optional fuzzy matching can be utilized via the <font face="courier">fuzzy.match</font> argument based on the use of <a href="http://stat.ethz.ch/R-manual/R-devel/library/base/html/agrep.html" target="_blank">agrep</a>.  Both of these argument increase accuracy but act at the cost of speed.  The use of <font face="courier">fuzzy.match = TRUE</font> is particularly computationally costly.

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

During the initial cleaning stage of analysis the researcher may choose to create a stemmed version of the dialogue, that is words are reduced to their root words.  The <a href="http://trinker.github.io/qdap_dev/stemmer.html" target="_blank"><code>stemmer</code></a> family of functions allow the researcher to create stemmed text.  The <a href="http://trinker.github.io/qdap_dev/stem2df.html" target="_blank"><code>stem2df</code></a> function wraps <a href="http://trinker.github.io/qdap_dev/stemmer.html" target="_blank"><code>stemmer</code></a> to quickly create a dataframe with the stemmed column added.

<font size="5" color="orange">&diams;</font> **Stemming**<font size="5" color="orange">&diams;</font>

```r
## stem2df EXAMPLE:
(stemdat <- stem2df(DATA, "state", "new"))
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
with(stemdat, trans_cloud(new, sex, title.cex = 2.5, 
    title.color = "blue", max.word.size = 5, title.padj = .7))
```

![plot of chunk unnamed-chunk-28](figure/unnamed-chunk-281.png) ![plot of chunk unnamed-chunk-28](figure/unnamed-chunk-282.png) 

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

At times it is handy to be able to grab from the beginning or end of a string to a specific character.  The <a href="http://trinker.github.io/qdap_dev/beg2char.html" target="_blank"><code>beg2char</code></a> function allows you to grab from the beginning of a string to the n<sup>th</sup> occurrence of a character.  The counterpart function, <a href="http://trinker.github.io/qdap_dev/char2end.html" target="_blank"><code>char2end</code></a>, grab from the n<sup>th</sup> occurrence of a character to the end of a string to. This behavior is useful if the transcript contains annotations at the beginning or end of a line that should be eliminated.

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

Often incomplete sentences have a different function than complete sentences.  The researcher may want to denote incomplete sentences for consideration in later analysis.  Traditionally, incomplete sentence are denoted with the following end marks (.., ..., .?, ..?, en & em).  The <a href="http://trinker.github.io/qdap_dev/incomplete_replace.html" target="_blank"><code>incomplete_replace</code></a> can identify and replace the traditional end marks with a standard form <font color="blue">"|"</font>.

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

The <a href="http://trinker.github.io/qdap_dev/capitalizer.html" target="_blank"><code>capitalizer</code></a> functions allows the researcher to specify words within a vector to be capitalized.  By default <font color="blue">I</font>, and contractions containing <font color="blue">I</font>, are capitalized.  Additional words can be specified through the <font face="courier">caps.list</font> argument.  To capitalize words within strings the <a href="http://trinker.github.io/qdap_dev/mgsub.html" target="_blank"><code>mgsub</code></a> can be used.

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

Many functions in the qdap package require that the dialogue is broken apart into individual sentences, failure to do so may invalidate many of the outputs from the analysis and will lead to lead to warnings.  After reading in and cleaning the data the next step should be to split the text variable into individual sentences.  The <a href="http://trinker.github.io/qdap_dev/sentSplit.html" target="_blank"><code>sentSplit</code></a> function outputs a dataframe with the text variable split into individual sentences and repeats the demographic variables as necessary.  Additionally, a turn of talk (<font color="red">tot column</font>) variable is added that keeps track of the original turn of talk (row number) and the sentence number per turn of talk.  The researcher may also want to create a second text column that has been stemmed for future analysis by setting <font face="courier">stem.col = TRUE</font>, though this is more time intensive.

<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/sentSplit.html" target="_blank"><code>sentSplit</code></a> Example**<font size="5" color="orange">&diams;</font>


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
##        person  tot sex adult code                       state                stem.text
## 1         sam  1.1   m     0   K1            Computer is fun.           Comput is fun.
## 2         sam  1.2   m     0   K1                Not too fun.             Not too fun.
## 3        greg  2.1   m     0   K2     No it's not, it's dumb.       No it not it dumb.
## 4     teacher  3.1   m     1   K3          What should we do?       What should we do?
## 5         sam  4.1   m     0   K4        You liar, it stinks!       You liar it stink!
## 6        greg  5.1   m     0   K5     I am telling the truth!     I am tell the truth!
## 7       sally  6.1   f     0   K6      How can we be certain?   How can we be certain?
## 8        greg  7.1   m     0   K7            There is no way.         There is no way.
## 9         sam  8.1   m     0   K8             I distrust you.          I distrust you.
## 10      sally  9.1   f     0   K9 What are you talking about? What are you talk about?
## 11 researcher 10.1   f     1  K10           Shall we move on?        Shall we move on?
## 12 researcher 10.2   f     1  K10                  Good then.               Good then.
## 13       greg 11.1   m     0  K11                 I'm hungry.              I'm hungri.
## 14       greg 11.2   m     0  K11                  Let's eat.                 Let eat.
## 15       greg 11.3   m     0  K11                You already?             You alreadi?
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


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/sentSplit.html" target="_blank"><code>sentSplit</code></a>** - *plot Method*<font size="5" color="orange">&diams;</font>


```r
plot(sentSplit(DATA, "state"), grouping.var = "person")
```

![plot of chunk unnamed-chunk-33](figure/unnamed-chunk-331.png) 

```r
plot(sentSplit(DATA, "state"), grouping.var = "sex")
```

![plot of chunk unnamed-chunk-33](figure/unnamed-chunk-332.png) 


<font size="5" color="orange">&diams;</font> **<a href="http://trinker.github.io/qdap_dev/sentSplit.html" target="_blank"><code>TOT</code></a> Example** <font size="5" color="orange">&diams;</font>


```r
## Convert tot column with sub sentences to turns of talk
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
## A dialogue dataframe and a demographics dataframe
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

![plot of chunk unnamed-chunk-41](figure/unnamed-chunk-411.png) 

```r
plot(dat, base = TRUE)
```

![plot of chunk unnamed-chunk-41](figure/unnamed-chunk-412.png) 


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

![plot of chunk unnamed-chunk-42](figure/unnamed-chunk-421.png) 

```r
gantt_wrap(dat2, "fam.aff_sex", facet.vars = "act",
    title = "Repeated Measures Gantt Plot")
```

![plot of chunk unnamed-chunk-42](figure/unnamed-chunk-422.png) 


<h4 id="adj">Create Adjacency Matrix</h4>

It is useful to convert data to an adjacency matrix for examining relationships between grouping variables in word usage.  The <a href="http://trinker.github.io/qdap_dev/adjaceny_matrix.html" target="_blank"><code>adjaceny_matrix</code></a> (aka: <a href="http://trinker.github.io/qdap_dev/adjmat.html" target="_blank"><code>adjmat</code></a>) provide this capability, interacting with a <a href="http://trinker.github.io/qdap_dev/termco.html" target="_blank"><code>termco</code></a> or <a href="http://trinker.github.io/qdap_dev/Word_Frequency_Matrix.html" target="_blank"><code>wfm</code></a> object.  In the first example below Sam and Greg share 4 words in common, whereas, the Teacher and Greg share no words.  The adjacency matrix can be passed to a network graphing package such as the <a href="http://igraph.sourceforge.net/" target="_blank">igraph</a> package for visualization of the data structure as seen in Example 3.


<font size="5" color="orange">&diams;</font> **Adjacency Matrix**: *Example 1*<font size="5" color="orange">&diams;</font>


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

<font size="5" color="orange">&diams;</font> **Adjacency Matrix**: *Example 2*<font size="5" color="orange">&diams;</font>


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

It is often useful to plot the adjacency matrix as a network.  The <a href="http://cran.r-project.org/web/packages/igraph/index.html">igraph package</a> provides this functionality.

<p id="plotadj"><font size="5" color="orange">&diams;</font> <b>Plotting an Adjacency Matrix</b>: <em>Example 1</em><font size="5" color="orange">&diams;</font></p>



```r
library(igraph)
dat <- adjacency_matrix(wfm(DATA$state, DATA$person, stopword = Top25Words))
g <- graph.adjacency(dat$adjacency, weighted=TRUE, mode ="undirected")
g <- simplify(g)
V(g)$label <- V(g)$name
V(g)$degree <- igraph::degree(g)
plot(g, layout=layout.auto(g))
```

![plot of chunk unnamed-chunk-45](figure/unnamed-chunk-45.png) 


The following example will visualize the presidential debates data as a network plot.

<p id="plotadj2"><font size="5" color="orange">&diams;</font> <b>Plotting an Adjacency Matrix</b>: <em>Example 2</em><font size="5" color="orange">&diams;</font></p>



























































































































































































































































































































































































































































