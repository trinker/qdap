NEWS 
====

Versioning
----------

Releases will be numbered with the following semantic versioning format:

<b>&lt;major&gt;.&lt;minor&gt;.&lt;patch&gt;</b>

And constructed with the following guidelines:

* Breaking backward compatibility bumps the major (and resets the minor 
  and patch)
* New additions without breaking backward compatibility bumps the minor 
  (and resets the patch)
* Bug fixes and misc changes bumps the patch



<b>CHANGES</b> IN qdap VERSION 1.0.0
----------------------------------------------------------------

The word lists and dictionaries in `qdap` have been moved to `qdapDictionaries`. 
This may break backward compatibility.  Thus this is a **major** release 
(ver. 1.0.0).

<b>BUG FIXES</b>

* `qheat`'s  argument `by.column = FALSE` resulted in an error.  This behavior 
  has been fixed.

* `question_type` did not work because of changes to `lookup` that did not 
  accept a two column matrix for `key.match`.  See GitHub <a href="https://github.com/trinker/qdap/issues/127">issue #127</a> for more.

* `combo_syllable.sum` threw an error if the text.var contained a cell with an 
  all non-character ([a-z]) string.  This behavior has been fixed.

* `todo` function created by `new_project` would not report completed tasks if 
  `report.completed = TRUE`.

* `termco` and `termco.d` threw an error if more than one consecutive regex 
  special character was passed to `match.list` or `match.string`.  See GitHub 
  <a href="https://github.com/trinker/qdap/issues/128">issue #128</a> for more. 

* `trans.cloud` threw an error if a single list with a named vector was passed 
  to `target.words`.  Thes behavior has been fixed.

* `sentSplit` now returns the "tot" column when `text.place = "original"`.  

* `all_words` output dataframe FREQ column class has been changed from factor to 
  numeric.  Additionally, the WORDS column prints using `left.just` but retains
  traditional character properties (print class added).  `all_words` also picks
  up apostrophe.remove and ldots (for `strip`) arguments.

* `gantt_plot` did not handle `fill.vars`, particularly if the fill was nested 
  within the `grouping.vars`.  This behavior has been fixed with corresponding 
  examples added.

* `url_dl` - Downloaded an empty file when not using a dropbox key.  This 
  behavior has been fixed.

<b>NEW FEATURES</b>

<b>MINOR FEATURES</b>

* `url_dl` now takes quoted string urls supplied to ... (no url argument is 
  supplied)

* `condense` is a function that condense dataframe columns that are a list of 
  vectors to a single vector of strings.  This outputs a dataframe with 
  condensed columns that can be wrote to csv/xlsx.

* `mcsv_w` now uses `condense` to attempt to attempt to condense columns that are 
  lists of vectors to a single vector of strings.  This adds flexibility to 
  `mcsv_w` with more data sets.  `mcsv_w` now writes lists of dataframes to 
  multiple csvs (e.g., the output from `termco` or `polarity`).  `mcsv_w` picks
  up a dataframes argument, an optional character vector supplied in lieu of 
  \ldots that grabs the dataframes from an environment (default id the Global
  environment).

* `ngrams` now has an argument ellipsis that passes further arguments supplied 
  to `strip`

* `dtm` added to compliment `tdm`, allowing for easier integration with other R 
  packages that utilize tdm/dtm.

* `dir_map` picks up a `use.path` argument that allows the user to specify a 
  more flexible path to the created pre-formed `read.transcript` scripts based 
  on something like `file.path(getwd(), )`.  This mean portability of code on 
  different machines.

* `hash_look` (and `%ha%`) a counterpart to `hash` added to allow quick access 
  to a hash table.  Intended for use within functions or multiple uses of the 
  same hash table, whereas lookup is intended for a single external (non 
  function) use which is more convenient though could be slower.

* `polarity_frame` a function to make a hash environment lookup for use with the 
  `polarity` function.

* `DATA.SPLIT` a `sentSplit` version of the `DATA` dataset has been added to 
  qdap.

* `gantt_plot` accepts `NULL` for `grouping.var` and figures for "all" rows as a 
  single grouping var.

* `replace_number` now handles 10^47 digits compared to 10^14 previously.

<b>CHANGES</b>

* The dictionaries and word lists for qdap have been moved to their own package, 
  `qdapDictionaries`.  This will allow easier access to these resources beyond 
  the qdap package as well as reducing the overall size of the qdap package.  
  Because this is a major change that make break the code of some users the 
  major release number has been upped to 1.  The following name changes have 
  occurred:

    - increase.amplification.words -&gt; became -&gt; amplification.words

    - The deamplification.words and env.pol wordlist and dictionary were added as 
        well.

* qdap gains an HTML package vignette to better explain the intended work flow 
  and function use for the package.

* `polarity` utilizes a new, unbounded algorithm based on weighting to determine 
  polarity.

* `gantt_wrap` no longer accepts unquoted strings to the `plot.var` argument.


<b>CHANGES</b> IN qdap VERSION 0.2.5
----------------------------------------------------------------

Patch release.  This version deals with the changes in the `openNLP` package 
  that effect qdap.  Next major release scheduled after `slidify` package is 
  pushed to CRAN.

qdap 0.2.3
----------------------------------------------------------------
<b>BUG FIXES</b>

* `new_project` placed a report in the CORRESPONDENCE directory rather than 
  CONTACT_INFO

* `strip` would not allow the characters "/" and "-" to be passed to 
  `char.keep`.  This has been fixed. (Jens Engelmann)

* `beg2end` would only grab first character of a string after n -1 occurrences of 
  the character.  For example: 
  `beg2char(c("abc-edw-www", "nmn-ggg", "rer-qqq-fdf"), "-", 2)` resulted in
  "abc-e" "nmn-g" "rer-q" rather than "abc-edw" "nmn-ggg" "rer-qqq"

<b>NEW FEATURES</b>

* `names2sex` a function for predicting gender from name.

* Added `NAMES` and `NAMES_SEX` datasets, based on 1990 U.S. census data.

* `tdm` added as an equivalent to TermDocumentMatrix from the tm package.  This 
  allows for portability across text analysis packages.

<b>MINOR FEATURES</b>

* `mgsub` now gets a `trim` argument that optionally removes trailing leading 
  white spaces.

* `lookup` now takes a list of named vectors for the key.match argument.

<b>CHANGES</b>

* `new_project` directory can now be transferred without breaking paths (i.e.,
  `file.path(getwd(), "DIR/file.ext")` is used rather than the full file path).


<b>CHANGES</b> IN qdap VERSION 0.2.2
----------------------------------------------------------------

<b>BUG FIXES</b>

* `genXtract` labels returned the word "right" rather than the right edge string.
  See http://stackoverflow.com/a/15423439/1000343 for an example of the old 
  behavior.  This behavior has been fixed.

* `gradient_cloud`'s `min.freq ` locked at 1.  This has been fixed. (Manuel 
  Fdez-Moya)

* `termco` would produce an error if single length named vectors were passed to 
  match.list and no multi-length vectors were supplied.  Also an error was thrown 
  if an unnamed multi-length vector was passed to match.list.  This behavior has 
  been fixed.

<b>NEW FEATURES</b>

* `tot_plot` a visualizing function that uses a bar graph to visualize patterns 
  in sentence length and grouping variables by turn of talk.

* `beg2char` and `cahr2end` functions to grab text from beginning of string to a
  character or from a character to the end of a string.

* `ngrams` function to calculate ngrams by grouping variable.

<b>MINOR FEATURES</b>

* `genX` and `bracketX` gain an extra argument `space.fix` to remove extra 
  spaces left over from bracket removal.

* Updated out of date dropbox url download in `url_dl`.  `url_dl` also takes the 
  dropbox key as well.

<b>CHANGES</b>

* qdap is now compiled for mac users (as openNLP now passes CRAN checks with no
  Errors on Mac).

<b>CHANGES</b> IN qdap VERSION 0.2.1
----------------------------------------------------------------

<b>BUG FIXES</b>

* `word_associate` colors the word cloud appropriately and deals with the error 
  caused by a grouping variable not containing any words from 1 or more of the 
  vectors of a list supplied to match string

* `trans.cloud` produced an error when expand.target was TRUE.  This error has 
  been eliminated.

* `termco` would eliminate &gt; 1 columns matching an identical search.term found 
  in a second vector of match.list.  termco now counts repeated terms multiple 
  times.

* `cm_df.transcript` did not give the correct speaker labels (fixed).

<b>NEW FEATURES</b>

* `gradient_cloud`: Binary gradient Word Cloud - A new plotting function 
  that plots and colors words for a binary variable based on which group of 
  the binary variable uses the term more frequently.

* `new_project`: A project template generating function designed to increase 
  efficiency and standardize work flow.  The project comes with a .Rproj file 
  for easy use with RStudio as well as a .Rprofile that makes loading and sourcing 
  of packages, data and project functions.  This function uses the reports package
  to generate an extensive reports folder.

<b>MINOR FEATURES</b>

* `stemmer`, `stem2df` and `stem.words` now explicitly have the argument 
  char.keep set to "~~" to enable retaining special character formerly stripped 
  away.

* `hms2sec`: A function to convert from h&#58;m&#58;s format to seconds.

* `mcsv_w` now takes a list of data.frames.

* `cm_range.temp` now takes the arguments text.var and grouping.var that will 
  automatically output these (grouping.var) columns as range coded indices.

* `wfm` gets as speed boost as the code has been re-written to be faster.

* `read.transcript` now reads .txt files as well as text similar to read.table.

<b>CHANGES</b>

* `sec2hms` is the new name for `convert` 

* `folder` and `delete` have been moved to the reports package which is imported 
  by qdap.  Previously `folder` would not generate a directory with the 
  time/date stamp if no directory name was given; this has been fixed, though 
  the function now resides in the reports package.

<b>CHANGES</b> IN qdap VERSION 0.2.0
----------------------------------------------------------------

* The first installation of the qdap package

* Package designed to bridge the gap between qualitative data and quantitative 
  analysis