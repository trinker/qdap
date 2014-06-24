NEWS 
====

Versioning
----------

Releases will be numbered with the following semantic versioning format:

&lt;major&gt;.&lt;minor&gt;.&lt;patch&gt;

And constructed with the following guidelines:

* Breaking backward compatibility bumps the major (and resets the minor 
  and patch)
* New additions without breaking backward compatibility bumps the minor 
  (and resets the patch)
* Bug fixes and misc. changes bumps the patch


**CHANGES** IN <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> VERSION 2.1.1
----------------------------------------------------------------

**BUG FIXES**

* `syllable_count` returned the sentence (recycled) in the `words` column of the
  output.  This behavior has been fixed.  See GitHub <a href="https://github.com/trinker/qdap/issues/188">issue #188</a> for details.

**NEW FEATURES**

* `cumulative` generic method added to view cumulative scores over time.

* `formality` picks up a `cumulative` method.

* `polarity` picks up a `cumulative` method.

* `end_mark` picks up a `class` (`end_mark`), `plot` method, and a `cumulative` 
  method.

* `syllable_sum`, `polysyllable_sum`, and `combo_syllable_sum` pick up a 
  `class`, `plot` method, and a `cumulative` method.

* `wfm` becomes a generic method currently applied to a `text.var` that is:
  `character`, `factor` (coerced to `character`), or `wfdf`.

**MINOR FEATURES**

**IMPROVEMENTS**

* `wfm` gains a speedup through generic classes and `tm` package integration 
  (`strip` is no longer used in `wfm`).

* `as.tdm.character` and `as.dtm.character` gain a speed boost with a `tm` 
  package integration.

* Added message to `as.data.frame.Corpus` for missing endmarks suggesting the 
  use of: `sent.split = FALSE`.

**CHANGES**

* `as.data.frame.Corpus`'s default value for `sent.split` is now `FALSE`.

**CHANGES** IN <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> VERSION 2.1.0
----------------------------------------------------------------

**BUG FIXES**

* `new_project` did not copy the .Rprofile over into the new project. This has 
  been fixed. Reference <a href="https://github.com/trinker/qdap/issues/184">issue #184</a>.

* `sentiment_frame` coerced words to factor.  `stringsAsFactors = FALSE` has 
  been added to prebent this.

* `polarity` did not work on &gt; 1 grams due to a bug in `sentiment_frame` 
  converting character to factor (chewth). See GitHub <a href="https://github.com/trinker/qdap/issues/185">issue #185</a> for details.

**NEW FEATURES**

* `unique_by` added to allow the user to find terms unique to individual 
  elements of a grouping variable.

* `build_qdap_vignette` replaces the temporary place holder version of the 
  *Introduction to <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> vignette*.  This function will replace the (1) HTML, 
  (2) source, & (3) R code found in `browseVignettes(package = 'qdap')`.

**MINOR FEATURES**

* `sub_holder` picks up a `alpha.type` argument that allows the user to specify 
  whether alpha or numeric keys should be used.

* `replace_number` picks up a `remove` argument that removes numbers from text.

**IMPROVEMENTS**

* `qheat` becomes a generic method.  This means some of the internal function 
  class checking has been moved to individual methods for those classes.  
  Additionally, `qheat` now works with logical matrices/data.frames.

* The `tm` package compatibility functions have been renamed in a more R-ish
  way and take the form of generic methods for specific classes.  For example,
  `df2tm_corpus` becomes `as.Corpus`.  Here is a complete list of changes:

    - `df2tm_courpus` is now  `as.Corpus`
    - `tm_corpus2df` is now `as.data.frame`
    - `as.wfm` is now a generic method
    - `tm_corpus2wfm` is now `as.wfm`
    - `tm2qdap` is now `as.wfm`
    - `tdm` is now `as.tdm` or `as.TermDocumentMatrix` 
    - `dtm` is now `as.dtm` or `as.DocumentTermMatrix`

**CHANGES**

* `colsplit2df` and `colpaste2df` no longer convert character columns to factor.

* `df2tm_corpus` is deprecated.  It will be removed in a subsequent version of 
  `qdap`.  Use `as.Corpus` instead.

* `tm_corpus2df` is deprecated.  It will be removed in a subsequent version of 
  `qdap`.  Use `as.data.frame` instead.

* `tm2qdap` is deprecated.  It will be removed in a subsequent version of 
  `qdap`.  Use `as.wfm` instead.

* `tm_corpus2wfm` is deprecated.  It will be removed in a subsequent version of 
  `qdap`.  Use `as.wfm` instead.

* `tdm` is deprecated.  It will be removed in a subsequent version of `qdap`.  
  Use `as.tdm` or `as.TermDocumentMatrix` instead.

* `dtm` is deprecated.  It will be removed in a subsequent version of `qdap`.  
  Use `as.dtm` or `as.DocumentTermMatrix` instead.

* The *Introduction to <a href="https://github.com/trinker/qdap" target="_blank">qdap</a>* .Rmd vignette has been moved to an internal 
  directory.  The HTML version is not built by default.  THis saves CRAN space 
  and time checking the package source.  The file has been replaced with a
  temporary place holder that contains instructions for building the actual 
  vignette.  The user may also use the `build_qdap_vignette` directly.

* `qdap` incorporates the chanegs from the `tm` package version: 0.6:
  <a href="http://cran.r-project.org/web/packages/tm/news.html" target="_blank">http://cran.r-project.org/web/packages/tm/news.html</a>  Reference <a href="https://github.com/trinker/qdap/issues/187">issue #187</a>.

**CHANGES** IN <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> VERSION 2.0.0
----------------------------------------------------------------

The `qdapTools` package now houses several former <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> functions.  While 
  `qdapTools` is a Dependency and all of these functions will be accessible to 
  the <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> user there is a break in backward compatibility if these functions
  are included in code.  For this reason this release is a major bump of <a href="https://github.com/trinker/qdap" target="_blank">qdap</a>.


**BUG FIXES**

* `replace_number` did not replace single digits numbers.  Spotted by Ben Bolker.
  This behavior has been fixed and unit testing added for this function. See
  <a href="https://github.com/trinker/qdap/issues/178">issue #178</a>.

**NEW FEATURES**

* `sub_holder` added; this function holds the place for particular character 
  values, allowing the user to manipulate the vector and then revert the place
  holders back to the original values.

* `Network` method added to make network plots of select <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> objects.  

* `qtheme`, `theme_nightheat`, `theme_duskheat`, theme_norah`, `theme_cafe`, 
  `theme_grayscale`, `theme_badkitchen`, and `theme_hipster` added to style 
  `Network` plots.

* `polarity` picks up a `Network` method.

* `formality` picks up a `Network` method.

* <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> officially begins utilizing the `testthat` package for unit testing, 
  though only a few functions have begun the process, more will be added over 
  time.

**MINOR FEATURES**

**IMPROVEMENTS**

**CHANGES**

* The `qdapTools` package now houses the following former `qdap` functions:
  `hash`, `%ha%`, `hash_look`, `hms2sec`, `id`, `lookup`, `%l%`, `%l+%`, `%l*%`, 
  `repo2github`, `sec2hms`, `text2color`, `url_dl`, `v_outer`, `list2df`, 
  `matrix2df`, `vect2df`, `list_df2df`, `list_vect2df`, `counts2list`,  
  `vect2list`, & `mtabulate`.  These functions will continue to be available to 
  <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> users in interactive mode (`qdapTools` is a Dependency and thus these 
  functions are loaded into the workspace by default).  This will allow this 
  bundle of functions to be used outside of <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> without calling the larger <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> 
  package per the request of Kirill Muller (see <a href="https://github.com/trinker/qdap/issues/165">issue #165</a>).

* As schedulaed the `dissimialrity` function has been removed from the <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> 
  package to avoid conflict with the `tm` package.  Use `Dissimilarity` function 
  instead.


**CHANGES** IN <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> VERSION 1.3.6
----------------------------------------------------------------

**MINOR FEATURES**

* `polarity` picks up a `constrain` argument that constrains the polarity values
  to be between -1 and 1.

**IMPROVEMENTS**

* `polarity`'s equation now uses primes on the de-amplifiers before they're 
  confined to be &gt;= -1.  This avoids confusion in the indicator function that 
  took the de-amplifiers variable and returned the same variable.

* `dist_tab`'s frequency columns used a capital F in Freq.  This was not 
  consistent across all column names and has been changed to lower case.

**CHANGES**

* `polarity_frame` is deprecated and will be removed in a subsequent release.
  Please use `sentiment_frame` instead.


**CHANGES** IN <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> VERSION 1.3.5
----------------------------------------------------------------

**BUG FIXES**

* The An Introduction to <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> vignette contained a broken link in the tm 
  Package Compatibility section.  This has been fixed.  Also the reliance on 
  `Rgraphviz` from the vignette has been removed.  This will eliminate CRAN
  WARN in CRAN checks (for some OS) but not the note for `tm`'s reliance on 
  `Rgraphviz`.

* `polarity` reported the incorrect number of words for sentences containing 
  commas.  This has been fixed (Max Ghenis).

**NEW FEATURES**

* `formality` picks up an `Animate` method.

* `end_mark_by` function added as a aggregated grouping version of `end_mark`.

**MINOR FEATURES**

* `raj.act.1POS` added.  `raj.act.1POS` is a data set for Romeo and Juliet: Act 1
  broken into parts of speech.

**IMPROVEMENTS**

* `discourse_map` picks up a `pause` argument that enables the user to pasue 
  between plots in interactive mode.

**CHANGES**

**CHANGES** IN <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> VERSION 1.3.4
----------------------------------------------------------------

**BUG FIXES**

**NEW FEATURES**

* `gantt` and `gantt_wrap` (single facet) pick up and `Animate` mathod.

* `polarity` picks up an `Animate` method.

* `vertex_apply` and `edge` apply added to make uniform changes to lists of 
  `igraph` objects.

**MINOR FEATURES**

**IMPROVEMENTS**

* `discourse_map` picks up a `condense` argument that allows the user to 
  condense sequential rows for like grouping variable sub groups.

* `list_df2df` names now use a zero padded numeric portion for defualt names.  
  For example `c("L1", "L2", "L3", ... "L10")`, becomes 
  `c("L01", "L02", "L03", ... "L10")`.

**CHANGES**

**CHANGES** IN <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> VERSION 1.3.3
----------------------------------------------------------------

**BUG FIXES**

* `colpaste2df` dropped the column name for a single retained column when 
  `keep.orig = FALSE`.  See GitHub <a href="https://github.com/trinker/qdap/issues/157">issue #157</a> for more.

* `multigsub` (`mgsub`) would return `NA` for replacement of length 1 after the 
  addition of the `order.pattern` (used to prevent substrings from 
  replacing meta-strings) in version 1.3.2.

**NEW FEATURES**

* `phrase_net` function provides functioning similar to the Many Eyes 
  Phrase Net plot.

* `discourse_map` function provides a network mapping of the flow of discourse 
  between social actors.  Function output is `Animate` ready as well.  See
  `?discourse_map` and <a href="http://trinker.github.io/qdap_examples/animation_dialogue" target="_blank">http://trinker.github.io/qdap_examples/animation_dialogue</a>
  for more.

* `Animate` function added to convert select <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> outputs to an animated  
  sequence.  See `?Animate.discourse_map` for more.

**MINOR FEATURES**

* `synonyms_frame` (`syn_frame`) added to allow the user to create a synonym
  hash for the revamped `synonyms` function.

* `repo2github` function added to send a directory to GitHub upon first commit.

**IMPROVEMENTS**

* `new_project` has an improved directory structure and works with any version 
  of the `reports` package.

* `synonyms` function used the `env.syl` hash data from <a href="https://github.com/trinker/qdap" target="_blank">qdap</a>Dictionaries 
  internally.  This approach could cause problems if used within other functions
  in a package.  It also limits the usability of synonyms.  The `synonyms` 
  function picks up a `synonym.frame` argument that allows the user to specify
  a synonym hash table.  This can be created via the `synonyms_frame` function
  (per a request from J. Aravind).

**CHANGES**


**CHANGES** IN <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> VERSION 1.3.2
----------------------------------------------------------------

This is a patch release to address the archiving of the `lsa` package.


**BUG FIXES**

* The **qdap-tm Package Compatibility** Vignette contained an error in the 
  Feinerer I, Hornik K, Meyer D (2008) reference (pages listed as 51-54 has been 
  corrected to pages 1-54 as well as incorrect journal). Caught by Kurt Hornik.

**MINOR FEATURES**

* `DocumentTermMatrix` and `TermDocumentMatrix` from the tm package pick up a 
  `Filter` method.

**IMPROVEMENTS**

* `multigsub` picks up an argument, `order.pattern`, to prevent substrings from 
  replacing meta-strings.

* The following data sets were added to <a href="https://github.com/trinker/qdap" target="_blank">qdap</a>Dictionaries package: 
    `Fry_1000`, `Leveled_Dolch`, `Dolch`

**CHANGES**

* The package `lsa` has been removed from Suggests field in the DESCRIPTIONN 
  file, examples, and vignettes.


**CHANGES** IN <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> VERSION 1.3.1
----------------------------------------------------------------

A version bump necessary for Re-Submission to CRAN.  

**CHANGES**

* `new_project` was reconfigured with the old code that does not require the 
  newest version of the reports package.

**CHANGES** IN <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> VERSION 1.3.0
----------------------------------------------------------------

**BUG FIXES**

* `read.transcript` could leave a QDAP_PLACE_HOLDER behind if a colon was found 
  in the person column.  This behavior has been fixed.

* `word_cor`'s plotting method threw an error if a word did not have any words 
  above the r threshold.  This behavior has been corrected.

* `Filter` overwrote a base R function; this has been fixed per Joshua Ulrich.

* `scores.polarity`'s print method would return an error if columns were not 
  indexed yet were rounded.  For instance, the following threw an error: 

  `scores(with(sentSplit(DATA, 4), polarity(state, person)))[, 1:4]` 

   This behavior has been fixed.

**NEW FEATURES**

* <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> adds an HTML vignette to better explain the intended work flow and 
  function use for the package.   Use `browseVignettes(package = "qdap")` to
  open.

* <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> adds a PDF vignette to describe the compatibility and navigation between 
  <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> and the `tm` packages. Use `browseVignettes(package = "qdap")` to open.

**MINOR FEATURES**

**IMPROVEMENTS**

* `apply_as_df` picks up a `stopwords` and `filter` arguments that allows the 
  user to remove stopwords and min/max length words.

* `plot.word_cor` picks up the argument `ncol` that allows the user to specify 
  the number of columns used.  This uses ggplot2's `facet_wrap` rather than 
  `facet_grid` (which is the default if `ncol =NULL`).

* `name2sex` relied upon having <a href="https://github.com/trinker/qdap" target="_blank">qdap</a>Dictionaries loaded.  This could be an issue 
  if the function were used internally.  The user now supplies a dictionary of 
  names and probabilities.

* `df2tm_corpus` gains a `demographics.vars` argument that allows the user to 
  add demographic information to the resulting corpus `DMetaDat`.

* `tm_corpus2df` gains the ability to convert `DMetaDat` into demographic 
  data.frame columns.


**CHANGES**


**CHANGES** IN <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> VERSION 1.2.0
----------------------------------------------------------------

**BUG FIXES**

**NEW FEATURES**

* `Filter` added to give the ability to provide a range of character 
  lengths to filter from a `wfm` object.

* `scores` generic method added to view scores from select <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> objects.

* `counts` generic method added to view counts from select <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> objects.

* `proportions` generic method added to view proportions from select <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> 
  objects.

* `preprocessed` generic method added to view preprocessed data from select <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> 
  objects.

* `apply_as_df` added to allow the user to apply <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> functions to a Corpus 
  directly.

**MINOR FEATURES**

* `tm_corpus2wfm` added to quickly convert from a tm package `Corpus` to a <a href="https://github.com/trinker/qdap" target="_blank">qdap</a>
  `wfm` object.

* `as.wfm` added as a means to attempt to coerce a matrix to a `wfm` object.

* `%l+%` added as a counterpart to `%l%` that assumes `missing = NULL`.

* `%bs%` added as quick counterpart to `boolean_search` for indexing.

**IMPROVEMENTS**

* `df2tm_corpus` now sets metaData information for ID and creator (based on) 
  `Sys.info()["user"]`.

* `matrix2df` now accepts a simple_triplet_matrix object as well.

* `word_cor` output that was a list (not a correlation matrix) did not have a 
  plot method.  The plot method for `word_cor` now handles both matrices and the 
  lsit of correlations.

* `rm_row` picks up the `contains` argument that allows the user to search for, 
  and remove rows of, within the string, not just the beginning.

* `read.transcript` now handles multiple character spaces as an argument to 
  `sep` when `text` argument is used.

**CHANGES**

* `dissimilarity` has been renamed to `Dissimilarity` to prevent tm package 
  conflicts.  The old version has been deprecated and will be removed in a the
  next version (minor or major) push to CRAN.


**CHANGES** IN <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> VERSION 1.1.0
----------------------------------------------------------------

A version bump necessary for Re-Submission to CRAN.  

**CHANGES**

* Downgraded the version requirement for the reports package to 
  reports (&gt;= 0.1.2) in order to upload to CRAN.  reports (&gt;= 0.2.0) is not yet
  available on CRAN.

**CHANGES** IN <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> VERSION 1.0.0
----------------------------------------------------------------

The word lists and dictionaries in `qdap` have been moved to `qdapDictionaries`. 
Additionally, many functions have been renamed with underscores instead of the 
former period separators.  These changes break backward compatibility.  Thus 
this is a **major** release (ver. 1.0.0).

It is the general practice to deprecate functions within a package before 
removal, however, the number of necessary changes in light of <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> being 
relatively new to CRAN, made these changes sensible at this point.


**BUG FIXES**

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
  to `target.words`.  This behavior has been fixed.

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

* The `cm_code.` family of functions had a bug in the output due to 
  `cm_long2dummy` and `cm_dummy2long`'s handling of stretching spans.  This has 
  been corrected.

* `cm_code.exclude` did not output the correct excluded spans.  This behavior
  has been corrected.

* The use of `comment` to convey object characteristics has been replaced with 
  the use of `class`.

* `question_type` did not include question words ending in 'd as part of the 
  category.  For instance "How'd you like it?" was not classified as a how 
  question.

* `beg2char` would not include the `char` if `include = TRUE` and `noc = 1`.

* `cm_range2long` returned `NA`s for vectors containing multiple single values.  
  See GitHub <a href="https://github.com/trinker/qdap/issues/144">issue #144</a> for more.

* `termco` family of functions did not handle `NA` values.  This has been fixed. 
  (Matt Williamson) See GitHub <a href="https://github.com/trinker/qdap/issues/147">issue #147</a> for details.

* `pos` threw an error for vectors of length 1.  This has been fixed (Kurt 
  Hornik). See GitHub <a href="https://github.com/trinker/qdap/issues/150">issue #150</a> for details.

* `formality` threw an error for vectors of length 1.  This has been fixed. (Kurt 
  Hornik) See GitHub <a href="https://github.com/trinker/qdap/issues/151">issue #151</a> for details.

**NEW FEATURES**

*  The `cm_xxx2long` family of functions (`cm_df2long`, `cm_range2long` and 
  `cm_time2long`) now have a generic wrapper, `cm_2long`, to generate the long
  formats.

* `hash_look` (and `%ha%`) a counterpart to `hash` added to allow quick access 
  to a hash table.  Intended for use within functions or multiple uses of the 
  same hash table, whereas `lookup` is intended for a single external (non 
  function) use which is more convenient though could be slower.

* `boolean_search`, a Boolean term search function, added to allow for indexed 
  searches of Boolean terms.

* `trans_context` is a printing function desired to grab the context (n rows 
  before and after) an event (an index from a vector of indices).  The function 
  prints the indices around the episode from a transcript to the console or a 
  .csv, .xlsx, .txt, or .doc file. 

* `colpaste2df` is a wrapper for `paste2` that pastes dataframe columns together 
  and outputs a dataframe.

* `colcomb2class` quickly combines columns for  number of <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> classes including 
  output from: `termco`, `question_type`, `pos_by`, adn 1character_table`.

* `lview` a function to unclass a list output that has a special print method 
  that returns only a portion of the output.  `lview` re-classes to "list".

* `word_cor` added to find words within grouping variables that are associated
  based on correlation.

* `tm2qdap` a function to convert `"TermDocumentMatrix"` and 
  `"DocumentTermMatrix"` to a `wfm` added to allow easier integration with the 
  `tm` package.

* `apply_as_tm` a function to allow functions intended to be used on the `tm` 
  package's `TermDocumentMatrix` to be applied to a `wfm` object.

* `tm_corpus2df` and `df2tm_corpus` added to convert a tm package corpus to a 
  dataframe for use in <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> or vice versa.

* `tdm` and `dtm` are now truly compatable with the `tm` package.  `tdm` and 
  `dtm` produce outputs of the class `"TermDocumentMatrix"` and 
  `"DocumentTermMatrix"` respectively.  This change (coupled with the renaming 
  of `stopwords` to `rm_stopwords`) should make the two packages logical 
  companions and further extend the <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> package to integrate with the many 
  packages that already handle `"TermDocumentMatrix"` and 
  `"DocumentTermMatrix"`.

* `cm_distance` now uses resampling of data from the null model to generate
  pvalues for the mean code distances.  Useful for determining if an association 
  (small distance) between codes is likely to happen if the null is true.

*  `dispersion_plot` added to enable viewing of word dispersion through 
  discourse.

* `word_proximity` added to compliment `dispersion_plot` and `word_cor` 
  functions.  `word_proximity` gives the average distance between words in 
  the unit of sentences.

**MINOR FEATURES**

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
  on something like `file.path(getwd(), )`.  This means portability of code on 
  different machines.

* `polarity_frame` a function to make a hash environment lookup for use with the 
  `polarity` function.

* `DATA.SPLIT` a `sentSplit` version of the `DATA` dataset has been added to 
  <a href="https://github.com/trinker/qdap" target="_blank">qdap</a>.

* `gantt_plot` accepts`NULL`.for `grouping.var` and figures for "all" rows as a 
  single grouping var.

* `replace_number` now handles 10^47 digits compared to 10^14 previously.

* The `new_project` function gains a `github` argument that optionally sends the 
  repo to GitHub public account upon creation.

* `qheat`, `polarity.plot` and `formality.plot` pick up the argument `plot` 
  which optionally suppresses the plotting.  This is useful if the user is 
  operating in knitr, sewave, etc. and wishes to alter/add onto the plot.

* `lookup` now takes `missing = NULL`.  This results in the original values in
  `terms` corresponding to the missing elements being retained.

* `cm_time.temp` picks up a `grouping.var` argument that works similarly to 
  `cm_range.temp`'s `grouping.var`.  `cm_time.temp` also takes hour values for
  `start` and `end` as in `end = "01:22:03"`.

* `gantt_rep` picks up a generic `plot` method.

* Functions in the `cm_code.xxx` and `cm_xxx2long` pick up a generic plot method
  that utilizes `gantt_wrap` to plot a Gantt plot of the span data.

* Functions in the `cm_code.xxx` and `cm_xxx2long` pick up a generic summary 
  method.  This summary method has its own plot method that utilizes `qheat` to 
  plot a heatmap of the summary statistics.  The generic print method 
  (`print.sum_cmspans`) is useful for output intended for publication.

* `qheat` picks up a `facet.vars` argument that allows a character vector of 
  length 1 or 2 to facet by.

* `question_type` gives the indices of questions via `$inds`.

* `colsplit2df` not splits multiple columns to match the capabilities of 
  `colpaste2df`.

* `sentSplit` now handles repeated measures and picks up a turn of talk plot 
  method.

* `tot_plot` now handles repeated measures and `grouping.var` to be nested 
  within the turn of talk.

* `wfm` now uses `mtabulate` and is ~10x faster.

* `plot.polarity` gains arguments for optional error bars using the standard 
  error of the mean polarity.

* `exclude` now works with `wfm` and the `tm` package's `DocumentTermMatrix` and
  `TermDocumentMatrix` classes.

* `rm_url` removes/repalces URLs in a string(s).

* `matrix2df` added (under `list2df`) to convert rownnames of matrix to a 
  dataframe column.

**CHANGES**

* The dictionaries and word lists for <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> have been moved to their own package, 
  `qdapDictionaries`.  This will allow easier access to these resources beyond 
  the <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> package as well as reducing the overall size of the <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> package.  
  Because this is a major change that make break the code of some users the 
  major release number has been upped to 1.  The following name changes have 
  occurred:

    - `increase.amplification.words` -&gt; became -&gt; `amplification.words`

    - The `deamplification.words` wordlist  and `env.pol` dictionary were added as 
        well.

* <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> gains an HTML package vignette to better explain the intended work flow 
  and function use for the package.  This is not currently a part of the build 
  but can be accessed via:

  <a href="http://htmlpreview.github.io/?https://github.com/trinker/qdap/blob/master/vignettes/qdap_vignette.html" target="_blank">http://htmlpreview.github.io/?https://github.com/trinker/qdap/blob/master/vignettes/qdap_vignette.html</a>

  *Note* that the vignette may include development version functions not yet 
  available in the current CRAN version

* `polarity` utilizes a new, unbounded algorithm based on weighting to determine 
  polarity.

* `gantt_wrap` no longer accepts unquoted strings to the `plot.var` argument.

* `cm_df.temp` loses the logical `csv` argument.  `file.name` have been replaced 
  with `file` to fit conventional R naming schemes.

* The plotting feature of `gantt` has been removed and a `plot` method has been 
  added.  The user can plot the output from `gantt` in `base` or `ggplot2` 
  graphics.

* `cm_time2long` loses the argument `start.end` to ensure that the `cmspans` 
  class produced would operate as expected.

* Most exported functions utilizing a period separator have been replaced with 
  underscore named versions.

* `wf_combine` renamed `wfm_combine` to be consistent.

* `question_type` algorithm improvements including implied do/does/did handling.

* `list2df` and `mtabulate` now exported.

* `stopwords` has been renamed to `rm_stopwords`(`rm_stop` shorthand) to better 
  fit what the action the function performs and to avoid conflicts with the 
  `tm` package.

* `replace_number`'s `num.paste` becomes logical rather than character input.
  This makes use easier as the user doesn't need to remember arguments.


**CHANGES** IN <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> VERSION 0.2.5
----------------------------------------------------------------

Patch release.  This version deals with the changes in the `openNLP` package 
  that effect <a href="https://github.com/trinker/qdap" target="_blank">qdap</a>.  Next major release scheduled after `slidify` package is 
  pushed to CRAN.

qdap 0.2.3
----------------------------------------------------------------
**BUG FIXES**

* `new_project` placed a report in the CORRESPONDENCE directory rather than 
  CONTACT_INFO

* `strip` would not allow the characters "/" and "-" to be passed to 
  `char.keep`.  This has been fixed. (Jens Engelmann)

* `beg2end` would only grab first character of a string after n -1 occurrences of 
  the character.  For example: 
  `beg2char(c("abc-edw-www", "nmn-ggg", "rer-qqq-fdf"), "-", 2)` resulted in
  "abc-e" "nmn-g" "rer-q" rather than "abc-edw" "nmn-ggg" "rer-qqq"

**NEW FEATURES**

* `names2sex` a function for predicting gender from name.

* Added `NAMES` and `NAMES_SEX` datasets, based on 1990 U.S. census data.

* `tdm` added as an equivalent to TermDocumentMatrix from the tm package.  This 
  allows for portability across text analysis packages.

**MINOR FEATURES**

* `mgsub` now gets a `trim` argument that optionally removes trailing leading 
  white spaces.

* `lookup` now takes a list of named vectors for the key.match argument.

**CHANGES**

* `new_project` directory can now be transferred without breaking paths (i.e.,
  `file.path(getwd(), "DIR/file.ext")` is used rather than the full file path).


**CHANGES** IN <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> VERSION 0.2.2
----------------------------------------------------------------

**BUG FIXES**

* `genXtract` labels returned the word "right" rather than the right edge string.
  See <a href="http://stackoverflow.com/a/15423439/1000343" target="_blank">http://stackoverflow.com/a/15423439/1000343</a> for an example of the old 
  behavior.  This behavior has been fixed.

* `gradient_cloud`'s `min.freq ` locked at 1.  This has been fixed. (Manuel 
  Fdez-Moya)

* `termco` would produce an error if single length named vectors were passed to 
  match.list and no multi-length vectors were supplied.  Also an error was thrown 
  if an unnamed multi-length vector was passed to `match.list`.  This behavior has 
  been fixed.

**NEW FEATURES**

* `tot_plot` a visualizing function that uses a bar graph to visualize patterns 
  in sentence length and grouping variables by turn of talk.

* `beg2char` and `char2end` functions to grab text from beginning of string to a
  character or from a character to the end of a string.

* `ngrams` function to calculate ngrams by grouping variable.

**MINOR FEATURES**

* `genX` and `bracketX` gain an extra argument `space.fix` to remove extra 
  spaces left over from bracket removal.

* Updated out of date dropbox url download in `url_dl`.  `url_dl` also takes the 
  dropbox key as well.

**CHANGES**

* <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> is now compiled for mac users (as openNLP now passes CRAN checks with no
  Errors on Mac).

**CHANGES** IN <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> VERSION 0.2.1
----------------------------------------------------------------

**BUG FIXES**

* `word_associate` colors the word cloud appropriately and deals with the error 
  caused by a grouping variable not containing any words from 1 or more of the 
  vectors of a list supplied to match string

* `trans.cloud` produced an error when expand.target was `TRUE`.  This error has 
  been eliminated.

* `termco` would eliminate &gt; 1 columns matching an identical search.term found 
  in a second vector of match.list.  termco now counts repeated terms multiple 
  times.

* `cm_df.transcript` did not give the correct speaker labels (fixed).

**NEW FEATURES**

* `gradient_cloud`: Binary gradient Word Cloud - A new plotting function 
  that plots and colors words for a binary variable based on which group of 
  the binary variable uses the term more frequently.

* `new_project`: A project template generating function designed to increase 
  efficiency and standardize work flow.  The project comes with a .Rproj file 
  for easy use with RStudio as well as a .Rprofile that makes loading and sourcing 
  of packages, data and project functions.  This function uses the reports package
  to generate an extensive reports folder.


**MINOR FEATURES**

* `stemmer`, `stem2df` and `stem.words` now explicitly have the argument 
  `char.keep` set to "~~" to enable retaining special character formerly stripped 
  away.

* `hms2sec`: A function to convert from h : m : s format to seconds.

* `mcsv_w` now takes a list of data.frames.

* `cm_range.temp` now takes the arguments text.var and grouping.var that will 
  automatically output these (grouping.var) columns as range coded indices.

* `wfm` gets as speed boost as the code has been re-written to be faster.

* `read.transcript` now reads .txt files as well as text similar to read.table.

**CHANGES**

* `sec2hms` is the new name for `convert` 

* `folder` and `delete` have been moved to the reports package which is imported 
  by <a href="https://github.com/trinker/qdap" target="_blank">qdap</a>.  Previously `folder` would not generate a directory with the 
  time/date stamp if no directory name was given; this has been fixed, though 
  the function now resides in the reports package.

**CHANGES** IN <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> VERSION 0.2.0
----------------------------------------------------------------

* The first installation of the <a href="https://github.com/trinker/qdap" target="_blank">qdap</a> package

* Package designed to bridge the gap between qualitative data and quantitative 
  analysis