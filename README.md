README
======

This gitrepository contains the data (raw and coded) and the analysis scripts
in R used for the manuscript
Montero-Melis & Jaeger (in prep.) "Non-native (and native) adaptation to recent
input"

Content by folders
------------------

### data/

Contains the data for the different tasks in the experiment, as well as
participant information. Raw files are processed by scripts in the `processing`
folder to yield more usable data formats (e.g., from transcriptions of what
participants actually said to coding that indicates whether they used a path
verb in a given description or not; see more below).


### processing/

A number of scripts to process and annotate (code) the data. For each description
of a video stimulus, we care about whether the description mentions the path
and/or the manner of the event, and how it is expressed syntactically (e.g., in
a main verb or in an adjunct). This coding was done using regular expressions
so that a record is kept of the coding.

The script `coding_manual-automatic.R` calls another script that annotates the
data using regex. It then saves the data to disk. Some uncommenting of certain
code lines might be required to completely reproduce the annotation, but this
should become clear from looking at the comments.

The script `compute_dependent_measures.R` creates the data files that are used
for the analyses reported in the paper. Analysis scripts are stored in the
`analysis` folder.


### analysis/

Initially, all the analyses reported in the paper were in 
`supplementary-info.Rmd`, which uses Rmarkdown to combine R-analyses and a 
report-like output. The idea was that this file could be included as 
supplementary information in the final submission. However, with new drafts
and revised ideas of how to set up the analyses, there are now a number of
new analysis scripts in the folder. I've tried to write the purpose of a script/
knitr document in the header or introduction.

The subfolders are self-explanatory: `functions/` factors out some functions that
are often used and have a long body, notably some that fit GAMs only if there is
not ready a GAM object of the same name that has been stored to disk. This is
done to avoid having to fit all models when knitting the Rmarkdown scripts.
