README
======

This folder contains all the data and R-scripts necessary to reproduce the
analyses reported in:

Montero-Melis, G., & Jaeger, T. F. (accepted). Changing expectations mediate
adaptation in L2 production. Bilingualism: Language and Cognition.


Note on reproducing the analyses
--------------------------------

To reproduce the analyses, download all the files in this repository into one
single folder, open the `analyses.Rmd` script with RStudio and "knit" (generate)
a knitr report. 

Please make sure you have all the necessary packages installed. See the last
section of the report `analyses.html`, which shows the session info at the time
of running, including all loaded packages. 

If you run into errors of not finding an object loaded from disk, please start
by checking path specifications.

If you discover errors or have any other queries, contact the first author,
Guillermo Montero-Melis at <guillermo.monteromelis@mpi.nl>.


Dependencies
------------

R, relevant R packages (see previous point), RStudio (I used version 1.2.1335).


Description of files
--------------------

### `analyses.html`

The [knitr](https://yihui.name/knitr/) report generated from `analyses.Rmd`.


### `analyses.Rmd`

A [knitr](https://yihui.name/knitr/) report that reproduces all analyses reported
in the paper and in the Supplementary Online Materials (SOM). When "knitted" the
output should be equivalent to `analyses.html`


### `data.csv`

The data file used for the analyses. Here is an explanation of each column:

- *Subject*: An identifier for each of the 118 participants
- *Group*: Whether the participant was a native speaker of Spanish (NS) or an
L2 learner of Spanish (L2)
- *Condition*: Whether the participant was assigned to the baseline, path-priming
or manner-priming condition
- *Trial*: The trial number in the experiment. Each trial corresponds to seeing
an event and having to described it (and previously having read out a priming
sentence for participants in the priming conditions)
- *VideoName*: An identifier for each of the 32 videoclips used as stimuli
(this what we refer to as "items" in the paper)
- *P_V*: binary variable indicating whether a path verb was used by a participant
to describe a given item (1=yes, 0=no)
- *M_V*: binary variable indicating whether a path verb was used by a participant
to describe a given item (1=yes, 0=no)
- *ClozeScore*: The result on the cloze score to assess L2 proficiency (the test
was only taken by L2 learners)
- *zClozeScore*: z-transformation of ClozeScore
- *cClozeScore*: centred version of ClozeScore 
- *PathPrimeV*: The priming sentence that preceded this videoclip for
path-primed participants
- *MannerPrimeV*: The priming sentence that preceded this videoclip for
manner-primed participants


### `load_or_fit_fnc.R`

Function used to load models if they have already been saved,  rather than
fitting them anew each time the script is called. It's called from the script
`analyses.Rmd`.


### `participant-info.csv`

Contains information about the participants. Here is an explanation of each
column:

- *Subject*:   as in `data.csv` above
- *Group*:     as in `data.csv` above
- *Condition*: as in `data.csv` above
- *Gender*: Participant gender (self-repoted)
- *Age*: Participant age (self-repoted)
- *ClozeScore*: as in `data.csv` above
- *L2_SelfRating*: Self-rated proficiency in L2 Spanish (L2_SelfRating) on a
scale from 1 (very low proficiency) to 7 (very high proficiency)
- *L2_AoO*: self-reported age of onset at which a participant (L2 learners
only) first started to learn or came into contact with Spanish
- *L2_instruction_years*: self-reported years of formal instruction in Spanish
(L2 learners only)
- *VocabTaskAcc*: accuracy on the vocabulary task reported in the main document
section Method > Procedure (L2 learners only)


### `plot_gams_fnc.R` and `plot_glmm_fnc.R`

Scripts containing custom functions to generate the result plots reported in
the paper and the SOM. Both are called from the script `analyses.Rmd`. 


### `README.md`

Please read the file to understand its content (this is a recursive joke).
