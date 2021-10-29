# Metacognitive asymmetries in visual perception



Stage-1 Registered Report is available here:

Mazor, M., Moran, R., & Fleming, S. M. (2021). Metacognitive asymmetries in visual perception. Neuroscience of Consciousness, 2021(1), [niab005](https://doi.org/10.1093/nc/niab005)

Stage-2 Registered Report is available here:

Mazor, M., Moran, R., & Fleming, S. M. (2021). Metacognitive asymmetries in visual perception. Neuroscience of Consciousness, 2021(1), [niab005](https://doi.org/10.1093/nc/niab025)

![Summary of results from all seven experiments](https://github.com/matanmazor/asymmetry/blob/master/docs/figures/summary.png)


## Experiment demos

[Experiment 1 (Q vs. O)](https://matanmazor.github.io/asymmetry/experiments/demos/exp1/index).

[Experiment 2 (C vs. O)](https://matanmazor.github.io/asymmetry/experiments/demos/exp2/index).

[Experiment 3a (line tilt, stimulus overlay mask)](https://matanmazor.github.io/asymmetry/experiments/demos/exp3a/index).

[Experiment 3b (line tilt, dollar signs mask)](https://matanmazor.github.io/asymmetry/experiments/demos/exp3b/index).

[Experiment 4 (line curvature)](https://matanmazor.github.io/asymmetry/experiments/demos/exp4/index).

[Experiment 5 (cube orientation)](https://matanmazor.github.io/asymmetry/experiments/demos/exp5/index).

[Experiment 6 (letter inversion)](https://matanmazor.github.io/asymmetry/experiments/demos/exp6/index).

[exploratory Experiment 7 (detection of gratings in noise)](https://matanmazor.github.io/asymmetry/experiments/demos/exp7/index).

## Raw data

All raw data is available in the experiments subdirectory. For example, raw data from Experiment 1 is available in [experiments/Experiment1/data/jatos_results_batch1.csv](https://github.com/matanmazor/asymmetry/blob/master/experiments/Experiment1/data/jatos_results_batch1.csv) in csv format. 

Data from discrimination trials is saved in the [Confidence Database](https://osf.io/s46pr/) format (Rahnev et al., 2020) in the data subdirectory.

## Reproducible analysis pipeline

Our full analysis script, including the data used to generate the figures and the final report, is available in [docs/asymmetry_RR2.rmd](https://github.com/matanmazor/asymmetry/blob/master/docs/asymmetry_RR2.rmd). 

To run the analysis from your browser, [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/matanmazor/asymmetry/master?urlpath=rstudio). Once Rmarkdown opens, open the file binder/asymmetry_RR.rmd. 
Then, clicking `Knit` would run the entire analysis pipeline and produce a report in html form. To see the report, make sure to enable pop-ups. By default, figures are loaded from memory instead of being generated by the code (for formatting reasons, e.g. including significance stars). However, the code that was used to generate the figures is all in the rmd file and can be uncommented.

## Final report

A gitbook version of the final report is available [here](https://matanmazor.github.io/asymmetry/binder/introduction). 



