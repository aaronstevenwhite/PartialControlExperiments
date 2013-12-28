# Partial Control Experiments

## Overview

This project contains materials, data, and analysis scripts for three acceptability judgment experiments investigating the partial control phenomenon. These experiments were designed and run by [Thomas Grano](http://ling.umd.edu/~tgrano) and [Aaron Steven White](http://ling.umd.edu/~aswhite) using [Ibex](http://code.google.com/p/webspr/). The experiment was hosted on [Ibex Farm](http://spellout.net/ibexfarm/) and deployed on [Mechanical Turk](https://www.mturk.com/mturk/). 

This work was presented at [Sinn und Bedeutung 18](https://sites.google.com/site/sub18bc/). A prepublication version of the proceedings paper for that conference can be found in **paper/** or on either of the authors' websites. 

## Contents

### materials/

This directory contains all the materials needed to run each of the three experiments. This includes item generation and Ibex templating scripts (**create_pc\*_items.py**), the configuration files (**pc\*_items.js**) generated by those scripts, and all the instructions given to participants during the experiments (in **html/**).

### data/

This directory contains the raw data files pulled from Ibex (**pc\*\_data**) as well as long-form CSV files (**pc\*\_data.csv**) generated from these files by merging metadata by participant.

### analysis/

This directory contains an R analysis script (**sub_proceedings_analysis.R**), plots generated by that script (**plots/**), and a configuration file (**features_sub.csv**) necessary for the analysis presented in section 3.3 of the paper.

To make sure you have all the requisite dependencies up-to-date, you may want to run: 

```
dependencies <- c("ggplot2", "plyr", "reshape", "MASS", "MCMCglmm", "randomForest", "party", "caret", "proxy", "doMC")

install.packages(dependencies)
```

If you would like to rerun the entire analysis, you can execute the following at the R interactive prompt while in **analysis/**:

```
source("sub_proceedings_analysis.R")    
```

Note that the model fitting and tuning functions can take hours to terminate, so you may want to run the script piecemeal. You may also need to change the root directory at line 49, depending on where you extracted the project to.

This script generates plots by writing them to a TikZ file. These plots can be found in **plots/**. The package for doing this is called [`tikzDevice`](https://r-forge.r-project.org/projects/tikzdevice/), which sadly is no longer available on [CRAN](http://cran.us.r-project.org/) (for R > 3.0). By default, the lines that open a tikzDevice are commented out. If you would like to regenerate the TikZ files, you will need to install `tikzDevice` from source and uncomment those lines. 

### paper/

This directory contains a prepublication version of the paper "An experimental investigation of partial control", which will appear in the *Proceedings of Sinn und Bedeutung 18*. The proceedings will be published on [Semantics Archive](http://semanticsarchive.net/).
