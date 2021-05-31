Designing drone surveys for N-mixture models
================

-   [Description](#description)
-   [Simulation experiments](#simulation-experiments)
-   [Repository contents](#repository-contents)
-   [Running code](#running-code)
-   [Examples](#examples)

#### ***Performance and optimal design of N-mixture models for spatiotemporally replicated drone-based surveys***

#### This repo is under construction…

**ToDo:**  

-   [x] Create MIT license
-   [ ] Upload raw simulation results in zenodo
-   [ ] Write examples in README
-   [ ] Generate table results
-   [ ] Build Rmd files for webpage

# Description

Repository for the manuscript *OPTIMALLY DESIGNING DRONE SURVEYS FOR
WILDLIFE ABUNDANCE MODELING WITH N-MIXTURE MODELS*.  
This repository contains the simulation results and `R` codes to
explore, in a very wide scan study, the performance (based on root mean
squared error) and optimal survey effort allocation for hierarchical
N-mixture models, focusing on the application for drone-based surveys
and the use of a double observer protocol to decompose the detection
process in availability and perception. For more details of the
simulation results see [this website](http...).

# Simulation experiments

The simulation study is divided in three parts:

-   #### Part 1: *optimal design of count surveys for N-mixture abundance estimation*

    **This first experiment assesses the performance of N-mixture models
    using double and single observer counts under different scenarios of
    population density and detection probability and address the optimal
    survey effort allocation in terms of spatial vs. temporal
    prioritization for each scenario.**  
    Main, high abundance and double effort.

-   #### Part 2: *exploring the benefit of the double observer protocol*

    **In this experiment, we investigate how the use of double observer
    protocol increases model performance and affects optimal survey
    effort allocation.**  
    Explain here…

-   #### Part 3: *can the double-observer protocol be used to reduce field survey effort?*

    **Here, we evaluate if the use of double observer protocol can
    reduce the effort needed in fieldwork to achieve the same model
    performance as in a single observer approach.**  
    Explaining a little bit…

# Repository contents

-   `data/`: simulation results files
    -   `raw_simul_resu/`: raw simulation results are not available in
        this repo (download info below)
    -   `processed_simul_resu/`: extracted and cleaned simulation
        results
-   `docs/`: files for the website
-   `ms/`: manuscript files
-   `outputs/`: figures, tables and intermediate outputs
    -   `figs/`: figures built with R scripts. RMSE curves for scenarios
        and RMSE relationships
    -   `tabs/`: tables built with R scripts. Optimal J tables and
        budget savings table
-   `R/`: R scripts and functions. Script file names reference to each
    simulation experiment with `Part` or `P`
    -   `ex.sumulation*.R`: script to run a single iteration simulating
        spatiotemporally replicated counts and analyzing with the
        respective N-mixture model. Examples with maximum likelihood
        estimation (`*_likeli.R`) and Bayesian approach (`*_bayes.R`).
    -   `func_*.R`: functions to support R scripts
    -   `simulNmix_scenarios_parallel.R`: script to run several
        simulation iterations. This script was used to produce raw
        simulation results using maximum likelihood estimation.
    -   `Part*/`: codes for each simulation experiment
        -   `C&E_.R*`: clean and extract raw simulation results and save
            in the processed folder
        -   `calcRMSE_*.R`: codes to calculate the RMSE for each
            scenario (under a combination of local abundance and
            availability) and get the optimal number of visits for each
            scenario.
        -   `fig*.R`: codes to generate figures

# Running code

## Download this repo

``` r
download.file(url = "https://github.com/ismaelvbrack/designNmix4droneSurveys/archive/main.zip", destfile = "designNmix4droneSurveys.zip")
```

## Download raw simulation results

To reproduce the results from raw simulation results, download them from
zenodo (XXX Mb).  
Otherwise, you can reproduce starting from processed simulation results
(cleaned and extracted available here) directly from this repo.

**Manual download [here](http..) and paste in `~/data/raw_simul_resu`**

or

**Download from RStudio**

``` r
library(zen4R)
download_zenodo(doi=xxx, path=here::here("data","raw_simul_resu"))
```

# Examples
