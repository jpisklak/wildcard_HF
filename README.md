# README: wildcard_HF

## Overview

This repository contains the data and R code associated with Experiment 2 of the study *Effects of Initial Experiences on Risky Choice* by Elliot A. Ludvig, Neil McMillan, Jeffrey M. Pisklak, Nick Simonsen, Alice Mason, Jason Long, Marcia L. Spetch, and Christopher R. Madan.

**GitHub Repository:**

- [https://github.com/jpisklak/wildcard_HF](https://github.com/jpisklak/wildcard_HF)

**Associated OSF Project:**

- [https://osf.io/pnrwy/](https://osf.io/pnrwy/)

The most accessible form of the data to download is `wc_full_data.csv` located within the `./data` directory.

To view the results document, download `wildcard-results--yyyy-mm-dd.html` and open it in a web browser.

## Contents

The directory, file, and variable names within this repository are designed to be self-explanatory.

- `generate_markdown.R`: An R script used to run all the files necessary to produce the HTML results document `wildcard-results--yyyy-mm-dd.html`.

- `/data`: Contains all the raw Pavlovia and Prolific files.

  - `wc_full_data.csv`: All the merged Pavlovia and Prolific files with improved column names, including incomplete and aborted data.

- `/r-scripts`: Contains scripts for specific tasks such as merging raw data files, plotting risky choice results, etc. These files are sourced in the markdown generation. To run one of these scripts independently, adjust the working directory to `/wildcard_HF` and run the dependencies listed in the script.

- `/plots`: Stores all generated plots in both .png and .svg formats.

- `/markdown`: Contains the R markdown files used to generate the HTML results document.
