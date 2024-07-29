# README: wildcard_HF

## Overview

This repository contains all the data and R code used for the analyses in the *wildcard_HF study* and generates an HTML document of the results.

To view the results document, download `wildcard-results--xxxx-xx-xx.html` and open it in your web browser.

## Contents

The directory, file, and variable names within this repository are designed to be self-explanatory.

- `generate_markdown.R`: An R script used to run all the files necessary to produce the HTML results document.

- `/data`: Contains all the raw Pavlovia and Prolific files.

  - `wc_full_data.csv`: All the merged Pavlovia and Prolific files with improved column names, including incomplete and aborted data.

- `/r-scripts`: Contains scripts for specific tasks such as merging raw data files, plotting risky choice results, etc. These files are sourced in the markdown generation. To run one of these scripts independently, adjust the working directory to `/wildcard_HF` and run the dependencies listed in the script.

- `/plots`: Stores all generated plots in both .png and .svg formats.

- `/markdown`: Contains the R markdown files used to generate the HTML results document.
