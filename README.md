# wildcard_HF

This repo contains all of the data and R code used for the wildcard_HF study.

To view the analysis summary, just download `Results-Summary-xxxx-xx-xx.html` and open it in your computer's web browser.

---

If you want to download the whole repo, the file `Gen_Rmd_Doc.R` will generate a new results summary html file.

- `./Data`: contains all the raw Pavlovia and Prolific files.
  - `wc_full_data.csv`: all the merged Pavlovia and Prolific files with better column names. This file contains all incomplete and aborted data as well.
- `./R Scripts`: contains a variety of scripts to do specific things like merge the raw data files, plot the risky choice results, etc. These files are sourced in the markdown generation. If you want to run one of these scripts on its own, you will need to adjust the working directory to `./wildcard_HF`.
- `./Plots`: stores all plots that I have generated as both a .png and .svg.
- `./Markdown`: contains files which generate the R Markdown document.
