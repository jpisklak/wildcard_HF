# Install and Load Necessary Packages
pkgs <- c(
  "tidyverse",
  "patchwork",
  "rcompanion",
  "nlme",
  "WRS2",
  "RColorBrewer"
)

installed_pkgs <- pkgs %in% rownames(installed.packages())

if (any(installed_pkgs == FALSE)) {
  install.packages(pkgs[!installed_pkgs])
}

invisible(lapply(pkgs, library, character.only = TRUE))

# Set tibble defaults
options(pillar.print_min = 75, pillar.width = Inf)

# Load custom ggplot theme
source("R Scripts/theme_custom.R")

