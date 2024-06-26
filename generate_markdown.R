# Run this script to generate the markdown file.
main_dir <- getwd()
source_dir <- file.path(main_dir, "markdown")
output_dir <- main_dir

# Knit Markdown
rmarkdown::render(
  input = file.path(source_dir, "results_main.Rmd"),
  output_format = "html_document",
  output_file = paste0("wildcard-results-", Sys.Date(), ".html"),
  output_dir = output_dir
)

