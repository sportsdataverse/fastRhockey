library(usethis)
library(devtools)
library(desc)

# Remove default DESC
unlink("DESCRIPTION")
# Create and clean desc
my_desc <- description$new("!new")

# Set your package name
my_desc$set("Package", "whockeyR")

# Set your name
my_desc$set(
  "Authors@R",
  "c(person('Ben', 'Howell', email = 'benhowell71@gmail.com', role = c('cre', 'aut')),
     person('Saiem', 'Gilani', email = 'saiem.gilani@gmail.com', role = c('ctb')),
    person('Alyssa', 'Longmuir', 'aklongmuir@gmail.com', role = 'ctb')
     )"
)

# Remove some author fields
my_desc$del("Maintainer")

# Set the version
my_desc$set_version("0.0.1")

# The title of your package
my_desc$set(Title = "Functions to Access Premier Hockey Federation Play-by-Play Data")
# The description of your package
my_desc$set(Description = "The whockeyR package was created to scrape play-by-play data and statistics from the Premier Hockey Federation (PHF), formerly known as the National Women's Hockey League (NWHL).")
# The urls
my_desc$set("URL", "http://www.github.com/benhowell71/whockey_scraper")
my_desc$set("BugReports", "http://www.github.com/benhowell71/whockey_scraper/issues")
# Save everyting
my_desc$write(file = "DESCRIPTION")




# Get the dependencies
usethis::use_package("cli", min_version = "1.1.0")
usethis::use_package("data.table", min_version = "1.14.0")
usethis::use_package("dplyr")
usethis::use_package("purrr", min_version = "0.3.0")
usethis::use_package("magrittr")
usethis::use_package("glue")
usethis::use_package("httr", type = "Suggests")
usethis::use_package("janitor")
usethis::use_package("jsonlite")
usethis::use_package("progressr", min_version = "0.6.0")
usethis::use_package("qs", min_version = "0.24.0")
# usethis::use_package("Rcpp", min_version = "1.0.7")
# usethis::use_package("RcppParallel", min_version = "5.1.4")
usethis::use_package("rlang", min_version = "0.4.0")
usethis::use_package("rvest", min_version = "1.0.0")
usethis::use_package("stringi", type = "Suggests")
usethis::use_package("stringr", min_version = "1.3.0")
usethis::use_package("tibble", min_version = "3.0", type = "Suggests")
usethis::use_package("tidyr", min_version = "1.0.0")
# usethis::use_package("tidyselect", min_version = "1.1.0")
usethis::use_package("ggplot2", type = "Suggests")
usethis::use_package("ggrepel",  type = "Suggests")

usethis::use_package("crayon", min_version = "1.3.4", type = "Suggests")
usethis::use_package("usethis", min_version = "1.6.0")
usethis::use_package("xgboost", min_version = "1.1")
usethis::use_package("DBI",  type = "Suggests")
usethis::use_package("furrr",  type = "Suggests")
usethis::use_package("future",  type = "Suggests")
usethis::use_package("rmarkdown", type =  "Suggests")
usethis::use_package("RSQLite", type =  "Suggests")
usethis::use_package("testthat", type =  "Suggests")
usethis::use_package("curl", type =  "Suggests")
usethis::use_package("xml2", min_version = "1.3", type = "Suggests")
# currently not included because of the animation packages delicate dependencies
# usethis::use_package("animation")
# usethis::use_package("magick")
# usethis::use_package("ggimage")
# usethis::use_package("png")
# usethis::use_package("av (>= 0.3)","Suggests")
# usethis::use_package("spelling","Suggests")
# usethis::use_package("jsonlite","Suggests")
# usethis::use_package("methods","Suggests")
# usethis::use_package("knitr","Suggests")
# usethis::use_package("rmarkdown","Suggests")
# usethis::use_package("rsvg","Suggests")
# usethis::use_package("webp","Suggests")
# usethis::use_package("pdftools","Suggests")
# usethis::use_package("gapminder","Suggests")
# usethis::use_package("IRdisplay","Suggests")
# usethis::use_package("tesseract (>= 2.0)","Suggests")
# usethis::use_package("gifski","Suggests")


# Clean your description
usethis::use_tidy_description()

# If you want to use the MIT licence, code of conduct, and lifecycle badge
usethis::use_mit_license(copyright_holder = "Ben Howell")
usethis::use_news_md()
usethis::use_readme_rmd()
usethis::use_lifecycle_badge("experimental")
usethis::use_pipe()
usethis::use_github_action_check_standard()
usethis::use_github_links()

usethis::use_package_doc()
usethis::use_pkgdown_github_pages()
usethis::use_roxygen_md()
usethis::use_testthat()
