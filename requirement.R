# Libraries required
lapply(
  c(
    "dplyr",
    "httr",         # for GQL function and other libraries dependency
    "janitor",
    "jsonlite",     # for GQL function
    "rvest",
    "stringr",
    "purrr",
    "telegram.bot", # Telegram communication
    "tidyr",
    "unmplymnt"
  ), function(x) if(!require(x, character.only = TRUE)) install.packages(x, dependencies = TRUE)
)

# Generic functions
for (f in list.files("function", ".R", full.names = TRUE)) source(f)
rm(f)
