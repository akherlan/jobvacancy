lapply(
  c(
    "dplyr",
    "httr",
    "janitor",
    "jsonlite",
    "rvest",
    "stringr",
    "telegram.bot",
    "tidyr"
  ), function(x) if(!require(x, character.only = TRUE)) install.packages(x)
)
