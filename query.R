library(jsonlite)
library(stringr)
library(dplyr)

source("functions.R")

# available job category
glints_category()

# query
vacancy <- glints_vacancy(50)
View(vacancy)

glints_post(1)
