library(tidyverse)
library(stringi)
library(magrittr)


df <- readRDS("translated_tdk.rds")

df %<>% subset(str_detect(structure, "CC"))
