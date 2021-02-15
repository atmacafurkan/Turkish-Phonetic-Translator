library(tidyverse)
library(stringi)
library(magrittr)


df <- readRDS("../../translated_tdk.rds")

df %<>% subset(str_detect(structure, "CC"))

df_words <- readRDS("words_tdk.rds") %>% subset(pronunciation !="null")
