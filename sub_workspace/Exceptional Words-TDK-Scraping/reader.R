library(tidyverse)
library(magrittr)
library(pbapply)


df_words <- readRDS("words_tdk.rds") %>% subset(pronunciation !="null")

vowel_changes <- df_words %>% subset(str_detect(pronunciation, ":"))

exceptional_stress <- df_words %>% subset(str_detect(pronunciation, "'"))

