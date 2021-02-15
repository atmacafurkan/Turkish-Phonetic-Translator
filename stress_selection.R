library(tidyverse)
library(prodlim)
library(stringi)
library(magrittr)
library(lubridate)
library(doParallel)
library(pbapply)
library(foreach)
library(doSNOW)


vowels <- read_csv2("underlying.csv") %>% subset(category == "vowel") %>% .$orthography %>% unique()
# exceptional stress words
df_exceptional <- readRDS("words_tdk.rds") %>% subset(str_detect(pronunciation, "'") & str_detect(word, " ", negate = T)) %>%
  mutate(word = tolower(word)) %>% distinct(word, .keep_all = T)

# Input
df_translated <- readRDS("translated_tdk.rds") %>% subset(str_detect(sentence, " ", negate = T)) %>%
  mutate(stress = "") %>% distinct(word, .keep_all = T) %>% dplyr::select(-sentence) %>% arrange(word)


# setup multiple core work
n_cores <- detectCores()
core_cluster <- makeCluster(n_cores[1]/2)
registerDoSNOW(core_cluster)


iterations <- length(df_translated$word)

start_point <- 1
loop_length <- iterations - start_point + 1

pb <- timerProgressBar(max = loop_length, style = 3, char = ">", width = 50)
progress <- function(n) setTimerProgressBar(pb, n)
opts <- list(progress = progress)


stress_frame <- foreach(each=1:iterations, .combine = c, .options.snow = opts) %dopar% {
  library(magrittr)
  library(tidyverse)
if(df_translated$word[each] %in% df_exceptional$word){

stress_plain <- df_exceptional$pronunciation[which(df_translated$word[each] == df_exceptional$word)[1]] %>%
  str_split("") %>% unlist() %>% as.data.frame() %>% 
  rownames_to_column(var = "order") %>% dplyr::select(order, character = ".") %>%
  subset(character %in% vowels | character == "'")
row.names(stress_plain) <- NULL

stressed_syl <- paste(which(stress_plain$character == "'")-1, collapse = "-")} else {stressed_syl <- df_translated$syl_num[each]}
stressed_syl
}

close(pb)
stopCluster(core_cluster)
df_translated$stress <- stress_frame
df_translated$exceptional_stress <- with(df_translated, ifelse(syl_num == stress, F,T))

saveRDS(df_translated, "aggregated_tdk.rds")

