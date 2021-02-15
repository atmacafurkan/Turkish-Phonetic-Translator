library(tidyverse)
library(prodlim)
library(stringi)
library(magrittr)
library(lubridate)
library(doParallel)
library(pbapply)
library(foreach)
library(doSNOW)

df_underlying <- read_csv2("underlying.csv")
df_allophones <- read_csv2("allophones.csv")


### SENTENCES INPUT ###
sentences <- readRDS("words_tdk.rds") %>% drop_na() %>% .$word %>% tolower() %>% unique()


vowels <- df_allophones %>% subset(category == "vowel") %>% .$allophone
sonorant <- df_allophones %>% subset(syllabifier == "son") %>% .$allophone

# setup multiple core work
n_cores <- detectCores()
core_cluster <- makeCluster(n_cores[1]/2)
registerDoSNOW(core_cluster)


iterations <- length(sentences)

start_point <- 1
loop_length <- iterations - start_point + 1

pb <- timerProgressBar(max = loop_length, style = 3, char = ">", width = 50)
progress <- function(n) setTimerProgressBar(pb, n)
opts <- list(progress = progress)



final_parse <- foreach(each=start_point:iterations, .combine = rbind, .options.snow = opts) %dopar% {
  library(tidyverse)
  library(stringi)
  library(magrittr)
  output_words <- tibble(sentence = character(), word = character(), transcribed = character(), syllabified = character(),
                         syllable = character(), syl_num = integer())
  words <- str_split(sentences[each], pattern = "\\s") %>% unlist() %>% stri_remove_empty()
  for (every in 1:length(words)){
    translated <- tibble(sentence = character(), word = character(), phonetic = character())
    letters <- str_split(words[every], pattern = "") %>% unlist()
    translation <- character()
    
    for (some in 1:length(letters)){
      # current underlying
      selected <- df_underlying[which(df_underlying$orthography == letters[some]),]
      
      # previous phonetic
      if (some == 1){prefix <- df_underlying[33,]}else{prefix <- df_allophones[which(df_allophones$allophone == translation[some-1]),] }
      
      # next underlying
      if (is.na(letters[some+1])){suffix <- df_underlying[34,]}else{suffix <- df_underlying[which(df_underlying$orthography == letters[some+1]),]}
      
      # Rule 1 - Front back vowels
      if(selected$add_2 !="long" & suffix$category !="vowel" & selected$orthography !="ı" &selected$vector_1 == "back" &
         (suffix$vector_2 == "liquid" | prefix$vector_2 =="liquid" | prefix$vector_2 == "lateral")){
        selected$add_1 <- "front"
        selector <- which(selected$category == df_allophones$category & selected$vector_1 == df_allophones$vector_1 &
                            selected$vector_2 == df_allophones$vector_2 & selected$vector_3 == df_allophones$vector_3 &
                            selected$add_1 == df_allophones$add_1 & selected$add_2 == df_allophones$add_2)
        selected <- df_allophones[selector,]
      }
      
      # Rule 2 - Open "e" more 
      if(selected$orthography == "e" & suffix$vector_3 == "sonorant"){
        selected <- df_allophones[5,]
      }
      
      # Rule 3 - Front consonants
      if(selected$category == "consonant" & selected$vector_3 == 0 &(selected$vector_1 == "velar"| selected$vector_1 =="glottal") &
         (prefix$vector_1 =="front" | suffix$vector_1 =="front")){
        selected$add_1 <- "front"
        selector <- which(selected$category == df_allophones$category & selected$vector_1 == df_allophones$vector_1 &
                            selected$vector_2 == df_allophones$vector_2 & selected$vector_3 == df_allophones$vector_3 &
                            selected$add_1 == df_allophones$add_1 & selected$add_2 == df_allophones$add_2)
        selected <- df_allophones[selector,]
      }
      
      # Rule 4 - Aspirate voiceless stops
      if((selected$vector_2 == "stop" | selected$vector_2 =="affricate") & selected$vector_3 == 0 &
         (suffix$category == "end" | suffix$category =="vowel"| (prefix$category =="start" & suffix$category == "consonant") |
          (prefix$category == "consonant" & suffix$category == "consonant"))){
        selected$add_2 <- "aspirated"
        selector <- which(selected$category == df_allophones$category & selected$vector_1 == df_allophones$vector_1 &
                            selected$vector_2 == df_allophones$vector_2 & selected$vector_3 == df_allophones$vector_3 &
                            selected$add_1 == df_allophones$add_1 & selected$add_2 == df_allophones$add_2)
        selected <- df_allophones[selector,]
      }
      
      # Rule 5 - "V" changes
      if(selected$orthography == "v" & 
         (prefix$vector_3 == "round" | suffix$vector_3 == "round")){
        selected$add_1 <- "round"
        selector <- which(selected$category == df_allophones$category & selected$vector_1 == df_allophones$vector_1 &
                            selected$vector_2 == df_allophones$vector_2 & selected$vector_3 == df_allophones$vector_3 &
                            selected$add_1 == df_allophones$add_1 & selected$add_2 == df_allophones$add_2)
        selected <- df_allophones[selector,]
      }
      
      if(selected$orthography == "v" & selected$add_1 == "round" & prefix$category == "vowel"& suffix$category == "vowel"){
        selected$add_2 <- "inter"
        selector <- which(selected$category == df_allophones$category & selected$vector_1 == df_allophones$vector_1 &
                            selected$vector_2 == df_allophones$vector_2 & selected$vector_3 == df_allophones$vector_3 &
                            selected$add_1 == df_allophones$add_1 & selected$add_2 == df_allophones$add_2)
        selected <- df_allophones[selector,]
      }  
      
      # Rule 6 - "F" changes
      if(selected$orthography == "f" & (prefix$vector_3 == "round" | suffix$vector_3 == "round")){
        selected$add_1 <- "round"
        selector <- which(selected$category == df_allophones$category & selected$vector_1 == df_allophones$vector_1 &
                            selected$vector_2 == df_allophones$vector_2 & selected$vector_3 == df_allophones$vector_3 &
                            selected$add_1 == df_allophones$add_1 & selected$add_2 == df_allophones$add_2)
        selected <- df_allophones[selector,]
      }  
      
      
      # Rule 7 - "R" changes
      if(selected$orthography == "r" & suffix$category =="end"){
        selected$add_2 <- "end"
        selector <- which(selected$category == df_allophones$category & selected$vector_1 == df_allophones$vector_1 &
                            selected$vector_2 == df_allophones$vector_2 & selected$vector_3 == df_allophones$vector_3 &
                            selected$add_1 == df_allophones$add_1 & selected$add_2 == df_allophones$add_2)
        selected <- df_allophones[selector,]
        
      }
      if(selected$orthography == "r" & prefix$category =="start"){
        selected$add_2 <- "start"
        selector <- which(selected$category == df_allophones$category & selected$vector_1 == df_allophones$vector_1 &
                            selected$vector_2 == df_allophones$vector_2 & selected$vector_3 == df_allophones$vector_3 &
                            selected$add_1 == df_allophones$add_1 & selected$add_2 == df_allophones$add_2)
        selected <- df_allophones[selector,]
      }
  
  
  
  # Rule 8 - "Ğ" changes
  # if(selected$orthography == "ğ" & prefix$category =="vowel" & suffix$category =="vowel" &
  #    (prefix$vector_1 == "front" | suffix$vector_1 =="front")){
  #   selected <- df_underlying[which(df_underlying$orthography =="y"),]
  # } 
  
  # Rule 8 - "L" changes
  
  
  translation %<>% append(selected$allophone)
  phonetic <- tibble(sentence = sentences[each], word = words[every], phonetic = selected$allophone)
  translated %<>% rbind(phonetic)
#letter loop      
    }

# word loop

df <- paste(translated$phonetic, collapse = "-") %>% strsplit("-") %>% unlist() %>%
    as.data.frame() %>% dplyr::select(letters = ".", allophone = ".") %>%
    tibble::rownames_to_column(var = "order") %>% left_join(df_allophones, by = "allophone")
df$order %<>% as.integer()
df$class <- ifelse(df$letters %in% vowels, "V","C")
  
# subset a df of nuclei
df_nucleus <- df %>% subset(class =="V")

# next if the word has no vowel
if (length(df_nucleus$class) == 0){next}
  
# number of letters
letters_num <- 1:length(df$order)
  
### word syllabifier
for (whole in 1:length(df_nucleus$class)){
  
        # nucleus 
        nucleus_num <- df_nucleus$order[whole]*1
        nucleus <- df$letters[nucleus_num]
        
        # remove the addressed nucleus
        letters_num %<>% setdiff(nucleus_num)
        
        # onset
        if(whole=="1"){
          onset_num <- ((1:df_nucleus$order[whole]) %>% setdiff(df_nucleus$order[whole]))*1
        } else if (df$class[nucleus_num-1] =="V"){
          onset_num <- integer()
        } else if ((nucleus_num-2) %in% letters_num){
          onset_num <- (df$order[nucleus_num-2]:df$order[nucleus_num-1])
        } else {
          onset_num <- df_nucleus$order[whole] - 1
        }
        
        if(length(onset_num)=="0"||onset_num=="0"){
          onset <- character()
        } else {
          onset <- paste(df$letters[onset_num], collapse="")
        }
        
        # remove the addressed onset
        letters_num %<>% setdiff(onset_num)
        
        # determine the next nucleus number
        next_nuc_num <- df_nucleus$order[whole+1]*1
        
        # coda number(s)
        if (is.na(next_nuc_num)& nucleus_num+1>length(df$letters)){
          coda_num <- integer()
        } else if (is.na(next_nuc_num) ){ 
          coda_num <- ((nucleus_num+1):(length(df$letters)))*1 # for word endings
        }else if (nucleus_num == (next_nuc_num -1)){ # for vowel clusters
          coda_num <- integer()
        } else if (length(nucleus_num:next_nuc_num)>4 & 
                   (df$letters[(nucleus_num+1):(next_nuc_num-1)] %>%
                    as.data.frame() %>% dplyr::select(letters = ".") %>%
                    mutate(sonority = ifelse(letters %in% sonorant,"S","N")) %>% dplyr::select(sonority) %>% unlist() %>%
                    paste(collapse = ""))=="NNS"){ # mid word onset cluster
          coda_num <- nucleus_num+1
        } else { # no mid word onset cluster separator
          next_ons_num <- df$order[df_nucleus$order[whole+1]*1-1]*1
          coda_num <- (nucleus_num+1):(next_nuc_num-1) %>% setdiff(next_ons_num)
        }
        
        # coda
        if (coda_num > length(df$letters)||length(coda_num)=="0"){
          coda <- character()
        } else {
          coda <- paste(df$letters[coda_num], collapse = "")
          # remove coda number
          letters_num %<>% setdiff(coda_num)
        }
        
        # make up the syllable
        syllable <- paste0(onset,nucleus,coda)
        syl_for_structure <- paste(paste(df$letters[onset_num], collapse="-"), nucleus, paste(df$letters[coda_num], collapse = "-"), sep ="-")
        
        # make up the syllable structure
        syllable_look <- syl_for_structure %>% str_split("-") %>% unlist() %>% stringi::stri_remove_empty() %>%
          as.data.frame() %>% dplyr::select(letters = ".") %>%
          mutate(letters = ifelse(letters %in% vowels,"V","C")) %>% unlist() %>%
          paste(collapse = "")
        
        # write the syllable and the word
        if (whole == "1"){
          syllabified <- syllable
          structure <- syllable_look
        } else {
          syllabified %<>% paste(syllable, sep = "-")
          structure %<>% paste(syllable_look, sep = "-")
        }
        
        syl_num <- whole*1
        syl <- tibble(sentence = sentences[each], word = words[every],
                      transcribed = paste(translated$phonetic, collapse = "-"),
                      syllabified, structure, syl_num)
}

# use syl
output_words %<>% rbind(syl)

if(length(letters_num)!="0"){break}
  }
output_words
  }

close(pb)
stopCluster(core_cluster)

saveRDS(final_parse, "translated_tdk.rds")


