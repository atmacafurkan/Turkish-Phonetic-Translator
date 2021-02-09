library(tidyverse)
library(prodlim)
library(stringi)
library(magrittr)
library(lubridate)
library(pbapply)


df_underlying <- read_csv2("underlying.csv")
df_allophones <- read_csv2("allophones.csv")

sentences <- readRDS("tdk_inventory.rds") %>% .$word %>% tolower() %>% unique()


# loop progress & timer
pb <- timerProgressBar(min = 0,      # Minimum value of the progress bar
                     max = length(sentences), # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = ">")

translated <- tibble(sentence = character(), word = character(), phonetic = character())


for (each in 1:length(sentences)){
  words <- str_split(sentences[each], pattern = "\\s") %>% unlist()
  
  for (every in 1:length(words)){
    letters <- str_split(words[every], pattern = "") %>% unlist()
    translation <- character()
    # translation <- c("e","l","e","k","t","r","i","k")
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
  }
# sentence loop
  # timer & progress
  setTimerProgressBar(pb, each)
}

close(pb)

translated %<>% group_by(sentence, word) %>% mutate(transcription = paste(phonetic, collapse = "-")) %>% 
  distinct(sentence, word, .keep_all = T) %>% dplyr::select(-phonetic) %>% ungroup()



vowels <- df_allophones %>% subset(category == "vowel") %>% .$allophone
sonorant <- df_allophones %>% subset(syllabifier == "son") %>% .$allophone


df_syl <- data.frame(word = character(), syllabified = character(),
                     structure = character(), syl_num = integer())

# loop progress & timer
new_pb <- timerProgressBar(min = 0,      # Minimum value of the progress bar
                     max = length(translated$word), # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = ">")



for (my_word in 1:length(translated$transcription)){
  
  word <- translated$transcription[my_word]
  
  df <- word %>% strsplit("-") %>% unlist() %>%
    as.data.frame() %>% dplyr::select(letters = ".", allophone = ".") %>%
    tibble::rownames_to_column(var = "order") %>% left_join(df_allophones, by = "allophone")
  
  df$order %<>% as.integer()
  df$class <- ifelse(df$letters %in% vowels, "V","C")
  
  # subset a df of nuclei
  df_nucleus <- df %>% subset(class =="V")
  if (length(df_nucleus$class) == 0){
    syl <- data.frame(word = NA, syllabified = NA,
                      structure = NA, syl_num = NA)
    df_syl %<>% rbind(syl)
    next
  }
  
  # number of letters
  letters_num <- 1:length(df$order)
  
  ### word syllabifier
  for (each in 1:length(df_nucleus$class)){
    # nucleus 
    nucleus_num <- df_nucleus$order[each]*1
    nucleus <- df$letters[nucleus_num]
    
    # remove the addressed nucleus
    letters_num %<>% setdiff(nucleus_num)
    
    # onset
    if(each=="1"){
      onset_num <- ((1:df_nucleus$order[each]) %>% setdiff(df_nucleus$order[each]))*1
    } else if (df$class[nucleus_num-1] =="V"){
      onset_num <- integer()
    } else if ((nucleus_num-2) %in% letters_num){
      onset_num <- (df$order[nucleus_num-2]:df$order[nucleus_num-1])
    } else {
      onset_num <- df_nucleus$order[each] - 1
    }
    
    if(length(onset_num)=="0"||onset_num=="0"){
      onset <- character()
    } else {
      onset <- paste(df$letters[onset_num], collapse="")
    }
    
    # remove the addressed onset
    letters_num %<>% setdiff(onset_num)
    
    # determine the next nucleus number
    next_nuc_num <- df_nucleus$order[each+1]*1
    
    # coda number(s)
    if (is.na(next_nuc_num)){
      coda_num <- ((nucleus_num+1):(length(df$letters)))*1 # for word endings
    } else if (nucleus_num == (next_nuc_num -1)){ # for vowel clusters
      coda_num = integer()
    } else if (length(nucleus_num:next_nuc_num)>4 & 
               (df$letters[(nucleus_num+1):(next_nuc_num-1)] %>%
                as.data.frame() %>% dplyr::select(letters = ".") %>%
                mutate(sonority = ifelse(letters %in% sonorant,"S","N")) %>% dplyr::select(sonority) %>% unlist() %>%
                paste(collapse = ""))=="NNS"){ # mid word onset cluster
      coda_num <- nucleus_num+1
    } else { # no mid word onset cluster separator
      next_ons_num <- df$order[df_nucleus$order[each+1]*1-1]*1
      coda_num <- (nucleus_num+1):(next_nuc_num-1) %>% setdiff(next_ons_num)
    }
    
    
    # coda
    if (coda_num>length(df$letters)||length(coda_num)=="0"){
      coda <- character()
    } else {
      coda <- paste(df$letters[coda_num], collapse = "")
      # remove coda number
      letters_num %<>% setdiff(coda_num)
    }
    
    # make up the syllable
    syllable <- paste0(onset,nucleus,coda)
    syl_for_structure <- paste(onset, nucleus, coda, sep ="-")
    
    # make up the syllable structure
    syllable_look <- syl_for_structure %>% str_split("-") %>% unlist() %>% stringi::stri_remove_empty() %>%
      as.data.frame() %>%
      dplyr::select(letters = ".") %>% mutate(letters = ifelse(letters %in% vowels,"V","C")) %>% unlist() %>%
      paste(collapse = "")
    
    # write the syllable and the word
    if (each == "1"){
      syllabified <- syllable
      structure <- syllable_look
    } else {
      syllabified %<>% paste(syllable, sep = "-")
      structure %<>% paste(syllable_look, sep = "-")
    }
    
    syl_num <- each*1
    syl <- data.frame(word, syllabified,structure, syl_num)
  }
  
  if(length(letters_num)=="0"){
    df_syl %<>% rbind(syl)
  }else{break}
  
  # progress bar
  setTimerProgressBar(new_pb, my_word)
}


df_syl %<>% dplyr::select(-word)

translated %<>% cbind(df_syl)

saveRDS(translated, "translated_tdk.rds")








