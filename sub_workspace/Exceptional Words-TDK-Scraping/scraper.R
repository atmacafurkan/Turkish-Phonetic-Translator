library(tidyverse)
library(magrittr)
library(readr)
library(httr)
library(rvest)
library(stringr)
library(pbapply)



words_list <- tibble(gts_id = integer(), word = character(), lender_code = integer(),
                   lender = character(), pronunciation = character())

bulk_tdk <- tibble(cat = character(), value = character())

# loop progress & timer
pb <- timerProgressBar(min = 0,      # Minimum value of the progress bar
                       max = 92407, # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 50,   # Progress bar width. Defaults to getOption("width")
                       char = ">")
for (each in 1:92407){
  
  # make link
  link <- sprintf("https://sozluk.gov.tr/gts_id?id=%s", each)
  
  # get the link
  access <- GET(link)
  
  access_status <- http_status(access)
  # break the loop is connection is not successful
  if (access_status$category !="Success"){
    print("Connection not successful")
    break
  }
  page <- content(access, as = "text") 
  section <- strsplit(page,",") %>% unlist() %>% as.data.frame() 
  colnames(section) <- "first"
  section %<>% tidyr::separate(col = first ,into = c("cat","value"), sep = ":", extra = "merge", fill = "right") 
  
  bulk_tdk %<>% rbind(section)
  
  section %<>% drop_na()

  word <- section$value[which(str_detect(section$cat,"madde\""))[1]] %>% 
    str_remove_all("[:punct:]")
  
  # lender code
  if(is_empty(which(str_detect(section$cat,"lisan_kodu")))){
  lender_code <- 0

  } else {
  lender_code <- section$value[which(str_detect(section$cat,"lisan_kodu"))[1]] %>% 
    str_remove_all("[:punct:]") 
  }
  
  # lender
  if(is_empty(which(str_detect(section$cat,"lisan_kodu")))){
    lender <- "0"
    
  } else {
    lender <- section$value[which(str_detect(section$cat,"lisan\""))[1]] %>% 
      str_remove_all("[:punct:]")
  }
  
  # pronunciation
  if(is_empty(which(str_detect(section$cat,"telaffuz")))){
    pronunciation <- "null"
    
  } else {
    pronunciation <- section$value[which(str_detect(section$cat,"telaffuz"))[1]] %>% 
      str_remove_all("\"")
  }
  word_x <- tibble(gts_id = each, word, lender_code, lender, pronunciation)
  
  words_list %<>% rbind(word_x)
  # timer & progress
  setTimerProgressBar(pb, each)
}




saveRDS(words_list,"words_tdk.rds")
saveRDS(bulk_tdk, "tdk_whole_gts_scrape.rds")
