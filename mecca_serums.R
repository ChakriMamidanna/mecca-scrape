library(tidyverse)
library(rvest)
library(progress)

totpages <-  read_html("https://www.mecca.com/en-au/skincare/facial-serums/?page=1")%>% 
  html_nodes(".css-1cg9fsu") %>%
  html_text() %>% 
  substr(.,nchar(.)-3+1, nchar(.)) %>% 
  as.numeric()

mecca_pages <- tibble(page_num = 1:ceiling(totpages/48))

#Create the pages of articles to scrape 
mecca_pages <- mecca_pages %>% 
  mutate(page = paste0("https://www.mecca.com/en-au/skincare/facial-serums/?page=", page_num))


#Create a function to grab the links
get_links <- function(page){
  # page <- "https://www.mecca.com/en-au/skincare/facial-serums/?page=1/"
  page <- read_html(page)
  
  page %>% 
    html_nodes(".css-17b0w3j .css-mijcyd") %>%
    html_attr("href") %>% 
    as_tibble() %>% 
    rename("links" = "value")
}

#Scrape the links
mecca_pages <- mecca_pages %>% 
  mutate(links = map(page, get_links))





mecca_pages_links <- mecca_pages %>% 
  unnest(links) %>% 
  mutate(links = paste0("https://www.mecca.com", links))

# pb <- progress_bar$new(total = nrow(mecca_pages_links),
#                        format = "executing [:bar] :percent eta::eta")
# pb$tick()
#Create a function to extract text
#.css-1347y8m.product-name
# .css-14mw569
get_text <- function(link){
  
  # pb$tick()
  Sys.sleep(1/100)
  # link <- "https://www.mecca.com/en-au/emma-lewisham/skin-reset-serum-V-057706/?cgpath=skincare-serums"
  # link <- "https://www.mecca.com/en-au/dr-jart/cicapair-intensive-soothing-repair-serum-I-069992/?cgpath=skincare-serums"
  # link <- "https://www.mecca.com/en-au/skinstitut/10-vitamin-c-serum-I-059377/?cgpath=skincare-serums"
  # link <- "https://www.mecca.com/en-au/alpha-h/midnight-reboot-serum-with-14-glycolic-acid-V-054165/?cgpath=skincare-serums"
  link <- read_html(link)
  
  
  # .css-1347y8m , .css-14mw569 , .css-4m07jg,.css-1r68oyn, .css-1sd34by, .css-1s5rb71 
  alllist <- link %>% 
    html_nodes(".css-1347y8m , .css-14mw569 , .css-4m07jg, .css-1r68oyn, .css-1sd34by, .css-1s5rb71 ") %>% 
    html_text() %>% 
    # head(-10) %>% 
    as_tibble() %>%
    # as.data.frame( responseName = "value") %>%
    t()  #%>%
  
  if (length(alllist) > 6) {
    alllist <- alllist[1:9]
  }else{
    alllist <- alllist
  }
  
  alllist
    # separate(value, c("product"), sep = ":", extra = "merge")

}



# get_text("https://www.mecca.com/en-au/dr-jart/cicapair-intensive-soothing-repair-serum-I-069992/?cgpath=skincare-serums")

#Scrape the actual text

#Progress bar is shown in console
mecca_pages_all <- mecca_pages_links %>% 
  mutate(results = map(links, get_text))


# h <- tidyr::unnest(mecca_pages_all, results) %>% tidyr::unnest(results)
i <- 5 
h <- mecca_pages_all %>% 
  rowwise() %>% 
  mutate(abc = length(results) ) %>% 
  # ungroup() %>% 
  mutate(brand = results[[1]],
        name =  results[[2]],
        price =  results[[3]],
        rating =  results[[4]],
        number_of_rating =  results[[5]],
        size1 = ifelse(abc > i, results[[i+1]] , NA),
        size2 = ifelse(abc > i+1, results[[i+2]] , NA),
        size3 = ifelse(abc > i+2, results[[i+3]] , NA))%>%
  select(-results)

abc <- h %>% 
  mutate(oldname = name,
         name = tolower(name), 
         name = gsub(" ml", "ml",  name)) %>% 
  mutate(size1 = ifelse(size1 == h$size1[1], NA,tolower( size1)),
         size2 = ifelse(size2 == h$size2[1], NA, tolower(size2)),
         size2 = ifelse(size2 == h$size2[12], NA,tolower(size2)),
         size3 = ifelse(size3 == h$size3[1], NA, tolower(size3)),
         size3 = ifelse(size3 == h$size3[12], NA,tolower(size3)),
         size3 = ifelse(size3 == h$size3[13], NA,tolower( size3))) %>% 
  mutate(size1  = gsub(" ml", "ml",  size1), 
         size2  = gsub(" ml", "ml",  size2), 
         size3  = gsub(" ml", "ml",  size3)) %>% 
  mutate(size =ifelse(length(regmatches(name, regexpr("(\\d+)(ml.*)", name, perl = TRUE))) > 0,
                      sub("\\ml.*","",regmatches( name, regexpr("(\\d+)(ml.*)", name, perl = TRUE))), 
                      NA)) %>% 
  mutate(othersize1 =ifelse(length(regmatches(size1, regexpr("(\\d+)(ml.*)", size1, perl = TRUE))) > 0,
                           sub("\\ml.*","",regmatches( size1, regexpr("(\\d+)(ml.*)", size1, perl = TRUE))), 
                           NA) , 
         othersize2 =ifelse(length(regmatches(size2, regexpr("(\\d+)(ml.*)", size2, perl = TRUE))) > 0,
                            sub("\\ml.*","",regmatches( size2, regexpr("(\\d+)(ml.*)", size2, perl = TRUE))), 
                            NA) , 
         othersize3 =ifelse(length(regmatches(size3, regexpr("(\\d+)(ml.*)", size3, perl = TRUE))) > 0,
                            sub("\\ml.*","",regmatches( size3, regexpr("(\\d+)(ml.*)", size3, perl = TRUE))), 
                            NA) ) %>%
  mutate(size = as.integer(size),
         othersize1 = as.integer(othersize1),
         othersize2 = as.integer(othersize2), 
         othersize3 = as.integer(othersize3)) %>% 
  mutate(across(14:17, replace_na, 0)) %>% 
  mutate(biggestsize = max(size, othersize1, othersize2, othersize3, na.rm = T)) %>% 
  mutate(dollarvalue =ifelse(length(regmatches(price, regexpr("(-.*)", price, perl = TRUE))) > 0, 
                             parse_number(str_match(regmatches(price, regexpr("(-.*)", price, perl = TRUE)),"\\$([0-9,.]+)")[,2]), 
                             parse_number(str_match(price,"\\$([0-9,.]+)")[,2]))) %>% 
  mutate(perml = ifelse(biggestsize > 0, dollarvalue/biggestsize, NA)) %>% 
  arrange(perml) %>% 
  mutate(per30ml = perml * 30)

write.csv(abc, paste0(here::here(),"/results/mecca serums ",Sys.Date(), ".csv"), row.names = F)
# name <- h %>% head(2) %>% tail(1) %>% pull(name)
# str_extract("Cicapair Intensive Soothing Repair Serum 30ml", "[ml]")
# sub("ml", "",  "Cicapair Intensive Soothing Repair Serum 30ml")
# tn <-"$235.00" #gsub(" ml", "ml","Gold Recovery Intense Concentrate PM2 6ml x 4")
# regmatches(tn, regexpr("(-.*)", tn, perl = TRUE))
# sub("\\.*$","",regmatches(tn, regexpr("(-.*)", tn, perl = TRUE)))
# parse_number(str_match(tn,"\\$([0-9,.]+)")[,2])

