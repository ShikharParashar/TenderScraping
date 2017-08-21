
rm(list=ls())

packages <- c('rvest', 'stringi', 'tidyverse','lubridate')
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

start_time <- proc.time()

##Main Page to scrap and get total no of records.
data <- read_html('https://eprocure.gov.in/mmp/latestactivetenders')
total_tenders_raw <- html_nodes(data,xpath = '//*[(@id = "table")]')
All_tenders <- data.frame(html_table(total_tenders_raw, header = TRUE))
links <- html_nodes(data, xpath='//*[(@id = "table")] | //td | //a')
links_fair <- html_attr(links,'href')
links_fair <- links_fair[grep("tendersfullview",links_fair)]
All_tenders <- cbind(All_tenders,links_fair)


##Reading the total number of records to fetch
Count_of_Recs_raw <- html_nodes(data, xpath = '//*[(@id = "edit-l-active-teners")]//div')
Count_of_Recs <- as.numeric(gsub("Total Tenders : ","",html_text(Count_of_Recs_raw[1])))


##Table Scrapping starts here. Page one has already been scrapped to get the initial data frame.
for (page_no in 2:round(Count_of_Recs/10)){
  url_bit1 <- 'https://eprocure.gov.in/mmp/latestactivetenders/page='
  url <- paste(url_bit1, page_no, sep="")
  cat(url,"\n")
  cat(proc.time() - start_time, "\n")
  data <- read_html(url)
  total_tenders_raw <- html_nodes(data,xpath = '//*[(@id = "table")]')
  Page_tenders <- data.frame(html_table(total_tenders_raw, header = TRUE))
  links <- html_nodes(data, xpath='//*[(@id = "table")] | //td | //a')
  links_fair <- html_attr(links,'href')
  links_fair <- links_fair[grep("tendersfullview",links_fair)]
  Page_tenders <- cbind(Page_tenders,links_fair)
  All_tenders <- rbind(All_tenders,Page_tenders)
}

nrow(All_tenders)

##Preparing links for all individual render records.  
base_url <- 'https://eprocure.gov.in'
All_tenders$links_fair <- sapply(All_tenders$links_fair,function(x) paste(base_url,x,sep=""))


Final_copy <- All_tenders


cat("READING THE SECOND PAGE STARTS NOW")


##Extracting information from each tender document.

for (counter in 1:nrow(All_tenders)){
  
  cat(counter, " of ", nrow(All_tenders), "\t", proc.time() - start_time, "\n")
  detail_data <- read_html(All_tenders$links_fair[counter])
  tender_cols <- html_nodes(detail_data, xpath=".//table[@class='viewtablebg']/tr/td[1]") %>% 
  html_text(trim=TRUE) %>% 
  stri_replace_last_regex("\ +:$", "") %>% 
  stri_replace_all_fixed(" ", "_") %>% 
  stri_trans_tolower()
  
  tender_cols1 <- html_nodes(detail_data, xpath=".//table[@class='viewtablebg']/tr/td[3]") %>% 
  html_text(trim=TRUE) %>% 
  stri_replace_last_regex("\ +:$", "") %>% 
  stri_replace_all_fixed(" ", "_") %>% 
  stri_trans_tolower()

  detail_data_semi_final <- html_nodes(detail_data, xpath=".//table[@class='viewtablebg']/tr/td[2]") %>% 
  html_text(trim=TRUE) %>% 
  as.list() %>% 
  set_names(tender_cols) %>% 
  flatten_df()
  
  detail_data_semi_final1 <- html_nodes(detail_data, xpath=".//table[@class='viewtablebg']/tr/td[4]") %>% 
  html_text(trim=TRUE) %>% 
  as.list() %>% 
  set_names(tender_cols1) %>% 
  flatten_df()
  
  detail_data_semi_final_combined <- cbind(detail_data_semi_final, detail_data_semi_final1)
  
  for (cntr2 in seq(9,29)){
    Final_copy[counter,cntr2] <- detail_data_semi_final_combined[cntr2-8]
  }
  
}

time_elapsed <- proc.time() - start_time
