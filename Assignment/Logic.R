source("DBS_Connection.R");

library(magrittr)
library(dplyr)
library(lubridate)
library(gt)


## Collect GP Practices
display_gp_prac <- function(dbs) {
  gp_names <- gp_practices(dbs)
  View(gp_names)
}

select_gp_prac <- function() {
  valid_id <- FALSE
  while (valid_id != TRUE) {
    user_practice_id <- toupper(readline('Enter a practice ID (Wxxxxx):'))
    if (grepl('^W[0-9]{5}$', user_practice_id)){
      valid_id <- TRUE
      cat('The pracrtice id ', user_practice_id, 'was found.')
    } else {
      cat('The entered practice ID (', user_practice_id,
          ') is not valid, please try again.\n', sep='')
    }
  }
  
  return(user_practice_id)
}


# Find Practice top 5 drugs prescribed
disply_gp_top5_drugs<- function(dbs, gp) {
  
  top_5 <- select_gp(dbs, gp)
  
  
  top_5_table <- top_5 %>% distinct() %>% filter(str_detect(bnfname, 'Tab')==TRUE) %>%
    mutate(total=sum(quantity)) %>% 
    group_by(bnfcode, bnfname) %>%
    summarise(pescribed=sum(items)) %>%
    arrange(desc(pescribed)) %>% head(5)
  #top_5_table <- top_5 %>% group_by(bnfcode, bnfname) %>% 
  #  count(bnfname) %>% ungroup() %>% arrange(desc(n)) %>% head(5)
  #top_5_data <- top_5_table %>% arrange(desc(n))
  
  top_5_table %>% gt() %>%
    tab_header(title = md("Top 5 drugs in terms of spendings for GP Practice")) %>%
    cols_label(
      bnfname = "Name",
      pescribed = "Total amount pescribed"
    )
  cat("Top 5 medication pescribed", gp," are:\n", sep="")
  print(top_5_table)
  
}

diagnoised_with_cancer <- function(dbs, gp) {
  all_patients <- select_gp(dbs, gp)
  count_of_patients <- nrow(all_patients)
  cat("Total number of patients:", count_of_patients)
  cancer_patients <- find_cancer_patients(dbs,gp)
  count_of_cancer_patients <- nrow(cancer_patients)
  cat("\nTotal number of patients with cancer:", count_of_cancer_patients,"\n")
  percent_of_cancer <- (count_of_cancer_patients/count_of_cancer_patients)
  print(percent_of_cancer)
}

library(data.table)
#install.packages('data.table')
#sum_data <- raw_data[,.(SAMPLE = .N),by=c("DRUG")] ###sample counts records by DRUG column
#sum_data[,RANK:=frank(-SAMPLE)] #ranks sample top n = 1
#sum_data[RANK<=10,] #top ten