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
      cat('The pracrtice id ', user_practice_id, 'is valid.')
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
    tab_header(title = md("Top 5 drugs perscribed for GP Practice")) %>%
    cols_label(
      bnfname = "Name",
      pescribed = "Total amount pescribed"
    )
  cat("Top 5 medication pescribed ", gp," are:\n", sep="")
  print(top_5_table)
  
}

diagnoised_with_cancer <- function(dbs, gp) {
  tryCatch({
    cancer_patients <- find_cancer_patients(dbs, gp)
    all_pateients <- find_all_patients(dbs, gp)
    
    cancer_patients %>% gt() %>%
      tab_header(title = md("Patients with Cancer at selected GP Practice"))
    
    count_cancer_patients <- as.numeric(nrow(cancer_patients))
    count_all_patients <- as.numeric(nrow(all_pateients))
    
    cat("The practice has a total of ", count_cancer_patients,  " patients diagnosed with cancer\n", sep = "")
    cat("The practice has a total of ", count_all_patients,  " patients diagnosed\n", sep = "")
    
    percentage = (nrow(cancer_patients)/nrow(all_pateients))*100
    cat("The total percentage of patients diagnosed with cancer is: ", percentage,  "%\n", sep = "")
  },
  error=function(e) {
    print("This practice does not have any cancer patients.")
  })
}

gp_region <- function( dbs, gp){
  selected_gp <- dbGetQuery(dbs, qq('select * from address
                      where practiceid = \'@{user_practice_id}\''))
  
  Region <- selected_gp %>% select(county) #county was chosen for regional analysis
  
  Region %>% gt() %>%
    tab_header(title = md("GP Practice county location")) %>%
    cols_label(
      county = "Name",
    )
  Region_county <- paste(Region$county)
  cat('\nThe practice ',user_practice_id, ' was located in following region.\n', Region_county, sep='')
  print(Region_county)
  return (Region_county)
} 

region_cancer_compare <- function(dbs, gp, gp_area) {
  cancer <- 
  all_cancer <- cancer %>% select(numerator) %>%  summarise(sum(numerator,na.rm = TRUE))
  all_patients <- cancer %>% select(field4) %>% summarise(sum(field4,na.rm = TRUE))
  cancer_rate_wales <-  (all_cancer/all_patients) *100 
  cat('\nThe cancer rate for all practices are:\n', sep='')
  print(cancer_rate_wales)
}



library(data.table)
#install.packages('data.table')
#sum_data <- raw_data[,.(SAMPLE = .N),by=c("DRUG")] ###sample counts records by DRUG column
#sum_data[,RANK:=frank(-SAMPLE)] #ranks sample top n = 1
#sum_data[RANK<=10,] #top ten