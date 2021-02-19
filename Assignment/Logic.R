source("DBS_Connection.R");

library(magrittr)
library(dplyr)
library(lubridate)
library(gt)
library(glue)


## Collect GP Practices
display_gp_prac <- function(dbs) {
  gp_names <- gp_practices(dbs)
  View(gp_names)
}

select_gp_prac <- function() { #(dbs)
  valid_id <- FALSE
  while (valid_id != TRUE) {
    user_practice_id <- toupper(readline('Enter a practice ID (Wxxxxx):'))
    if (grepl('^W[0-9]{5}$', user_practice_id)){
      valid_id <- TRUE
      cat('The pracrtice id ', user_practice_id, 'is valid.')
      #tryCatch({
      #  select_gp(dbs,user_practice_id)
      #},
      #error=function(e) {
      #  print("GP practice ID matches format but does not exist in the database, please try again.")
      #})
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
  
  tryCatch({
    top_5_table <- top_5 %>% distinct() %>% filter(str_detect(bnfname, 'Tab')==TRUE) %>%
      mutate(total=sum(quantity)) %>% 
      group_by(bnfcode, bnfname) %>%
      summarise(pescribed=sum(items)) %>%
      arrange(desc(pescribed)) %>% head(5)
    
    cat("Top 5 medication pescribed ", gp," are:\n", sep="")
    print(top_5_table)
    
    top_5_table %>% gt() %>%
      tab_header(title = md("Top 5 drugs perscribed for GP Practice")) %>%
      cols_label(
        bnfname = "Name",
        pescribed = "Total amount pescribed"
      )
    
  },
  error=function(e) {
    print("This practice does not have any records or an error has occured.")
    print("Please try again or select another GP practice ID.")
  })
}

diagnoised_with_cancer <- function(dbs, gp) {
  tryCatch({
    cancer_patients <- find_cancer_patients(dbs, gp)
    all_patients <- find_all_patients(dbs, gp)
    
    cancer_count <- cancer_patients %>% select(numerator) %>%  summarise(sum(numerator,na.rm = TRUE))
    #print(cancer_count)
    
    all_patient_count <- all_patients %>% select(numerator) %>%  summarise(sum(numerator,na.rm = TRUE))
    #print(all_pateient_count)
    
    cancer_percent <- round(((cancer_count/all_patient_count)*100), digits = 2)
    
    output_message <- glue('Total number of cancer patients: {cancer_count}
         Total number of practice diagnosis: {all_patient_count}
         Percentage of cancer patients: {cancer_percent}%')
    
    print(output_message)
    
    cancer_patients %>% gt() %>%
      tab_header(title = md("Patients with Cancer at selected GP Practice"))
  },
  error=function(e) {
    print("This practice does not have any cancer patient records or an error has occured.")
  })
}

# Get region cancer details
region_cancer_compare <- function(dbs, gp, gp_area) {
  # get details from the database
  gp_cancer_details <- find_cancer_patients(dbs, gp)
  region_cancer_details <- find_region_cancer_patients(dbs, gp_area)
  wales_cancer_details <- find_wales_cancer_patients(dbs)
  
  gp_cancer_count <- gp_cancer_details %>% select(numerator) %>%  summarise(sum(numerator,na.rm = TRUE))
  region_cancer_count <- region_cancer_details %>% select(numerator) %>%  summarise(sum(numerator,na.rm = TRUE))
  wales_cancer_count <- wales_cancer_details %>% select(numerator) %>%  summarise(sum(numerator,na.rm = TRUE))
  
  output_message <- glue('Total number of cancer gp patients: {gp_cancer_count}
         Total number of area cancer patients: {region_cancer_count}
         Total number of Wales cancer patients: {wales_cancer_count}')
  
  print(output_message)
  
  df <- data.frame(Area = c("GP", "Region", "Wales"), 
                   Number = c(as.integer(gp_cancer_count),as.integer(region_cancer_count),
                              as.integer(wales_cancer_count))
                   )
  
  p<-ggplot(data=df, aes(x=Area, 
                         y=Number, 
                         color = Area, 
                         fill = Area, 
                         label = Number)
            ) +
    geom_bar(stat="identity")
  
  p + geom_text(vjust=-1)
  #print(gp_cancer_count)
  #print(region_cancer_count)
  #print(wales_cancer_count)
  
  #  all_cancer <- cancer %>% select(numerator) %>%  summarise(sum(numerator,na.rm = TRUE))
  #all_patients <- cancer %>% select(field4) %>% summarise(sum(field4,na.rm = TRUE))
  #cancer_rate_wales <-  (all_cancer/all_patients) *100 
  #cat('\nThe cancer rate for all practices are:\n', sep='')
  #print(cancer_rate_wales)
}

# Find the GP practice Area
gp_region <- function(dbs, gp){
  selected_gp <- dbGetQuery(dbs, qq('select * from address
                      where practiceid = \'@{gp}\''))
  
  Region <- selected_gp %>% select(county) #county was chosen for regional analysis
  
  Region_county <- paste(Region$county)
  cat('\nThe practice ',user_practice_id, ' was located in following region.\n', Region_county, sep='')
  
  Region %>% gt() %>%
    tab_header(title = md("GP Practice county location")) %>%
    cols_label(
      county = "Name",
    )
  
  #print(Region_county)
  return (Region_county)
} 

gp_spend_medication <- function(dbs) {
  ##get practice details.
  all_gps <- gp_practices(dbs)
  drugs_df = data.frame(gp = character(), total_patients = numeric(),  spend = numeric())
  #print(all_gps[1,1])
  #idx_counter = 1
  
  all_gp_spend <- list()
  
  for(i in 1:nrow(all_gps)) {
    #print(all_gps[i,1])
    selected_gp <- all_gps[i,1]
    gp_data <- select_gp(dbs,selected_gp)
    gp_patients <- find_all_patients(dbs,selected_gp)
    
    
    if (nrow(gp_data) != 0) {
      
      if (nrow(gp_patients) != 0) {
        number_of_patients <- gp_patients %>% select(numerator) %>%  summarise(sum(numerator,na.rm = TRUE))
      }
      else {
        number_of_patients <- 0
      }
      
      meds_cost <- gp_data  %>% select(actcost) %>%  summarise(sum(actcost,na.rm = TRUE))
      meds_cost <- round(meds_cost, digits = 2)
      
      output_message <- glue("The practice {selected_gp} spent a total of £{meds_cost} on drugs")
      print(output_message)
      
      drugs_df = rbind(drugs_df, data.frame(gp = selected_gp, total_patients = as.numeric(number_of_patients),  spend = as.numeric(meds_cost)))
      
    }
    
    #rm(gp_data)
  }

  return (drugs_df)
}

spend_correlation_check <- function(dbs, selected_gp) {
  # get spend of gp -> 'At practice level'
  gp_patients <- find_all_patients(dbs,selected_gp)
  gp_data <- select_gp(dbs,selected_gp)
  meds_cost <- gp_data  %>% select(actcost) %>%  summarise(sum(actcost,na.rm = TRUE))
  meds_cost <- round(meds_cost, digits = 2)
  # correlate spend to cancer
  gp_cancer <- find_cancer_patients(dbs, selected_gp)
  gp_cancer_count <- gp_cancer %>% select(numerator) %>%  summarise(sum(numerator,na.rm = TRUE))
  print(gp_cancer)
  
  cancer_cor <- cor.test(as.numeric(gp_cancer_count),as.numeric(meds_cost), method=c("pearson", "kendall", "spearman"))
  print(cancer_cor)
  # correlate spend to diabetes
  
  # correlate spend to dementia
  
  # correlate spend to hypertension
}



#library(data.table)
#install.packages('data.table')
#sum_data <- raw_data[,.(SAMPLE = .N),by=c("DRUG")] ###sample counts records by DRUG column
#sum_data[,RANK:=frank(-SAMPLE)] #ranks sample top n = 1
#sum_data[RANK<=10,] #top ten