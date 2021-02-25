source("DBS_Connection.R");

library(magrittr)
library(dplyr)
library(lubridate)
library(gt)
library(glue)


##################### Display GP Practices IDs #################################
display_gp_prac <- function(dbs) {
  gp_names <- gp_practices(dbs)
  View(gp_names)
}


################# GP User Selection Validation check ###########################
select_gp_prac <- function() { #(dbs)
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


################## User Selected GP Region Selector ############################
gp_region <- function(dbs, gp){
  selected_gp <- dbGetQuery(dbs, qq('select * from address
                      where practiceid = \'@{gp}\''))
  
  Region <- selected_gp %>% select(county) #county was chosen for regional analysis
  
  Region_county <- paste(Region$county)
  cat('\nThe practice ',user_practice_id, ' was located in following region.\n', 
      Region_county, sep='')
  
  Region %>% gt() %>%
    tab_header(title = md("GP Practice county location")) %>%
    cols_label(
      county = "Name",
    )
  
  return (Region_county)
} 


################# Region User Selection Validation check #######################
region_select <- function(dbs) {
  available_regions <- select_region(dbs)
  print(available_regions) ## Put in pop out?
  
  valid_id <- FALSE
  while (valid_id != TRUE) {
    user_selected_region <- toupper(readline('Enter a Region: '))
    if (user_selected_region %in% available_regions$upper ){
      valid_id <- TRUE
      cat('The region ', user_selected_region, 'is valid.')
    } else {
      cat('The entered region (', user_selected_region,
          ') is not valid, please try again.\n', sep='')
    }
  }
  return(user_selected_region)
}


######################### Selected GP Top 5 Drugs ##############################
disply_gp_top5_drugs <- function(dbs, gp) {
  top_5 <- select_gp(dbs, gp)
  tryCatch({
    top_5_table <- top_5 %>% distinct() %>% 
      filter(str_detect(bnfname, 'Tab')==TRUE) %>%
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


##################### Selected Region Top 5 Drugs ##############################
region_top_5_drugs <- function(dbs, gp_location) {
  all_region_gps <- region_details(dbs, gp_location)
    
  region_top_5_table <- all_region_gps %>% 
      distinct() %>% filter(str_detect(bnfname, 'Tab')==TRUE) %>%
      group_by(bnfcode, bnfname) %>%
      summarise(pescribed=sum(items)) %>%
      arrange(desc(pescribed)) %>% head(5)
    
    cat("Top 5 medication pescribed ", gp_location," are:\n", sep="")
    print(region_top_5_table)

    region_top_5_table %>% gt() %>%
      tab_header(title = md("Top 5 drugs perscribed in selected region")) %>%
      cols_label(
        bnfname="Name",
        pescribed="Total amount pescribed"
      )
}


######################## GP Cancer Diagnosis % #################################
diagnoised_with_cancer <- function(dbs, gp) {
  tryCatch({
    cancer_patients <- find_cancer_patients(dbs, gp)
    all_patients <- find_all_patients(dbs, gp)
    
    cancer_count <- cancer_patients %>% select(numerator) %>%  
      summarise(sum(numerator,na.rm = TRUE))
    
    all_patient_count <- all_patients %>% select(numerator) %>%  
      summarise(sum(numerator,na.rm = TRUE))
    
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


####################### Region Smoker Diagnosis % ##############################
declared_as_smokers <- function(dbs,gp_area){
  tryCatch({
    smoker_patients <- find_smoker_patients(dbs, gp_area)
    all_region_patients <- find_all_region_patients(dbs, gp_area)
    
    smokers_count <- smoker_patients %>% select(numerator) %>%
      summarise(sum(numerator,na.rm = TRUE))
    all_region_count <- all_region_patients %>% select(numerator) %>%  
      summarise(sum(numerator,na.rm = TRUE))
    smoker_percent <- round(((smokers_count/all_region_count)*100), digits = 2)
    
    output_message <- glue('Total number of patients declared as smokers: {smokers_count}
         Total number of region diagnosis: {all_region_count}
         Percentage of smoking patients: {smoker_percent}%')
    print(output_message)
    
    smoker_patients %>% gt() %>%
      tab_header(title = md("Patients who are smokers at selected region"))
  },
  error=function(e) {
    print("This practice does not have any cancer patient records or an error has occured.")
  })
}


################### Region Cancer Diagnosis Compare Wales ######################
region_cancer_compare <- function(dbs, gp, gp_area) {
  # get details from the database
  gp_cancer_details <- find_cancer_patients(dbs, gp)
  region_cancer_details <- find_region_cancer_patients(dbs, gp_area)
  wales_cancer_details <- find_wales_inicator_patients(dbs, 'CAN')
  
  gp_cancer_count <- gp_cancer_details %>% select(numerator) %>%  
    summarise(sum(numerator,na.rm = TRUE))
  region_cancer_count <- region_cancer_details %>% select(numerator) %>%  
    summarise(sum(numerator,na.rm = TRUE))
  wales_cancer_count <- wales_cancer_details %>% select(numerator) %>%  
    summarise(sum(numerator,na.rm = TRUE))
  
  output_message <- glue('Total number of cancer gp patients: {gp_cancer_count}
         Total number of area cancer patients: {region_cancer_count}
         Total number of Wales cancer patients: {wales_cancer_count}')
  print(output_message)
  
  df <- data.frame(Area = c("GP", "Region", "Wales"), 
                   Total = c(as.integer(gp_cancer_count),
                             as.integer(region_cancer_count),
                             as.integer(wales_cancer_count))
                   )
  
  p<-ggplot(data=df, aes(x=Area, 
                         y=Total, 
                         color = Area, 
                         fill = Area, 
                         label = Total)
            ) +
    geom_bar(stat="identity")
  
  p + geom_text(vjust=-1)
}


################### Region Smoker Diagnosis Compare Wales ######################
region_smoking_compare <- function(dbs,gp_area) {
  smoker_patients <- find_smoker_patients(dbs, gp_area)
  wales_smoker_details <- find_wales_inicator_patients(dbs, 'SMO')
  
  region_smoker_count <- smoker_patients %>% select(numerator) %>%  
    summarise(sum(numerator,na.rm = TRUE))
  wales_smoker_count <- wales_smoker_details %>% select(numerator) %>%  
    summarise(sum(numerator,na.rm = TRUE))
  
  output_message <- glue('
         Total number of people in the region declared as a smoker: {region_smoker_count}
         Total number of people in Wales declared as a smoker: {wales_smoker_count}')
  print(output_message)
  
  df <- data.frame(Area = c("Region", "Wales"), 
                   Total = c(as.integer(region_smoker_count),
                             as.integer(wales_smoker_count))
  )
  
  p<-ggplot(data=df, aes(x=Area, 
                         y=Total, 
                         color = Area, 
                         fill = Area, 
                         label = Total)
  ) +
    geom_bar(stat="identity")
  p + geom_text(vjust=-1)
}


####################### Region Drugs Spend ####################
gp_region_medication <- function(dbs, region) {
  all_regions <- select_all_region_details(dbs) ##region_select(dbs, region)
  
  print(all_regions)
  
  #all_region_spen
    drugs_df = data.frame(region = character(), 
                          total_patients = numeric(),  
                          spend = numeric())
  
  for(i in 1:nrow(all_regions)) {
    selected_region <- all_regions[i,1]## n2c
    region_data <- region_select(dbs, region)## n2c
    gp_patients <- find_all_patients(dbs,selected_gp)## n2c
    
    
    if (nrow(gp_data) != 0) {
      if (nrow(gp_patients) != 0) {
        number_of_patients <- gp_patients %>% select(numerator) %>% 
          summarise(sum(numerator,na.rm = TRUE))
      }
      else {
        number_of_patients <- 0
      }
      
      meds_cost <- gp_data  %>% select(actcost) %>%  
        summarise(sum(actcost,na.rm = TRUE))
      meds_cost <- round(meds_cost, digits = 2)
      
      output_message <- glue("The practice {selected_gp} spent a total of 
                             £{meds_cost} on drugs")
      print(output_message)
      
      drugs_df = rbind(drugs_df, 
                       data.frame(region = selected_region, 
                                           total_patients = as.numeric(number_of_patients),  
                                           spend = as.numeric(meds_cost)))
    }
  }
  
  p<-ggplot(data=drugs_spend, aes(x = total_patients, 
                                  y = spend)
  ) +
    geom_point()
  print(p) 
  
  return (drugs_df)
}


######################## GP Total spend on Drugs ###############################
gp_spend_medication <- function(dbs) {
  ##get practice details.
  all_gps <- gp_practices(dbs)
  drugs_df = data.frame(gp = character(), 
                        total_patients = numeric(),  
                        spend = numeric())
 
  for(i in 1:nrow(all_gps)) {
    selected_gp <- all_gps[i,1]
    gp_data <- select_gp(dbs,selected_gp)
    gp_patients <- find_all_patients(dbs,selected_gp)
    
    if (nrow(gp_data) != 0) {
      if (nrow(gp_patients) != 0) {
        number_of_patients <- gp_patients %>% select(numerator) %>% 
          summarise(sum(numerator,na.rm = TRUE))
      }
      else {
        number_of_patients <- 0
      }
      
      meds_cost <- gp_data  %>% select(actcost) %>%  summarise(sum(actcost,na.rm = TRUE))
      meds_cost <- round(meds_cost, digits = 2)
      
      output_message <- glue("The practice {selected_gp} spent a total of £{meds_cost} on drugs")
      print(output_message)
      
      drugs_df = rbind(drugs_df, data.frame(gp = selected_gp, 
                                            total_patients = as.numeric(number_of_patients),  
                                            spend = as.numeric(meds_cost)))
    }
  }
  
  p<-ggplot(data=drugs_spend, aes(x = total_patients, 
                                  y = spend)
  ) +
    geom_point()
  
  print(p) 

  return (drugs_df)
}


#################### Calculate GP Spend Corrolation ############################
spend_correlation_check <- function(dbs) {
  # get spend of gp -> 'At practice level'
  #gp_patients <- find_all_patients(dbs,selected_gp)
  #gp_data <- select_gp(dbs,selected_gp)
  #meds_cost <- gp_data  %>% select(actcost) %>%  summarise(sum(actcost,na.rm = TRUE))
  #meds_cost <- round(meds_cost, digits = 2)
  # correlate spend to cancer
  all_gp_spend <- find_all_gp_spend(dbs)
  gp_cancer <- find_all_indi_patients(dbs, 'CAN%')
  gp_diabet <- find_all_indi_patients(dbs, 'DM%')
  gp_demenstia <- find_all_indi_patients(dbs,'DEM%')
  gp_hypten <- find_all_indi_patients(dbs,'HYP%')
  
  print(nrow(all_gp_spend))
  print(nrow(gp_cancer))
  print(nrow(gp_diabet))
  print(nrow( gp_demenstia))
  print(nrow( gp_hypten))
  
  all_gp_count <- all_gp_spend %>% select(actcost) %>%  
    summarise(sum(actcost,na.rm = TRUE))
  gp_cancer_count <- gp_cancer %>% select(numerator) %>%  
    summarise(sum(numerator,na.rm = TRUE))
  gp_diabet_count <- gp_diabet %>% select(numerator) %>%  
    summarise(sum(numerator,na.rm = TRUE))
  gp_demenstia_count <- gp_demenstia %>% select(numerator) %>%  
    summarise(sum(numerator,na.rm = TRUE))
  gp_hypten_count <- gp_hypten %>% select(numerator) %>%  
    summarise(sum(numerator,na.rm = TRUE))
  #print(gp_cancer)
  
  #wales_cancer_details <- find_wales_inicator_patients(dbs, 'CAN')
  
  cancer_cor <- cor.test(as.numeric(gp_cancer$numerator),
                         as.numeric(all_gp_spend$actcost), method=c("pearson", "kendall", "spearman"))
  print(cancer_cor)
  diabet_cor <- cor.test(as.numeric(gp_diabet$numerator),
                         as.numeric(all_gp_spend$actcost), method=c("pearson", "kendall", "spearman"))
  print(diabet_cor)
  demenstia_cor <- cor.test(as.numeric(gp_demenstia$numerator),
                            as.numeric(all_gp_spend$actcost), method=c("pearson", "kendall", "spearman"))
  print(demenstia_cor)
  hypten_cor <- cor.test(as.numeric(gp_hypten$numerator),
                         as.numeric(all_gp_spend$actcost), method=c("pearson", "kendall", "spearman"))
  print(hypten_cor)
  # correlate spend to diabetes
  
  # correlate spend to dementia
  
  # correlate spend to hypertension
}


################################### EOF ########################################