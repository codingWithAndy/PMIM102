####################### Source Additional R Scripts ############################
source("DBS_Connection.R");

#################### Install Required Packages #################################
#install.packages("magrittr")
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("gt")
#install.packages("glue")
#install.packages("plotly")

##################### Load Required Packages ###################################
library(magrittr)
library(dplyr)
library(lubridate)
library(gt)
library(glue)
library(plotly)

##################### Display GP Practices IDs #################################
display_gp_prac <- function(dbs) {
  gp_names <- gp_practices(dbs)
  View(gp_names)
}

################# GP User Selection Validation check ###########################
select_gp_prac <- function(dbs) { #(dbs)
  available_gp <- gp_practices(dbs)
  valid_id <- FALSE
  while (valid_id != TRUE) {
    user_practice_id <- toupper(readline('Enter a practice ID (Wxxxxx):'))
    if (grepl('^W[0-9]{5}$', user_practice_id) & user_practice_id %in% available_gp$practiceid){
      valid_id <- TRUE
      cat('The practice id ', user_practice_id, 'is valid.')
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
  
  print(Region)
  
  return (Region_county)
} 

################# Region User Selection Validation check #######################
region_select <- function(dbs) {
  available_regions <- select_region(dbs)
  View(available_regions) ## Put in pop out?
  
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
      group_by(bnfcode, bnfname) %>%
      summarise(pescribed=sum(items)) %>%
      arrange(desc(pescribed)) %>% head(5)
    
    cat("\nTop 5 medication pescribed ", gp," are:\n", sep="")
    print(top_5_table)
    
    output_table <- top_5_table %>% gt() %>%
      tab_header(title = md("Top 5 drugs perscribed for GP Practice")) %>%
      cols_label(
        bnfname = "Name",
        pescribed = "Total amount pescribed"
      )
    
    print(output_table)
  },
  error=function(e) {
    print("This practice does not have any records or an error has occured.")
    print("Please try again or select another GP practice ID.")
  })
}

##################### Selected Region Top 5 Drugs ##############################
region_top_5_drugs <- function(dbs, gp_location) {
  all_region_gps <- region_details(dbs, gp_location)
  
  tryCatch({
    region_top_5_table <- all_region_gps %>% 
      distinct() %>% 
      group_by(bnfcode, bnfname) %>%
      summarise(pescribed=sum(items)) %>%
      arrange(desc(pescribed)) %>% head(5)
    
    cat("\nTop 5 medication pescribed ", gp_location," are:\n", sep="")
    print(region_top_5_table)
    
    output_table <- region_top_5_table %>% gt() %>%
      tab_header(title = md("Top 5 drugs perscribed in selected region")) %>%
      cols_label(
        bnfname="Name",
        pescribed="Total amount pescribed"
      )
    
    print(output_table)
  },
  error=function(e) {
    print("This region does not have any records or an error has occured.")
    print("Please try again or select another GP practice ID.")
  })
}

######################## GP Cancer Diagnosis % #################################
diagnoised_with_cancer <- function(dbs, gp) {
  tryCatch({
    cancer_patients <- find_cancer_patients(dbs, gp)
    all_patients <- find_all_patients(dbs, gp)
    
    cancer_count <- cancer_patients$numerator
    all_patient_count <- cancer_patients$field4
    cancer_percent <- round(((cancer_count/all_patient_count)*100), digits = 2)

    output_message <- glue('\n
         ############### GP Cancer Patient Details ################
         Total number of cancer patients: {cancer_count}
         Total number of practice diagnosis: {all_patient_count}
         Percentage of cancer patients: {cancer_percent}%')
    print(output_message)
    
    output_table <- cancer_patients %>% gt() %>%
      tab_header(title = md("Patients with Cancer at selected GP Practice"))
    print(output_table)
    
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
    all_region_count <- smoker_patients %>% select(field4) %>%  
      summarise(sum(field4,na.rm = TRUE))
    smoker_percent <- round(((smokers_count/all_region_count)*100), digits = 2)
    
    output_message <- glue('\n
         ############### Region Smoker Patient Details ################
         Total number of patients declared as smokers: {smokers_count}
         Total number of region diagnosis: {all_region_count}
         Percentage of smoking patients: {smoker_percent}%')
    print(output_message)
    
    table_output <- smoker_patients %>% gt() %>%
      tab_header(title = md("Patients who are smokers at selected region"))
  
    print(table_output)
  },
  error=function(e) {
    print("This practice does not have any smoking patient records or an error has occured.")
  })
}

################### Region Cancer Diagnosis Compare Wales ######################
region_cancer_compare <- function(dbs, gp, gp_area) {
  gp_cancer_details <- find_cancer_patients(dbs, gp)
  all_gp_patients <- find_all_patients(dbs, gp)
  region_cancer_details <- find_region_cancer_patients(dbs, gp_area)
  all_region_patients <- find_all_region_diagnosis(dbs, gp_area)
  wales_cancer_details <- find_wales_inicator_patients(dbs, 'CAN001')
  
  tryCatch({
    gp_cancer_count <- gp_cancer_details$numerator 
    all_gp_count <- gp_cancer_details$field4
    
    region_cancer_count <- region_cancer_details %>% select(numerator) %>%  
      summarise(sum(numerator,na.rm = TRUE))
    all_region_count <- region_cancer_details %>% select(field4) %>%  
      summarise(sum(field4,na.rm = TRUE))
    wales_cancer_count <- wales_cancer_details$numerator
    all_wales_count <- wales_cancer_details$field4
    
    gp_percentage <- (gp_cancer_count/all_gp_count)*100
    gp_percentage <- round(gp_percentage,digits = 2)
    region_percent <- (region_cancer_count/all_region_count)*100
    region_percent <- round(region_percent,digits = 2)
    wales_percent <- (wales_cancer_count/all_wales_count)*100
    wales_percent <- round(wales_percent,digits = 2)
    
    output_message <- glue('\n
         ############# GP, Region and Wales Cancer % ##############
         Total number of cancer gp patients: {gp_cancer_count}
         GP cancer patient percentage: {gp_percentage}%
         Total number of area cancer patients: {region_cancer_count}
         Region cancer patient percentage: {region_percent}%
         Total number of Wales cancer patients: {wales_cancer_count}
         Wales cancer patient percentage: {wales_percent}%')
    print(output_message)
  
    df2 <- data.frame(Area = c("GP", "Region", "Wales"), 
                      Total = c(as.numeric(gp_percentage),
                                as.numeric(region_percent),
                                as.numeric(wales_percent))
    )
    
    p2<-ggplot(data=df2, aes(x=Area, 
                             y=Total, 
                             color = Area, 
                             fill = Area, 
                             label = paste(Total,"%"))
    ) +
      geom_bar(stat="identity") +
      ggtitle("Cancer % comparison between, GP, Region and Wales")
    
    plot(p2 + geom_text(vjust=-1))
  },
  error=function(e) {
    print("This practice does not have any cancer patient records or an error has occured.")
  })
}

################### Region Smoker Diagnosis Compare Wales ######################
region_smoking_compare <- function(dbs,gp_area) {
  smoker_patients <- find_smoker_patients(dbs, gp_area)
  wales_smoker_details <- find_wales_inicator_patients(dbs, 'SMO SCR')

  tryCatch({
    region_smoker_count <- smoker_patients %>% select(numerator) %>%  
      summarise(sum(numerator,na.rm = TRUE))
    wales_smoker_count <- wales_smoker_details %>% select(numerator) %>%  
      summarise(sum(numerator,na.rm = TRUE))
    
    all_region_diagnosis <-smoker_patients %>% select(field4) %>%  
      summarise(sum(field4,na.rm = TRUE))
    
    all_wales_diagnosis <- wales_smoker_details %>% select(field4) %>%  
      summarise(sum(field4,na.rm = TRUE))
    
    region_smoker_percent <- round(((region_smoker_count/all_region_diagnosis)*100), digits = 2)
    wales_smoker_percent <- round(((wales_smoker_count/all_wales_diagnosis)*100), digits = 2)
    
    output_message <- glue('\n
         #################### Total Number of Regional Smokers #########################
         Total number of people in the region declared as a smoker: {region_smoker_count}
         The % of patients in this region declared as a smoker: {region_smoker_percent}%
         Total number of people in Wales declared as a smoker: {wales_smoker_count}
         The % of patients in Wales declared as a smoker: {wales_smoker_percent}%')
    print(output_message)
    
    df2 <- data.frame(Area = c(gp_area, "Wales"), 
                      Percentage = c(as.numeric(region_smoker_percent),
                                     as.numeric(wales_smoker_percent)),
                      Total = c(as.numeric(region_smoker_count),
                                as.numeric(wales_smoker_count)))
    
    p2 <- ggplot(data=df2, aes(x=Area, 
                               y=Percentage,
                               fill = Area,
                               text = paste("Total patients:",Total),
                               label = paste(Percentage, "%"))
    ) +
      geom_bar(stat="identity") +
      geom_text(vjust=-3) +
      ggtitle("Smoker % comparison Region and Wales")
    
    fig <- ggplotly(p2)
    
    print(fig)
  },
  error=function(e) {
    print("This practice does not have any cancer patient records or an error has occured.")
  })
}


######################### Region Drugs Spend ###################################
gp_region_medication <- function(dbs) {
  available_regions <- select_region(dbs)
  drugs_df = data.frame(region = character(), 
                        total_patients = numeric(),  
                        spend = numeric())
  
  print("\n############### Region's Spend on Drugs compared to Wales ##############")
  
  for (i in 1:nrow(available_regions)) {
    selected_region <- available_regions[i,1]
    regional_data <- region_patient_and_drugs_spend(dbs, selected_region, 'SMO SCR')
    
    if (nrow(regional_data) != 0) {
      regional_patient_count <- regional_data %>% select(numerator) %>% 
        summarise(sum(numerator,na.rm = TRUE))
      
      regional_meds_cost <- regional_data  %>% select(actcost) %>%  
        summarise(sum(actcost,na.rm = TRUE))
      regional_meds_cost <- round(regional_meds_cost, digits = 2)
      
      output_message <- glue("The practice {selected_region} spent a total of £{regional_meds_cost} on drugs with {regional_patient_count} patient count")
      print(output_message)
      
      drugs_df = rbind(drugs_df, 
                       data.frame(region = selected_region, 
                                  total_patients = as.numeric(regional_patient_count),  
                                  spend = as.numeric(regional_meds_cost)))
    }
  }
  
  p<-ggplot(data=drugs_df, aes(x = total_patients, 
                                  y = spend)
  ) +
    geom_point()+
    ggtitle("Region Spend on drugs in Wales")
  
  plot(p) 
}


##################### Regional Correlation Check ###############################
region_correlation_check <- function(dbs, region) {
    all_region_spend <- find_all_region_actcost(dbs, region)
    
    # calculate smoking correlation
    region_smoke <- find_all_indi_patients(dbs, 'SMO SCR') %>% 
      select(practiceid = orgcode, smoking_numerator = numerator, 
             smoking_denominator = field4, smoking_ratio = ratio, 
             smoking_indicator = indicator)
    all_region_spend_smoke <- all_region_spend %>% 
      left_join(region_smoke, by = 'practiceid')
    
    smoke_cor <- cor.test(as.numeric(all_region_spend_smoke$smoking_numerator),
                          as.numeric(all_region_spend_smoke$total_spend), 
                          method=c("pearson", "kendall", "spearman"))
    
    # calculate demenstia correlation
    region_demenstia <- find_all_indi_patients(dbs,'DEM001') %>% 
      select(practiceid = orgcode, demenstia_numerator = numerator, 
             demenstia_denominator = field4, demenstia_ratio = ratio, 
             demenstia_indicator = indicator)
    all_region_spend_demenstia <- all_region_spend %>% left_join(region_demenstia, 
                                                             by = 'practiceid')
    dementia_cor <- cor.test(as.numeric(all_region_spend_demenstia$demenstia_numerator),
                             as.numeric(all_region_spend_demenstia$total_spend), 
                             method=c("pearson", "kendall", "spearman"))
    
    # calculate hypertension correlation
    region_hypten <- find_all_indi_patients(dbs,'HYP001') %>% 
      select(practiceid = orgcode, hypten_numerator = numerator, 
             hypten_denominator = field4, hypten_ratio = ratio, 
             hypten_indicator = indicator)
    all_region_spend_hypten <- all_region_spend %>% left_join(region_hypten, 
                                                          by = 'practiceid')
    hypten_cor <- cor.test(as.numeric(all_region_spend_hypten$hypten_numerator),
                           as.numeric(all_region_spend_hypten$total_spend), 
                           method=c("pearson", "kendall", "spearman"))
    
    # calculate heart disease correlation
    region_heart_disease <- find_all_indi_patients(dbs,'CHD001') %>% 
      select(practiceid = orgcode, heart_disease_numerator = numerator, 
             heart_disease_denominator = field4, heart_disease_ratio = ratio, 
             heart_disease_indicator = indicator)
    all_region_spend_heart <- all_region_spend %>% left_join(region_heart_disease, 
                                                         by = 'practiceid')
    heart_disease_cor <- cor.test(as.numeric(all_region_spend_heart$heart_disease_numerator),
                                  as.numeric(all_region_spend_heart$total_spend), 
                                  method=c("pearson", "kendall", "spearman"))
    
    output_message <- glue("\n
                          ######## Correlation to Indicator and Spend in Regions Drugs ########
                          Smoking to drugs correlation statistic: {smoke_cor$statistic}
                          Smoking to drugs correlation estimate: {smoke_cor$estimate}
                          Dementia to drugs correlation statistic: {dementia_cor$statistic}
                          Dementia to drugs correlation estimate: {dementia_cor$estimate}
                          Hypertension to drugs correlation statistic: {hypten_cor$statistic}
                          Hypertension to drugs correlation estimate: {hypten_cor$estimate}
                          Heart disease to drugs correlation statistic: {heart_disease_cor$statistic}
                          Heart disease to drugs correlation estimate: {heart_disease_cor$estimate}")
    print(output_message)
    
    corx <- c("Smoking","Dementia","Hypertension","Heart Disease")
    conditionRelationtodrug_spend <-c(smoke_cor[["statistic"]][["t"]],
                                      dementia_cor[["statistic"]][["t"]],
                                      hypten_cor[["statistic"]][["t"]],
                                      heart_disease_cor[["statistic"]][["t"]])
    relationdf <- data.frame(corx,conditionRelationtodrug_spend)
    relationbar <- ggplot(relationdf,aes(corx,conditionRelationtodrug_spend,
                                         fill=corx, 
                                         label=paste(conditionRelationtodrug_spend)))+
      geom_bar(stat="identity")+
      scale_fill_manual(values=c("blue","green","red","pink","yellow"))+
      ggtitle("Colleration Comparison on Drugs spend and Indicator in the Region")
    
    plot(relationbar)
}


######################## GP Total spend on Drugs ###############################
gp_spend_medication <- function(dbs) {
  ##get practice details.
  all_gps <- gp_practices(dbs)
  drugs_df = data.frame(gp = character(), 
                        total_patients = numeric(),  
                        spend = numeric())
  
  print("\n###################### GP Total Spend and Per Patient ##################")
 
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
      
      cost_per_patient <- round(meds_cost/number_of_patients, digits = 2)
      
      output_message <- glue("The practice {selected_gp} spent a total of £{meds_cost} on drugs with a per patient cost of £{cost_per_patient}")
      print(output_message)
      
      drugs_df = rbind(drugs_df, data.frame(gp = selected_gp, 
                                            total_patients = as.numeric(number_of_patients),  
                                            spend = as.numeric(cost_per_patient)))
    }
  }
  # Plot graph here
  p <- ggplot(data=drugs_df, aes(x = total_patients, 
                                  y = spend)
  ) +
    geom_point() +
    ggtitle("GP total spend on drugs per person.")
  
  plot(p)
}


#################### Calculate GP Spend Corrolation ############################
spend_correlation_check <- function(dbs) {
  
  all_gp_spend <- find_all_gp_actcost(dbs)
  
  # calculate cancer correlation
  gp_cancer <- find_all_indi_patients(dbs, 'CAN001') %>% select(practiceid = orgcode, 
                                                                cancer_numerator = numerator, 
                                                                cancer_denominator = field4, 
                                                                cancer_ratio = ratio, 
                                                                cancer_indicator = indicator)
  all_gp_spend_cancer <- all_gp_spend %>% left_join(gp_cancer, by = 'practiceid')
  cancer_cor <- cor.test(as.numeric(all_gp_spend_cancer$cancer_numerator),
                         as.numeric(all_gp_spend_cancer$total_spend), 
                         method=c("pearson", "kendall", "spearman"))

  # calculate diabetes correlation
  gp_diabet <- find_all_indi_patients(dbs, 'DM001') %>% 
    select(practiceid = orgcode, diabetes_numerator = numerator, 
           diabetes_denominator = field4, diabetes_ratio = ratio, 
           diabetes_indicator = indicator)
  all_gp_spend_diabet <- all_gp_spend %>% left_join(gp_diabet, by = 'practiceid')
  diabet_cor <- cor.test(as.numeric(all_gp_spend_diabet$diabetes_numerator),
                         as.numeric(all_gp_spend_diabet$total_spend), 
                         method=c("pearson", "kendall", "spearman"))
  
  # calculate demenstia correlation
  gp_demenstia <- find_all_indi_patients(dbs,'DEM001') %>% 
    select(practiceid = orgcode, demenstia_numerator = numerator, 
           demenstia_denominator = field4, demenstia_ratio = ratio, 
           demenstia_indicator = indicator)
  all_gp_spend_demenstia <- all_gp_spend %>% left_join(gp_demenstia, 
                                                       by = 'practiceid')
  dementia_cor <- cor.test(as.numeric(all_gp_spend_demenstia$demenstia_numerator),
                            as.numeric(all_gp_spend_demenstia$total_spend), 
                            method=c("pearson", "kendall", "spearman"))
  
  # calculate hypertension correlation
  gp_hypten <- find_all_indi_patients(dbs,'HYP%') %>% 
    select(practiceid = orgcode, hypten_numerator = numerator, 
           hypten_denominator = field4, hypten_ratio = ratio, 
           hypten_indicator = indicator)
  all_gp_spend_hypten <- all_gp_spend %>% left_join(gp_hypten, by = 'practiceid')
  hypten_cor <- cor.test(as.numeric(all_gp_spend_hypten$hypten_numerator),
                         as.numeric(all_gp_spend_hypten$total_spend), 
                         method=c("pearson", "kendall", "spearman"))
  
  output_message <- glue("\n
                          ######## Correlation to Indicator and Spend on GP Drugs ########
                          Cancer to drugs correlation statistic: {cancer_cor$statistic}
                          Cancer to drugs correlation estimate: {cancer_cor$estimate}
                          Diabites to drugs correlation statistic: {diabet_cor$statistic}
                          Diabites to drugs correlation estimate: {diabet_cor$estimate}
                          Dementia to drugs correlation statistic: {dementia_cor$statistic}
                          Dementia to drugs correlation estimate: {dementia_cor$estimate}
                          Hypertension to drugs correlation statistic: {hypten_cor$statistic}
                          Hypertension to drugs correlation estimate: {hypten_cor$estimate}")
  print(output_message)
  
  corx <- c("Cancer","Diabites","Dementia","Hypertension")
  conditionRelationtodrug_spend <-c(cancer_cor[["statistic"]][["t"]],
                                    diabet_cor[["statistic"]][["t"]],
                                    dementia_cor[["statistic"]][["t"]],
                                    hypten_cor[["statistic"]][["t"]])
  relationdf <- data.frame(corx,conditionRelationtodrug_spend)
  relationbar <- ggplot(relationdf,aes(corx,conditionRelationtodrug_spend,fill=corx, 
                                       label=paste(conditionRelationtodrug_spend)))+
    geom_bar(stat="identity")+
    scale_fill_manual(values=c("blue","green","red","pink","yellow"))+
    ggtitle("Colleration Comparison on Drugs spend and Indicator")
  
  plot(relationbar)
}


################################### EOF ########################################