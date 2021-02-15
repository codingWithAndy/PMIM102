library(RPostgreSQL)    
library(GetoptLong)      
library(tidyverse)      
library(dbplyr)
library(ggplot2)
#Connecting to the PostGres gp_practice_data database.
connect_to_dbs <- function(){
  #Make sure the RPostgreSQL package is available.
  require("RPostgreSQL");
  
  #Specify what driver is needed to connect to the database.
  drv = dbDriver("PostgreSQL");
  dbConnect(drv, dbname = "gp_practice_data", 
            host = "localhost", port = 5433,
            user = "postgres", password = rstudioapi::askForPassword())
}

disconnect_dbs <- function(dbs) {
  dbDisconnect(dbs)
  dbUnloadDriver(drv)
  print("Database and driver has been disconnected.")
}

#Not sure what this is doing with the qof_indicator table.
qof_indicator_columns <- function(dbs) {
  dbGetQuery(dbs, '
    select column_name as name, ordinal_position as position,
           data_type as type, character_maximum_length as length,
           numeric_precision as precision
    from information_schema.columns
    where table_schema = \'public\' and
          table_name = \'qof_indicator\';')
}


#Calls all columns from the qof_achievement table.
qof_indicator_columns_info <- function(dbs){
  dbGetQuery(dbs, '
    select *
    from information_schema.columns
    where table_schema = \'public\' and
          table_name = \'qof_indicator\';')
}

gp_data_up_to_2015_columns <- function(dbs){ 
  dbGetQuery(dbs, '
    select column_name as name, ordinal_position as position,
           data_type as type, character_maximum_length as length,
           numeric_precision as precision
    from information_schema.columns
    where table_schema = \'public\' and
          table_name = \'gp_data_up_to_2015\';')
}



#Calls all columns from the qof_achievement table.
qof_achievement_columns <- function(dbs){
  dbGetQuery(dbs, '
    select column_name as name, ordinal_position as position,
           data_type as type, character_maximum_length as length,
           numeric_precision as precision
    from information_schema.columns
    where table_schema = \'public\' and
          table_name = \'qof_achievement\';')
}

#Calls all columns from the qof_achievement table.
qof_achievement_columns <- function(dbs){
  dbGetQuery(dbs, '
    select *
    from information_schema.columns
    where table_schema = \'public\' and
          table_name = \'qof_achievement\';')
}

gp_practices <- function(DBS) {
    dbGetQuery(DBS, '
             select Distinct(practiceid), area, county 
               from address
               where practiceid like \'W%\'
               order by county ASC')
}

############### Main questions start!

## Question 1a
select_gp <- function(DBS, selected_practiceid) {
  ## Find the top drugs prescribed by the selected GP
  dbGetQuery(DBS, qq('
    select * from gp_data_up_to_2015
    where practiceid = \'@{selected_practiceid}\''))
  
}


find_cancer_patients <- function(dbs, selected_practiceid) {
  dbGetQuery(dbs, qq('select qf.*, ad.county from qof_achievement qf
  join address ad
  on qf.orgcode = ad.practiceid
  where qf.indicator like \'CAN%\' and qf.orgcode = \'@{selected_practiceid}\''));
}


find_all_patients <- function(dbs, selected_practiceid) {
  dbGetQuery(dbs, qq('select qf.*, ad.county from qof_achievement qf
  join address ad
  on qf.orgcode = ad.practiceid
  where qf.orgcode = \'@{selected_practiceid}\''));
}


find_region_cancer_patients <- function(dbs, gp_area) {
  dbGetQuery(dbs, qq('select qf.*, ad.county from qof_achievement qf
  join address ad
  on qf.orgcode = ad.practiceid
  where ad.county = \'@{gp_area}\' and qf.indicator like \'CAN%\''));
}

find_wales_cancer_patients <- function(dbs) {
  dbGetQuery(dbs, qq('select qf.*, ad.county from qof_achievement qf
  join address ad
  on qf.orgcode = ad.practiceid
  where qf.indicator like \'CAN%\''));
}

# Need to add a SQL like statement for gp county -> looking for inconsistent spelling maybe?
region_noncancer_compare <- function(dbs, gp_county) {
  dbGetQuery(dbs, qq('select qf.*, ad.county from qof_achievement qf
  join address ad
  on qf.orgcode = ad.practiceid
  where ad.county = \'@{gp_county}\''));
}

# Need to adapt this code


#Need to adapt this code
wales_noncancer_compare <- function(dbs, gp_county) {
  dbGetQuery(dbs, qq('select qf.*, ad.county from qof_achievement qf
  join address ad
  on qf.orgcode = ad.practiceid
  where ad.county = \'@{gp_county}\' and qf.indicator like \'CAN%\''));
}




#drugs_count <- gp_drugs %>% distinct() %>%
#  filter(str_detect(bnfname, 'Tab')==TRUE) %>%
#  mutate(cost=quantity/items*actcost) %>%
#  group_by(bnfcode, bnfname) %>% 
#  summarise(cost=mean(cost)) %>%
#  ungroup() %>%
#  arrange(desc(cost)) %>% head(5)