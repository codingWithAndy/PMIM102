library(RPostgreSQL)    
library(GetoptLong)      
library(tidyverse)      
library(dbplyr)
library(ggplot2)
#Connecting to the PostGres gp_practice_data database.
connect_to_dbs <- function(port_number){
  #Make sure the RPostgreSQL package is available.
  require("RPostgreSQL");
  
  #Specify what driver is needed to connect to the database.
  drv <- dbs_drv()
  con <- dbConnect(drv, dbname = "gp_practice_data", 
            host = "localhost", port = port_number,
            user = "postgres", password = rstudioapi::askForPassword())
  
  print("A connection was made.")
  return(con)
}

dbs_drv <- function(){
  drv = dbDriver("PostgreSQL");
  return(drv)
}


disconnect_dbs <- function(con,drv) {
  #dbDisconnect(dbs)
  #drv <- dbs_drv()
  #dbUnloadDriver(drv)
  lapply(dbListConnections(drv = dbDriver("PostgreSQL")), function(x) {dbDisconnect(conn = x)})
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
select_gp <- function(dbs, selected_practiceid) {
  ## Find the top drugs prescribed by the selected GP
  dbGetQuery(dbs, qq('
    select * 
    from gp_data_up_to_2015
    where practiceid = \'@{selected_practiceid}\''))
  
}

select_region <- function(dbs) {
  dbGetQuery(dbs, qq('
    select distinct(UPPER(county))
    from address
    where county is not null'))
}

region_select <- function(dbs, region) {
  dbGetQuery(dbs, qq('
  select qf.*, ad.county 
  from qof_achievement qf
  left join address ad
  on qf.orgcode = ad.practiceid
  where ad.county like \'%@{region}%\''))
}

select_all_region_details <- function(dbs) {
  dbGetQuery(dbs, qq('
  select qf.*, gp.*, ad.county 
from qof_achievement qf
left join address ad
on qf.orgcode = ad.practiceid
left join gp_data_up_to_2015 gp
on ad.practiceid = gp.practiceid'))
}

select_all_gp <- function(dbs) {
  ## Find the top drugs prescribed by the selected GP
  dbGetQuery(dbs, qq('
    select *
    from gp_data_up_to_2015
    where practiceid like \'W%\''))
}

region_details <- function(dbs, gp_area) {
  dbGetQuery(dbs, qq('
  select gp.*, ad.county
  from gp_data_up_to_2015 gp
  left join address ad
  on gp.practiceid = ad.practiceid
  where upper(ad.county) like \'%@{gp_area}%\'
                     '));
}

find_cancer_patients <- function(dbs, selected_practiceid) {
  dbGetQuery(dbs, qq('
  select qf.*, ad.county 
  from qof_achievement qf
  left join address ad
  on qf.orgcode = ad.practiceid
  where qf.indicator like \'CAN%\' 
                     and qf.orgcode = \'@{selected_practiceid}\'
                     '));
}

find_smoker_patients <- function(dbs, gp_area) {
  dbGetQuery(dbs, qq('
  select qf.*, ad.county 
  from qof_achievement qf
  left join address ad
  on qf.orgcode = ad.practiceid
  where qf.indicator like \'SMO%\' 
                     and ad.county like \'%@{gp_area}%\'
                     '));
}

find_all_region_patients <- function(dbs, gp_area) {
  dbGetQuery(dbs, qq('
  select qf.*, ad.county 
  from qof_achievement qf
  left join address ad
  on qf.orgcode = ad.practiceid
  where ad.county like \'%@{gp_area}%\'
                     '));
}

find_all_patients <- function(dbs, selected_practiceid) {
  dbGetQuery(dbs, qq('
  select qf.*, ad.county 
  from qof_achievement qf
  left join address ad
  on qf.orgcode = ad.practiceid
  where qf.orgcode = \'@{selected_practiceid}\'
                     '));
}


find_region_cancer_patients <- function(dbs, gp_area) {
  dbGetQuery(dbs, qq('
  select qf.*, ad.county 
  from qof_achievement qf
  left join address ad
  on qf.orgcode = ad.practiceid
  where ad.county like \'%@{gp_area}%\' 
                     and qf.indicator like \'CAN%\''));
}

find_wales_inicator_patients <- function(dbs, ind) {
  dbGetQuery(dbs, qq('
  select qf.* 
  from qof_achievement qf
  where qf.indicator like \'@{ind}%\'
                     and qf.orgcode = \'WAL\''));
}

# Need to add a SQL like statement for gp county -> looking for inconsistent spelling maybe?
region_noncancer_compare <- function(dbs, gp_county) {
  dbGetQuery(dbs, qq('
  select qf.*, ad.county 
  from qof_achievement qf
  left join address ad
  on qf.orgcode = ad.practiceid
  where ad.county = \'@{gp_county}\'
                     '));
}

find_all_indi_patients <- function(dbs, indi) {
  dbGetQuery(dbs, qq('
    select * 
    from qof_achievement
    where indicator like \'@{indi}\''));
}


## dont do a * for the sake of it.
find_all_gp_spend <- function(dbs) {
  dbGetQuery(dbs, qq('
    select * 
    from gp_data_up_to_2015'));
}

find_all_gp_actcost <- function(dbs) {
  dbGetQuery(dbs, qq('
    select practiceid, sum(actcost) as total_spend
    from gp_data_up_to_2015
                     group by practiceid'));
}

find_all_region_diagnosis <- function(dbs, gp_area) {
  dbGetQuery(dbs, qq('
  select sum(qf.numerator)
from address ad
left join qof_achievement qf
on ad.practiceid = qf.orgcode
where county like \'%@{gp_area}%\'
                     '));
}

find_all_wales_patients <- function(dbs) {
  dbGetQuery(dbs, qq('
    select sum(numerator) as total_patients
    from qof_achievement
    where orgcode = \'WAL\'
    '));
}

region_patient_and_drugs_spend <- function(dbs, gp_area, indi) {
  dbGetQuery(dbs, qq('
    select *
    from address ad
    left join gp_data_up_to_2015 gp
    on ad.practiceid = gp.practiceid
    left join qof_achievement qf
    on ad.practiceid = qf.orgcode
    where ad.county like \'%@{gp_area}%\'
    and qf.indicator like \'@{indi}%\'
                     '));
}

#drugs_count <- gp_drugs %>% distinct() %>%
#  filter(str_detect(bnfname, 'Tab')==TRUE) %>%
#  mutate(cost=quantity/items*actcost) %>%
#  group_by(bnfcode, bnfname) %>% 
#  summarise(cost=mean(cost)) %>%
#  ungroup() %>%
#  arrange(desc(cost)) %>% head(5)