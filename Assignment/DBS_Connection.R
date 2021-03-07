#################### Install Required Packages #################################
#install.packages("RPostgreSQL")    
#install.packages("GetoptLong")     

##################### Load Required Packages ###################################
library(RPostgreSQL)    
library(GetoptLong)

########## Connecting to the PostGres gp_practice_data database #RP###############
connect_to_dbs <- function(port_number){
  message1 <- cat('\014\n                           Welcome to Andy\'s code - \n
                      Part 1 looks at individual GPs', 
  '\n                     Part 2 looks at a region as a whole.\n')
   message2 <- cat('----------------------------------------------------------------------------',
      '------\n\n', sep='')
  
  require("RPostgreSQL");
  
  #Specify what driver is needed to connect to the database.
  drv <- dbs_drv()
  con <- dbConnect(drv, dbname = "gp_practice_data", 
            host = "localhost", port = port_number,
            user = "postgres", password = rstudioapi::askForPassword())
  
  print("A connection to the database was made and successful.")
  
  print(dbListTables(con))
  
  return(con)
}

dbs_drv <- function(){
  drv = dbDriver("PostgreSQL");
  return(drv)
}

########################## Database Disconnect #################################
disconnect_dbs <- function(con,drv) {
  lapply(dbListConnections(drv = dbDriver("PostgreSQL")), function(x) {dbDisconnect(conn = x)})
  print("Database and driver has been disconnected.")
  print("Thank you for using this system.")
}

####################### General Exploring of Tables ############################
get_columns <- function(dbs, table) {
  columns <- dbGetQuery(dbs,
                        qq('select column_name as name, ordinal_position as position,
           data_type as type, character_maximum_length as length,
           numeric_precision as precision
    from information_schema.columns
    where table_schema = \'public\' and
          table_name = \'@{table}\';'))
  cat('\nThe table', table, 'has the following structure:\n', sep=' ')
  print(columns)
  return(columns)
}

select_all_table <- function(dbs, table) {
  tables_content <- dbGetQuery(dbs,
                        qq('select *
                            from @{table}'))
  cat('\nThe table', table, 'has the following contents:\n', sep=' ')
  return(tables_content)
}

#################### Display all GP Practice ID ################################
gp_practices <- function(DBS) {
    dbGetQuery(DBS, '
             select Distinct(practiceid), area, county 
               from address
               where practiceid like \'W%\'
               order by county ASC')
}

###################### Select a GP by Practice ID ##############################
select_gp <- function(dbs, selected_practiceid) {
  ## Find the top drugs prescribed by the selected GP
  dbGetQuery(dbs, qq('
    select * 
    from gp_data_up_to_2015
    where practiceid = \'@{selected_practiceid}\''))
}

########################## Select a Region #####################################
select_region <- function(dbs) {
  dbGetQuery(dbs, qq('
    select distinct(UPPER(county))
    from address
    where county is not null'))
}

############### Selected Region Address and GP Data Details ####################
region_details <- function(dbs, gp_area) {
  dbGetQuery(dbs, qq('
  select gp.*, ad.county
  from gp_data_up_to_2015 gp
  left join address ad
  on gp.practiceid = ad.practiceid
  where upper(ad.county) like \'%@{gp_area}%\'
                     '));
}

################# Find Selected GP Cancer Patient Details ######################
find_cancer_patients <- function(dbs, selected_practiceid) {
  dbGetQuery(dbs, qq('
  select qf.*, ad.county 
  from qof_achievement qf
  left join address ad
  on qf.orgcode = ad.practiceid
  where qf.indicator like \'CAN001\' 
                     and qf.orgcode = \'@{selected_practiceid}\'
                     '));
}

############ Find Selected Region Smoking Patient Details ######################
find_smoker_patients <- function(dbs, gp_area) {
  dbGetQuery(dbs, qq('
  select qf.*, ad.county 
  from qof_achievement qf
  left join address ad
  on qf.orgcode = ad.practiceid
  where qf.indicator like \'SMO SCR\' 
                     and ad.county like \'%@{gp_area}%\'
                     '));
}

############ Find All Selected Region's Patient Details ######################
find_all_region_patients <- function(dbs, gp_area) {
  dbGetQuery(dbs, qq('
  select qf.*, ad.county 
  from qof_achievement qf
  left join address ad
  on qf.orgcode = ad.practiceid
  where ad.county like \'%@{gp_area}%\'
                     '));
}

##################### Find All selected GP QoF Details #########################
find_all_patients <- function(dbs, selected_practiceid) {
  dbGetQuery(dbs, qq('
  select qf.*, ad.county 
  from qof_achievement qf
  left join address ad
  on qf.orgcode = ad.practiceid
  where qf.orgcode = \'@{selected_practiceid}\'
                     '));
}

################### Find all Region's Cancer Patients ##########################
find_region_cancer_patients <- function(dbs, gp_area) {
  dbGetQuery(dbs, qq('
  select qf.*, ad.county 
  from qof_achievement qf
  left join address ad
  on qf.orgcode = ad.practiceid
  where ad.county like \'%@{gp_area}%\' 
                     and qf.indicator like \'CAN001%\''));
}

################# Find all Wales's selected Indicator Patients #################
find_wales_inicator_patients <- function(dbs, ind) {
  dbGetQuery(dbs, qq('
  select qf.* 
  from qof_achievement qf
  where qf.indicator like \'@{ind}%\'
                     and qf.orgcode = \'WAL\''));
}

##################### Find all GP Indicator Patients ###########################
find_all_indi_patients <- function(dbs, indi) {
  dbGetQuery(dbs, qq('
    select * 
    from qof_achievement
    where indicator like \'@{indi}\''));
}

###################### All GPs Spend on Drugs ##################################
find_all_gp_actcost <- function(dbs) {
  dbGetQuery(dbs, qq('
    select practiceid, sum(actcost) as total_spend
    from gp_data_up_to_2015
                     group by practiceid'));
}

################### All Regions Spend on Drugs #################################
find_all_region_actcost <- function(dbs, region) {
  dbGetQuery(dbs, qq('
    select gp.practiceid, sum(gp.actcost) as total_spend
    from gp_data_up_to_2015 gp
    left join address ad
    on gp.practiceid = ad.practiceid
    where county like \'%@{region}\'
                     group by gp.practiceid'));
}

################### All Regions Patient Diagnosis ##############################
find_all_region_diagnosis <- function(dbs, gp_area) {
  dbGetQuery(dbs, qq('
  select sum(qf.numerator)
  from address ad
  left join qof_achievement qf
  on ad.practiceid = qf.orgcode
  where county like \'%@{gp_area}%\'
                     '));
}

######################## Regions Spend on Drugs ################################
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

############################### EOF ############################################