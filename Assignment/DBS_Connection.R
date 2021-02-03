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


#Not sure what this is doing with the qof_indicator table.
qof_indicator_columns <- function(DBS) {
  dbGetQuery(DBS, '
    select column_name as name, ordinal_position as position,
           data_type as type, character_maximum_length as length,
           numeric_precision as precision
    from information_schema.columns
    where table_schema = \'public\' and
          table_name = \'qof_indicator\';')
}


#Calls all columns from the qof_achievement table.
qof_indicator_columns_info <- function(DBS){
  dbGetQuery(DBS, '
    select *
    from information_schema.columns
    where table_schema = \'public\' and
          table_name = \'qof_indicator\';')
}

gp_data_up_to_2015_columns <- function(DBS){ 
  dbGetQuery(DBS, '
    select column_name as name, ordinal_position as position,
           data_type as type, character_maximum_length as length,
           numeric_precision as precision
    from information_schema.columns
    where table_schema = \'public\' and
          table_name = \'gp_data_up_to_2015\';')
}



#Calls all columns from the qof_achievement table.
qof_achievement_columns <- function(DBS){
  dbGetQuery(DBS, '
    select column_name as name, ordinal_position as position,
           data_type as type, character_maximum_length as length,
           numeric_precision as precision
    from information_schema.columns
    where table_schema = \'public\' and
          table_name = \'qof_achievement\';')
}

#Calls all columns from the qof_achievement table.
qof_achievement_columns <- function(DBS){
  dbGetQuery(DBS, '
    select *
    from information_schema.columns
    where table_schema = \'public\' and
          table_name = \'qof_achievement\';')
}

gp_practices <- function(DBS) {
  dbGetQuery(DBS, '
    select practiceid as practice_id, ordinal_position as position,
           data_type as type, character_maximum_length as length,
           numeric_precision as precision
    from information_schema.columns
    where table_schema = \'public\' and
          table_name = \'gp_data_up_to_2015\';')
}

############### Main questions start!

## Question 1a
top_5_drugs <- function(DBS, selected_practiceid) {
  ## Find the top drugs prescribed by the selected GP
  gp_drugs <- dbGetQuery(con, qq('
    select * from gp_data_up_to_2015
    where practiceid = \'@{selected_practiceid}\''))
  
  drugs_count <- gp_drugs %>% distinct() %>%
    filter(str_detect(bnfname, 'Tab')==TRUE) %>%
    mutate(cost=quantity/items*actcost) %>%
    group_by(bnfcode, bnfname) %>% 
    summarise(cost=mean(cost)) %>%
    ungroup() %>%
    arrange(desc(cost)) %>% head(5)
}