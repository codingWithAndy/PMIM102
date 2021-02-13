source("DBS_Connection.R");
source("Logic.R");

# Connect to database
data = connect_to_dbs();


#### Remove when the time comes
dbListTables(data)

Table1 <- qof_indicator_columns(data)

print(Table1)
#########

# Display unique Practice IDs
#gp_pracices_availbale <- gp_practices(data)###display_gp_prac(data)
#print(gp_pracices_availbale)

# Display available practice IDs. 
# Note: IDs appear in a seperate window, check tabs.
display_gp_prac(data)

#User needs to enter practice_id.
#If id is incorrect user will be prompted for another.
user_practice_id <- select_gp_prac()
gp_location <- gp_region(data,user_practice_id)

#Q1 P1

# Display top five drugs the practice prescribe the most.
disply_gp_top5_drugs(data,user_practice_id)

# Display the percentage of this practice’s patients that have been diagnosed with cancer
#find_cancer_patients(data,user_practice_id) ### delete by the end of project!!!
diagnoised_with_cancer(data,user_practice_id)


# Create a plot that shows how this practice’s rate of cancer compares to the cancer rate for region the practice is in, 
# as well as the rate for all of Wales Note: Do this via health board (hb in gp_data_up_to_2015)
region_cancer_compare(data,user_practice_id,gp_location)

# Q1 Part 2

# Visualise how the spend on medication per patient varies by practice across Wales.

# Use statistical analysis to show whether the level of spending on medication is associated with the rates of the following diseases at a practice level: 
# cancer, diabetes, dementia, hypertension. If you find statistically significant relationships, what disease is most strongly associated with spend on medication?




#close the connection and unload the drivers.
disconnect_dbs(data)




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
qof <- qof_indicator_columns_info(data)
view(qof)
