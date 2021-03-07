source("DBS_Connection.R");
source("Logic.R");

######################### Database Connect #####################################
port_number <- 5433
data <- connect_to_dbs(port_number);

#################### Database General Exploration ###############################

# Change the value in the '' to view different tables
selected_indicator_columns1 <- get_columns(data, 'qof_indicator')
selected_indicator_columns2 <- get_columns(data, 'gp_data_up_to_2015')

selected_table1 <- select_all_table(data, 'qof_indicator')
View(selected_table1)
selected_table2 <- select_all_table(data, 'gp_data_up_to_2015')
View(selected_table2)

########################## GP Selection ########################################

# Display available practice IDs. 
# Note: IDs appear in a separate window.
display_gp_prac(data)

# User select for practice_id.
user_practice_id <- select_gp_prac(data)
gp_location <- gp_region(data,user_practice_id)

############################## PART 1 ##########################################
# Top 5 drugs prescribed for the selected GP
disply_gp_top5_drugs(data,user_practice_id)

# GP patients diagnosed with cancer details.
diagnoised_with_cancer(data,user_practice_id)

# GP, Region and Wales Cancer diagnosis compare with visualisation table.
region_cancer_compare(data,user_practice_id,gp_location)

# GP Spend on medication per person with GP region.
gp_spend_medication(data)

# GP correlation check against: Cancer, Dementia, Hypertension, Diabetes.
spend_correlation_check(data)

############################## PART 2 ##########################################
# Region select.
# Note: Regions appear in a separate window.
selected_region <- region_select(data) ## Come back to this later

# Top 5 drugs prescribed for the selected region.
region_top_5_drugs(data,selected_region)

# % of patients that identify as a smoker in selected region compared to Wales with a visualisation.
declared_as_smokers(data,selected_region)

# Region total smoking compared to Wales with visulisation.
region_smoking_compare(data,selected_region)

# Selected region drugs spend comparison with visulisation.
gp_region_spend <- gp_region_medication(data)

# Regional correlation check against: Smoking, Dementia, Hypertension, Heart Disease.
region_correlation_check(data, selected_region)

####################### Database Disconnect ####################################
#close the connection and unload the drivers.
disconnect_dbs()

############################## EOF #############################################