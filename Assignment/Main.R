source("DBS_Connection.R");
source("Logic.R");

######################### Database Connect #####################################
port_number <- 5433
data <- connect_to_dbs(port_number);

#################### Database General Exploration ###############################

## Add a way to just view all of the tables i.e select * from etc
qof_indicator_columns <- get_columns(data, 'qof_indicator')
gp_data_up_to_2015_columns <- get_columns(data, 'gp_data_up_to_2015')

########################## GP Selection ########################################

# Display available practice IDs. 
# Note: IDs appear in a seperate window, check tabs.
display_gp_prac(data)

#User needs to enter practice_id.
#If id is incorrect user will be prompted for another.
#w98044
user_practice_id <- select_gp_prac(data)
gp_location <- gp_region(data,user_practice_id)

############################## PART 1 ##########################################
# Top 5 drugs perscribed for the selcted GP
disply_gp_top5_drugs(data,user_practice_id)

# GP patients diagnosed with cancer details.
diagnoised_with_cancer(data,user_practice_id)

# GP, Region and Wales Cancer diagnosis compare with visualisation table.
region_cancer_compare(data,user_practice_id,gp_location)

# GP Spend on medication per person with GP region.
gp_spend_medication(data)

#### GP correlation check against: Cancer, Dimentia, Hypertension, Diabeties.
spend_correlation_check(data)

############################## PART 2 ##########################################
## Region select.
selected_region <- region_select(data) ## Come back to this later

#### Top 5 drugs perscribed for the selcted region.
region_top_5_drugs(data,selected_region)

#### % of patients that identify as a smoker in selected region compared to Wales with a visualisation.
declared_as_smokers(data,selected_region)

### Region total smoking compared to Wales + Vis
region_smoking_compare(data,selected_region)

### Selected region drugs spend comparison with visulisation.
## Need to check: 05/03
gp_region_spend <- gp_region_medication(data)

#### Regional correlation check against: Smoking, Dementia, Hypertension, Heart Disease.
region_correlation_check(data, selected_region)

####################### Database Disconnect ####################################
#close the connection and unload the drivers.
disconnect_dbs()

############################## EOF #############################################