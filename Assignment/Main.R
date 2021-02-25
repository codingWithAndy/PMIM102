source("DBS_Connection.R");
source("Logic.R");

######################### Database Connect #####################################
port_number <- 5433
data <- connect_to_dbs(port_number);

########################## GP Selection ########################################

# Display available practice IDs. 
# Note: IDs appear in a seperate window, check tabs.
display_gp_prac(data)

#User needs to enter practice_id.
#If id is incorrect user will be prompted for another.
#w98044
user_practice_id <- select_gp_prac()
gp_location <- gp_region(data,user_practice_id)

############################## PART 1 ##########################################
# Display top five drugs table displayed in viewer and output in console below.
disply_gp_top5_drugs(data,user_practice_id)

# Display practice's patients diagnosed with cancer table in viewer and output in console below.
diagnoised_with_cancer(data,user_practice_id)

# Outputs regional cancer message to the console below and in Plots.
region_cancer_compare(data,user_practice_id,gp_location)

# Q1 Part 2
# Individual values printed to the console and a Wales scatter plot displayed in plots.
drugs_spend <- gp_spend_medication(data)
#print(drugs_spend)

# Use statistical analysis to show whether the level of spending on medication is associated with the rates of the following diseases at a practice level: 
# cancer, diabetes, dementia, hypertension. If you find statistically significant relationships, what disease is most strongly associated with spend on medication?
spend_correlation_check(data)


############################## PART 2 ##########################################
## Region select like practice id?
selected_region <- region_select(data) ## Come back to this later

#### Top 5 drugs for WAL
region_top_5_drugs(data,selected_region)

#### % of patients that smoke in gp, region, Wales + Vis
declared_as_smokers(data,selected_region)

### Region total smoking compared to Wales + Vis
region_smoking_compare(data,selected_region)

### Some drugs spend equvilant
gp_region_medication(data,selected_region)

#### Some correlation check



####################### Database Disconnect ####################################
#close the connection and unload the drivers.
disconnect_dbs()

#Calls all columns from the qof_achievement table.
qof <- qof_indicator_columns_info(data)
view(qof)
