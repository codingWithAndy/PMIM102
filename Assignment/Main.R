source("DBS_Connection.R");
source("Logic.R");

# Connect to database
port_number <- 5433
data = connect_to_dbs(port_number);

# Display available practice IDs. 
# Note: IDs appear in a seperate window, check tabs.
display_gp_prac(data)

#User needs to enter practice_id.
#If id is incorrect user will be prompted for another.
#w98044
user_practice_id <- select_gp_prac()
gp_location <- gp_region(data,user_practice_id)

#Q1 P1

# Display top five drugs the practice prescribe the most.
disply_gp_top5_drugs(data,user_practice_id)

# Table displayed in viewer and output in console below.
diagnoised_with_cancer(data,user_practice_id)

# Outputs regional cancer message to the console below and in Plots.
region_cancer_compare(data,user_practice_id,gp_location)

# Q1 Part 2
# Visualise how the spend on medication per patient varies by practice across Wales.
drugs_spend <- gp_spend_medication(data)
print(drugs_spend)

# Use statistical analysis to show whether the level of spending on medication is associated with the rates of the following diseases at a practice level: 
# cancer, diabetes, dementia, hypertension. If you find statistically significant relationships, what disease is most strongly associated with spend on medication?
spend_correlation_check(drugs_spend)


###### PART 2
#### Top 5 drugs for WAL
region_top_5_drugs(data, gp_location)

#### % of patients that smoke in gp, region, wal + Vis


### GP total spend on smoking drugs compared to non + Vis


#close the connection and unload the drivers.
disconnect_dbs(data)

#Calls all columns from the qof_achievement table.
qof <- qof_indicator_columns_info(data)
view(qof)
