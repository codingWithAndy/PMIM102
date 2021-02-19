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

# Display the percentage of this practice’s patients that have been diagnosed with cancer
#find_cancer_patients(data,user_practice_id) ### delete by the end of project!!!
diagnoised_with_cancer(data,user_practice_id)

# Create a plot that shows how this practice’s rate of cancer compares to the cancer rate for region the practice is in, 
# as well as the rate for all of Wales Note: Do this via health board (hb in gp_data_up_to_2015)
region_cancer_compare(data,user_practice_id,gp_location)

# Q1 Part 2

# Visualise how the spend on medication per patient varies by practice across Wales.
drugs_spend <- gp_spend_medication(data)


print(drugs_spend)

#aes(x= gp, 
#y= spend, 
#color = gp, 
#fill = gp, 
#label = spend)
p<-ggplot(data=drugs_spend, aes(x = total_patients, 
          y = spend, 
          color = gp, 
          fill = gp, 
          label = spend)
) +
  geom_bar(stat="identity")

p 

+ geom_text(vjust=-1)

# Use statistical analysis to show whether the level of spending on medication is associated with the rates of the following diseases at a practice level: 
# cancer, diabetes, dementia, hypertension. If you find statistically significant relationships, what disease is most strongly associated with spend on medication?
spend_correlation_check(data,user_practice_id)

#close the connection and unload the drivers.
disconnect_dbs(data)

#Calls all columns from the qof_achievement table.
qof <- qof_indicator_columns_info(data)
view(qof)
