source("DBS_Connection.R");
#This calls the function that pops up a login box, and returns your username and password
#As a list with 2 items.
data = connect_to_dbs();

dbListTables(data)

Table1 <- qof_indicator_columns(data)

print(Table1)


#Q1 P1
# Display unique Practice IDs


# Display top five drugs the practice prescribe the most.


# Display the percentage of this practice’s patients that have been diagnosed with cancer


# Create a plot that shows how this practice’s rate of cancer compares to the cancer rate for region the practice is in, 
# as well as the rate for all of Wales 


# Q1 Part 2

# Visualise how the spend on medication per patient varies by practice across Wales.

# Use statistical analysis to show whether the level of spending on medication is associated with the rates of the following diseases at a practice level: 
# cancer, diabetes, dementia, hypertension. If you find statistically significant relationships, what disease is most strongly associated with spend on medication?