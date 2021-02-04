source("DBS_Connection.R");
source("Logic.R");
#This calls the function that pops up a login box, and returns your username and password
#As a list with 2 items.
data = connect_to_dbs();

dbListTables(data)

Table1 <- qof_indicator_columns(data)

print(Table1)


# Display unique Practice IDs
gp_prac <- gp_practices(data)###display_gp_prac(data)

print(gp_prac)

#User needs to enter practice_id.
#If id is incorrect user will be prompted for another.
valid_id <- FALSE
user_practice_id <- readline('Enter a practice ID (Wxxxxx):')
if (grepl('^W[0-9]{5}$', user_practice_id)){
  valid_id <- TRUE
} else {
  cat('The entered practice ID (', user_practice_id,
      ') is not valid, please try again.\n', sep='')
}
if (!valid_id){
  stop('Halting: the practice ID was not valid, no data will be found.')
}



#Q1 P1

# Display top five drugs the practice prescribe the most.

###### Taken from supervision session need to addapt to show top 5 on drugs.
# Create the 'yearsep' column:
library(magrittr)
library(dplyr)
library(lubridate)

vashmds <- vashmds %>% mutate(yearsep = year(sepdate))
head(vashmds)

# Create the 'by-year' table and sum the entries using tidyverse.
vashmds_table <- vashmds %>% group_by(yearsep) %>% count() %>% ungroup() %>%
  mutate(percent=(n*100)/sum(n), cum=percent)
# Need a for loop to update the cumulative percent column item by item.
for(i in 2:nrow(vashmds_table)){
  vashmds_table$cum[i]=vashmds_table$cum[i-1]+vashmds_table$percent[i]
}
head(vashmds_table, 17)
vashmds_table %>% select(n) %>% sum()

# This is more straightforward using base-R.
table(vashmds$yearsep)
sum(table(vashmds$yearsep))

# Step 4: Create a morbseq variable
# Group the data by the 'rootlpno' then arrange the data in those groups by the
# admission date and number the rows (still in the groups). When done, remember
# to remove the grouping.
vashmds <- vashmds %>%
  group_by(rootlpno) %>%
  arrange(admdate) %>%
  mutate(morbseq = row_number()) %>%
  ungroup()
head(vashmds)
########

# Display the percentage of this practice’s patients that have been diagnosed with cancer


# Create a plot that shows how this practice’s rate of cancer compares to the cancer rate for region the practice is in, 
# as well as the rate for all of Wales 


# Q1 Part 2

# Visualise how the spend on medication per patient varies by practice across Wales.

# Use statistical analysis to show whether the level of spending on medication is associated with the rates of the following diseases at a practice level: 
# cancer, diabetes, dementia, hypertension. If you find statistically significant relationships, what disease is most strongly associated with spend on medication?