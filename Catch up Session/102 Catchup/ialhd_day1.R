# ------------------------------------------------------------------------------
# PMIM-302: Introductory Analysis of Linked Health Data
# Author:   Pete Arnold
# Date:     December 2020
# Subtitle: Day 1: Solutions in R / Tidyverse
# ------------------------------------------------------------------------------

## Preparation
# Create a project or package and copy all the data files to the data directory.
# Start a new R session by opening the project and running here() to define the
# root location to use when searching for files.
# Copy the R files to the R directory and create a data directory and copy the
# RData files there.
install.packages("here")
library(here)
list.files(here())
list.files(here('data'))

# Exercise 1
# Part A: Creating a morbseq variable to explore inpatient data
# Step 1: Open the vashmds data file (vashmds.RData)
  
# Alternatively, if you click on the table symbol to the right of the vashmds
# item in the Environment panel, it will execute `View(vashmds)`.
load('data/vashmds.RData')
head(vashmds)

# Step 2: Determine record number in dataset.
# You can also find this information in the Environment panel.
# 'dim' will provide the number of columns as well.
nrow(vashmds)
dim(vashmds)

# Step 3: Create 'yearsep' variable and determine number of separations by year.
# For this we will use the pipe function from magrittr, the mutate function from
# dplyr and the year function from lubridate.
# If any of these packages has not been installed, use install.packages() to
# install it (uncomment, edit and run the next line; then comment it out again).
# install.packages('lubridate')

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

# Step 5: Use morbseq to determine the number of patients in dataset and number
#         of patients with single or multiple hospital records
# Count the number of patients by counting the number of '1's in the morbseq.
num_patients <- vashmds %>% filter(morbseq==1) %>% nrow()
num_patients

# Count the number of patients with multiple records by counting the number of 
# '2's in the morbseq.
num_multiple <- vashmds %>% filter(morbseq==2) %>% nrow()
num_multiple

# Step 6: Determine mean patient age at first admission
mean_age_at_admission <- vashmds %>%
    filter(morbseq==1) %>% ungroup() %>%
    summarise(mean=mean(age))

# The above produces a 1x1 data frame, to convert this to a scalar, we can:
mean_age_at_admission <- mean_age_at_admission[[1]]

# But the calculation is simpler in base-R. Note that the 'with' function makes
# the following code easier to read - it indicates that all the names used in
# the second argument refer to the data.frame named in the first argument.

mean(with(vashmds, age[morbseq==1], na.rm=TRUE))

# In this case, the preceding line is equivalent to:
mean(vashmds$age[vashmds$morbseq==1], na.rm=TRUE)

# If you want the standard deviation, use 'sd':
sd_age_at_admission <- vashmds %>%
    filter(morbseq==1) %>% ungroup() %>%
    summarise(sd=sd(age))
sd_age_at_admission <- sd_age_at_admission[[1]]
# Or:
sd(with(vashmds, age[morbseq==1], na.rm=TRUE))

# The 'psych' package includes the 'describe' function which will do this for
# you.
install.packages('psych')
library(psych)
describe(with(vashmds, age[morbseq == 1]))

# If we want to adjust the number of digits or significants places we can use
# print with the 'digits' or 'signif' parameters.
print(describe(with(vashmds, age[morbseq == 1])), digits=3)

# Step 7: Use aggregate command (or SAS and Stata equivalent) to determine the
#         mean, median and maximum number of hospital records per person.
num_records <- vashmds %>% select(rootlpno) %>%
    group_by(rootlpno) %>% summarise(count=n())
describe(num_records$count)

# Part B: Converting a Type II to a Type I file
# Step 8: Open the vascancer data file (vascancer.RData)
load('data/vascancer.RData')
head(vascancer)

# Step 9: Create a morbseq variable
# Create the variable then display the same rows in order (rootlpno) as shown in
# the workbook.
vascancer <- vascancer %>%
    group_by(rootlpno) %>%
    arrange(candate) %>%
    mutate(morbseq = row_number()) %>%
    ungroup() %>%
    arrange(rootlpno)
head((vascancer %>% arrange(rootlpno))[20:33,], 14)

# Step 10: Assess distribution of morbseq.
table(vascancer$morbseq)

vascancer_table <- vascancer %>%
    group_by(morbseq) %>% count() %>% ungroup() %>%
    mutate(percent=(n*100)/sum(n), cum=percent)
# Need a for loop to update the cumulative percent column item by item.
for(i in 2:nrow(vascancer_table)){
    vascancer_table$cum[i]=vascancer_table$cum[i-1]+vascancer_table$percent[i]
}
head(vascancer_table)

# Step 11: Reconstruct the file as one record per individual.
library(tidyr)
vascancer2 <- vascancer %>% arrange(rootlpno, morbseq) %>%
    pivot_wider(names_from=morbseq, values_from=c(cansite, cantis, candate),
        names_sep='') %>%
    relocate(rootlpno, cansite1, cantis1, candate1, cansite2, cantis2, candate2)
head((vascancer2 %>% arrange(rootlpno))[19:28,], 10)

# Step 12: Save the reconstructed file as vascancer2.
save(vascancer2, file='data/vascancer2.RData')

# Step 13: Make sure you have saved your program file.

