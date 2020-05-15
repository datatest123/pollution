library(tidyverse)
first <- "/home/chase/Desktop/Data Science/EPA/tri_"
last <- "_us.csv"

# import totals
sums <- read_csv("/home/chase/Desktop/Data Science/EPA/state_sums.csv")

# loop over all years in dataset
for (i in 2003:2018){
  file <- str_c(first, i, last)
  data <- read_csv(file)
  
  # clean columns
  names(data) <- names(data) %>%
    {gsub("^[0-9]{,3}\\. ", "", .)} %>%
    {gsub("^[0-9]\\.[0-9]", "", .)} %>%
    {gsub("^ ", "", .)} %>%
    {gsub("^\\.[A-Z0-9]{,2} - |^- ", "", .)} %>%
    {gsub("^[A-Z] - ", "", .)} %>%
    {gsub(" |-", "_", .)} %>%
    {gsub("__|___", "_", .)} 
  
  names(data) <- str_to_lower(names(data))
  
  # neglect dioxin compounds
  data <- data %>%
    filter(st == 'AK' & unit_of_measure == 'Pounds') %>%
    select(facility_name, on_site_release_total) 
  
  # create Red Dog data frame
  if (i == 2003){
    red_dog <- data %>%
      filter(facility_name == 'RED DOG OPERATIONS') %>%
      summarize(year = i, red_dog_total = sum(on_site_release_total),
                total = sum(sums[toString(i)]))
  } else {
    red_dog <- rbind(red_dog, c(i, sum(data$on_site_release_total),
                                sum(sums[toString(i)])))
  }
}

red_dog <- mutate(red_dog, percent = red_dog_total/total*100)
red_dog_file <- "/home/chase/Desktop/Data Science/EPA/red_dog.csv"
write_csv(red_dog, red_dog_file)