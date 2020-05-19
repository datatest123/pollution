library(tidyverse)
first <- "https://raw.githubusercontent.com/datatest123/pollution/master/CSVs/tri_"
last <- "_us.csv"

# region helper function used to assign each State to a region
assign_regions <- function(x){
  ne <- c('CT','ME','MA','NH','RI','VT','NJ','NY','PA')
  mw <- c('IL','IN','MI','OH','WI','IA','KS','MN','MO','NE','ND','SD')
  w <- c('AZ','CO','ID','MO','NV','NM','UT','WY','AK','CA','HI','OR','WA')
  
  if (x %in% ne){
    reg <- 'NE'
  } else if (x %in% mw) {
    reg <- 'MW'
  } else if (x %in% w){
    reg <- 'W'
  } else {
    reg <- 'S'
  }
  return(reg)
}

# loop over each year in the dataset
for (i in 2003:2018){
  file <- str_c(first, i, last)
  data <- read_csv(file)
  
  # clean the columns of the data
  names(data) <- names(data) %>%
    {gsub("^[0-9]{,3}\\. ", "", .)} %>%
    {gsub("^[0-9]\\.[0-9]", "", .)} %>%
    {gsub("^ ", "", .)} %>%
    {gsub("^\\.[A-Z0-9]{,2} - |^- ", "", .)} %>%
    {gsub("^[A-Z] - ", "", .)} %>%
    {gsub(" |-", "_", .)} %>%
    {gsub("__|___", "_", .)} 
  
  names(data) <- str_to_lower(names(data))
  #data <- filter(data, !(facility_name == 'RED DOG OPERATIONS'))
  data <- select(data, st, unit_of_measure, on_site_release_total)
  
  # replace grams with pounds
  mask <- data$unit_of_measure == 'Grams'
  temp <- data$on_site_release_total[mask] * .002204623
  data$on_site_release_total[mask] <- temp 
  
  # group by states and remove missing data
  data <- data %>%
    select(-unit_of_measure) %>%
    filter(!(st == '19543' | st == '33404' | st == '68467')) %>%
    group_by(st)  
  
  # create total amounts by state
  by_state <- summarize(data, sums = sum(on_site_release_total))
  names(by_state)[2] <- i
  
  # create data frame for totals
  if (i == 2003) {
    sums <- by_state
  } else {
    col = select(by_state, toString(i))
    sums[toString(i)] <- col
  }
  
  # exclude territories and create region summary
  states <- data %>%
    ungroup() %>%
    filter(!(st == 'AS' | st == 'GU'
                   | st == 'MP' | st == 'PR'
                   | st == 'VI')) %>%
    mutate(region = sapply(.$st, assign_regions)) %>%
    group_by(region) %>%
    summarize(med = median(on_site_release_total), 
              avg = mean(on_site_release_total))
  
  states <- rbind(states,
              data.frame(region = "US",
                         med = median(data$on_site_release_total),
                         avg = mean(data$on_site_release_total)))
  
  # create data frame for mean data
  if (i == 2003){
    means <- select(states, region, avg)
    names(means)[2] <- i
  } else {
    col <- select(states, avg)
    means[toString(i)] <- col
  }
  
  # create data frame for median data
  if (i == 2003){
    meds <- select(states, region, med)
    names(meds)[2] <- i
  } else {
    col <- select(states, med)
    meds[toString(i)] <- col
  }
}

# create csvs (replace "user" with your username)
# note: created on Linux; change path for Windows
sum_file = "/home/user/Documents/state_sums.csv"
mean_file = "/home/user/Documents/reg_mean.csv"
med_file = "/home/user/Documents/reg_med.csv"
#mean_file2 = "/home/user/Documents/reg_mean2.csv"

# create CSVs for later
write_csv(sums, sum_file)
write_csv(means, mean_file)
write_csv(meds, med_file)
