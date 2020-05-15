library(tidyverse)

# disable scientific notation (scipen=0 restores this)
options(scipen=999)
#notes
# all years are 2018
# no unclassified waste transfers 
# all gram unit data dioxin/.like 
# look at on site releases 
# convert all to grams/lbs
file <- "/home/chase/Desktop/Data Science/EPA/tri_2018_us.csv"
data <- read_csv(file)

names(data) <- names(data) %>%
  {gsub("^[0-9]{,3}\\. ", "", .)} %>%
  {gsub("^[0-9]\\.[0-9]", "", .)} %>%
  {gsub("^ ", "", .)} %>%
  {gsub("^\\.[A-Z0-9]{,2} - |^- ", "", .)} %>%
  {gsub("^[A-Z] - ", "", .)} %>%
  {gsub(" |-", "_", .)} %>%
  {gsub("__|___", "_", .)} 
  
names(data) <- str_to_lower(names(data))

data <- select(data, 
               facility_name, 
               st, 
               latitude,
               longitude,
               industry_sector,
               chemical,
               unit_of_measure,
               on_site_release_total)

# replace grams with pounds
mask <- data$unit_of_measure == 'Grams'
temp <- data$on_site_release_total[mask] * .002204623
data$on_site_release_total[mask] <- temp 

# group by states 
data <- data %>%
  select(-unit_of_measure) %>%
  filter(!(st == '19543' | st == '33404')) %>%
  group_by(st) 

# Wrangle statewise data
st_tot <- select(data, st, on_site_release_total)

i = 2008
by_state <- summarize(st_tot, i = sum(on_site_release_total))

summary(by_state)

# exclude territories 
states <- filter(data, !(st == 'AS' | st == 'GU'
                         | st == 'MP' | st == 'PR'
                         | st == 'VI'))

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

states <- states %>%
  ungroup() %>%
  mutate(region = sapply(.$st, assign_regions))

mm <- states %>%
  group_by(region) %>%
  summarize(med = median(on_site_release_total), 
            avg = mean(on_site_release_total),
            low = min(on_site_release_total),
            high = max(on_site_release_total))

ggplot(data = mm) +
  geom_bar(mapping = aes(x=region, y=avg),
           stat = 'identity')
ggplot(data = mm) +
  geom_bar(mapping = aes(x=region, y=med),
           stat = 'identity')

ggplot(data = by_state) +
  geom_bar(mapping = aes(x=st, y=sums),
           stat = 'identity')

# who the fuck is spilling in AK (Alaska)?
ak <- data %>%
  select(st,
         facility_name, 
         industry_sector, 
         chemical, 
         on_site_release_total) %>%
  filter(st == 'AK') %>%
  group_by(facility_name) %>%
  arrange(desc(on_site_release_total), .by_group=TRUE)

ak2 <- summarize(ak, sums = sum(on_site_release_total))

ggplot(data = ak2) +
  geom_bar(mapping = aes(x=1:30, y=sums),
           stat = 'identity') +
  scale_x_continuous(breaks=seq(1,30,1))  
  #theme(axis.text.x = element_text(angle=45))

# Red Dog percent of total
# max(st_tot$on_site_release_total)/sum(st_tot$on_site_release_total)

#min/max st
by_state$st[by_state$sums == min(by_state$sums)] # MP min
by_state$st[by_state$sums == max(by_state$sums)] # AK max

# ideas
# clean data
# remove territories
# region analysis

data <- read_csv("/home/chase/Desktop/Data Science/EPA/state_sums.csv")
# cannot append cols to csv file due to their storage method on disk