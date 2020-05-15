library(tidyverse)
options(scipen=999)

sums <- read_csv("/home/chase/Desktop/Data Science/EPA/state_sums.csv")
means <- read_csv("/home/chase/Desktop/Data Science/EPA/reg_mean.csv")
medians <- read_csv("/home/chase/Desktop/Data Science/EPA/reg_med.csv")

sums <- sums %>%
  pivot_longer(-st, names_to='year', values_to='releases')

means <- means %>%
  pivot_longer(-region, names_to='year', values_to='mean')

medians <- medians %>%
  pivot_longer(-region, names_to='year', values_to='median')


plot1 <- ggplot(means, aes(year, mean, group=region)) +
  geom_line(aes(col=region)) +
  labs(title = 'Mean On-Site Releases',
       caption = 'Source: EPA TRI Program') +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") +
  ylab("Mean Releases (lbs)") 

plot2 <- ggplot(medians, aes(year, median, group=region)) +
  geom_line(aes(col=region)) +
  labs(title = 'Median On-Site Releases',
       caption = 'Source: EPA TRI Program') +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") +
  ylab("Median Releases (lbs)") 

means2 <- filter(means, region != 'US')
medians2 <- filter(medians, region != 'US')
avg_us <- filter(means, region == 'US')
med_us <- filter(medians, region == 'US')

plot3 <- ggplot(means2, aes(year,mean,group=region)) +
  geom_point() +
  facet_grid(rows=vars(region), scales='free_y') +
  geom_smooth(method='lm') + 
  labs(title = 'Average releases are declining except in W',
       caption = 'Source: EPA TRI Program') +
  theme(plot.title = element_text(hjust = 0)) +
  xlab("Year") +
  ylab("Mean On-Site Releases (lbs)") 

plot4 <- ggplot(medians2, aes(year,median,group=region)) +
  geom_point() +
  facet_grid(rows=vars(region), scales='free_y') +
  geom_smooth(method='lm') + 
  labs(title = 'Median releases are declining in all regions',
       caption = 'Source: EPA TRI Program') +
  theme(plot.title = element_text(hjust = 0)) +
  xlab("Year") +
  ylab("Median On-Site Releases (lbs)") 

plot5 <- ggplot(avg_us, aes(as.integer(year),mean)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(title = 'Average releases slightly increase across the US',
       caption = 'Source: EPA TRI Program') +
  theme(plot.title = element_text(hjust = 0)) +
  xlab("Year") +
  ylab("Mean On-Site Releases (lbs)") 

usline <- lm(mean ~ as.integer(year), data=avg_us)
summary(usline)

plot6 <- ggplot(med_us, aes(as.integer(year),median)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(title = 'Median releases declining across the US',
       caption = 'Source: EPA TRI Program') +
  theme(plot.title = element_text(hjust = 0)) +
  xlab("Year") +
  ylab("Mean On-Site Releases (lbs)") 


sum95 <- filter(sums, releases >= quantile(releases, prob=.95))

plot7 <- ggplot(sum95, aes(year,releases,group=st)) +
  geom_line(aes(col=st)) +
  labs(title = 'Releases are disproportionately high in Alaska',
       caption = 'Source: EPA TRI Program') +
  theme(plot.title = element_text(hjust = 0)) +
  xlab("Year") +
  ylab("Total On-Site Releases (lbs)")

ecdf(sums$releases)(300000000)

# 1) US and region mean/median trends - done! 
# 2) Sums by quantiles - done!
# 3) AK outlier analysis 