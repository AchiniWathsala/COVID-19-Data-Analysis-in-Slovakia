library(sta3262)
get_individual_project_country("AS2021506")
library(coronavirus)
data("coronavirus")
View(coronavirus)

unique(coronavirus$country)


library(tidyverse) 
library(magrittr)



library(skimr)
skim(Slovakia_corona)
skim(coronavirus)

slovakia_df <-  coronavirus %>% filter(country == "Slovakia")
view(slovakia_df)

Slovakia_corona <- slovakia_df %>% select(date, country, cases, type)
view(Slovakia_corona)
summary(Slovakia_corona)

slovakia_confirmed <- Slovakia_corona %>% filter(type == "confirmed") %>% select(date , cases,type)
view(slovakia_confirmed)

slovakia_recovery <- Slovakia_corona %>% filter(type == "recovery") %>% select(date , cases , type)
view(slovakia_recovery)

slovakia_death <- Slovakia_corona %>% filter(type == "death") %>% select(date , cases ,type)
view(slovakia_death)

active_cases <- Slovakia_corona %>% select(date,cases,type) %>% mutate(cases = replace(cases, which(cases < 0), 0))  %>%  pivot_wider(names_from = type, values_from = cases)
active_cases$active <- numeric(nrow(active_cases))
view(active_cases)
active_cases$active[1] <- active_cases$confirmed[1] - active_cases$death[1] - active_cases$recovery[1]

#go through loop from 2 to all the data
for (i in 2:nrow(active_cases)) {
  active_cases$active[i] <- active_cases$active[i-1] + active_cases$confirmed[i] - active_cases$death[i] - active_cases$recovery[i]
}
view(active_cases)

Austria_exists <- "Austria" %in% coronavirus$country
Austria_exists

# filter neighbor countries and get pivoted table
selected_countries <- filter(coronavirus, country %in% c("Slovakia","Austria","Czechia","Romania")) %>% mutate(cases = replace(cases, which(cases<0), 0)) %>% select(date,population,country,type,cases)%>% pivot_wider(names_from = type, values_from = cases)


# create mean table for countries
mean_cases <- selected_countries %>% group_by(country) %>% summarise(population = mean(population), confirmed_mean = mean(confirmed), Total_confirmed = sum(confirmed) ,death_mean = mean(death), Total_death = sum(death), recovery_mean = mean(recovery), Total_recovered = sum(recovery))
view(mean_cases)

