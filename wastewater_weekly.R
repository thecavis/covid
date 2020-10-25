library(tidyverse)
library(lubridate)
library(zoo)

deer_df <- read.csv("wastewater.csv",header = TRUE,stringsAsFactors = FALSE) %>%
  mutate(date = mdy(date)) 

# Weekly viral data

weekly_df <- read.csv("parsed_weekly_102220.csv",header = TRUE,stringsAsFactors = FALSE) %>%
  gather(date,count,-city) %>%
  mutate(parsed_date = mdy(str_replace_all(str_sub(date,2,200),'\\.','/'))) %>%
  select(city,parsed_date,count) 

north_city <- c("NorthBoston","Wilmington","Reading","Wakefield","Bedford","Burlington","Woburn","Stoneham","Lexington","Winchester","Melrose","Waltham","Arlington","Belmont","Medford","Malden","Revere","Everett","Chelsea","Winthrop","Somerville","Cambridge","Watertown","Belmont")
south_city <- c("SouthBoston","Ashland","Framingham","Natick","Wellesley","Needham","Newton","Brookline","Dedham","Westwood","Norwood","Walpole","Canton","Stoughton","Milton","Randolph","Quincy","Randolph","Braintree","Holbrook","Weymouth","Hingham")

weekly_summary_df <- weekly_df %>%
  mutate(old_date = parsed_date - 7) %>%
  left_join(.,weekly_df %>% rename(old_date = parsed_date,old_count = count)) %>%
  mutate(new_cases = count - old_count) %>%
  filter(!is.na(new_cases)) %>%
  rename(date = parsed_date) %>%
  select(city,date,new_cases)

weekly_temp_df <- data.frame(
  date = c(as.Date('2020-06-10'):as.Date('2020-10-21'))
) %>%
  mutate(date = as.Date(date)) %>%
  left_join(.,weekly_summary_df %>%
              rbind(.,weekly_summary_df %>% 
                      filter(city == "Boston") %>% 
                      mutate(city = "NorthBoston",new_cases = new_cases / 2)) %>%
              rbind(.,weekly_summary_df %>%
                      filter(city == "Boston") %>%
                      mutate(city = "SouthBoston",new_cases = new_cases / 2))
  ) %>%
  mutate(deer = ifelse(city %in% north_city,"north","none")) %>%
  mutate(deer = ifelse(city %in% south_city,"south",deer)) %>%
  group_by(date,deer) %>%
  summarize(positives = sum(new_cases))


weekly_graph_df <- weekly_temp_df %>%
  left_join(.,deer_df %>%
              rename(north = northern,south = southern) %>%
              select(date,north,south) %>%
              filter(date > '2020-06-01') %>%
              gather(deer,viral,-date)
  )

viral_factor <- 12
ggplot(weekly_graph_df %>% filter(deer == "north"),
       aes(x = date)) +
  theme_bw() + 
  geom_line(aes(y = positives, colour = "Positive cases")) + 
  geom_point(aes(y = viral_factor*viral,colour = "Viral load")) + 
  scale_y_continuous(name = "Positive cases",sec.axis = sec_axis(~./viral_factor,name = "Viral load")) + 
  facet_grid(rows = vars(deer))

viral_factor <- 8
ggplot(weekly_graph_df %>% filter(deer == "south"),
       aes(x = date)) +
  theme_bw() + 
  geom_line(aes(y = positives, colour = "Positive cases")) + 
  geom_point(aes(y = viral_factor*viral,colour = "Viral load")) + 
  scale_y_continuous(name = "Positive cases",sec.axis = sec_axis(~./viral_factor,name = "Viral load")) + 
  facet_grid(rows = vars(deer))
