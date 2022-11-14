###Data extraction from Twitter


setwd("C:/Users/chizu/OneDrive/Desktop/Twitter Exercise")

#
#
library(rtweet)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(readr)
library(jsonlite)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(tidyr)
library(maps)
#
#
token <- create_token(
  app = "SocialMedia_Analysis_App",
  consumer_key = "VuS0BHa25K5eilp4ZkCoF8p6F",
  consumer_secret = "VXsnuFCpf6hVOIcOiuOSzsLt2rYhtlPm0od7qxPqcUjYN90Oo1",
  access_token = "1316300333824569344-DEjjQUHvfLi7VF7q6p2UQj10gCpEKG",
  access_secret = "S8hCY4r7m56cv07dGUv38dNuSSZG5zWCF6bvLUpFUjBFN")
#
#
## Search for 500 tweets using the #playstation hashtag
rt <- search_tweets(
  q = "#playstation", n = 500
)

## preview tweets data
rt

#Look at the structure
#
dim(rt)
names(rt)
rt$text
rt$location
#
## preview users data - this is a subset of the information retrieved using search_tweets 
ud <- users_data(rt)
ud
#
dim(ud)
names(ud)
#
# Quickly visualize frequency of tweets over time using ts_plot() in 1 minute interval
ts_plot(rt, "mins")
#
## plot frequency in 1 second interval
ts_plot(rt, "secs") 
#
## plot frequency in 30 seconds interval
ts_plot(rt, "30 secs")
#
ts_plot(rt, "secs", trim = 1)
#
ts_plot(rt, "secs") +
  theme_minimal() + # white background 
  theme(plot.title = element_text(face = "bold")) + # boldface title
  labs(
    x = NULL, y = NULL, # no labels on the axes
    title = "Frequency of #playstation Twitter statuses",
    subtitle = "Twitter status (tweet) counts aggregated using 1-seconds intervals",
    caption = "Source: Data collected from Twitter's REST API via rtweet"
  )
#
## Now group_by retweets and non-retweets
#
## tabulate retweets vs non-retweets
rt %>%
  group_by(is_retweet) %>%
  summarize(n =n()) 
#############################################################################
rt %>%
  group_by(is_retweet) %>%
  ts_plot("30 secs", lwd = 1.5) + # Specify time interval and line width
  labs( x = "Time", 
        y = "Number of tweets",
        title = "Frequency of #playstation Twitter statuses",
        subtitle = "Twitter status (tweet) counts aggregated every 30 seconds",
        caption = "Source: Data collected from Twitter's REST API via rtweet",
        colour = "Retweet") +
  theme(axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        title = element_text(size = 20))  
#
##########################################
#
# view column with screen names - top 6
head(rt$screen_name)
# get a list of unique usernames
unique(rt$screen_name)
# how many locations are represented?
length(unique(rt$location))
#
#
#bar plot of the locations
#
rt %>%
  ggplot(aes(location)) +
  geom_bar() + coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Where Twitter users using #Playstation are from")
# Huge number of locations!
# This plot can be improved!
#
#
# Let's sort by count and just plot the top locations. To do this you use head(). 
# head(10) will return the top 10 locations. 
#
#
rt %>%
  count(location, sort = TRUE) %>% # count the frequency for each location
  mutate(location = reorder(location, n)) %>% # make sure that locations are ordered according to frequency
  head(10) %>%
  ggplot(aes(x = location, y = n)) +
  geom_col(fill = "blue") + # This is the same as geom_bar(stat = "identity") + 
  coord_flip() + # flip x and y axes
  labs(x = "Top Locations",
       y = "Frequency",
       title = "Where Twitter users using #Playstation are from") + 
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        title = element_text(size = 18))
# It looks like we have some blank values.
# Let's transform them in NAs and then remove them with na.omit()
#
rt$location[rt$location==""] <- NA
#
rt %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location,n)) %>%
  na.omit() %>% # remove NAs
  head(10) %>%
  ggplot(aes(x = location,y = n)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(x = "Top Locations",
       y = "Frequency",
       title = "Where Twitter users using #Playstation are from") + 
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        title = element_text(size = 18))
####################################################################################################################################
### Information on the device used to tweet about #Playstation
###
#
## Tweet Source
#
# Identify the top 10 devices used to tweet about #Playstation
# and create a bar plot
rt %>% 
  group_by(source) %>% 
  summarise(Total=n()) %>% 
  arrange(desc(Total)) %>% 
  head(10) %>%
  ggplot(aes(reorder(source, Total), Total, fill = source)) +
  geom_col() +
  coord_flip() + 
  labs(title="Top Tweet Sources for users tweeting about #Playstation", 
       x="Device", 
       caption = "Source: Data collected from Twitter's REST API via rtweet") 
#
# search for up to 500 users using the hashtag #Playstation in their profiles
users <- search_users("#Playstation",
                      n = 500
)
#
#
# produce a plot showing the user profiles
#
users %>% 
  group_by(screen_name) %>% 
  arrange(desc(followers_count)) %>% 
  head(10) %>% 
  ggplot(aes(reorder(screen_name, followers_count), followers_count, fill = name)) + 
  geom_col() + 
  coord_flip() +
  labs(title='Top Users with #Playstation On Their Profile', 
       x="Users", 
       caption = "Source: Data collected from Twitter's REST API via rtweet") 
#























