#Script Author: Justine Neville
#Tidy Tuesday Two: this script will bring in Tidy Tuesday data from May 28, 2019, which reports wine ratings. The ultimate goal is to create two figures that utilize advanced visualization techniques learned in BAE 590 module-8

library(countrycode)
library(tidyverse)
# 1. load in data from tidy tuesday repository and explore it
wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")
str(wine_ratings)
view(wine_ratings)

#Analysis question 1: which continents produce the largest number of high scoring wine? 
wine_ratings$country[wine_ratings$country=="England"] <- "United Kingdom"
wine_ratings %>%
  drop_na(country)->wine_ratings_clean

#add continent column
wine_ratings_clean%>%
  mutate(continent = countrycode(country, 'country.name', 'continent')) %>%   
  group_by(continent)->wine_ratings_cont
  

ggplot(data=wine_ratings_cont,mapping= aes(x= points) )+
  geom_histogram(binwidth =1)+
  facet_wrap(~continent)+
  labs(title="Wine Score Distribution by Continent",x = "Points", y="Number of Wines",fill="Continent",caption= "Data Source: Kaggle")+
  theme_bw()
#add title, update axes, add data credit, update legend, change colors to be more similar to shades of wines
ggsave("outputs/top_wine_continents.pdf")


#Question 2: of the two highest continents, what are the top 5 countries producing wines with ratings >90? How expensive are wines of these ratings across the top 5 countries? 

#filter out highests scoring continents from distributions
eu_amer_wine_ratings <- filter(wine_ratings_cont,continent=="Americas"| continent=="Europe")

#filter out wines with scores of >=90
eu_amer_wine_ratings %>%
  filter(points>=90) %>%
  arrange(desc(points))->top_wines

count(top_wines, country) %>%
  arrange(desc(n)) -> wine_count
top_5_countries <- wine_count[1:5,]
  
#filter for only top 5 producing countries  
top_5_countries_wine_ratings <- filter(top_wines,country=="US" |country =="France" |country =="Italy"|country == "Austria" |country =="Portugal")
view(top_5_countries_wine_ratings)



#plot figure that shows average point and price values per country and then make the point size based on the number of wines the country produced with a point value >90
#create df with necessary data for plotting
top_5_countries_wine_ratings %>%
  group_by(country) %>%
  summarise(avg_points = mean(points), avg_price= mean(price,na.rm=TRUE)) ->top_5_avgs
top_5_avgs %>%
  full_join(top_5_countries,by="country")->top_5_avgs
#define color palette
colors=c("#e41a1c",
  "#377eb8",
  "#4daf4a",
  "#984ea3",
  "#ff7f00",)

ggplot(data=top_5_avgs, mapping=aes(x= avg_price, y=avg_points))+
  geom_point(stat="identity",mapping=aes(color= country),size=8)+
  
  labs(title="Average Wine Rating vs. Price \n for the Top 5 Wine Countries", y= "Average Rating", x="Average Price per Bottle USD", caption="Data Source:Kaggle", color="Country",size = "Number of Wines \n Rated Above 90")+
  scale_color_manual(values=colors)+
  theme_gray()
ggsave("outputs/Avg_rating_vs_price_top5_wine_countries.pdf")
  
