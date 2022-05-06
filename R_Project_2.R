#Installation of Packages
library(tidyverse)
library(readxl)
library(xlsx)
library(dplyr)
library(lubridate)
library(ggplot2)
library(car)
library(broom)

#Data Importation, Cleaning, & Feature Generation
merged_data2 = read_excel('merged_index_data.xlsx')
names(merged_data2) <- gsub(' ','.', names(merged_data2))
merged_data2$Year <- as.Date(ISOdate(merged_data2$Year, 1, 1))  # beginning of year
merged_data2['Year'] <- year(merged_data2$Year)
merged_data2 = transform(merged_data2, World.Rank = as.integer(World.Rank))
merged_data2 = transform(merged_data2, Region.Rank = as.integer(Region.Rank))
merged_data2 = transform(merged_data2, Score = as.numeric(Score))
merged_data2 = transform(merged_data2, Property.Rights = as.numeric(Property.Rights))
merged_data2 = transform(merged_data2, Judical.Effectiveness = as.numeric(Judical.Effectiveness))
merged_data2 = transform(merged_data2, Government.Integrity = as.numeric(Government.Integrity))
merged_data2 = transform(merged_data2, Tax.Burden = as.numeric(Tax.Burden))
merged_data2 = transform(merged_data2, Gov.t.Spending = as.numeric(Gov.t.Spending))
merged_data2 = transform(merged_data2, Fiscal.Health = as.numeric(Fiscal.Health))
merged_data2 = transform(merged_data2, Business.Freedom = as.numeric(Business.Freedom))
merged_data2 = transform(merged_data2, Labor.Freedom = as.numeric(Labor.Freedom))
merged_data2 = transform(merged_data2, Monetary.Freedom = as.numeric(Monetary.Freedom))
merged_data2 = transform(merged_data2, Trade.Freedom = as.numeric(Trade.Freedom))
merged_data2 = transform(merged_data2, Investment.Freedom = as.numeric(Investment.Freedom))
merged_data2 = transform(merged_data2, Financial.Freedom = as.numeric(Financial.Freedom))
merged_data2 = transform(merged_data2, Tariff.Rate = as.numeric(Tariff.Rate))
merged_data2 = transform(merged_data2, Income.Tax.Rate = as.numeric(Income.Tax.Rate))
merged_data2 = transform(merged_data2, Corporate.Tax.Rate = as.numeric(Corporate.Tax.Rate))
merged_data2 = transform(merged_data2, Tax.Burden.of.GDP = as.numeric(Tax.Burden.of.GDP))
merged_data2 = transform(merged_data2, Gov.t.Expenditure.of.GDP = as.numeric(Gov.t.Expenditure.of.GDP))
merged_data2 = transform(merged_data2, Population.Millions = as.numeric(Population.Millions))
merged_data2 = transform(merged_data2, GDP.Billions.PPP = as.numeric(GDP.Billions.PPP))
merged_data2 = transform(merged_data2, GDP.Growth.Rate = as.numeric(GDP.Growth.Rate))
merged_data2 = transform(merged_data2, X5.Year.GDP.Growth.Rate = as.numeric(X5.Year.GDP.Growth.Rate))
merged_data2 = transform(merged_data2, GDP.per.Capita = as.numeric(GDP.per.Capita))
merged_data2 = transform(merged_data2, Unemployment = as.numeric(Unemployment))
merged_data2 = transform(merged_data2, Inflation = as.numeric(Inflation))
merged_data2 = transform(merged_data2, FDI.Inflow = as.numeric(FDI.Inflow))
merged_data2 = transform(merged_data2, Public.Debt = as.numeric(Public.Debt))
merged_data2$Country.Name[merged_data2$Country.Name=="Hong Kong SAR"]<-"Hong Kong"
merged_data2 = merged_data2 %>% drop_na()

summary(merged_data2) 
sapply(merged_data2, sd) 

merged_data2 = merged_data2 %>% mutate(Quartile = ifelse(0 < Score & Score < 60 , 'Repressed',
                                           ifelse(Score < 70, 'Moderately Free', 'Most Free')))

merged_data2['Mean_Rule_of_Law'] = mean(c(Property.Rights,Government.Integrity,Judical.Effectiveness))
merged_data2['Mean_Rule_of_Law'] = (merged_data2['Property.Rights'] + merged_data2['Government.Integrity'] + merged_data2['Judical.Effectiveness'])/3
merged_data2['Mean_Regulatory_Efficiency'] = (merged_data2['Business.Freedom'] + merged_data2['Labor.Freedom'] + merged_data2['Monetary.Freedom'])/3
merged_data2['Mean_Government_Size'] = (merged_data2['Gov.t.Spending'] + merged_data2['Tax.Burden'] + merged_data2['Fiscal.Health'])/3
merged_data2['Mean_Open_Markets'] = (merged_data2['Trade.Freedom'] + merged_data2['Investment.Freedom'] + merged_data2['Financial.Freedom'])/3


top_GDPs = merged_data2 %>%
  group_by(Country.Name, Year) %>%
  slice_max(GDP.Billions.PPP, n = 5) %>%
  arrange(desc(GDP.Billions.PPP)) %>%
  select(Country.Name, Year, GDP.Billions.PPP, Score, Region) 


top_Scores = merged_data2 %>%
  group_by(Country.Name, Year) %>%
  slice_max(Score, n = 5) %>%
  arrange(desc(Score)) %>%
  select(Country.Name, Year, GDP.Billions.PPP, GDP.per.Capita, Score, Region) 

data_2017_x = filter(merged_data2,Year=='2017')
data_2017_x = select(data_2017, Country.Name, Region.Rank, World.Rank, Score)
data_2022_x = filter(merged_data2,Year=='2022')
data_2022_x = select(data_2022, Country.Name, Region.Rank, World.Rank, Score)

merged_2017_2022 = inner_join(data_2017_x,data_2022_x, by='Country.Name')
merged_2017_2022 = merged_2017_2022 %>%
  rename(region_rank_2017 = Region.Rank.x, world_rank_2017 = World.Rank.x, score_2017 = Score.x, region_rank_2022 = Region.Rank.y,
         world_rank_2022 = World.Rank.y, score_2022 = Score.y)

merged_2017_2022['Change.in.World.Rank'] = merged_2017_2022['world_rank_2022'] - merged_2017_2022['world_rank_2017']
merged_2017_2022['Change.in.Region.Rank'] = merged_2017_2022['region_rank_2022'] - merged_2017_2022['region_rank_2017']
merged_2017_2022['Change.in.Score'] = merged_2017_2022['score_2022'] - merged_2017_2022['score_2017']


merged_data2 = inner_join(merged_data2, merged_2017_2022, by='Country.Name')

#Data Manipulation
merged_data2 %>%
  group_by(Region) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(5, mean_score) %>%
  arrange(desc(mean_score))


merged_data2 %>%
  group_by(Country.Name) %>%
  top_n(2, GDP.Billions.PPP) %>%
  arrange(desc(GDP.Billions.PPP)) %>%
  select(Country.Name, Year, GDP.Billions.PPP, Score, Region) 

top_Scores %>%
  group_by(Country.Name) %>%
  summarise(Mean_GDP_Per_Capita = mean(GDP.per.Capita)) %>%
  top_n(5, Mean_GDP_Per_Capita) %>%
  ggplot() + geom_col(aes(Country.Name, Mean_GDP_Per_Capita))


top_Scores %>%
  group_by(Country.Name) %>%
  summarise(Mean_GDP_Per_Capita = mean(GDP.per.Capita)) %>%
  top_n(-5, Mean_GDP_Per_Capita) %>%
  ggplot() + geom_col(aes(Country.Name, Mean_GDP_Per_Capita))

top_Scores %>%
  group_by(Region) %>%
  top_n(-5, GDP.per.Capita) %>%
  distinct(Country.Name)

head(top_Scores %>% group_by(Region, Year) %>% slice_max(order_by = Score, n = 5), 5)
head(top_Scores %>% group_by(Region, Year) %>% slice_min(order_by = Score, n = 5), 5)

top_Scores %>%
  group_by(Region) %>%
  top_n(5, GDP.per.Capita) %>%
  distinct(Country.Name)

merged_data2 %>%
  group_by(Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(-5, mean_score)

merged_data2 %>%
  group_by(Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(-5, mean_score)
merged_data2 %>%
  group_by(Year, Region) %>%
  summarise(Mean_Rule_of_Law = mean(c(Property.Rights,Government.Integrity,Judical.Effectiveness)))


merged_data2 %>%
  group_by(Year, Region) %>%
  summarise(Mean_Government_Size = mean(c(Gov.t.Spending,Tax.Burden,Fiscal.Health)))

merged_data2 %>%
  group_by(Year, Region) %>%
  summarise(Mean_Regulatory_Efficiency = mean(c(Business.Freedom,Labor.Freedom,Monetary.Freedom)))

merged_data2 %>%
  group_by(Region, Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(5, mean_score) %>%
  arrange(desc(mean_score))

merged_data2 %>%
  group_by(Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(5, mean_score)

merged_data2 %>%
  group_by(Region) %>%
  summarise(mean_change_in_world_rank = mean(Change.in.World.Rank))

merged_data2 %>%
  group_by(Region) %>%
  summarise(mean_change_in_region_rank = mean(Change.in.Region.Rank))

merged_data2 %>%
  group_by(Region) %>%
  summarise(mean_change_in_score = mean(Change.in.Score))


merged_data2 %>%
  group_by(Country.Name, Year) %>%
  arrange(desc(Change.in.World.Rank))

filter(merged_data2, Region == 'Americas') %>%
  group_by(Country.Name) %>%
  summarise(mean_change_in_score = mean(Change.in.Region.Rank)) %>%
  top_n(5, mean_change_in_score) %>%
  arrange(desc(mean_change_in_score)) 

filter(merged_data2, Region == 'Americas') %>%
  group_by(Country.Name) %>%
  summarise(mean_change_in_score = mean(Change.in.Region.Rank)) %>%
  top_n(-5, mean_change_in_score) %>%
  arrange(mean_change_in_score)

filter(merged_data2, Region == 'Sub-Saharan Africa') %>%
  group_by(Country.Name) %>%
  summarise(mean_change_in_score = mean(Change.in.Region.Rank)) %>%
  top_n(5, mean_change_in_score) %>%
  arrange(desc(mean_change_in_score))

filter(merged_data2, Region == 'Asia-Pacific') %>%
  group_by(Country.Name) %>%
  summarise(mean_change_in_score = mean(Change.in.Region.Rank)) %>%
  top_n(5, mean_change_in_score) %>%
  arrange(mean_change_in_score)

filter(merged_data2, Region == 'Asia-Pacific') %>%
  group_by(Country.Name) %>%
  summarise(mean_change_in_score = mean(Change.in.World.Rank)) %>%
  top_n(-5, mean_change_in_score) %>%
  arrange(mean_change_in_score)

filter(merged_data2, Region == 'Asia-Pacific') %>%
  group_by(Country.Name) %>%
  summarise(mean_change_in_score = mean(Change.in.Score)) %>%
  top_n(-5, mean_change_in_score) %>%
  arrange(mean_change_in_score)

filter(merged_data2, Region == 'Asia-Pacific') %>%
  group_by(Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(5, mean_score) %>%
  arrange(desc(mean_score))

filter(merged_data2, Region == 'Asia-Pacific') %>%
  group_by(Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(-5, mean_score) %>%
  arrange(mean_score)

filter(merged_data2, Region == 'Europe') %>%
  group_by(Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(5, mean_score) %>%
  arrange(desc(mean_score))

filter(merged_data2, Region == 'Europe') %>%
  group_by(Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(-5, mean_score) %>%
  arrange(mean_score)


filter(merged_data2, Region == 'Middle East and North Africa') %>%
  group_by(Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(5, mean_score) %>%
  arrange(desc(mean_score))

filter(merged_data2, Region == 'Middle East and North Africa') %>%
  group_by(Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(-5, mean_score) %>%
  arrange(mean_score)

merged_data2 %>%
  group_by(Year, Region) %>%
  summarise(mean_score = mean(Score)) %>%
  arrange(desc(mean_score))

merged_data2 %>%
  group_by(Year, Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(5, mean_score) %>%
  arrange(mean_score)

merged_data2 %>%
  group_by(Year, Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(5, mean_score) %>%
  arrange(desc(mean_score))

merged_data2 %>%
  group_by(Year, Country.Name, Region.Rank) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(5, mean_score) %>%
  arrange(desc(mean_score))

merged_data2 %>%
  group_by(Year, Region) %>%
  summarise(Mean_Rule_of_Law = mean(c(Property.Rights,Government.Integrity,Judical.Effectiveness)))

#Data Visualization
merged_data2 %>%
  group_by(Quartile) %>%
  ggplot() +
  geom_boxplot(
    aes(Quartile, GDP.per.Capita),
  )

ggplot(data = merged_data2, aes(x = Property.Rights, y = Score)) +
  geom_point()

merged_data2 %>%
  group_by(Quartile) %>%
  ggplot() +
  geom_boxplot(aes(Quartile, Mean_Rule_of_Law))

merged_data2 %>%
  group_by(Quartile) %>%
  ggplot() +
  geom_boxplot(aes(Quartile, Mean_Regulatory_Efficiency))

merged_data2 %>%
  group_by(Year, Region) %>%
  ggplot() +
  geom_point(aes(Mean_Rule_of_Law, Score, color=factor(Region))) +
  facet_wrap(~ Year)

ggplot(data = merged_data2, aes(x = GDP.per.Capita, y = Score)) +
  geom_point() + geom_smooth(method = "lm")

ggplot(data = merged_data2, aes(x = Region, y = Score)) +
  geom_point(aes(color = Region)) 

ggplot(data = merged_data2, aes(x = GDP.per.Capita, y = Score)) +
  geom_point(aes(color = Region)) + facet_wrap(. ~ Region)

ggplot(data = merged_data2, aes(x = Region, y = Score)) + geom_boxplot()


ggplot(data = merged_data2,aes(x = Score)) + geom_bar(aes(fill = Region))


top_Scores %>% ggplot() +
  geom_point(aes(Mean_Rule_of_Law, Score, color=factor(Region))) +
  facet_wrap(~ Year)
  
top_Scores %>%
  group_by(Region) %>%
  ggplot() + geom_bar(aes)

ggplot(data = top_Scores %>%
         group_by(Region), aes(x=Country.Name)) + geom_bar(aes(fill = Score))

ggplot(data = head(top_Scores, 40)) + aes(x = GDP.Billions.PPP, y = Score) + geom_point(aes(color = Country.Name)) +
  facet_wrap(~ Year)
ggplot(data = tail(top_Scores, 20)) + aes(x = GDP.Billions.PPP, y = Score) + geom_point(aes(color = Country.Name))

ggplot(data = head(top_GDPs, 50)) + aes(x = GDP.Billions.PPP, y = Score) + geom_point(aes(color = Country.Name))
ggplot(data = tail(top_GDPs, 50)) + aes(x = GDP.Billions.PPP, y = Score) + geom_point(aes(color = Country.Name))

ggplot(data=merged_data2 %>%
         group_by(Country.Name) %>%
         top_n(2, GDP.Billions.PPP) %>%
         arrange(desc(GDP.Billions.PPP)) %>%
         select(Country.Name, Year, GDP.Billions.PPP, Score, Region)) + aes(x = GDP.Billions.PPP, y = Score) +
  geom_point(aes(color = Region))
  

ggplot(data=merged_data2 %>%
         group_by(Region) %>%
         summarise(mean_score = mean(Score)) %>%
         top_n(5, mean_score) %>%
         arrange(desc(mean_score))) +
  geom_bar(aes(x=Region,fill=mean_score), position = 'dodge')

ggplot(data=merged_data2 %>%
         group_by(Country.Name) %>%
         summarise(mean_score = mean(Score)) %>%
         top_n(5, mean_score), aes(x=reorder(Country.Name, -mean_score), y=mean_score)) +
  geom_bar(stat = 'identity', aes(fill = factor(mean_score))) + xlab('Country') + ylab('Mean Score') + theme(legend.position = "none") + scale_y_continuous(breaks = c(0, 20, 40, 60, 80)) + theme(axis.text =
                                                                                                                                                                                                     element_text(size = 12),
                                                                                                                                                                                                   axis.title =
                                                                                                                                                                                                     element_text(size = 14, face = "bold"))
merged_data2 %>%
  group_by(Region, Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(-5, mean_score) %>%
  arrange(mean_score)

filter(merged_data2, Region == 'Americas') %>%
  group_by(Country.Name, Year) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(2, mean_score) %>%
  arrange(desc(mean_score))  %>%
  ggplot() +
  geom_point(aes(mean_score, Country.Name, color=factor(Country.Name))) +
  facet_wrap(~ Year)

filter(merged_data2, Region == 'Americas') %>%
  group_by(Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(-5, mean_score) %>%
  arrange(mean_score)

filter(merged_data2, Region == 'Americas') %>%
  group_by(Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(5, mean_score) %>%
  arrange(mean_score)

ggplot(data=filter(merged_data2, Region == 'Americas') %>%
         group_by(Country.Name) %>%
         summarise(mean_score = mean(Score)) %>%
         top_n(5, mean_score) %>%
         arrange(mean_score), aes(x=reorder(Country.Name, -mean_score), y=mean_score)) +
  geom_bar(stat='identity', aes(fill=factor(mean_score))) + xlab('Country') + ylab('Mean Score') + theme(legend.position = "none") + scale_y_continuous(limits=c(0, 80)) + theme(axis.text=element_text(size=12),
                                                                                                                                                                                 axis.title=element_text(size=14,face="bold"))

ggplot(
  data = filter(merged_data2, Region == 'Americas') %>%
    group_by(Country.Name) %>%
    summarise(mean_score = mean(Score)) %>%
    top_n(-5, mean_score) %>%
    arrange(mean_score),
  aes(x = reorder(Country.Name,-mean_score), y = mean_score)
) +
  geom_bar(stat = 'identity', aes(fill = factor(mean_score))) + xlab('Country') + ylab('Mean Score') + theme(legend.position = "none") + scale_y_continuous(limits =
                                                                                                                                                              c(0, 80))  + theme(axis.text = element_text(size = 12),
                                                                                                                                                                                 axis.title =
                                                                                                                                                                                   element_text(size = 14, face = "bold"))

filter(merged_data2, Region == 'Sub-Saharan Africa') %>%
  group_by(Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(5, mean_score) %>%
  arrange(desc(mean_score))

ggplot(data=filter(merged_data2, Region == 'Sub-Saharan Africa') %>%
         group_by(Country.Name) %>%
         summarise(mean_score = mean(Score)) %>%
         top_n(5, mean_score) %>%
         arrange(mean_score), aes(x=reorder(Country.Name, -mean_score), y=mean_score)) +
  geom_bar(stat='identity', aes(fill=factor(mean_score))) + xlab('Country') + ylab('Mean Score') + theme(legend.position = "none") + scale_y_continuous(limits=c(0, 80)) + theme(axis.text=element_text(size=12),
                                                                                                                                                                                 axis.title=element_text(size=14,face="bold"))


filter(merged_data2, Region == 'Sub-Saharan Africa') %>%
  group_by(Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(-5, mean_score) %>%
  arrange(mean_score)

ggplot(data=filter(merged_data2, Region == 'Sub-Saharan Africa') %>%
         group_by(Country.Name) %>%
         summarise(mean_score = mean(Score)) %>%
         top_n(-5, mean_score) %>%
         arrange(mean_score), aes(x=reorder(Country.Name, -mean_score), y=mean_score)) +
  geom_bar(stat='identity', aes(fill=factor(mean_score))) + xlab('Country') + ylab('Mean Score') + theme(legend.position = "none") + scale_y_continuous(limits=c(0, 80))+ theme(axis.text=element_text(size=12),
                                                                                                                                                                                axis.title=element_text(size=14,face="bold"))


filter(merged_data2, Region == 'Asia-Pacific') %>%
  group_by(Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(5, mean_score) %>%
  arrange(desc(mean_score))

ggplot(data=filter(merged_data2, Region == 'Asia-Pacific') %>%
         group_by(Country.Name) %>%
         summarise(mean_score = mean(Score)) %>%
         top_n(5, mean_score) %>%
         arrange(mean_score), aes(x=reorder(Country.Name, -mean_score), y=mean_score)) +
  geom_bar(stat='identity', aes(fill=factor(mean_score))) + xlab('Country') + ylab('Mean Score') + theme(legend.position = "none") + scale_y_continuous(breaks=c(0,20,40,60,80)) + theme(axis.text=element_text(size=12),
                                                                                                                                                                                         axis.title=element_text(size=14,face="bold"))


filter(merged_data2, Region == 'Asia-Pacific') %>%
  group_by(Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(-5, mean_score) %>%
  arrange(mean_score)

ggplot(data=filter(merged_data2, Region == 'Asia-Pacific') %>%
         group_by(Country.Name) %>%
         summarise(mean_score = mean(Score)) %>%
         top_n(-5, mean_score) %>%
         arrange(mean_score), aes(x=reorder(Country.Name, -mean_score), y=mean_score)) +
  geom_bar(stat='identity', aes(fill=factor(mean_score))) + xlab('Country') + ylab('Mean Score') + theme(legend.position = "none") + scale_y_continuous(limits=c(0, 80)) + theme(axis.text=element_text(size=12),
                                                                                                                                                                                 axis.title=element_text(size=14,face="bold"))


filter(merged_data2, Region == 'Europe') %>%
  group_by(Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(5, mean_score) %>%
  arrange(desc(mean_score))

ggplot(data=filter(merged_data2, Region == 'Europe') %>%
         group_by(Country.Name) %>%
         summarise(mean_score = mean(Score)) %>%
         top_n(5, mean_score) %>%
         arrange(mean_score), aes(x=reorder(Country.Name, -mean_score), y=mean_score)) +
  geom_bar(stat='identity', aes(fill=factor(mean_score))) + xlab('Country') + ylab('Mean Score') + theme(legend.position = "none") + scale_y_continuous(breaks=c(0,20,40,60,80)) + theme(axis.text=element_text(size=12),
                                                                                                                                                                                         axis.title=element_text(size=14,face="bold"))


filter(merged_data2, Region == 'Europe') %>%
  group_by(Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(-5, mean_score) %>%
  arrange(mean_score)

ggplot(data=filter(merged_data2, Region == 'Europe') %>%
         group_by(Country.Name) %>%
         summarise(mean_score = mean(Score)) %>%
         top_n(-5, mean_score) %>%
         arrange(mean_score), aes(x=reorder(Country.Name, -mean_score), y=mean_score)) +
  geom_bar(stat='identity', aes(fill=factor(mean_score))) + xlab('Country') + ylab('Mean Score') + theme(legend.position = "none") + scale_y_continuous(limits=c(0, 80))+ theme(axis.text=element_text(size=12),
                                                                                                                                                                                axis.title=element_text(size=14,face="bold"))

filter(merged_data2, Region == 'Middle East and North Africa') %>%
  group_by(Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(5, mean_score) %>%
  arrange(desc(mean_score))

ggplot(data=filter(merged_data2, Region == 'Middle East and North Africa') %>%
         group_by(Country.Name) %>%
         summarise(mean_score = mean(Score)) %>%
         top_n(5, mean_score) %>%
         arrange(mean_score), aes(x=reorder(Country.Name, -mean_score), y=mean_score)) +
  geom_bar(stat='identity', aes(fill=factor(mean_score))) + xlab('Country') + ylab('Mean Score') + theme(legend.position = "none")+scale_y_continuous(limits=c(0, 80))+ theme(axis.text=element_text(size=12),
                                                                                                                                                                              axis.title=element_text(size=14,face="bold"))


filter(merged_data2, Region == 'Middle East and North Africa') %>%
  group_by(Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(-5, mean_score) %>%
  arrange(mean_score)

ggplot(data=filter(merged_data2, Region == 'Middle East and North Africa') %>%
         group_by(Country.Name) %>%
         summarise(mean_score = mean(Score)) %>%
         top_n(-5, mean_score) %>%
         arrange(mean_score), aes(x=reorder(Country.Name, -mean_score), y=mean_score)) +
  geom_bar(stat='identity', aes(fill=factor(mean_score))) + xlab('Country') + ylab('Mean Score') + theme(legend.position = "none") + scale_y_continuous(limits=c(0, 80)) + theme(axis.text=element_text(size=12),
                                                                                                                                                                                 axis.title=element_text(size=14,face="bold"))

merged_data2 %>%
  group_by(Year, Region) %>%
  summarise(mean_score = mean(Score)) %>%
  arrange(desc(mean_score)) %>%
  ggplot()+ geom_col(aes(Region, mean_score, color= factor(Region))) + facet_wrap(~ Year)

#Data Manipulation & Visualization
merged_data2 %>%
  group_by(Year, Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(5, mean_score) %>%
  arrange(mean_score)

merged_data2 %>%
  group_by(Year, Country.Name) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(5, mean_score) %>%
  arrange(desc(mean_score))

merged_data2 %>%
  group_by(Year, Country.Name, Region.Rank) %>%
  summarise(mean_score = mean(Score)) %>%
  top_n(5, mean_score) %>%
  arrange(desc(mean_score))

merged_data2 %>%
  group_by(Country.Name) %>%
  summarise(mean_change_in_score = mean(Change.in.Score)) %>%
  top_n(5, mean_change_in_score) %>%
  arrange(mean_change_in_score)

ggplot(data=merged_data2 %>%
         group_by(Country.Name) %>%
         summarise(mean_change_in_score = mean(Change.in.Score)) %>%
         top_n(5, mean_change_in_score) %>%
         arrange(mean_change_in_score), aes(x=reorder(Country.Name, -mean_change_in_score), y=mean_change_in_score)) +
  geom_bar(stat='identity', aes(fill=factor(mean_change_in_score))) + xlab('Country') + ylab('Change in Score') + theme(legend.position = "none") + theme(axis.text=element_text(size=12),
                                                                                                                                                          axis.title=element_text(size=14,face="bold"))

merged_data2 %>%
  group_by(Country.Name) %>%
  summarise(mean_change_in_score = mean(Change.in.Score)) %>%
  top_n(-5, mean_change_in_score) %>%
  arrange(mean_change_in_score)

ggplot(data=merged_data2 %>%
         group_by(Country.Name) %>%
         summarise(mean_change_in_score = mean(Change.in.Score)) %>%
         top_n(-5, mean_change_in_score) %>%
         arrange(mean_change_in_score), aes(x=reorder(Country.Name, -mean_change_in_score), y=mean_change_in_score)) +
  geom_bar(stat='identity', aes(fill=factor(mean_change_in_score))) + xlab('Country') + ylab('Change in Score') + theme(legend.position = "none") + theme(axis.text=element_text(size=12),
                                                                                                                                                          axis.title=element_text(size=14,face="bold"))

#Data Visualization cont.
ggplot(data = merged_data2 %>%
         group_by(Year, Country.Name, Region) %>%
         summarise(Mean_Rule_of_Law = mean(c(Property.Rights,Government.Integrity,Judical.Effectiveness))), 
       aes(x = Region, y = Mean_Rule_of_Law)) + geom_boxplot()

merged_data2 %>%
  group_by(Year, Region) %>%
  summarise(Mean_Government_Size = mean(c(Gov.t.Spending,Tax.Burden,Fiscal.Health)))

ggplot(data = merged_data2 %>%
         group_by(Year, Region) %>%
         summarise(Mean_Government_Size = mean(c(Gov.t.Spending,Tax.Burden,Fiscal.Health))), 
       aes(x = Region, y = Mean_Government_Size)) + geom_boxplot()

merged_data2 %>%
  group_by(Year, Region) %>%
  summarise(Mean_Regulatory_Efficiency = mean(c(Business.Freedom,Labor.Freedom,Monetary.Freedom)))

ggplot(merged_data2 %>%
         group_by(Year, Region) %>%
         summarise(Mean_Regulatory_Efficiency = mean(c(Business.Freedom,Labor.Freedom,Monetary.Freedom))), 
       aes(x = Region, y = Mean_Regulatory_Efficiency)) + geom_boxplot()

merged_data2 %>%
  group_by(Year, Region) %>%
  summarise(Mean_Open_Markets = mean(c(Trade.Freedom,Investment.Freedom,Financial.Freedom)))

ggplot(merged_data2 %>%
         group_by(Year, Region) %>%
         summarise(Mean_Open_Markets = mean(c(Trade.Freedom,Investment.Freedom,Financial.Freedom))), 
       aes(x = Region, y = Mean_Open_Markets)) + geom_boxplot()

#Linear Regression
model = lm(Score ~ X5.Year.GDP.Growth.Rate + GDP.per.Capita + Public.Debt, data = merged_data2)

AIC(model)
vif(model)

summary(model)

model = lm(Score ~ Tariff.Rate + Corporate.Tax.Rate + Tax.Burden.of.GDP
           + X5.Year.GDP.Growth.Rate + GDP.per.Capita + Inflation + Public.Debt, data = merged_data2) 


AIC(model)
BIC(model)

summary(model)
vif(model)

model = lm(GDP.Billions.PPP ~ Property.Rights + Judical.Effectiveness + Government.Integrity + Tax.Burden + Gov.t.Spending + Fiscal.Health + Business.Freedom
           + Labor.Freedom + Monetary.Freedom + Investment.Freedom + Financial.Freedom + Tariff.Rate 
           + Income.Tax.Rate + Corporate.Tax.Rate + Gov.t.Expenditure.of.GDP + Population.Millions +
             Score + FDI.Inflow + Public.Debt, data = merged_data2) #

summary(model)

model = lm(GDP.per.Capita ~ Score + Mean_Regulatory_Efficiency + Mean_Government_Size + Mean_Open_Markets, data = merged_data2)
summary(model)

names(merged_data2)

model = lm(World.Rank ~ Property.Rights + Judical.Effectiveness + Government.Integrity + Tax.Burden + Gov.t.Spending +
             Fiscal.Health + Business.Freedom + Labor.Freedom + Monetary.Freedom + Trade.Freedom + 
             Investment.Freedom + Financial.Freedom, data = merged_data2)

summary(model)

coefs <- tidy(model)
coefs[order(coefs$estimate, decreasing = FALSE),]

coefs

model = lm(GDP.per.Capita ~ Score + Mean_Rule_of_Law + Mean_Regulatory_Efficiency + Mean_Government_Size, data = merged_data2)
summary(model)

model = lm(GDP.per.Capita ~ Score + Mean_Rule_of_Law + Mean_Regulatory_Efficiency + Mean_Government_Size, data = merged_data2)
summary(model)


