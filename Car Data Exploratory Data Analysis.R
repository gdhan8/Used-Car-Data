library(DataExplorer)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(scales) 
library(funModeling) 
library(Hmisc)

my_theme <- theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    panel.grid.major = element_line(colour = "grey70", size = 0.2),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 

##### Exploratory Data Analysis #####
#Now that we have the data and cleaned it, we can just load it for from a file for further analysis.
#set working directory so that any rogue saves go to the correct location
setwd("~/Desktop/R Projects/Car Data Project")
used_cars <- read.csv("~/Desktop/R Projects/Car Data Project/Final Data/used_car_data_2021-03-21.csv")

str(used_cars)
summary(used_cars)

#in the writing and saving csv steps we gained some extra variables
#we will also be dropping range and link as we do not need them for this analysis
used_cars <- used_cars[-c(1,2,3,13:18)]
used_cars$Year <- as.integer(used_cars$Year)
used_cars$Accidents <- as.integer(used_cars$Accidents)
used_cars$Owners <- as.integer(used_cars$Owners)
used_cars$Fleet_Use <- as.factor(used_cars$Fleet_Use)

#Are there any listings that were collected twice??, if so drop.
used_cars <-  unique(used_cars)

#we dropped over 12,000 observations which were duplicates from collection. This is because some of the top twenty search locations during collection had a search radius which overlapped resulting in double collection from different search locations. 

#we omit anything with NA data, this is a crude and brute solution, but the data frame will have completeness
used_cars <-  used_cars %>% na.omit()

used_cars %>% introduce() #completeness check from the DataExplorer package
used_cars %>% plot_histogram()
used_cars %>% plot_bar()
used_cars %>% plot_qq()
used_cars %>% plot_correlation()

describe(used_cars)
freq(used_cars)
plot_num(used_cars)


## Top 10 Makes by Count Visualization

t10makes_n <- used_cars %>% 
  group_by(Make) %>% 
  summarise(count = n()) %>%
  arrange(desc(count)) 

t10makes_n <- t10makes_n[1:10,]

t10makes_n_list <- t10makes_n[,1]

makeColors <- hue_pal()(nrow(t10makes_n_list))       
names(makeColors) <- c("Toyota", "Ford","Nissan","Honda","Chevrolet","Mercedes-Benz","BMW","Lexus","Jeep", "Hyundai")
makeColors
my_scale_fill <- scale_fill_manual(name = "Make", values = makeColors) 
my_scale_col <- scale_color_manual(name = "Make", values = makeColors) 

#####
#First Plot
p1 <- t10makes_n %>%
  ggplot(aes(y = reorder(Make,(count)), x = count, fill = Make)) + 
  geom_bar(stat = 'identity') + 
  geom_text(aes(label = count), vjust = .5, nudge_x = -200) +
  xlab("Count")+
  ylab("Make")+
  ggtitle("Top 10 Most Common Makes in the Used Car Dataset") + 
  my_theme +
  theme(legend.position="none") + 
  my_scale_fill

#Consider Omitting or expanding
## Top 10 Makes by Price Visualization
#Now By Price

t10makes_price <- used_cars %>% 
  group_by(Make) %>% 
  summarise(PriceAvg = mean(Price)) %>%
  arrange(desc(PriceAvg))

t10makes_price <- t10makes_price[1:10,]

t10makes_price %>%
  ggplot(aes(y = reorder(Make,(PriceAvg)), x = PriceAvg, fill = Make)) + 
  geom_bar(stat = 'identity') + 
  geom_text(aes(label = round(PriceAvg, 0)), vjust = .5, nudge_x = -11000) +
  xlab("PriceAvg")+
  ylab("Make")+
  ggtitle("Top 10 Most Expensive Makes in the Used Car Dataset") + 
  my_theme +
  theme(legend.position="none")

t10makes_price_list <- t10makes_price[,1]


### Top 20 Models by Count,Price Visualization
#Lets find the top twenty most common models
t20models_n <- used_cars %>% 
  group_by(Make, Model) %>% 
  summarise(count = n()) %>%
  filter(count > 40) %>%
  arrange(desc(count)) 

#Just the top twenty
t20models_n <- t20models_n[1:20,] 

#Visualize the Top 20 Models in the data set, color by make 
t20models_n %>%
  ggplot(aes(y = reorder(Model,(count)), x = count, fill = Make)) + 
  geom_bar(stat = 'identity') + 
  ylab("Model") + 
  xlab("Count") +
  ggtitle("Most Common Car Models in the Used Car Dataset") + 
  my_theme

#Store the sample models list for filtering later
t20model_n_list = t20models_n[,2]

##Now By Price
t20models_price <- used_cars %>% 
  group_by(Make, Model) %>% 
  summarise(PriceAvg = mean(Price)) %>%
  arrange(desc(PriceAvg)) 

#Just the top twenty
t20models_price <- t20models_price[1:20,] 

#Visualize the Top 20 Models in the data set, color by make 
t20models_price %>%
  ggplot(aes(y = reorder(Model,(PriceAvg)), x = PriceAvg, fill = Make)) + 
  geom_bar(stat = 'identity') + 
  ylab("Model") + 
  xlab("Count") +
  ggtitle("Most Expensive Car Models in the Used Car Dataset") + 
  my_theme

#Store the sample models list for filtering later
t20model_price_list = t20models_price[,2]

#Reference subsets
t10makes_n_list
t10makes_price_list
t20model_n_list
t20model_price_list

# stacked density plots
used_cars %>%
  filter(Price <=100000) %>% 
  ggplot(aes(x = Price))+
  scale_x_continuous() +
  geom_density(alpha = 0.4, position = "stack")+
  my_theme +
  ylab("Density")

#####
#Second Plot
p2 <-used_cars %>%
  filter(Make %in% t10makes_n_list$Make& Price <=100000) %>% 
  group_by(Make) %>%
  ggplot(aes(x = Price, fill = Make))+
  scale_x_continuous() +
  geom_density(alpha = 0.4, position = "stack")+
  my_theme +
  ylab("Density")+ 
  my_scale_fill

#Consider Omitting below here
 used_cars %>%
   filter(Make %in% t10makes_price_list$Make) %>% 
   group_by(Make) %>%
   ggplot(aes(x = Price, fill = Make))+
   scale_x_continuous() +
   geom_density(alpha = 0.4, position = "stack")+
   my_theme +
   ylab("Density")
 
 used_cars %>%
   filter(Model %in% t20model_n_list$Model[1:10]) %>% 
   group_by(Model) %>%
   ggplot(aes(x = Price, fill = Model))+
   scale_x_continuous() +
   geom_density(alpha = 0.4, position = "stack")+
   my_theme +
   ylab("Density")
 
 used_cars %>%
   filter(Model %in% t20model_price_list$Model[1:14]) %>% 
   group_by(Model) %>%
   ggplot(aes(x = Price, fill = Model)) +
   scale_x_continuous() +
   geom_density(alpha = 0.4, position = "stack") +
   my_theme +
   ylab("Density")
 

 


#we dropped over 1400 observations from the presences of NA in the data. 

#filter out models with less than 40 model and year, here we are filtering data to only analyze those with sufficient observations
#is this necessary??
used_cars <- used_cars %>% 
  group_by(Model, Year) %>%
  mutate(nModel = n()) %>%
  filter(nModel > 40)

used_cars <- used_cars %>% 
  group_by(Make) %>%
  mutate(nMake = n()) %>%
  filter(nMake > 40) %>% 
  arrange(desc(nMake))
#we dropped over 45,000 observations because we did not have sufficient observations for the year and model while adding make and model 




used_cars %>% 
  filter(Make == "Toyota") %>%
  group_by(Year) %>%
  ggplot(aes(x = Year, y = Price, group = Year, col = Make) ) + 
  geom_boxplot() +
  my_theme+
  labs(
    x = "Year",
    y = "Average Price",
    colour = "Color",
    title = "Used Car Price by Year"
  )

#####
#Third Plot 
#Box plot of used car prices by make with jitter
p3<- used_cars %>% 
  filter( Make %in% t10makes_n_list$Make & Price <=100000 )%>%
  group_by(Make) %>%
  ggplot(aes(x = reorder(Make,-Price), y = Price, group = Make, col = Make) ) + 
  geom_boxplot(outlier.colour= "NA")+
  geom_jitter(aes( alpha =.99), 
              position = position_jitter(width = .2, height=-0.7),
              size=.1)+
  my_theme+
  my_scale_col+
  labs(
    x = "Make",
    y = "Price",
    colour = "Color",
    title = "Used Car Price Boxplot by Make"
  )+
  theme(legend.position="none") 

#Consider Omitting Below
  
used_cars %>% 
  filter(Make == "Ford" & Price < 100000)%>%
  group_by(Year) %>%
  ggplot(aes(x = Year, y = Price, group = Year, col = Model) ) + 
  geom_boxplot() +
  facet_wrap(~Model)+
  my_theme+
  labs(
    x = "Year",
    y = "Average Price",
    colour = "Color",
    title = "Used Car Price by Year"
  )+
  theme(legend.position="none")

#####
#Fourth Plot
p4 <- used_cars %>% 
  filter(Make == "Ford" & Model == "F-150" & Price < 100000)%>%
  group_by(Year) %>%
  ggplot(aes(x = Year, y = Price, group = Year, col = Model) ) + 
  geom_boxplot() +
  facet_wrap(~Model)+
  my_theme+
  labs(
    x = "Year",
    y = "Average Price",
    colour = "Color",
    title = "Used Car Price by Year"
  )+
  theme(legend.position="none") 

#Consider Omitting this
#
used_cars %>% 
  filter(Price < 100000 & Year=="2018" & Make %in% t10makes_n_list$Make)%>%
  group_by()%>%
  arrange(Price)%>%
  ggplot(aes(x = Price, y = reorder(Make, Price), group = Make, col = Make)) + 
  geom_boxplot() +
  my_theme+
  labs(
    x = "Price",
    y = "Make",
    colour = "Color",
    title = "Used Car Prices by Make 2018"
  )+
  theme(legend.position="none")

#####
#Fifth Plot
p5 <- used_cars %>% 
  filter(Price < 100000  & Make %in% t10makes_n_list$Make)%>%
  group_by(Make, Year) %>%
  summarise(PriceAvg = mean(Price)) %>%
  ggplot(aes(x = Year, y = PriceAvg, group = Make, col = Make)) + 
  geom_line() +
  my_theme+
  labs(
    x = "Year",
    y = "Price",
    colour = "Color",
    title = "Top 10 Most Common Models Price vs Year by Make"
  )+
  my_scale_col

#####
#Sixth Plot
p6<- used_cars %>% 
  filter(Price < 100000  & Make %in% t10makes_n_list$Make)%>%
  group_by(Make, Year) %>%
  summarise(PriceAvg = mean(Price), MileageAvg = mean(Mileage)) %>%
  ggplot(aes(x = MileageAvg, y = PriceAvg, group = Make, col = Make)) + 
  geom_line() +
  my_theme+
  labs(
    x = "Average Mileage",
    y = "Average Price",
    colour = "Color",
    title = "Top 10 Most Common Models Price vs Mileage by Make"
  )+
  my_scale_col

p1
p2
p3
p4
p5
p6
#


test <- used_cars %>%
  filter(Make %in% t10makes_n_list$Make & Owners == 1) %>%
  mutate(AccidentPerYear = Accidents/(2022-Year)) %>%
  group_by(Make, Model, Year, Exterior) %>%
  summarise(AccidentAvgPerYear = mean(AccidentPerYear), count = n()) %>%
  filter(count > 40) 

#I want to display time and the number of accidents with car make model and association to a single driver
ggplot(aes(x=Year, y = AccidentAvg, col=Make))
  

summary(test)
describe(used_cars)

fourrunner <- used_cars %>%
  filter(Make == "Toyota" & Model == "4Runner") %>%
  group_by(Year) %>%
  summarise( AveragePrice = mean(Price))
  

#Pivot tables


#identify similar cars and find the average price based on make, model, year, mileage, accidents

# Create some data visualizations

# Create a study on the effects of car color on car price  Price ~ Year + Mileage + Accidents vs Price ~ Year + Mileage + Accidents + Exterior + Interior

# For a given Make and Model, Predict Price ~ Year, Mileage, Accidents

# For a given Make and Model, find the difference between Predict price and the actual price
