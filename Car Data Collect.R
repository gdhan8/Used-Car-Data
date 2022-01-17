#Collect Car Data into a dataframe
library(rvest)
library(dplyr)
library(DescTools)
library(stringr)
library(tidyr)

##### 1. Collect, Clean, Collate Data ######

#I manually created a location list with the required link format and top 20 most populous areas
#This way the scraping link is shielded and 
locations <- read.csv("~/Desktop/R Projects/Car Data Project/location_list.csv", stringsAsFactors=FALSE)
locations <- locations[1:20,]
locations$LinkPartial2 <- "&searchRadius=200&sort[]=best_match"

#loop through locations
for(j in seq(from = 1, to = length(locations$Location))){

  #use webscraping to compile the cars
  #Create null data frames for data to inputed into in the for loop,...or read in the most recent file, initialize dataframes and i count to be used in the for loop, we need i to periodically close the connection to website
  car_data = data.frame()
  all_car_data = data.frame()
  i = 1
  
#Loop through car listings to collect key listing information
for(page_result in seq(from = 2, to = 101 )){
  
  #store & read html
  #concatenate link partials from the location list and the page result to generate a custum iterative url link
  url1 = paste0(locations$LinkPartial[j], page_result,locations$LinkPartial2[j])
  page <- read_html(url1)
  
  #identify the nodes from the html and create vectors to store a page's data
  #used google chrome and the selector gadget extension
  #sometimes an individual page will have fewer or greater than 30 results
  #I only want 30 of the results so that dimensions work later
  car_title = page %>% html_nodes(".vehicle-header-make-model") %>% html_text()
  car_title = car_title[1:30]
  car_year = page %>% html_nodes(".vehicle-card-year") %>% html_text()
  car_year = car_year[1:30]
  car_price = page %>% html_nodes(".vehicle-card-bottom-max-50") %>% html_text()
  car_price = car_price[1:30]
  car_mileage = page %>% html_nodes(".font-size-1.text-truncate:nth-child(1)") %>% html_text()
  car_mileage = car_mileage[1:30]
  car_accidents = page %>% html_nodes(".border-top+ .margin-top-1") %>% html_text()
  car_accidents = car_accidents[1:30]
  car_location = page %>% html_nodes(".justify-content-between+ .margin-top-1") %>% html_text()
  car_location = car_location[1:30]
  car_color = page %>% html_nodes(".margin-top-1.text-truncate") %>% html_text()
  car_color = car_color[1:30]
  car_page = page %>% html_nodes(".vehicle-card-overlay") %>% html_attr("href") %>% paste0("x",.)
  car_page = car_page[1:30]
  #Record today's date
  Date = Sys.Date()
  #Record the search location center
  Search_Location = locations$Location[j]
  #construct the 
  car_data = data.frame(Date,car_title,car_year,car_price, car_mileage, car_accidents, car_color, car_page, car_location, Search_Location,stringsAsFactors = FALSE)
  
  #bind one page of car data to the previously stored car data
  all_car_data = rbind.data.frame(all_car_data,car_data)
  
  #this may take some time. this way we can track progress, uesed for troubleshooting
  #print(paste0("Page: ", page_result, " Center Location: ",locations$Location[j], " Percent Complete: ", j*page_result/(100*length(locations$Location))*100,"%"))
  
  #we must be kind to the website we are scraping, allow the system to sleep for a bit.
  i=i+1
  if(is.integer(i/10)){Sys.sleep(10)
    closeAllConnections()}
  else{Sys.sleep(2)}
}

#get more data from the car_page

##### 2. Clean the Data #####

#Clean the car mileage, remove commas, coerce to numeric, remove miles text
all_car_data$car_mileage<- as.character(StrLeft(all_car_data$car_mileage,-6))
all_car_data$car_mileage<- as.numeric(gsub(",","",all_car_data$car_mileage))

#clean the car year data, coerce to numeric
all_car_data$car_year <- as.numeric(all_car_data$car_year)

#clean the car price data, remove the $ sign and comma
all_car_data$car_price <- as.numeric(str_replace_all(all_car_data$car_price, "[^[:alnum:]]", "")) 

#separate car accident data because it contains three different variables. 
all_car_data <- separate(all_car_data, col =  car_accidents, into = c("Accidents", "Owners","Fleet_Use"), sep = ", " )

#remove accidents/accident, convert no accidents to 0
all_car_data$Accidents <- StrLeft(all_car_data$Accidents,1)
all_car_data$Accidents<- as.factor(replace(all_car_data$Accidents, all_car_data$Accidents=="N", 0))

#remove owner/owners
all_car_data$Owners <- as.numeric(StrLeft(all_car_data$Owners,1))
all_car_data$Owners <- as.factor(replace(all_car_data$Owners, all_car_data$Owners == "NA",1))

#Fleet Use to 1, Personal Use to Zero, Unknown?
all_car_data$Fleet_Use <- replace(all_car_data$Fleet_Use, all_car_data$Fleet_Use=="Fleet use", 1)
all_car_data$Fleet_Use <- replace(all_car_data$Fleet_Use, all_car_data$Fleet_Use=="Personal use", 0)
all_car_data$Fleet_Use <- as.factor(all_car_data$Fleet_Use)
                                            
#separate car_color data because it contains two different color variables
all_car_data <- separate(all_car_data, col =  car_color, into = c("Exterior", "Interior"), sep = ", " )
all_car_data$Exterior <- StrLeft(all_car_data$Exterior,-9)
all_car_data$Interior <- StrLeft(all_car_data$Interior,-9)

#clean car location
#pull out range
#separate city from state
all_car_data <- separate(all_car_data, col =  car_location, into = c("Range", "Location"), sep = " mi - " )
all_car_data$Range <- as.numeric(all_car_data$Range)
all_car_data <- separate(all_car_data, col =  Location, into = c("City", "State"), sep = ", " )

#clean make and model
all_car_data <- separate(all_car_data, col =  car_title, into = c("Make", "Model"), extra = "merge", fill = "left", sep = " " )

#Clean the column names
colnames(all_car_data)[colnames(all_car_data) == "car_year"] <- "Year"
colnames(all_car_data)[colnames(all_car_data) == "car_price"] <- "Price"
colnames(all_car_data)[colnames(all_car_data) == "car_mileage"] <- "Mileage"
colnames(all_car_data)[colnames(all_car_data) == "car_page"] <- "Link"

###Write the car data to a csv file###
#Write the car data into a csv file in the appropriate working directory
setwd("~/Desktop/R Projects/Car Data Project/Location Data")
write.csv(all_car_data, paste0(locations$City[j], "_", locations$Abb[j] ,"_car_data_", Sys.Date(), ".csv"))

}
#we can run all code above here as a job 
#due to connection issues, the code periodically cut out, forcing collection in parts, the final data will be collated and cleaned at the start of exploratory data analysis.
#one soluion is to write .csv more frequently when an entire location is completed
#in doing so, we can complile it all on the back end after the files have been created

### Collation of individual files ###
#additional cleaning and collation due to breaks in connection during collection

car_data <- data.frame()

for(j in seq(from = 1, to = length(locations$Location))){
  
file_location <- paste0("~/Desktop/R Projects/Car Data Project/Location Data/", locations$City[j], "_", locations$Abb[j] ,"_car_data_", Sys.Date(), ".csv")

all_car_data <- read.csv(file_location)

car_data <- rbind.data.frame(car_data, all_car_data)

}

#write a final .csv for reproducible use later, be careful to not overwrite the final file
write.csv(car_data, paste0("used_car_data_", Sys.Date(), ".csv"))

