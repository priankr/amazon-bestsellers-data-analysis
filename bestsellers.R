install.packages('tidyverse')
library(tidyverse)

install.packages('ggplot2')
library(ggplot2)

install.packages("gghighlight")
library(gghighlight)

#Importing the csv file to a books dataframe
books <- read.csv("bestsellers with categories.csv")

#Viewing the dataframe
head(books)

#Identifying the column names
names(books)

### Correcting Formatting Issues
# Change the column name from User.Rating to User_Rating
names(books)[names(books) == "User.Rating"] <- "User_Rating"

### Checking for Missing Values

#Identifying total number of missing values
sum(is.na(books))

#Statistics

#Identifying Number of books
total_books = count(books)

print(paste("There are",total_books,"books in total."))

#Identifying Number of books in each genre
count(books,Genre)

#Identifying the average user rating, rounded to 2 decimal places
avg_rating <- round(mean(books$User_Rating),digits =2)

#Identifying the average price, rounded to 2 decimal places
avg_price <- round(mean(books$Price),digits =2)

#Identifying the average number of reviews, rounded to 2 decimal places
avg_reviews <- round(mean(books$Reviews),digits =2)

#Identifying the highest user rating
max_rating <- max(books$User_Rating)

#Identifying the highest price
max_price <- max(books$Price)

#Identifying the highest price
max_reviews<- max(books$Reviews)

#Printing the information
cat("For books in the Bestseller list: \nAverage user rating is",avg_rating,"and the highest user rating is",max_rating,"\nAverage price of $",avg_price,"and the highest price is $",max_price, "\nAverage number of reviews is",avg_reviews,"and the highest number of reviews is",max_reviews)


##Genre

#Grouping by genre
genre <- books %>% group_by(Genre)

#Summarizing average price for for books each year in each genre
genre_stats <- genre %>% summarise(Price = mean(Price))

#Renaming the price column to Average_price
colnames(genre_stats)[which(names(genre_stats) == "Price")] <- "Average_price"

#Summarizing average number of reviews for for books each year in each genre
genre_avg_reviews <- genre %>% summarise(Reviews = mean(Reviews))

#Adding the "Reviews" column from "genre_avg_reviews" to the genre dataframe as "average_reviews" 
genre_stats$Average_number_of_reviews <- genre_avg_reviews$Reviews

#Summarizing average user rating for for books each year in each genre
genre_avg_rating <- genre %>% summarise(User_Rating = mean(User_Rating))

genre_stats$Average_user_rating <- genre_avg_rating$User_Rating

genre_stats


## PLOTS

##User Rating
#plotting bar graph for user rating vs. number of reviews
ggplot(data=books,aes(x=User_Rating,y=Reviews)) + geom_bar(stat="identity") + labs(title="User Rating vs. Number of Reviews")

#Grouping by user rating
by_rating_price <- books %>% group_by(User_Rating)

#Summarizing average price for each user rating value
by_rating_price <- by_rating_price %>% summarise(Price = mean(Price))

#plotting bar graph for user rating vs. price
ggplot(data=by_rating_price,aes(x=User_Rating,y=Price)) + geom_col() + labs(title="User Rating vs. Average Price")

###Price

#plotting scatter plot for price vs. number of reviews
ggplot(data=books,aes(x=Price,y=Reviews)) + geom_point() + labs(title="Price vs. Number of Reviews") + gghighlight(Price<=25)


#plotting scatter plot for price vs. number of reviews
#Highlighting books with more than 25,000 reviews 
ggplot(data=books,aes(x=Price,y=Reviews)) + geom_point() + facet_wrap(~Genre) + labs(title="Price vs. Number of Reviews by Genre") + gghighlight(Reviews>25000)

### Year
#Grouping by year
by_year <- books %>% group_by(Year)

#Summarizing average price for for books each year
by_year_price <- by_year %>% summarise(Price = mean(Price))

#Summarizing average price for for books each year
by_year_reviews <- by_year %>% summarise(Reviews = mean(Reviews))

#plotting bar graph for year vs. average number of reviews
ggplot(data=by_year_reviews,aes(x=Year,y=Reviews)) + geom_col() + labs(title="Average Number of Reviews Per Year")

#plotting bar graph for year vs. average price
ggplot(data=by_year_price,aes(x=Year,y=Price)) + geom_col() + labs(title="Average Price Per Year")
