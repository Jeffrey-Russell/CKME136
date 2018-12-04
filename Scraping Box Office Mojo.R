urlTopMovies <- "https://www.boxofficemojo.com/yearly/chart/?yr=2018&p=.htm"
webpage_topMovies <- read_html(urlTopMovies)
urlTopMovies_html <- html_nodes(webpage,"table .td")
html_text(urlTopMovies_html[[5]])
html_attrs(urlTopMovies_html)





#Importing movie URL list
movieURL <- read.csv("Box_Office_Mojo_URL.csv")

#Creating movie data frame
movie_data_frame <- data.frame(title = character(),genre = factor(),
                               mpaaRating = character(), budget = character())

movie_data_frame <- NULL

#Getting Box Office Mojo data for each movie
for (i in 887:925) {
        url <- movieURL[i,1]
        webpage <- read_html(as.character(url))

#Using CSS selectors to scrap the rankings section
        movie_data_html <- html_nodes(webpage,'b')

#Converting the data to text
        movie_data <- html_text(movie_data_html)
        movie_data_frame <- rbind(movie_data_frame, (c(movie_data[2], 
                                movie_data[6], movie_data[8], movie_data[9])))

}
write.csv(movie_data_frame, "moviedata.csv",row.names = FALSE)


summary(movie_data_frame)
str(movie_data_frame)

