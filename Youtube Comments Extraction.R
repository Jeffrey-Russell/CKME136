#Extracting Youtube trailer comments for years 2012-2018

# Authorizing access to Youtube API
yt_oauth("420115523152-mn8sbgmsb08inh2k2ed1d6u854lre13b.apps.googleusercontent.com", 
         "rTD-JcREgSogKQyN1r6JeCnh", cache = FALSE)


#Importing Movie Id List
Movie_ids <- read.csv("DataSet/movie_ids.csv", stringsAsFactors = FALSE)

#Downloading Comments from Youtube Movie Trailers
for (i in 1:875) {
Comments <- NULL
Comments2 <- NULL
path <- NULL
Comments <- get_all_comments(video_id = Movie_ids[i,1])
Comments2 <- Comments[,c(5,7)]
path <- file.path("Youtube Comments Extraction/Comment Files", paste(Movie_ids[i,1],".csv", sep = ""))
write.csv(Comments2, path, row.names = FALSE)
}






















