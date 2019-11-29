#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)
#install.packages("plotly")
library(plotly)
#install.packages("devtools")
require(devtools)

#install.packages("httr")
require(httr)

# Can be github, linkedin etc depending on application
oauth_endpoints("github")

# Change based on what you 
myapp <- oauth_app(appname = "Accessing_Github",
                   key = "31fe11f083b34c20ec8a",
                   secret = "ed1254a820a3fea515081ec6b4722917a3b3a927")
  
  x = jsonlite::fromJSON(jsonlite::toJSON(content(GET("https://api.github.com/users/jinzhu/followers?client_id=31fe11f083b34c20ec8a&client_secret=ed1254a820a3fea515081ec6b4722917a3b3a927"))))
  # Convert to a data.frame
gitDF = jsonlite::fromJSON(jsonlite::toJSON(x))

#vector of usernames
Id = x$login
UserIds = c(Id)

#Create empty vectors and data frame
AllUsers = c()

AllUsersDF = data.frame(
  Username = integer(),
  Following = integer(),
  Followers = integer(),
  Repositories = integer(),
  DateCreated = integer()
)

#Loop through usernames to find users to add to the list
for (i in 1:length(UserIds))
{
  #Retrieve users following list
  FollowingUrl1 = paste("https://api.github.com/users/", UserIds[i], "/following?client_id=31fe11f083b34c20ec8a&client_secret=ed1254a820a3fea515081ec6b4722917a3b3a927", sep = "")
  Following1 = GET(FollowingUrl1)
  FollowingContent1 = content(Following1)
  
  #Skip user if they don't have any followings
  if (length(FollowingContent1) == 0)
  {
    next
  }
  
  #Add followings to a dataframe and get usernames
  FollowingDF1 = jsonlite::fromJSON(jsonlite::toJSON(FollowingContent1))
  FollowingLogin = FollowingDF1$login
  
  #Loop through following
  for (j in 1:length(FollowingLogin))
  {
    #Check that the user is not already in
    if (is.element(FollowingLogin[j], AllUsers) == FALSE)
    {
      #Add user to list of users
      AllUsers[length(AllUsers) + 1] = FollowingLogin[j]
      
      #Get data on each user
      FollowingUrl2 = paste("https://api.github.com/users/", FollowingLogin[j], "?client_id=31fe11f083b34c20ec8a&client_secret=ed1254a820a3fea515081ec6b4722917a3b3a927", sep = "")
      Following2 = GET(FollowingUrl2)
      FollowingContent2 = content(Following2)
      FollowingDF2 = jsonlite::fromJSON(jsonlite::toJSON(FollowingContent2))
      
      #Each users following
      FollowingNumber = FollowingDF2$following
      
      #Each users followers
      FollowersNumber = FollowingDF2$followers
      
      #Each users number of repositories
      ReposNumber = FollowingDF2$public_repos
      
      #Year which each user joined Github
      YearCreated = substr(FollowingDF2$created_at, start = 1, stop = 4)
      
      #Add users data to dataframe
      AllUsersDF[nrow(AllUsersDF) + 1, ] = c(FollowingLogin[j], FollowingNumber, FollowersNumber, ReposNumber, YearCreated)
      
    }
    next
  }
  #Stop when there are more than 400 users
  if(length(AllUsers) > 400)
  {
    j = 9999999999999
  }
  next
}

AllUsersDF = unique(AllUsersDF)

#Produce the scatter plot of Followers vs Number of Repositories,
#colour coded by the data which they joined Github
MyPlot1 = plot_ly(data = AllUsersDF, x = ~Repositories, y = ~Followers, 
                  text = ~paste("Followers: ", Followers, "<br>Repositories: ", 
                                Repositories, "<br>Date Created:", DateCreated), color = ~DateCreated)
MyPlot1
#Upload the plot to Plotly
Sys.setenv("plotly_username" = "oconnr29")
Sys.setenv("plotly_api_key" = "gH0dbnJx7vobW20RxLQw")
api_create(MyPlot1, filename = "Followers vs Repositories by Date")
#PLOTLY LINK: https://plot.ly/~oconnr29/1

#Produce a scatter plot of Followers vs Following
MyPlot = plot_ly(data = AllUsersDF, x = ~Following, y = ~Followers, text = ~paste("Following: ", Following, 
                                                                                  "<br>Followers: ", Followers))
MyPlot

#Upload the plot to Plotly
Sys.setenv("plotly_username" = "oconnr29")
Sys.setenv("plotly_api_key" = "gH0dbnJx7vobW20RxLQw")
api_create(MyPlot, filename = "Following vs Followers")
#PLOTLY LINK: https://plot.ly/~oconnr29/5

#Sums of columns for the AllUsersDF dataframe
colSums(Filter(is.numeric, AllUsersDF))


#LANGUAGES
#The following code finds the most popular language for each user

#Create empty vector
Languages = c()

#Loop through all the users
for (i in 1:length(AllUsers))
{
  #Access each users repositories and save in a dataframe
  RepositoriesUrl1 = paste("https://api.github.com/users/", AllUsers[i], "/repos", "?client_id=31fe11f083b34c20ec8a&client_secret=ed1254a820a3fea515081ec6b4722917a3b3a927", sep = "")
  Repositories1 = GET(RepositoriesUrl1)
  RepositoriesContent1 = content(Repositories1)
  RepositoriesDF1 = jsonlite::fromJSON(jsonlite::toJSON(RepositoriesContent1))
  
  #Find names of all the repositories for the given user
  RepositoriesNames = RepositoriesDF1$name
  
  #Loop through all the repositories of an individual user
  for (j in 1: length(RepositoriesNames))
  {
    #Find all repositories and save in data frame
    RepositoriesUrl2 = paste("https://api.github.com/repos/", AllUsers[i], "/", RepositoriesNames[j], "?client_id=31fe11f083b34c20ec8a&client_secret=ed1254a820a3fea515081ec6b4722917a3b3a927", sep = "")
    Repositories2 = GET(RepositoriesUrl2)
    RepositoriesContent2 = content(Repositories2)
    RepositoriesDF2 = jsonlite::fromJSON(jsonlite::toJSON(RepositoriesContent2))
    
    #Find the language which each repository was written in
    Language = RepositoriesDF2$language
    
    #Skip a repository if it has no language
    if (length(Language) != 0 && Language != "<NA>")
    {
      #Add the languages to a list
      Languages[length(Languages)+1] = Language
    }
    next
  }
  next
}

#Save the top 20 languages in a table
LanguageTable = sort(table(Languages), increasing=TRUE)
LanguageTableTop20 = LanguageTable[(length(LanguageTable)-19):length(LanguageTable)]

#Save this table as a data frame
LanguageDF = as.data.frame(LanguageTableTop20)

#Plot the data frame of languages
MyPlot2 = plot_ly(data = LanguageDF, x = LanguageDF$Languages, y = LanguageDF$Freq, type = "bar")
MyPlot2

#Upload the plot to Plotly
Sys.setenv("plotly_username" = "oconnr29")
Sys.setenv("plotly_api_key" = "gH0dbnJx7vobW20RxLQw")
api_create(MyPlot2, filename = "20 Most Popular Languages")
#PLOTLY LINK: https://plot.ly/~oconnr29/3