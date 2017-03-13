# ########################################################################################
# Get data from web API
# 
# ########################################################################################


# ########################################################################################
# Import
# ########################################################################################

library(httr)


###########################################################################################
# Get data from website API

myapp=oauth_app("twitter",key="yourConsumerKey",secret="yourConsumerSecret")
sig=sign_oauth1.0(myapp,token="yourtoken",token_secret="yourtokenSecret")
homeTL=GET("https://api.twitter.com/1.1/statuses/home_timeline.json",sig)

#Read JSON
json1=content(homeTL)
json2=jsonlite::fromJSON(toJSON(json1))
json2[1,1:4]