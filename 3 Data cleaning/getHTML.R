# ########################################################################################
# Get data from HTML
# 
# ########################################################################################


# ########################################################################################
# Import
# ########################################################################################

library(XML)
library(httr)
#r-blogger web scrapping

###########################################################################################
# Get data from website

#Get HTML string
htmlConnect = url("http://scholar.google.com/citations?user=HI-I60AAAAJ&hl=en")
htmlCode = readlines(htmlConnect)
close(htmlConnect)

#Get HTML as XML
url="http://scholar.google.com/citations?user=HI-I60AAAAJ&hl=en"
html=htmlTreeParse(url,useInternalNodes = TRUE)
xpathSApply(html,"//title",xmlValue)
xpathSApply(html,"//td[@id='col-citedby']",xmlValue)

#Get HTML with httr/XML when authentication is needed
url="http://scholar.google.com/citations?user=HI-I60AAAAJ&hl=en"
html=GET(url,authenticate("user","password"))
htmlAsText=content(html,as="text")
parsedHtml=htmlParse(htmlAsText,asText = TRUE)
xpathSApply(parsedHtml,"//title",xmlValue)

google=handle("http:google.com")
html=GET(handle=google,path="/")



