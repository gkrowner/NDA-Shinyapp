# setup
library(readr)
library(data.table)
library(igraph)
library(ggplot2)
library(shiny)
library(rsconnect)
library(stringr)
library(shinyjs)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(devtools)
library(dashboardthemes)
library(visNetwork)
library(fmsb)
library(plotly)
library(wordcloud2)

# Preprocessing
#dt.spotify <- as.data.table(read.csv("top10s.csv")) # your repository
#dt.spotify[, 2:4] <- lapply(dt.spotify[, 2:4], as.character)
# to remove those character that can't be recognised
#dt.spotify[, title := str_replace_all(title,"�", "")]
#dt.spotify[, artist := str_replace_all(artist,"�", "é")]
#colnames(dt.spotify)[-1] <- c("Title", "Artist", "Genre", "Year", "Beats_Per_Min",
#                              "Energy", "Danceability", "Loudness", "Liveness", "Valence",
#                              "Duration", "Acousticness", "Speechiness", "Popularity")
#save(dt.spotify, file = "top10s.RData")
# for encoding problem, it's better to directly load the top10s.RData file