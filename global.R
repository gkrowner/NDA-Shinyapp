# data preparation
source("setup.R")
load("top10s.RData")

# prepare plot theme
plot.theme <- theme(
  panel.background = element_rect(fill = "transparent"), 
  plot.background = element_rect(fill = "transparent", color = NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill = "transparent"), 
  legend.box.background = element_rect(fill = "transparent"),
  axis.title = element_text(colour = "ghostwhite"),
  axis.text = element_text(colour = "ghostwhite"),
  axis.ticks = element_line(colour = "ghostwhite"),
  axis.line = element_line(colour = "ghostwhite")
)

# Descriptive Statistics -------------------------------------------------------
c.danceability <- "Danceability: Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. 
\nA value of 0.0 is least danceable and 1.0 is most danceable."
c.acousticness <- "Acousticness: A measure from 0.0 to 1.0 of whether the track is acoustic."
c.energy <- "Energy: Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. 
\nTypically, energetic tracks feel fast, loud, and noisy."
c.liveness <- "Liveness: Detects the presence of an audience in the recording. 
\nHigher liveness values represent an increased probability that the track was performed live."
c.loudness <- "Loudness: The overall loudness of a track in decibels (dB). 
\nLoudness values are averaged across the entire track. Values typical range between -60 and 0 db."
c.speechiness <- "Speechiness: Speechiness detects the presence of spoken words in a track. 
\nThe more exclusively speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value."
c.tempo <- "Tempo: The overall estimated tempo of a track in beats per minute (BPM). 
\nIn musical terminology, tempo is the speed or pace of a given piece and derives directly from the average beat duration."
c.valence <- "Valence: A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. 
\nTracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry)."
c.duration <- "Duration: The duration of a track in second (s)."
c.popularity <- "Popularity: A measure from 0 to 100 of how popular the track is in Billboard ranking."

dt.top.artists <- dt.spotify
dt.top.artists[, Top_Artist := Artist]
top.artists <- head(dt.top.artists[, unique(Top_Artist)], 30)
dt.top.artists <- dt.top.artists[Artist %in% top.artists, ][, Number_of_Songs := .N, by = "Top_Artist"]
dt.top.artists[, Mean_Popularity := mean(Popularity), by = "Artist"]
dt.top.artists <- unique(dt.top.artists[, list(Top_Artist, Number_of_Songs, Mean_Popularity)])

# Network Exploration -------------------------------------------------------------
title.vertices <- dt.spotify[, list(name = unique(Title))]
genre.vertices <- dt.spotify[, list(name = unique(Genre))]
artist.vertices <- dt.spotify[, list(name = unique(Artist))]
pop.vertices <- dt.spotify[, list(name = unique(Popularity))]
val.vertices <- dt.spotify[, list(name = unique(Valence))]
# node title links genre
dt.title.genre <- dt.spotify[, list(Title, Genre)]
tg.vertices <- rbind(
  title.vertices[, list(name, type = FALSE, shape = "dot")], 
  genre.vertices[, list(name, type = TRUE, shape = "square")]
)
g.title.genre <- graph.data.frame(dt.title.genre, directed = F, vertices = tg.vertices)
dt.tg <- data.table(
  Statistic = c("Number of nodes", "Number of edges", "Clustering coefficient", "Diameter", "Average path length"),
  Value = c(length(V(g.title.genre)), length(E(g.title.genre)), transitivity(g.title.genre), 
            diameter(g.title.genre), average.path.length(g.title.genre))
)
dt.degree.tg <- as.data.table(degree(g.title.genre))
colnames(dt.degree.tg) <- "degree"
g.tg <- ggplot(dt.degree.tg) + geom_histogram(aes(degree), fill = "steelblue", colour = "ghostwhite") + plot.theme
g.proj.title.genre <- bipartite.projection(g.title.genre)$proj1
vis.proj.title.genre <- visIgraph(g.proj.title.genre)

# node artist links genre
dt.artist.genre <- dt.spotify[, list(Artist, Genre)]
ag.vertices <- rbind(
  artist.vertices[, list(name, type = FALSE, shape = "dot")], 
  genre.vertices[, list(name, type = TRUE, shape = "square")]
)
g.artist.genre <- graph.data.frame(dt.artist.genre, directed = F, vertices = ag.vertices)
dt.ag <- data.table(
  Statistic = c("Number of nodes", "Number of edges", "Clustering coefficient", "Diameter", "Average path length"),
  Value = c(length(V(g.artist.genre)), length(E(g.artist.genre)), transitivity(g.artist.genre), 
            diameter(g.artist.genre), average.path.length(g.artist.genre)))
dt.degree.ag <- as.data.table(degree(g.artist.genre))
colnames(dt.degree.ag) <- "degree"
g.ag <- ggplot(dt.degree.ag) + geom_histogram(aes(degree), fill = "steelblue", colour = "ghostwhite") + plot.theme

# node pop links genre
dt.pop.genre <- dt.spotify[, list(Popularity, Genre)]
pg.vertices <- rbind(
  pop.vertices[, list(name, type = FALSE, shape = "dot")], 
  genre.vertices[, list(name, type = TRUE, shape = "square")]
)
g.pop.genre <- graph.data.frame(dt.pop.genre, directed = F, vertices = pg.vertices)
dt.pg <- data.table(
  Statistic = c("Number of nodes", "Number of edges", "Clustering coefficient", "Diameter", "Average path length"),
  Value = c(length(V(g.pop.genre)), length(E(g.pop.genre)), transitivity(g.pop.genre), 
            diameter(g.pop.genre), average.path.length(g.pop.genre)))
dt.degree.pg <- as.data.table(degree(g.pop.genre))
colnames(dt.degree.pg) <- "degree"
g.pg <- ggplot(dt.degree.pg) + geom_histogram(aes(degree), fill = "steelblue", colour = "ghostwhite") + plot.theme

# node title links artist
dt.title.artist <- dt.spotify[, list(Title, Artist)]
ta.vertices <- rbind(
  title.vertices[, list(name, type = FALSE, shape = "dot")], 
  artist.vertices[, list(name, type = TRUE, shape = "square")]
)
g.title.artist <- graph.data.frame(dt.title.artist, directed = F, vertices = ta.vertices)
dt.ta <- data.table(
  Statistic = c("Number of nodes", "Number of edges", "Clustering coefficient", "Diameter", "Average path length"),
  Value = c(length(V(g.title.artist)), length(E(g.title.artist)), transitivity(g.title.artist),
            diameter(g.title.artist), average.path.length(g.title.artist)))
dt.degree.ta <- as.data.table(degree(g.title.artist))
colnames(dt.degree.ta) <- "degree"
g.ta <- ggplot(dt.degree.ta) + geom_histogram(aes(degree), fill = "steelblue", colour = "ghostwhite") + plot.theme

#node title links val
dt.title.val <- dt.spotify[, list(Title, Valence)]
tv.vertices <- rbind(
  title.vertices[, list(name, type = FALSE, shape = "dot")], 
  val.vertices[, list(name, type = TRUE, shape = "square")]
)
g.title.val <- graph.data.frame(dt.title.val, directed = F, vertices = tv.vertices)
dt.tv <- data.table(
  Statistic = c("Number of nodes", "Number of edges", "Clustering coefficient", "Diameter", "Average path length"),
  Value = c(length(V(g.title.val)), length(E(g.title.val)), transitivity(g.title.val), 
            diameter(g.title.val), average.path.length(g.title.val)))
dt.degree.tv <- as.data.table(degree(g.title.val))
colnames(dt.degree.tv) <- "degree"
g.tv <- ggplot(dt.degree.tv) + geom_histogram(aes(degree), fill = "steelblue", colour = "ghostwhite") + plot.theme

# Recommendation system -------------------------------------------------------
dt.radar <- dt.spotify[, c(7, 8, 11, 13, 14)]
bind <- as.data.frame(rbind(c(98, 97, 98, 99, 48), rep(0, 5)))
names(bind) <- c("Energy", "Danceability", "Valence", "Acousticness", "Speechiness")
dt.radar <- rbind(bind, dt.radar)
dt.spotify.artists <- dt.spotify[order(Artist)][, Artist]

# About Page
c.about.help <- p("Dear user,",
                  br(),br(),
                  "The purpose of this application is to explore the network of music.",
                  br(),br(),
                  "We created an application that provides you a deeper look into music trends of 
                  the past decade. This data set is retrieved from ",
                  tags$a("Kaggle", href= "https://www.kaggle.com/leonardopena/top-spotify-songs-from-20102019-by-year",
                         style = "color: turquoise; text-decoration:underline"),
                  " and contains top songs of the Billboard Spotify playlist from 2011 to 2019. ",
                  br(), br(),
                  "With this app, we hope to share knowledge in listening behaviour of Spotify users
                  and shared characteristics of the top songs. ",
                  br(), br(),
                  tags$li(
                    "The ",
                    span("main page", style = "color: turquoise;font-weight: bold"),
                    " gives an ", 
                    span("overview of the descriptive statistics", style = "color: turquoise"),
                    " of the data set using various visualizations methods. ",
                    span("Audio Features", style = "color: turquoise"), 
                    " allows you to look into patterns and trends of the top songs based on a variety
                    of features and different years. Moreover, you can explore the full data set that
                    is provided under the ",
                    span("Raw Data", style = "color: turquoise"), " tab."),
                  br(),
                  tags$li(
                    "In the second tab, you can take a look into several networks. The app uses an ",
                    span("interactive network", style = "color: turquoise;font-weight: bold"),
                    " visualizer and provides the statistics of the networks. Zoom in and find how your 
                    favourite songs are connected!"
                  ),
                  br(),
                  tags$li(
                    "Finally, the ",
                    span("recommendation system", style = "color: turquoise;font-weight: bold"),
                    " finds similar music based on the valence of the song and your preferred tempo. 
                    Choose your favourite top song and try out some new music! ",
                    br(), br(),
                    "Enjoy!", br(), br(),
                    "Deveny Cheung, Yide Han, Shu-Hsuan Lee, Ping Au Lin", 
                    br(),
                    "Group 14"
                  ))

c.about.features <- p("Spotify provides audio features for song analysis. Here are the audio features available in this app:",
                      br(), br(),
                      tags$li(
                        span("Danceability", style = "color: turquoise;font-weight: bold"),
                        ": Danceability describes how suitable a track is for dancing based on a combination 
                        of musical elements including tempo, rhythm stability, beat strength, and overall 
                        regularity. A value of 0.0 is least danceable and 1.0 is most danceable."
                      ),
                      br(),
                      tags$li(
                        span("Acousticness", style = "color: turquoise;font-weight: bold"),
                        ": A measure from 0.0 to 1.0 of whether the track is acoustic."
                      ),
                      br(),
                      tags$li(
                        span("Energy", style = "color: turquoise;font-weight: bold"),
                        ": Energy is a measure from 0.0 to 1.0 and represents a perceptual measure 
                        of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy."
                      ),
                      br(),
                      tags$li(
                        span("Liveness", style = "color: turquoise;font-weight: bold"), 
                        ": Detects the presence of an audience in the recording. Higher liveness values represent an increased probability that 
                        the track was performed live."
                      ),
                      br(),
                      tags$li(
                        span("Loudness", style = "color: turquoise;font-weight: bold"),
                        ": The overall loudness of a track in decibels (dB). Loudness values are averaged across the entire track. Values typical
                        range between -60 and 0 db."
                      ),
                      br(),
                      tags$li(
                        span("Speechiness", style = "color: turquoise;font-weight: bold"),
                        ": Speechiness detects the presence of spoken words in a track. The more exclusively speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value."
                      ),
                      br(),
                      tags$li(
                        span("Tempo", style = "color: turquoise;font-weight: bold"),
                        ": The overall estimated tempo of a track in beats per minute (BPM). In musical terminology, tempo is the speed or pace of a given piece and derives directly from the average beat duration."
                      ),
                      br(),
                      tags$li(
                        span("Valence", style = "color: turquoise;font-weight: bold"),
                        ": A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry)."
                      ),
                      br(),
                      tags$li(
                        span("Duration", style = "color: turquoise;font-weight: bold"),
                        ": The duration of a track in second (s)."
                      ),
                      br(),
                      tags$li(
                        span("Popularity", style = "color: turquoise;font-weight: bold"),
                        ": A measure from 0 to 100 of how popular the track is in Billboard ranking."
                      ),
                      br(),br()
                      )