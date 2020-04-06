# server ------------------------------------------------------------------
server <- function(input, output, session) {
  # Descriptive Statistics ------------------------------------------------
  # Overview --------------------------------------------------------------
  output$topArtistsBtn <- renderUI({
    dropdownButton(
      tags$h4("List of Input"),
      selectInput(
        inputId = "topNumberArtists",
        label = "Top Artists",
        choices = seq(10, 50, by = 10),
        selected = 30,
        width = 400
      ),
      airYearpickerInput(
        inputId = "yearInput1",
        label = "Choose a year or a range",
        range = TRUE,
        value = c("2010-12-13", "2019-12-31"),
        minDate = "2010-12-31",
        maxDate = "2019-12-31",
        dateFormat = "yyyy",
        autoClose = TRUE,
        addon = "none",
        placeholder = "2010 - 2019",
        width = 400
      ),
      circle = TRUE, status = "danger", icon = icon("gear"), width = 250, size = "sm", inline = TRUE,
      tooltip = tooltipOptions(title = "Click to see inputs!")
    )
  })
  
  f.spotify.top.artists <- eventReactive({
    input$yearInput1
    input$topNumberArtists
  }, {
    dt.spotify.top.artists <- dt.spotify
    dt.spotify.top.artists[, Top_Artist := Artist]
    if (is.na(input$yearInput1[2])) {
      dt.spotify.top.artists <- dt.spotify.top.artists[Year == year(input$yearInput1[1]), ]
    } else {
      dt.spotify.top.artists <- dt.spotify.top.artists[
        Year >= year(input$yearInput1[1]) & Year <= year(input$yearInput1[2]), 
        ]
    }
    top.artists <- head(dt.spotify.top.artists[, unique(Top_Artist)], as.numeric(input$topNumberArtists))
    dt.spotify.top.artists <- dt.spotify.top.artists[Artist %in% top.artists, ][, Number_of_Songs := .N, by = "Top_Artist"]
    dt.spotify.top.artists[, Mean_Popularity := mean(Popularity), by = "Artist"]
    dt.spotify.top.artists <- unique(dt.spotify.top.artists[, list(Top_Artist, Number_of_Songs, Mean_Popularity)])
    dt.spotify.top.artists
  })
  
  
  
  output$topArtists <- renderPlotly({
    if (isTruthy(input$yearInput1) & isTruthy(input$topNumberArtists)) {
      ggplotly(
        ggplot(f.spotify.top.artists()) + 
          geom_point(aes(
            x = Number_of_Songs, 
            y = Mean_Popularity,
            size = Number_of_Songs, 
            color = Top_Artist
          )) +
          theme(
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
          ) +
          theme(legend.position = "none")
      )
    } else {
      ggplotly(
        ggplot(dt.top.artists) + 
          geom_point(aes(
            x = Number_of_Songs, 
            y = Mean_Popularity,
            size = Number_of_Songs, 
            color = Top_Artist
          )) +
          theme(
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
          ) +
          theme(legend.position = "none")
      )
    }
  })
  
  output$topArtistsSongs <- DT::renderDataTable(
    unique(dt.spotify[order(-Popularity)][Artist %in% f.spotify.top.artists()[, Top_Artist], list(Artist, Title)])
  )
  
  observeEvent(input$refreshGenreBttn, {session$reload()})
  
  output$topGenresBtn <- renderUI({
    dropdownButton(
      selectInput(
        inputId = "topNumberGenres",
        label = "Top Genres",
        choices = c(seq(10, 150, by = 10), "All"),
        selected = 30,
        width = 400
      ),
      airYearpickerInput(
        inputId = "yearInput2",
        label = "Choose a year or a range",
        value = c("2010-12-31", "2019-12-31"),
        range = TRUE,
        minDate = "2010-12-31",
        maxDate = "2019-12-31",
        dateFormat = "yyyy",
        autoClose = TRUE,
        addon = "none",
        placeholder = "2010 - 2019",
        width = 400
      ),
      actionBttn("refreshGenreBttn", "Refresh to adjust plot size", icon = icon("sync-alt"), size = "xs"),
      circle = TRUE, status = "danger", icon = icon("gear"), width = 250, size = "sm", inline = TRUE,
      tooltip = tooltipOptions(title = "Click to see inputs!")
    )
  })
  
  f.spotify.top.genres <- eventReactive({
    input$yearInput2
    input$topNumberGenres
  }, {
    dt.spotify.top.genres <- dt.spotify
    dt.spotify.top.genres[, Top_Genre := Genre]
    if (is.na(input$yearInput2[2])) {
      dt.spotify.top.genres <- dt.spotify.top.genres[Year == year(input$yearInput2[1]), ]
    } else {
      dt.spotify.top.genres <- dt.spotify.top.genres[
        Year >= year(input$yearInput2[1]) & Year <= year(input$yearInput2[2]), 
        ]
    }
    if (is.numeric(input$topNumberGenres)) {
      top.genres <- head(dt.spotify.top.genres[, unique(Top_Genre)], as.numeric(input$topNumberGenres))
      dt.spotify.top.genres <- dt.spotify.top.genres[Genre %in% top.genres, ][, .N, by = "Top_Genre"]
      dt.spotify.top.genres[, list(word = Top_Genre, freq = N)]
    } else {
      top.genres <- dt.spotify.top.genres[, unique(Top_Genre)]
      dt.spotify.top.genres <- dt.spotify.top.genres[Genre %in% top.genres, ][, .N, by = "Top_Genre"]
      dt.spotify.top.genres[, list(word = Top_Genre, freq = N)]
    }
  })
  
  output$topGenres <- renderWordcloud2({
    wordcloud2(f.spotify.top.genres(), color = "random-light", size = 5, backgroundColor = "transparent")
  })
  
  output$topGenresSongs <- DT::renderDataTable(
    unique(dt.spotify[order(-Popularity)][Genre %in% f.spotify.top.genres()[, word], list(Genre, Title)])
  )
  
  output$yearDistribution <- renderPlotly({
    ggplotly(
      ggplot(dt.spotify) + 
        geom_histogram(aes(Year), fill = "steelblue", colour = "ghostwhite", 
                       breaks = seq(2010.5, 2019.5, by = 1)) + 
        scale_x_continuous(breaks = seq(2010, 2019, by = 1)) + 
        theme(
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
    )
  })
  
  # Audio Features --------------------------------------------------------------
  f.feature.time <- eventReactive({
    input$yearInput3
    input$featureInput1
  }, {
    dt.spotify.feature.time <- dt.spotify
    if (is.na(input$yearInput3[2])) {
      dt.spotify.feature.time <- dt.spotify.feature.time[Year == year(input$yearInput3[1]), ]
    } else {
      dt.spotify.feature.time <- dt.spotify.feature.time[
        Year >= year(input$yearInput3[1]) & Year <= year(input$yearInput3[2]), 
        ]
    }
    dt.spotify.feature.time
  })
  output$summaries <- renderPrint(summary(f.feature.time()[, get(input$featureInput1)]))
  
  f.featureExplain <- reactive({
    switch(input$featureInput1,
           "Beats_Per_Min" = c.tempo,
           "Energy" = c.energy, 
           "Danceability" = c.danceability, 
           "Loudness" = c.loudness, 
           "Liveness" = c.liveness, 
           "Valence" = c.valence,
           "Duration" = c.duration, 
           "Acousticness" = c.acousticness, 
           "Speechiness" = c.speechiness, 
           "Popularity" = c.popularity)
  })
  
  output$featureExplain <- renderText(f.featureExplain())
  
  output$featureHists <- renderPlotly({
    ggplotly(
      ggplot() + 
        geom_histogram(aes((f.feature.time()[, get(input$featureInput1)])),
                       fill = "steelblue", colour = "ghostwhite") + 
        xlab(input$featureInput1) + 
        theme(
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
    )
  })
  
  f.spotify.feature.mean <- reactive({
    dt.spotify.feature.mean <- dt.spotify[, list(mean(get(input$featureInput3))), by = "Year"]
    colnames(dt.spotify.feature.mean)[2] <- paste0("Mean", "_", input$featureInput3)
    dt.spotify.feature.mean
  })
  
  output$featurePattern <- renderPlotly({
    dt.spotify.feature.mean <- f.spotify.feature.mean()
    ggplotly(
      ggplot(dt.spotify.feature.mean) + 
        geom_line(aes(
          x = Year, y = get(paste0("Mean", "_", input$featureInput3))
        ), colour = "steelblue") + 
        ylab(paste0("Mean", "_", input$featureInput3)) +
        geom_point(aes(
          x = Year, y = get(paste0("Mean", "_", input$featureInput3)), text = paste("Mean (Y-value): ", get(paste0("Mean", "_", input$featureInput3)), "\nYear: ", Year)
        ), colour = "ghostwhite") +
        scale_x_continuous(breaks = seq(2010, 2019, by = 1)) +
        theme(
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
        ), tooltip = "text"
    )
  })
  
  # Raw Data --------------------------------------------------------------
  output$rawData <- DT::renderDataTable(dt.spotify[, -1])
  
  
  # Network Exploration ---------------------------------------------------
  output$networkID1 <- renderText(paste("Current Network:", input$graph))
  
  output$networkID2 <- renderText(paste("Current Network:", input$graph))
  
  data <- eventReactive(input$go, {input$search.for.music})
  
  graphInput <- reactive({
    switch(input$graph,
           "Title and Genre" = g.title.genre, 
           "Artist and Genre" = g.artist.genre,
           "Popularity and Genre" = g.pop.genre,
           "Title and Artist" = g.title.artist,
           "Title and Valence" = g.title.val)
  })
  
  dataInput <- reactive({
    switch(input$graph,
           "Title and Genre" = dt.tg, 
           "Artist and Genre" = dt.ag,
           "Popularity and Genre" = dt.pg,
           "Title and Artist" = dt.ta,
           "Title and Valence" = dt.tv)
  })
  degreegraphInput <- reactive({
    switch(input$graph,
           "Title and Genre" = g.tg, 
           "Artist and Genre" = g.ag,
           "Popularity and Genre" = g.pg,
           "Title and Artist" = g.ta,
           "Title and Valence" = g.tv)
  })
  degreeInput <- reactive({
    switch(input$degree,
           "Degree" = degree(graphInput()), 
           "Closeness centrality" = closeness(graphInput()),
           "Betweenness centrality" = betweenness(graphInput()),
           "Eigenvector centrality" = evcent(graphInput())$vector
    )
  })
  
  output$plotNet <- renderVisNetwork({
    set.seed(1234)
    visIgraph(graphInput()) %>% visInteraction(navigationButtons = TRUE)
  })
  
  output$type <- renderTable({dataInput()})
  
  output$degrees <- renderPrint({summary(degreeInput())})
  
  output$plotdegree <- renderPlotly({degreegraphInput()})
  
  output$centralitiesTable <- DT::renderDataTable({
    dt.centralities <- as.data.table(cbind(V(graphInput())$name, 
                                           degree(graphInput()),
                                           closeness(graphInput()),
                                           betweenness(graphInput()),
                                           evcent(graphInput())$vector))
    colnames(dt.centralities) <- c(input$graph, 
                                   "Degree", 
                                   "Closeness Centrality", 
                                   "Betweenness Centrality", 
                                   "Eigenvector Centrality")
    dt.centralities[order(-Degree)]
    dt.centralities
  })
  
  output$projTG <- renderVisNetwork({
    set.seed(123)
    visIgraph(g.proj.title.genre) %>% visInteraction(navigationButtons = TRUE)
  })
  
  # Song Analysis --------------------------------------------------------- 
  observe({
    updateSelectInput(session, "songs",
                      label = "Choose a song of the artist:",
                      choices = dt.spotify[order(Title)][Artist == input$singer, Title]
    )
  })
  f.number <- reactive({
    dt.spotify[Title == input$songs, X][1]
  })
  output$radar <- renderPlot({
    par(col = "lightblue", font = 2)
    radarchart(dt.radar[c(1, 2, f.number())], pcol = rgb(0.4,0.8,0.4,1), pfcol = rgb(0.4,0.8,0.4,0.8), 
               plwd = 2, cglty = 1, cglcol = "grey")
  }, bg = "transparent"
  )
  output$var <- renderTable({dt.radar[c(f.number())]})
  
  # Music Recommendation ----------------------------------------------
  
  f.number2 <- reactive({
    dt.spotify[Title == input$songs, Valence][1]
  })
  
  f.high <- reactive({
    high <- as.numeric(input$slider) + 10})
  
  f.low <- reactive({
    low <- as.numeric(input$slider) - 10})
  
  output$musicTable <- renderTable({
    as.data.table(
      dt.spotify[which(
        (Valence == f.number2() | Valence == f.number2() + 1 | Valence == f.number2() - 1) & 
          Beats_Per_Min >= f.low() & Beats_Per_Min <= f.high()
      ), 2:3]
    )
  })
  
  output$songChoice <- renderText(input$songs)
  output$artistChoice <- renderText(dt.spotify[Title == input$songs, Artist][1])
  
  
  f.one <- reactive({
    dt.spotify[Title == input$songs, Beats_Per_Min]
  })
  
  output$beats <- renderText({
    paste0("<font size=1>The Beats per Minute for ", "<i>", input$songs[1], "<i>", " by ", "<i>", input$singer[1], "<i>", " is <B>", f.one()[1], "</B>.")
  })
}