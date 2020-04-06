# ui --------------------------------------------------------------------------
ui <- dashboardPage(
  title = "Music Recommendation",
  
  # header --------------------------------------------------------------------
  dashboardHeader(
    title = tags$a(tags$div(img(src = "Spotify_Billboard_logo.png", height = 38))),
    titleWidth = 280
  ),
  
  # sidebar -------------------------------------------------------------------
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem(
        "Descriptive Statistics", 
        icon = icon("list-ol"),
        startExpanded = TRUE,
        menuSubItem("Overview", tabName = "overview", selected = TRUE, icon = icon("angle-right")),
        menuSubItem("Audio Features", tabName = "audioFeatures", icon = icon("angle-right")),
        menuSubItem("Raw Data", tabName = "rawData", icon = icon("angle-right"))
      ),
      br(),
      
      menuItem(
        "Network Exploration", 
        tabName = "netExplore", 
        icon = icon("search"),
        menuSubItem("Network Visualisation", tabName = "netVisual", icon = icon("angle-right")),
        menuSubItem("Network Statistics", tabName = "netStats", icon = icon("angle-right")),
        prettyRadioButtons(
          inputId = "graph", 
          label = "Choose a network:",
          status = "warning",
          choices = c("Title and Genre", "Artist and Genre", "Popularity and Genre", 
                      "Title and Artist","Title and Valence")
        )
      ),
      br(),
      
      menuItem(
        "Recommender System",
        tabName = "recSys",
        icon = icon("thumbs-up"),
        menuItem("Song Analysis & Recommendation", tabName = "songAnalysis", icon = icon("angle-right")),
        selectInput(
          inputId = "singer",
          label = "Choose an artist:",
          choices = dt.spotify.artists
        ),
        selectInput(
          inputId = "songs",
          label = "Choose a song:",
          choices = ""
        ),
        knobInput(
          inputId = "slider",
          label = "Choose your Tempo (Beats per Minute):",
          value = 120,
          min = 43,
          max = 206,
          displayPrevious = TRUE, 
          lineCap = "round",
          fgColor = rgb(0.4,0.8,0.4,0.8),
          inputColor = rgb(0.4,0.8,0.4,0.8)
        )
      ),
      br(),
      
      menuItem(
        "About",
        tabName = "about",
        icon = icon("info-circle"),
        menuSubItem("Help", tabName = "help", icon = icon("angle-right")),
        menuSubItem("Audio Feature", tabName = "audiofeature", icon = icon("angle-right"))
      )
    )
  ),
  
  
  
  # BODY --------------------------------------------------------------------
  
  dashboardBody(
    shinyDashboardThemes(theme = "purple_gradient"), 
    tabItems(
      
      tabItem(
        tabName = "overview", 
        fluidRow(
          valueBox(603, "Observations", icon = icon("list-ol"), color = "aqua", width = 4),
          valueBox(584, "Songs", icon = icon("music"), color = "olive", width = 4),
          valueBox(50, "Artists", icon = icon("users"), color = "yellow", width = 4)
        ),
        fluidRow(
          valueBox(184, "Genres", icon = icon("compact-disc"), color = "blue", width = 4),
          valueBox(10, "Audio Features", icon = icon("sliders-h"), color = "maroon", width = 4),
          valueBox(10, "Years", icon = icon("clock"), color = "purple", width = 4)
        ),
        fluidRow(
          box(
            title = p(
              uiOutput("topArtistsBtn", inline = TRUE), 
              str_dup(intToUtf8(160), 1),
              strong("Top Artists by Song Number")
            ),
            status = "info", 
            width = 6,
            column(
              width = 12,
              align = "center",
              plotlyOutput("topArtists", width = "98%")
            )
          ),
          box(
            title = p(
              uiOutput("topGenresBtn", inline = TRUE), 
              str_dup(intToUtf8(160), 1),
              strong("Top Genres by Song Number")
            ),
            status = "info", 
            width = 6,
            column(
              width = 12,
              align = "center",
              withSpinner(wordcloud2Output("topGenres"))
            )
          )
        ),
        fluidRow(
          box(
            title = strong("List of Top Artists & Songs"),
            collapsible = TRUE,
            collapsed = TRUE,
            DT::dataTableOutput("topArtistsSongs")
          ),
          box(
            title = strong("List of Top Genres & Songs"),
            collapsible = TRUE,
            collapsed = TRUE,
            DT::dataTableOutput("topGenresSongs")
          )
        ),
        fluidRow(
          box(
            title = strong("Distribution of Observations (Songs) by Year"), 
            status = "info", 
            width = 12,
            height = 570,
            column(
              width = 12,
              align = "center",
              withSpinner(plotlyOutput("yearDistribution", height = 500, width = "95%"))
            )
          )
        )
      ),
      
      tabItem(
        tabName = "audioFeatures",
        h2("Audio Features"),
        fluidRow(
          box(
            title = strong("Specify Audio Feature & Time Frame"),
            status = "warning",
            width = 6,
            height = 400,
            fluidRow(
              column(
                width = 6,
                selectInput(
                  inputId = "featureInput1",
                  label = "Audio feature",
                  choice = list("Beats_Per_Min",
                                "Energy", "Danceability", "Loudness", "Liveness", "Valence",
                                "Duration", "Acousticness", "Speechiness", "Popularity"),
                  width = 400
                )
              ),
              column(
                width = 6,
                airYearpickerInput(
                  inputId = "yearInput3",
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
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                h5(strong("Summary of audio feature")),
                verbatimTextOutput("summaries")
              )
            ),
            fluidRow(
              column(
                width = 12,
                h5(strong("Explanation")),
                verbatimTextOutput("featureExplain")
              )
            )
          ),
          box(
            title = strong("Distribution"),
            status = "primary",
            width = 6,
            height = 400,
            withSpinner(plotlyOutput("featureHists", height = 330))
          )
        ),
        fluidRow(
          box(
            title = strong("Pattern of Audio Feature over Years"), 
            status = "info", 
            width = 12,
            selectInput(
              inputId = "featureInput3",
              label = "Choose an audio feature",
              choice = c("Beats_Per_Min",
                         "Energy", "Danceability", "Loudness", "Liveness", "Valence",
                         "Duration", "Acousticness", "Speechiness", "Popularity"),
              width = 400
            ),
            withSpinner(plotlyOutput("featurePattern", width = "98%"))
          )
        )
      ),
      
      tabItem(
        tabName = "rawData",
        h2("Descriptive Statistics"),
        fluidRow(
          box(
            title = strong("Orignal Dataset"),
            status = "primary",
            width = 12,
            DT::dataTableOutput("rawData")
          )
        )
      ),
      
      # Network Exploration
      tabItem(
        tabName = "netVisual",
        h2("Network Visualisation"),
        fluidRow(
          tabBox(
            width = 12,
            height = 700,
            title = strong(textOutput("networkID1")),
            tabPanel(
              title = strong("Bipartite Network"),
              withSpinner(visNetworkOutput("plotNet", height = 630))
            ),
            tabPanel(
              title = strong("Projection Network (Titles based on Genres Only)"),
              withSpinner(visNetworkOutput("projTG", height = 630))
            )
          )
        )
      ),
      
      tabItem(
        tabName = "netStats",
        h2(textOutput("networkID2")),
        fluidRow(
          box(
            title = strong("Network Statistics"), 
            status = "info",
            width = 4,
            height = 250,
            column(width = 12, align = "center", tableOutput("type"))
          ),
          box(
            title = strong("Summary for Centrality"),
            status = "info",
            width = 6,
            height = 250,
            selectInput(
              inputId = "degree",
              label = "Choose a centrality measure:",
              choices = c("Degree", "Closeness centrality", 
                          "Betweenness centrality", "Eigenvector centrality")
            ),
            h5(strong("Results:")),
            verbatimTextOutput("degrees")
          )
        ),  
        fluidRow(
          box(title = strong("Degree Distribution"), status = "info", width = 10, withSpinner(plotlyOutput("plotdegree")))
        ),
        fluidRow(
          box(title = strong("Centrality Details"), status = "info", width = 12, DT::dataTableOutput("centralitiesTable"))
        )
      ),
      
      # Recommender System
      tabItem(
        tabName = "songAnalysis",
        h2("Song Analysis & Recommendation"),
        fluidRow(
          box(
            title = span("Deep Down in Your Favourite", style = "font-size: 18px; font-weight: bold"),
            status = "primary",
            width = 6,
            column(
              width = 12,
              align = "center",
              h4(strong(textOutput("songChoice"))),
              h4(textOutput("artistChoice")),
              withSpinner(plotOutput("radar", height = 480)),
              tableOutput("var")
            )
          ),
          box(
            title = span("Keep up Your Tempo!", style = "font-size: 18px; font-weight: bold"),
            status = "primary",
            width = 6, 
            span("Your Recommendations:", style = "font-size: 16px; font-weight: bold"),
            column(
              width = 12, align = "center", 
              tableOutput("musicTable")
            ),
            htmlOutput("beats")
          )
        )
      ),
      
      # About -----------------------------------------------------------------
      tabItem(
        tabName = "help",
        h2("About"),
        fluidRow(
          box(
            width = 12,
            c.about.help
          )
        )
      ),
      
      tabItem(
        tabName = "audiofeature",
        h2("About Audio Features"),
        fluidRow(
          box(
            width = 11,
            c.about.features
          )
        )
      )
    )
  )
)