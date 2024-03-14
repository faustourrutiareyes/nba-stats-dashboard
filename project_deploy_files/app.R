#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(plotly)

player_info <- read.csv("player_info.csv")
team_summ <- read.csv("team_summ.csv")

# Define UI for application that draws a histogram
ui <- navbarPage(
  "Project NBA",

  # First Page Starts Here
  tabPanel(
    "Seasonal Shooting Stats",


    # Application title
    titlePanel("Shooting Percentages for each NBA player in a given season"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        sliderInput("selected_season",
          "Season:",
          min = 1960,
          max = 2024,
          value = 2023,
          sep = ""
        ),
        selectizeInput("desired_players", paste("Desired Players:"),
          choices = NULL,
          multiple = TRUE
        ),
        p("Note: Players with two teams in a single season will show as separate dots (e.g Kyrie Irving played for the Brooklyn Nets and Dallas Mavericks in the 2023 Season, see it by selecting Kyrie Irving above.)"),
        p("Player dots will appear colored according to their team colors, to differentiate feel free to hover on the graph"),
        p("As well, players that were inactive in the selected season (i.e. retired or not yet drafted) will not appear, you can use the slider above to find their active seasons."),
        HTML("<br><br>"),
        checkboxInput("inside_outside", "Show player dots inside pack", TRUE)
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotlyOutput("distPlot")
      )
    )
  ),

  # Second Page UI starts here TabPanel()
  tabPanel(
    "Teams' Off-Def Rating Change",

    # Application title
    titlePanel("Teams' Off-Def Rating Change over the Seasons"),

    # Sidebar
    sidebarLayout(
      sidebarPanel(
        selectizeInput("selected_teams", paste("Desired Teams:"),
          choices = NULL,
          options = list(placeholder = "Select teams (e.g. Atlanta Hawks, Miami Heat)"),
          multiple = TRUE
        ),
        sliderInput("selected_seasons",
          "Season:",
          min = 1960,
          max = 2024,
          value = c(2002, 2023),
          sep = ""
        ),
        p("Offensive rating is the number of points produced by a team per 100 possessions"),
        p("Defensive rating is the number of points ALLOWED by a team per 100 possessions"),
        p("Therefore, a higher offensive rating is better, while a lower defensive rating is best. Teams would prefer to land in the upper-right corner."),
        h4("This graph shows how teams (and games overall) across the league have become higher scoring
                       \n (i.e. Teams are scoring more points, but at the same time allowing more points per possession.)"),
        p("Note: X marks the beginning rating; while O marks the final net ranking")
      ),



      # Show a plot of the generated distribution
      mainPanel(
        plotlyOutput("OffDef")
      )
    )
  ),

  # About section
  tabPanel(
    "About",
    p("This visualization belongs to faustourrutiareyes@gmail.com")
  ),
)

# SERVER STARTS HERE
server <- function(input, output, session) {
  #   TO be DO
  #   choices <- reactive({
  #     choices = unique(player_info[player_info$season == input$selected_season,]$player)
  #   })

  updateSelectizeInput(session, "desired_players", choices = unique(player_info$player), server = TRUE)
  updateSelectizeInput(session, "selected_teams", choices = unique(team_summ$team), server = TRUE)

  # Button to put player dots inside/outside
  inside <- reactive({
    if (input$inside_outside == TRUE) {
      inside <- 0
    }
  })

  # Main Plot for Page 1
  output$distPlot <- renderPlotly({
    inside <- inside()
    desired_players <- c(input$desired_players)

    players_plot <- plot_ly() %>%
      add_trace(
        data = player_info[player_info$season == input$selected_season, ],
        x = ~distance,
        y = ~sht_percentage,
        type = "box",
        boxpoints = "all",
        jitter = .8,
        pointpos = 0,
        text = ~player_all_info_text,
        pointpos = 0,
        marker = list(color = "rgb(0, 0, 0)", opacity = 0.05),
        line = list(color = "rgba(0,0,0,0)"),
        fillcolor = "rgba(0,0,0,0)",
        name = "Entire League"
      )

    for (player in desired_players) {
      players_plot <- players_plot %>% add_trace(
        data = player_info[(player_info$player == player) & (player_info$season == input$selected_season), ],
        type = "box",
        boxpoints = "all",
        jitter = 0,
        pointpos = inside,
        x = ~distance,
        y = ~sht_percentage,
        name = ~player_add_team_name,
        text = ~player_all_info_text,
        color = ~ I(color),
        marker = list(size = 10),
        line = list(color = "rgba(0,0,0,0)")
      )
    }

    players_plot <- players_plot %>%
      layout(
        xaxis = list(
          categoryorder = "array",
          categoryarray = c(
            "FTP% =",
            "2P% =",
            "3P% ="
          )
        ),
        title = list(text = paste(input$selected_season, "Season"))
      )
  })

  # Plot for Page 2
  output$OffDef <- renderPlotly({
    if (length(input$selected_teams) == 0) {
      selected_teams <- c("Miami Heat", "Atlanta Hawks")
    } else {
      selected_teams <- input$selected_teams
    }

    ggplotly(
      ggplot(data = team_summ[team_summ$season %in% input$selected_seasons &
        !team_summ$team %in% selected_teams, ]) +
        # Point data of non selected teams
        geom_point(
          data = team_summ[team_summ$season %in% input$selected_seasons[1] &
            !team_summ$team %in% selected_teams, ],
          aes(
            x = o_rtg,
            y = -d_rtg,
            group = team,
            text = paste(
              season, "Season Ratings for", team,
              "\nOffensive Rating:", o_rtg,
              "\nDefensive Rating:", d_rtg,
              "\nNet Rating:", round(o_rtg - d_rtg, 1)
            )
          ),
          size = 3,
          alpha = 0.1
        ) +
        # Same as above for alternate season
        geom_point(
          data = team_summ[team_summ$season %in% input$selected_seasons[2] &
            !team_summ$team %in% selected_teams, ],
          aes(
            x = o_rtg,
            y = -d_rtg,
            group = team,
            text = paste(
              season, "Season Ratings for", team,
              "\nOffensive Rating:", o_rtg,
              "\nDefensive Rating:", d_rtg,
              "\nNet Rating:", round(o_rtg - d_rtg, 1)
            )
          ),
          size = 3,
          alpha = 0.1
        ) +
        geom_line(
          aes(
            x = o_rtg,
            y = -d_rtg,
            group = team
          ),
          alpha = 0.1
        ) +
        labs(
          x = "Offensive Rating (-> higher is better)",
          y = "Defensive Rating (-> lower is better)"
        ) +

        # Add average for the beggining season
        geom_hline(yintercept = mean(team_summ[team_summ$season == input$selected_seasons[1], ]$d_rtg) * -1) +
        geom_vline(xintercept = mean(team_summ[team_summ$season == input$selected_seasons[1], ]$o_rtg)) +

        # Selected Teams
        geom_line(
          data = team_summ[team_summ$season %in% input$selected_seasons & team_summ$team %in% selected_teams, ],
          aes(
            x = o_rtg,
            y = -d_rtg,
            color = team
          )
        ) +
        geom_point(
          data = team_summ[team_summ$season %in% input$selected_seasons[2] &
            team_summ$team %in% selected_teams, ],
          aes(
            x = o_rtg,
            y = -d_rtg,
            color = team,
            text = paste(
              season, "Season Ratings for", team,
              "\nOffensive Rating:", o_rtg,
              "\nDefensive Rating:", d_rtg,
              "\nNet Rating:", round(o_rtg - d_rtg, 1)
            )
          ),
          size = 6,
          alpha = 1
        ) +
        # Lighter dot to signal movement
        geom_point(
          data = team_summ[team_summ$season %in% input$selected_seasons[1] &
            team_summ$team %in% selected_teams, ],
          aes(
            x = o_rtg,
            y = -d_rtg,
            color = team,
            text = paste(
              season, "Season Ratings for", team,
              "\nOffensive Rating:", o_rtg,
              "\nDefensive Rating:", d_rtg,
              "\nNet Rating:", round(o_rtg - d_rtg, 1)
            )
          ),
          size = 4,
          shape = 4
        )
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
