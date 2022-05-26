#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(rjson)
library(tidyr)
library(dplyr)
library(colorspace)
library(forcats)
library(ggforce)

result <- fromJSON(file = "../data/matchDetails.json")

subextractor <- function(x, id, date, duration) {
    return(
        data.frame(
            id = id,
            name = x$summonerName,
            champ = x$championName,
            lane = x$lane,
            kills = x$kills,
            win = x$win,
            assits = x$assists,
            deaths = x$deaths,
            team_id = x$teamId,
            duration = duration,
            date = as.Date(as.POSIXct(date / 1000, origin = "1970-01-01"))
        )
    )
}

extractor <- function(match) {

    # ignore bugged matches (no match details)
    if (length(match$info$participants) == 10) {
        duration <- match$info$gameDuration
        if (!is.null(match$info$gameEndTimestamp)) {
            duration <- match$info$gameEndTimestamp - match$info$gameStartTimestamp
        }


        intermediate_result <- lapply(match$info$participants, function(x) {
            subextractor(
                x,
                match$metadata$matchId,
                match$info$gameCreation,
                duration / (60000)
            )
        })

        return(
            do.call(
                rbind, intermediate_result
            )
        )
    }
}

extractor_result <- lapply(result, extractor)
match_data <- do.call(rbind, extractor_result)
sautax_rows <- subset(match_data, name %in% c("sautax"))
match_count <- nrow(sautax_rows)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Parties de sautax sur League of Legends"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("datesMerge",
                "Dates considérées",
                min = match_data$date[[nrow(match_data)]],
                max = match_data$date[[1]],
                value = c(
                    match_data$date[[nrow(match_data)]],
                    match_data$date[[1]]
                ),
                timeFormat = "%Y-%m-%d"
            ),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            navbarPage(
                "Représentations",
                navbarMenu(
                    "Graphiques",
                    "Barres",
                    tabPanel(
                        "Apparition des contacts",
                        plotOutput("contacts_plot")
                    ),
                    tabPanel(
                        "Répartition des champions",
                        plotOutput("champ_bar_plot")
                    ),
                    tabPanel(
                        "Taux de victoire par champion",
                        plotOutput("champ_wr_bar_plot")
                    ),
                    "----",
                    "Camembert",
                    tabPanel(
                        "Répartition des champions",
                        plotOutput("champ_pie_plot")
                    ),
                    "----",
                    "Boîte à moustaches",
                    tabPanel(
                        "Durée de la partie en fonction de la version majeure",
                        plotOutput("version_major_bar_plot")
                    ),
                    tabPanel(
                        "Durée de la partie en fonction de la version majeure et mineure",
                        plotOutput("version_minor_bar_plot")
                    ),
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$contacts_plot <- renderPlot({
        match_data_sub <- subset(
            match_data,
            match_data$date >= input$datesMerge[1] &
                match_data$date <= input$datesMerge[2]
        )

        without <- subset(
            match_data_sub,
            name != "sautax"
        )
        sub_match_count <- nrow(subset(match_data_sub, name == "sautax"))

        apparition <- table(without$name)

        apparition <- apparition[apparition > 3]

        out <- data.frame(
            name = names(apparition),
            count = as.vector(apparition)
        )

        ggplot(out, aes(
            y = fct_reorder(name, count),
            x = (count / sub_match_count) * 100,
            fill = name
        )) +
            theme_minimal() +
            labs(
                x = "Fréquence d’apparition", y = "Nom du joueur",
                title = "Pourcentage de parties jouées avec un contact"
            ) +
            theme(legend.position = "none") +
            geom_bar(stat = "identity") +
            geom_text(aes(
                label = paste(round((count / sub_match_count) * 100, 0),
                    "%",
                    sep = ""
                ),
            ), hjust = -0.02)
    })
}

# Run the application
shinyApp(ui = ui, server = server)