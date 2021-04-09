library(shiny)

# Define UI for miles per gallon app ----
ui <- fluidPage(
  # App title ----
  titlePanel("NFL 2019 Team Predicted Scores"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Selector for variable to plot against mpg ----
      selectInput("variable", "Variable:",
                  c("Arizona" = "ARI",
                    "Atlanta" = "ATL",
                    "Baltimore" = "BAL",
                    "Buffalo" = "BUF",
                    "Carolina" = "CAR",
                    "Chicago" = "CHI",
                    "Cincinnati" = "CIN",
                    "Cleveland" = "CLE",
                    "Dallas" = "DAL",
                    "Denver" = "DEN",
                    "Detroit" = "DET",
                    "Green Bay" = "GB",
                    "Houston" = "HOU",
                    "Indianapolis" = "IND",
                    "Jacksonville" = "JAX",
                    "Kansas City" = "KC",
                    "L.A. Chargers" = "LAC",
                    "L.A. Rams" = "LAR",
                    "Las Vegas" = "LV",
                    "Miami" = "MIA",
                    "Minnesota" = "MIN",
                    "N.Y. Giants" = "NYG",
                    "N.Y. Jets" = "NYJ",
                    "New England" = "NE",
                    "New Orleans" = "NO",
                    "Philadelphia" = "PHI",
                    "Pittsburgh" = "PIT",
                    "San Francisco" = "SF",
                    "Seattle" = "SEA",
                    "Tampa Bay" = "TB",
                    "Tennessee" = "TEN",
                    "Washington" = "WAS")),
      # Input: Checkbox for whether outliers should be included ----
      #checkboxInput("outliers", "Show outliers", TRUE)
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      # Output: Plot of the requested variable against mpg ----
      plotOutput("mpgPlot")
    )
  )
)

# Data pre-processing ----
# Tweak the "am" variable to have nicer factor labels -- since this
# doesn't rely on any user inputs, we can do this once at startup
# and then use the value throughout the lifetime of the app
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste("mpg ~", input$variable)
  })
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  output$mpgPlot <- renderPlot({
    boxplot(as.formula(formulaText()),
            data = mpgData,
            outline = input$outliers,
            col = "#75AADB", pch = 19)
  })
}

#Create and Run the Shiny App
shinyApp(ui, server)
runApp("~/Desktop/Projects/NFL-Projects/shinyapp/app.R")

