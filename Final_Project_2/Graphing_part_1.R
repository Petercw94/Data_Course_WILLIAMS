##################################################
#   This script creates graphs for visualizing   #
#   the data                                     #
#                                                #
#   Author: Peter Williams                       #
#                                                #
##################################################

# Load the libraries ####
library(tidyverse)
library(gganimate)
library(shiny)

# Load the data set and set appropriate classes ####
stress = read.csv("./Stress_&_Yoga_clean_04_2019.csv", stringsAsFactors = TRUE)
stress$Reading = as.numeric(stress$Reading)
stress$Time = factor(stress$Time, ordered = TRUE, c("Pre_Stressor", "Stressor", "Post_Stressor"))
stress$Measurement = factor(stress$Measurement)
stress$Gender..1.male..2.female = factor(stress$Gender..1.male..2.female) 
stress$Yoga.Instruction.1.No..2.Yes = factor(stress$Yoga.Instruction.1.No..2.Yes )
stress$Yoga.Practice..1.never..2.sometimes..3.regularly = factor(stress$Yoga.Practice..1.never..2.sometimes..3.regularly)
stress$Subject.ID = factor(stress$Subject.ID)
stress = stress %>%
  filter(Measurement != "NA")

# Making a shiny app ####

# Build your User interface Page
ui <- fluidPage(
  pageWithSidebar(
    headerPanel("Stress and Yoga"), 
    sidebarPanel(
      selectInput(inputId = "dat",
                  label = "Choose your Data",
                  choices = levels(stress$Measurement),
                  selected = "HR"),
      selectInput(inputId = "x", 
                  label = "X-axis",
                  choices = names(stress), 
                  selected = "Time"),
      selectInput(inputId = "y", 
                  label = "Y-axis",
                  choices = names(stress),
                  selected = "Reading"),
      selectInput(inputId = "col",
                  label = "Color",
                  choices = names(stress),
                  selected = "Yoga.Practice..1.never..2.sometimes..3.regularly")
    ),
    mainPanel(
      plotOutput("plot")
    )))
# Build your server
server <- function(input, output, session) {
  # Combine the selected variable into a new data frame
  selectedData <- reactive({
    stress %>% 
      filter(Measurement == input$dat)
  })
  output$plot = renderPlot({
    ggplot(selectedData(), aes_string(x = input$x, y = input$y, color = input$col)) +
      geom_boxplot(scales = "free") + 
      theme(
        panel.background = element_rect(fill = "White"),
        panel.grid = element_line(colour = "Light Gray")
      )
  })
}
# Run the App
shinyApp(ui = ui, server = server)

# Making a plot of the data using gganimate ####
p1 = ggplot(stress, aes(x = Yoga.Practice..1.never..2.sometimes..3.regularly, 
                         y = Reading)) + 
  geom_boxplot(fill = "#2cccdd") + 
  scale_x_discrete(labels = c("Never", "Sometimes", "Regularly"), breaks = 1:3) +
  labs(
    x = "Yoga Practice",
    title = "Yoga's Effect on Heart Rate",
    subtitle = "{closest_state}"
  ) + facet_wrap(~Measurement, scales = "free") +
  theme_bw() +
  
  # Here is the gganimate code
  transition_states(
    Time,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')
p1

# Tukey Plot ####
plot(TukeyHSD(modHR))