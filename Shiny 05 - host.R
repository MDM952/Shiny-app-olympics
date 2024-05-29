#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

complete_medal_counts <- read.csv('complete_medal_counts.csv')
complete_medal_counts
medale_kolor <- c("Bronze"="#9D755d", "Silver"="#a8a8a8", "Gold"="#FECB52")
medale_nazwy <- c("Bronze"="Brąz", "Silver"="Srebro", "Gold"="Złoto")

s <- 10
m <- 12
l <- 15

# TYLKO FILTR NOC, x=YEAR, y=MEDAL 
### ============= UI ============= ### 
ui <- fluidPage(
  #Header
  h1("Liczba medalistów z danego kraju na przestrzeni lat"),
  fluidRow(
    column(6,
           selectInput(
             inputId = "NOC",
             label = "Wybierz kraj",
             choices = sort(unique(complete_medal_counts$NOC)),
             selected = "USA"
           )
    ),
    column(6,
           radioButtons(
             inputId = "Season",
             label = "Wybierz sezon",
             choices = unique(complete_medal_counts$Season),
             selected = "Summer"
           )
    )
  ),
  div(class = "plot-container",
      plotOutput("plot")
  )
)

### ============= SERVER ============= ### 
server <- function(input, output, session) {
  output$plot <- renderPlot({
    
    years <- if (input$Season == "Summer") c(1896, 2016) 
    else c(1924, 2014)
    ticks <- if (input$Season == "Summer") seq(1896, 2016, by = 4) 
    else c(seq(1924, 1992, by = 4), seq(1994, 2014, by = 4))
    
    complete_medal_counts %>%
      filter(NOC == input$NOC, Season == input$Season) %>%
      ggplot(aes(x=Year, y=Count, color=Medal)) +
      theme_minimal() + theme_light() +
      geom_line(size=0.8) +
      theme(text = element_text(family = "Courier New")) +
      labs(color = "Medal:", y = "Ilość medali", x = "Rok", title = "") +
      theme(
        axis.title.x = element_text(size=m, margin = margin(t = 20)),
        axis.title.y = element_text(size=m),
        axis.text.x = element_text(size=s),
        axis.text.y = element_text(size=s),
        legend.title = element_text(size=s),
        legend.text = element_text(size=s),
        legend.justification = c(0,0.5),
        plot.title = element_text(colour="Black", size=l, family="Courier New")) +
      scale_x_continuous(limits = years, breaks = ticks, minor_breaks=NULL) +
      scale_color_manual(values=medale_kolor, labels=medale_nazwy)
  })
}

shinyApp(ui, server)
