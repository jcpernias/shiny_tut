library(shiny)
library(tidyverse)
library(magrittr)

library(ec1047)

regions <- list(
    "Andalucía" = "AND",
    "Aragón" = "ARA",
    "Asturias" = "AST",
    "Baleares" = "BAL",
    "Canarias" = "CNR",
    "Cantabria" = "CNT",
    "Castilla y León" = "CYL",
    "Castilla-La Mancha" = "CLM",
    "Cataluña" = "CAT",
    "Comunidad Valenciana" = "VAL",
    "Extremadura" = "EXT",
    "Galicia" = "GAL",
    "Madrid" = "MAD",
    "Murcia" = "MUR",
    "Navarra" = "NAV",
    "País Vasco" = "PVA",
    "La Rioja" = "RIO",
    "Ceuta" = "CEU",
    "Melilla" = "MEL"
)

lorenz_esp <- ecv2018 %$%
    lorenz(ydisp_cu, weight * people)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Curva de Lorenz"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("region",
                        "Comunidad autónoma:",
                        choices = regions,
                        selected = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("lorenzPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$lorenzPlot <- renderPlot({
        region_name <- names(regions)[which(regions == input$region)]
        lorenz_reg <- ecv2018 %>%
            filter(region == input$region) %$%
            lorenz(ydisp_cu, weight * people)

        plot_data <- bind_rows(add_column(lorenz_esp, region = "España"),
                               add_column(lorenz_reg, region = region_name))
        plot_data %>%
            ggplot(aes(x = x, y = y, colour = region)) +
            geom_line() +
            geom_line(data = tibble(x = c(0, 1), y = c(0, 1)), color = 'black') +
            coord_fixed() +
            scale_x_continuous(limits = c(0, 1), breaks = (0:5)/5) +
            scale_y_continuous(limits = c(0, 1), breaks = (0:5)/5) +
            theme_classic() +
            theme(axis.title.y = element_text(angle = 0),
                  panel.grid.major = element_line(color = "gray95"),
                  panel.grid.minor = element_line(color = "gray85", size = 0.1))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
