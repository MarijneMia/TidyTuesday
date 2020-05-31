#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(kableExtra)

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')

cocktails <- data.frame(lapply(cocktails, function(v) {
    if (is.character(v)) return(tolower(v))
    else return(v)
}))

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("sandstone"), 
    # theme = "bootstrap.lux.css",

    includeCSS("styles.css"),
    
    headerPanel("Let me help you pick a drink!"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText("Select from the inputs below to find out."),
            
            selectInput("category_input",
                        label = "Choose your category",
                        choices = c("no preference", sort(unique(cocktails$category)))), 
            
            selectInput("glass_input",
                        label = "Choose your glass",
                        choices = c("no preference", sort(unique(cocktails$glass)))),
            
            selectInput("alcoholic_input",
                        label = "Choose your alcohol",
                        choices = c("no preference", sort(unique(cocktails$alcoholic)))),
            
            checkboxGroupInput("ingredient_input", 
                               label = "Choose your ingredients", 
                               choices = c("vodka", "gin", "light rum", "amaretto", "triple sec", "rum", "red wine"))
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tableOutput("cocktails_kable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$cocktails_kable <- function(){
        
        cocktails_tmp <- cocktails
        
        if(input$category_input %in% cocktails_tmp$category){
            cocktails_tmp <- cocktails_tmp %>% filter(category == !!input$category_input)
        }
        
        if(input$glass_input %in% cocktails_tmp$glass){
            cocktails_tmp <- cocktails_tmp %>% filter(glass == !!input$glass_input)
        }
        
        if(input$alcoholic_input %in% cocktails_tmp$alcoholic){
            cocktails_tmp <- cocktails_tmp %>% filter(alcoholic == !!input$alcoholic_input)
        }
        
        if(length(input$ingredient_input) > 0){
            cocktails_tmp_ingredient_sel <- cocktails_tmp %>% 
                group_by(drink) %>%
                filter(ingredient %in% input$ingredient_input) %>%
                pull(drink) %>% unique()
            
            cocktails_tmp <- cocktails_tmp %>%
                filter(drink %in% cocktails_tmp_ingredient_sel)
        }

        if(length(unique(cocktails_tmp$drink)) > 5){
            cocktails_tmp_sel <- unique(cocktails_tmp$drink) %>%
                sample(size = 5, replace = FALSE) %>% 
                sort()
        }else{
            cocktails_tmp_sel <- sort(unique(cocktails_tmp$drink))
        }
        
        cocktails_tmp %>% 
            arrange(drink, ingredient) %>%
            dplyr::select(drink, ingredient) %>% 
            unique() %>%
            filter(drink %in% cocktails_tmp_sel) %>%
            kable("html") %>%
            kable_styling(full_width = F) %>%
            collapse_rows(columns = 1, valign = "top")
        
    }
}

# Run the application 
shinyApp(ui = ui, server = server)
