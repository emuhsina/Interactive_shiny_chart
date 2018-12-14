#Load the packages
library(shiny)
library(tidyverse)
library(gapminder)
library(plotly)

#Load child mortality data
mortality<- read_csv("datasets/child_mortality.csv");

#Load GDP Per Capita data
income<- read_csv("datasets/income.csv")

#Load mean year in school data
education<- read_csv("datasets/education.csv")

#Load dataset for anemia in pregnent women
anemia<- read_csv("datasets/prg_anem.csv")


# Use gather to move the year dimension as a column
mortality <- gather(mortality, year, child_mortality, -country)
education <- gather(education, year, mean_year, -country)
income <- gather(income, year, gdp, -country)
anemia<- gather(anemia, year, prg_anem, -country)


#Merge education and mortality data as df
df <- merge(education, mortality, by=c("country","year"), 
            header =T)
#Filter out all missing values
df <- df%>% 
  filter(!is.na(child_mortality))
#Merge df with income and anemia
df <- merge(df, income, by=c("country","year"), header =T)
df <- merge(df, anemia, by=c("country", "year"), header= T)
#Extract continents from gapminder dataset
continent<- gapminder%>%
  group_by(continent, country)%>%
  distinct(country, continent)
continent<- data.frame(lapply(continent, as.character), stringsAsFactors= FALSE)
#Filter out all countries that do not exist in the continent table
df <- df %>% 
  filter(country %in% unique(continent$country))
# Add the continent column to finalize the data set
df <- merge(df, continent, by=c("country"), header =T)
#Change the datatype of year 
df$year <- as.numeric(df$year)


# Global variable
unique_continents <- unique(gapminder$continent)
# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Child Mortality"),
  # Sidebar panel
  sidebarLayout(
    # CheckboxGroup input
    sidebarPanel(
      checkboxGroupInput(
        "continents",
        "Continents:",
        choices = unique_continents,
        selected = unique_continents),
      
      # Set alpha level
      sliderInput(inputId = "alpha", 
                  label = "Alpha:", 
                  min = 0, max = 1, 
                  value = 0.5)
      
    ),
    # Outputs
    mainPanel(  
      tabsetPanel(type= "tabs",
                  tabPanel("GDP Per Capita",
                           plotlyOutput("gdp")),
                  tabPanel("Mean year in School(Female)",
                           plotlyOutput("education")),
                  tabPanel("Anemia in Pregnant women",
                           plotlyOutput("anemia"))
      )
    )
  )
)


# Define server function required to create the scatterplot
server <- function(input, output) {
  # reactive filtering
  reactive_df <- reactive(
    df %>%
      filter(continent %in% input$continents)
  )
  
  
  #Create the plot object for the first tab
  output$gdp <- renderPlotly({
    validate(
      need(input$continents, 'Check at least one continent!')
    )
    
    
    #Create the scatter plot
    reactive_df() %>% 
      plot_ly(x =~ child_mortality, y = ~gdp,
              type = "scatter", mode = "markers",
              color = ~continent, text = ~country, 
              alpha= input$alpha, frame = ~year, 
              hoverinfo = "text") %>%
      layout(yaxis = list(type = "log"))
  })
  
  
  #Create plot object for the second tab
  output$education <- renderPlotly({
    validate(
      need(input$continents, 'Check at least one continent!')
    )
    reactive_df() %>% 
      plot_ly(x =~child_mortality, y = ~mean_year,
              type = "scatter", mode = "markers",
              color = ~continent, text = ~country, 
              alpha= input$alpha,frame = ~year,
              hoverinfo = "text") %>%
      layout(xaxis = list(type = "log"))
  })
  
  
  #Create plot object for third tab
  output$anemia <- renderPlotly({
    validate(
      need(input$continents, 'Check at least one continent!')
    )
    reactive_df() %>% 
      plot_ly(x =~child_mortality, y = ~prg_anem,
              type = "scatter", mode = "markers",
              color = ~continent, text = ~country, 
              alpha= input$alpha,frame = ~year, 
              hoverinfo = "text") %>%
      layout(xaxis = list(type = "log"))
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
