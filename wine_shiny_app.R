library(shiny)
library(tidyverse)
library(data.table)

wine_df <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", header = FALSE)
colnames(wine_df) <- c('Wine_Type', 'Alcohol','Malic_acid','Ash',
                       'Alcalinity_of_ash','Magnesium','Total_phenols', 'Flavanoids',
                       'Nonflavanoid_phenols', 'Proanthocyanins', 'Color_intensity', 'Hue',
                       'OD280_OD315_of_diluted_wines', 'Proline')

# Define UI
ui <- fluidPage(
  
  titlePanel("Wine Chemical Analysis"),
  
  sidebarPanel(
    
    selectInput("attr_1", "Attibute 1", c("Not Selected", names(wine_df))),
    selectInput("attr_2", "Attribute 2", c("Not Selected", names(wine_df))),
    br(),
    actionButton("run_button", "Run Analysis", icon = icon("play"))
  ),
  
  mainPanel(
    
    # Output: Tabset
    tabsetPanel(type = "tabs",
                tabPanel(title ="Plot", plotOutput("plot_1")),
                tabPanel(
                  title = "Summary",
                  
                  fluidRow(
                    strong(textOutput("attr_1_title")),
                    tableOutput("attr_1_summary_table")
                  ),
                  fluidRow(
                    strong(textOutput("attr_2_title")),
                    tableOutput("attr_2_summary_table")
                  ),
                  fluidRow(
                    strong(textOutput("attr_1_vs_attr_2_title")),
                    verbatimTextOutput("lm_summary")
                  )
                ),
                
                tabPanel("Data", dataTableOutput("dynamic"))
    )
  )
)

# Define functions
draw_plot <- function(wine_df, attr_1, attr_2){
  if(attr_1 != "Not Selected" & attr_2 != "Not Selected"){
    ggplot(data = wine_df,
           aes_string(x = attr_1, y = attr_2)) +
      geom_point()+
      geom_smooth(method = lm)
  }
  
  else if(attr_1 != "Not Selected" & attr_2 == "Not Selected"){
    ggplot(data = wine_df,
           aes_string(x = attr_1)) +
      geom_histogram()
  }
  else if(attr_1 == "Not Selected" & attr_2 != "Not Selected"){
    ggplot(data = wine_df,
           aes_string(x = attr_2)) +
      geom_histogram()
  }
}
create_attr_table <- function(data, attr){
  if(attr != "Not Selected"){
    attach(data)
    col <- get(attr)
    detach(data)
    statistic <- c("sample size", "sample mean", "sample standard deviation", "mean with 95% confidence interval")
    sample_n <- length(col)
    sample_mean <- round(mean(col),2)
    sample_sd <- round(sd(col),2)
    ttest <- t.test(col)
    lwr <- round(ttest$conf.int[1],2)
    upr <- round(ttest$conf.int[2],2)
    mean_with_95_percent_confidence_interval <- paste("[",toString(lwr),", ",toString(upr),"]",sep="")
    value <- c(sample_n, sample_mean, sample_sd, mean_with_95_percent_confidence_interval)
    data.table(statistic, value)
  }
}
create_lm_summary <- function(data, attr_1, attr_2){
  if(attr_1 != "Not Selected" & attr_2 != "Not Selected"){
    attach(data)
    attr_1 <- get(attr_1)
    attr_2 <- get(attr_2)
    detach(data)
    simple.fit <- lm(attr_1~attr_2, data=data)
    return(summary(simple.fit))
  }
}

#Define server
server <- function(input, output) {
  
  output$dynamic <- renderDataTable(wine_df)
  
  # Summaries
  
  attr_1 <- eventReactive(input$run_button,input$attr_1)
  attr_2 <- eventReactive(input$run_button,input$attr_2)
  
  attr_1_summary_table <- eventReactive(input$run_button,{
    create_attr_table(wine_df, attr_1())
  })
  attr_2_summary_table <- eventReactive(input$run_button,{
    create_attr_table(wine_df, attr_2())
  })
  lm_summary <- eventReactive(input$run_button,{
    create_lm_summary(wine_df, attr_1(), attr_2())
  })
  
  output$attr_1_title <- renderText(paste("Attribute 1:",attr_1()))
  output$attr_2_title <- renderText(paste("Attribute 2:",attr_2()))
  output$attr_1_vs_attr_2_title <- renderText(paste(attr_1(), "vs.",attr_2()))
  
  
  output$attr_1_summary_table <- renderTable(attr_1_summary_table(),colnames = FALSE)
  output$attr_2_summary_table <- renderTable(attr_2_summary_table(),colnames = FALSE)
  output$lm_summary <- renderPrint({lm_summary()})
# Plot
plot_1 <- eventReactive(input$run_button,{
  draw_plot(wine_df, attr_1(), attr_2())
})

output$plot_1 <- renderPlot(plot_1())

}

# Run the app ----
shinyApp(ui = ui, server = server)

