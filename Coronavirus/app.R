#Coronavirus tracking

# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(coronavirus)) install.packages("coronavirus", repos = "https://cran.r-project.org")

# import data
data("coronavirus")
head(coronavirus)

### DATA ###
coronavirus %>%
select(country, date,type, cases, lat, long) %>%
    filter(date == max(date), type == "confirmed") %>%
    group_by(country, type)

# function to plot new COVID cases by date
new_cases_plot = function(cv_aggregated, plot_date) {
    plot_df_new = subset(cv_aggregated, date<=plot_date)
    g1 = ggplot(plot_df_new, aes(x = date, y = cases, colour = country)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
        # geom_bar(position="stack", stat="identity") + 
        ylab("New cases (weekly)") + xlab("Date") + theme_bw() + 
        scale_colour_manual(values=c(covid_col)) +
        scale_y_continuous(labels = function(l) {trans = l / 1000000; paste0(trans, "M")}) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
              plot.margin = margin(5, 12, 5, 5))
    g1
}

basemap <- map_data("world")




### SHINY UI ###

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 tracker"),
    windowTitle = "COVID-19 tracker",
    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            helpText("Create demographic maps with 
               information from COVID-19 tracker."),
            
            #date range
            dateRangeInput("plot_date", label=h5("Select mapping date")),
            
            #slider
            sliderInput("range", 
                        label = "Number of countries to display:",
                        min = 1, value = 10, max = 50
        )
        ),
        mainPanel(
            leafletOutput("mymap", width="100%", height="100%"),
        )
        )
)



######## SHINY SERVER #####
# Define server logic required to draw a histogram

server = function(input, output, session) {
    
    output$mymap <- renderLeaflet({ 
        basemap
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
