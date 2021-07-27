library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

project_data  <- read.csv("GFDDData.csv")
project_data  <- project_data [-c(5:49)]
project_data  <- subset(project_data , select = -c(X))
project_data ["Score"] <- rowSums(is.na(project_data ) | project_data  == "")

final_data <- filter(project_data , Score == 0)
colnames(final_data) <- c("Country Name", "Country Code", "Indicator Name", "Indicator Code", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "Score")

table(final_data$`Country Name`)

final_data ["Sum_Value"] <- rowSums(final_data[,5:17])
final_data$Sum_Value=log(final_data$Sum_Value)
final_data <- filter(final_data , Sum_Value != '-Inf')
final_data <- filter(final_data , Sum_Value != 'NaN')

#gives frequency table of indicators per country
freqtable <- data.frame(table(final_data$`Country Name`))
colnames(freqtable) <- c("Country", "Freq")

#hdi dataset
hdi <- read.csv("hdi.csv")
hdi <- hdi[-c(3:29)]
hdi <- hdi[-c(4)]
hdi <- hdi[-c(190:212),]
hdi <- data.frame(lapply(hdi, as.character), stringsAsFactors=FALSE)
hdi$Country[46] <- "Cote d'Ivoire"
colnames(hdi) <- c("HDI Rank", "Country", "HDI")
hdi <- hdi[-c(1)]

#gdp-population dataset
gdp <- read.csv("gdp-pop.csv")
gdp["Score"] <- rowSums(is.na(gdp) | gdp == "")
gdp <- filter(gdp, Score == 0)
gdp <- filter(gdp, Year == 2017)
gdp <- subset(gdp, select = -c(Score))
gdp <- data.frame(lapply(gdp, as.character), stringsAsFactors=FALSE)
colnames(gdp) <- c("Country", "Code", "Year", "Population Density", "GDP per capita", "Total Population")
gdp <- gdp[-c(2:4)]

#merged dataframe for 1st layer
gh <- merge(gdp, hdi, by = "Country")
ghf <- merge(gh, freqtable, by = "Country")
ghf <- filter(ghf, Freq > 0)
ghf$`GDP per capita` <- as.numeric(ghf$`GDP per capita`)
ghf$`Total Population`<- as.numeric(ghf$`Total Population`)
ghf$HDI <- as.numeric(ghf$HDI)
colnames(ghf) <- c("Country", "GDP Per Capita", "Total Population", "HDI", "No. of Indicators")
Country_list <- ghf$Country

ui <- dashboardPage(
    dashboardHeader(title = "World Development Indicators", titleWidth = 300),
    dashboardSidebar(
        selectInput("country", "Select a country", choices= Country_list),
        selectInput("IndicatorValue", "Select an Indicator", "placeholder" ),
        collapsed = TRUE
    ),
    dashboardBody(
        fluidRow(
            box(collapsible = TRUE, title = "World Developmenmt Index Plot", status = "primary", solidHeader = TRUE, width = NULL, collapsed = TRUE,
                plotlyOutput("plot1", height = "500px")),
            box(collapsible = TRUE, title = "Development Indicators Details by Country", status = "primary", solidHeader = TRUE, width = NULL, collapsed = TRUE,
                plotlyOutput("plot2", height = "700px")),
            box(collapsible = TRUE, title = "Year wise data for Indicator", status = "primary", solidHeader = TRUE, width = NULL, collapsed = TRUE,
                plotOutput("plot3", height = "700px")),
        ),
    )
)

server <- function(input, output, session) {
    output$plot1 <- renderPlotly({
        bubbleplot <- plot_ly(ghf, x = ~ghf$HDI, y = ~ghf$`GDP Per Capita`, type = 'scatter',
                              color = ~HDI, colors = 'Reds', mode = 'markers', 
                              size = ~ghf$`Total Population`,sizes = c(10, 50),
                              marker = list(opacity = 0.5, sizemode = 'diameter'),
                              hoverinfo = 'text',
                              text = ~paste('Country:', ghf$Country, 
                                            '<br>GDP per Capita :', ghf$`GDP Per Capita`,
                                            '<br>Population :', ghf$`Total Population`,
                                            '<br>HDI :', ghf$HDI,
                                            '<br>No. of Indicator :', ghf$`No. of Indicator`)
        ) %>% layout(xaxis = list(title = "Human Development Index"), yaxis = list(title = "GDP Per Capita"), legend = list(title = list(text='<b> HDI </b>')))
        print(bubbleplot)
    })
    
    output$plot2 <- renderPlotly({
        filtered_data1 <- filter(final_data, final_data$`Country Name` == input$country)
        barplot <- ggplot(filtered_data1, aes(y=filtered_data1$Sum_Value, x=filtered_data1$`Indicator Name`))+ 
            geom_bar(position="dodge", stat="identity", fill='#D81815')+
            coord_flip()+
            xlab("Indicator Names")+ 
            ylab("Sum of Values of Years")
        
        barplot <- ggplotly(barplot)
        print(barplot)
    })
    
    observe({
        filtered_data2 <- filter(final_data, final_data$`Country Name` == input$country)
        updateSelectInput(session, "IndicatorValue", choices = filtered_data2$`Indicator Name`)
    })
    
    output$plot3 <- renderPlot({
        filtered_data3 <- filter(final_data, final_data$`Country Name` == input$country)
        filtered_data4 <- filter(filtered_data3, filtered_data3$`Indicator Name` == input$IndicatorValue)
        modified_data <- tidyr::gather(data=filtered_data4, key='period', value='obsValue', c(5:17))
        lineplot <- ggplot(modified_data, aes(x=period, y=obsValue, group = 1))+
            geom_point(color = '#FFC300', size = 5)+
            geom_line(color = '#0733B3', size = 0.8)+
            xlab("Years")+ 
            ylab("Values per Year")
        print(lineplot)
    })
}

shinyApp(ui, server)