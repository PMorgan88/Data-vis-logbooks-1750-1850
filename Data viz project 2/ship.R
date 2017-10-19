library(readr)                    #Libraries from GGplot, and kaggle examples
library(ggplot2)                  #General layout and idea for visualisation was gotten from a different examples. Code is copied with modifications.
library(mapproj)                  #https://www.kaggle.com/domcastro/wars-and-fights-draft/code, https://www.kaggle.com/dchudz/all-the-lat-lons-in-the-data/code, https://www.kaggle.com/poznyakovskiy/lat-lons-by-nationality/code
library(png)                      #basic examples of how to use buttons and a gui at https://shiny.rstudio.com/articles/action-buttons.html
library(shiny)

ui <- fluidPage(
  
  actionButton("am", "American"),
  actionButton("br", "British"),
  actionButton("dn", "Danish"),
  actionButton("du", "Dutch"),
  actionButton("fr", "French"),
  actionButton("hm", "Hamburg"),
  actionButton("sw", "Swedish"),
  actionButton("sp", "Spanish"),
  actionButton("all", "All"),
  mainPanel(
    plotOutput("plot", width = "100%")
  )
)

server <- function(input, output) {

setwd('c:/users/stubo/documents/data viz project 2')
data <- read_csv("CLIWOC15.csv")[,c("Lon3","Lat3","Nationality")]
british = data[data$Nationality=="British", ]
american = data[data$Nationality=="American", ]
danish = data[data$Nationality=="Danish", ]
dutch = data[data$Nationality=="Dutch", ]
french = data[data$Nationality=="French", ]
hamburg = data[data$Nationality=="Hamburg", ]
swedish = data[data$Nationality=="Swedish", ]
spanish = data[data$Nationality=="Spanish", ]



img <- readPNG("wind.png") 

world <- map_data ("world")


g = ggplot() +
  annotation_raster(img, xmin=-160, xmax=Inf, ymin=-55, ymax=Inf) +                          #https://en.wikipedia.org/wiki/Trade_winds
  geom_point(data = data, mapping = aes(Lon3,Lat3,color=Nationality), alpha=.45,size=.15) +
  geom_map(data=world, map=world,
           aes(long,lat, map_id=region),
           color="white", fill="grey20", size=0.05, alpha=1/4) +
  scale_y_continuous(expand = c(0,0), limits = c (-75,75)) +
  scale_x_continuous(expand = c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3))) +
  scale_colour_brewer(palette="Set1") +
  labs(title = "Log Book Entries by Nationality, 1750 - 1850", x = 'Longitude', y = 'Latitude')

am = ggplot() +
  geom_point(data = american, mapping = aes(Lon3,Lat3,color=Nationality), alpha=.45,size=3) +
  geom_map(data=world, map=world,
           aes(long,lat, map_id=region),
           color="white", fill="grey20", size=0.05, alpha=1/4) +
  scale_y_continuous(expand = c(0,0), limits = c (-75,75)) +
  scale_x_continuous(expand = c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  guides(colour = FALSE) +
  scale_colour_brewer(palette="Set1") +
  labs(title = "American Logbook Entries, 1750 - 1850", x = 'Longitude', y = 'Latitude')

br = ggplot() +
  geom_point(data = british, mapping = aes(Lon3,Lat3,color=Nationality), alpha=.10,size=3) +
  geom_map(data=world, map=world,
           aes(long,lat, map_id=region),
           color="white", fill="grey20", size=0.05, alpha=1/4) +
  scale_y_continuous(expand = c(0,0), limits = c (-75,75)) +
  scale_x_continuous(expand = c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  guides(colour = FALSE) +
  scale_colour_brewer(palette="Set1") +
  labs(title = "British Logbook Entries, 1750 - 1850", x = 'Longitude', y = 'Latitude')

dn = ggplot() +
  geom_point(data = danish, mapping = aes(Lon3,Lat3,color=Nationality), alpha=.45,size=3) +
  geom_map(data=world, map=world,
           aes(long,lat, map_id=region),
           color="white", fill="grey20", size=0.05, alpha=1/4) +
  scale_y_continuous(expand = c(0,0), limits = c (-75,75)) +
  scale_x_continuous(expand = c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  guides(colour = FALSE) +
  scale_colour_brewer(palette="Set1") +
  labs(title = "Danish Logbook Entries, 1750 - 1850", x = 'Longitude', y = 'Latitude')

sp = ggplot() +
  geom_point(data = spanish, mapping = aes(Lon3,Lat3,color=Nationality), alpha=.10,size=3) +
  geom_map(data=world, map=world,
           aes(long,lat, map_id=region),
           color="white", fill="grey20", size=0.05, alpha=1/4) +
  scale_y_continuous(expand = c(0,0), limits = c (-75,75)) +
  scale_x_continuous(expand = c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  guides(colour = FALSE) +
  scale_colour_brewer(palette="Set1") +
  labs(title = "Spanish Logbook Entries, 1750 - 1850", x = 'Longitude', y = 'Latitude')

du = ggplot() +
  geom_point(data = dutch, mapping = aes(Lon3,Lat3,color=Nationality), alpha=.10,size=3) +
  geom_map(data=world, map=world,
           aes(long,lat, map_id=region),
           color="white", fill="grey20", size=0.05, alpha=1/4) +
  scale_y_continuous(expand = c(0,0), limits = c (-75,75)) +
  scale_x_continuous(expand = c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  guides(colour = FALSE) +
  scale_colour_brewer(palette="Set1") +
  labs(title = "Dutch Logbook Entries, 1750 - 1850", x = 'Longitude', y = 'Latitude')

fr = ggplot() +
  geom_point(data = french, mapping = aes(Lon3,Lat3,color=Nationality), alpha=.45,size=3) +
  geom_map(data=world, map=world,
           aes(long,lat, map_id=region),
           color="white", fill="grey20", size=0.05, alpha=1/4) +
  scale_y_continuous(expand = c(0,0), limits = c (-75,75)) +
  scale_x_continuous(expand = c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  guides(colour = FALSE) +
  scale_colour_brewer(palette="Set1") +
  labs(title = "French Logbook Entries, 1750 - 1850", x = 'Longitude', y = 'Latitude')

hm = ggplot() +
  geom_point(data = hamburg, mapping = aes(Lon3,Lat3,color=Nationality), alpha=.45,size=3) +
  geom_map(data=world, map=world,
           aes(long,lat, map_id=region),
           color="white", fill="grey20", size=0.05, alpha=1/4) +
  scale_y_continuous(expand = c(0,0), limits = c (-75,75)) +
  scale_x_continuous(expand = c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  guides(colour = FALSE) +
  scale_colour_brewer(palette="Set1") +
  labs(title = "Hamburg Logbook Entries, 1750 - 1850", x = 'Longitude', y = 'Latitude')

sw = ggplot() +
  geom_point(data = swedish, mapping = aes(Lon3,Lat3,color=Nationality), alpha=.45,size=3) +
  geom_map(data=world, map=world,
           aes(long,lat, map_id=region),
           color="white", fill="grey20", size=0.05, alpha=1/4) +
  scale_y_continuous(expand = c(0,0), limits = c (-75,75)) +
  scale_x_continuous(expand = c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  guides(colour = FALSE) +
  scale_colour_brewer(palette="Set1") +
  labs(title = "Swedish Logbook Entries, 1750 - 1850", x = 'Longitude', y = 'Latitude')



  observeEvent(input$am, {
    output$plot = renderPlot({ am }, height=1080, width=1980)
  })
  observeEvent(input$br, {
    output$plot = renderPlot({ br }, height=1080, width=1980)
  })
  observeEvent(input$sw, {
    output$plot = renderPlot({ sw }, height=1080, width=1980)
  })
  observeEvent(input$dn, {
    output$plot = renderPlot({ dn }, height=1080, width=1980)
  })
  observeEvent(input$du, {
    output$plot = renderPlot({ du }, height=1080, width=1980)
  })
  observeEvent(input$fr, {
    output$plot = renderPlot({ fr }, height=1080, width=1980)
  })
  observeEvent(input$all, {
    output$plot = renderPlot({ g }, height=1080, width=1980)
  })
  observeEvent(input$hm, {
    output$plot = renderPlot({ hm }, height=1080, width=1980)
  })
  observeEvent(input$sp, {
    output$plot = renderPlot({ sp }, height=1080, width=1980)
  })
  
}

shinyApp(ui, server)