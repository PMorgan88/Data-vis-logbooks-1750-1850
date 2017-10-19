library(plotly)
library(dplyr)
library(tidyr)

data <- read_csv("CLIWOC15.csv")[,c("Lon3","Lat3","Nationality")]
#data<-data[!(data$Nationality =="British"),]
america = data[data$Nationality=="American", ]


# map projection


library(tidyr)
d <- data %>%
  gather(key, value, -id) %>%
  separate(key, c("l", "line"), "\\.") %>%
  spread(l, value)



geo <- list(
  showland = TRUE,
  showlakes = TRUE,
  showcountries = TRUE,
  showocean = TRUE,
  countrywidth = 0.5,
  landcolor = toRGB("grey90"),
  lakecolor = toRGB("white"),
  oceancolor = toRGB("white"),
  projection = list(
    type = 'orthographic',
    rotation = list(
      lon = -100,
      lat = 40,
      roll = 0
    )
  ),
  lonaxis = list(
    showgrid = TRUE,
    gridcolor = toRGB("gray40"),
    gridwidth = 0.5
  ),
  lataxis = list(
    showgrid = TRUE,
    gridcolor = toRGB("gray40"),
    gridwidth = 0.5
  )
)

p <- plot_geo(america, lat = ~Lat3, lon = ~Lon3) %>%
  add_markers(color = ~Nationality, opacity = .5) %>%
  layout(
    showlegend = FALSE, geo = geo,
    title = 'Contour lines over globe<br>(Click and drag to rotate)'
  )
p
