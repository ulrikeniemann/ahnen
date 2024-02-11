library(leaflet)
home <- leaflet() %>% addTiles() %>% #setView(11.39834, 53.59934, zoom = 17)
  #addCircleMarkers(lng = 11.39834, lat = 53.59934, popup = "Wolfgang")
  addMarkers(lng = 11.39834, lat = 53.59934, popup = "Wolfgang + Renate",
             label = "Euer Zuhause", 
             labelOptions = labelOptions(noHide = T, direction = "bottom")) |> 
  addMarkers(lng = 13.58124, lat = 52.68694, popup = "Robert + Ulrike",
             label = "Unser Zuhause", labelOptions = labelOptions(noHide = T, direction = "bottom"))

library(htmlwidgets)
saveWidget(home, file="home.html")
