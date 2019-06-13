list.of.packages <- c("sp","rgdal","leaflet","data.table","ggplot2","scales","rgeos","maptools","openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/map-for-amy")
ke = readOGR("source_data/kenya/kenya.shp")

activities = read.xlsx("source_data/20190613 - Initial d-portal download.xlsx")
coordinates(activities)=~location_longitude+location_latitude

proj4string(activities) = proj4string(ke)
over_dat = over(activities,ke)
over_dat$commitment = activities$commitment
over_tab = data.table(over_dat)[,.(commitment=sum(commitment,na.rm=T)),by=.(ID_2)]

ke = merge(ke,over_tab,by="ID_2",all.x=T)

# Let the palette
pal <- colorBin(
  palette = "YlOrRd",
  domain = ke@data[,"commitment"],
  na.color="#d0cccf",
  bins = c(100000000,500000000,900000000,1500000000,2000000000,30000000000)
)

# You may need to highlight all lines at once to get this to run
leaflet(data=ke) %>%
  setView(37, 0.5, zoom=6) %>% 
  addPolygons(color = pal(ke@data[,"commitment"])
              ,fillOpacity = 1
              ,stroke=F
              ,smoothFactor=0.2
              ,popup=paste0(
                "<b>District name</b>: ",ke@data$NAME_2,"<br/>",
                "<b>Sum of commitments (USD): </b>$",format(round(ke@data$commitment),big.mark=","),"<br/>"
              )) %>%
  addPolylines(
    color="#eeeeee",
    weight=0.5,
    opacity=1,
    smoothFactor=0.2) %>%
  addMarkers(
    data=activities
    ,popup=paste0(
      "<b>Title: </b>",activities@data$title,"<br/>",
      "<b>Status: </b>",activities@data$status_code,"<br/>",
      "<b>Sector: </b>",activities@data$sector_code,"<br/>",
      "<b>Location precision: </b>",activities@data$location_precision,"<br/>",
      "<b>Location name: </b>",activities@data$location_name,"<br/>",
      "<b>D-portal link: </b><a href='",activities@data$aid,"'>Click here</a><br/>",
      "<b>Commitment (USD): </b>$",format(round(activities@data$commitment),big.mark=","),"<br/>"
      )
    ) %>%
  addLegend(
    "bottomright"
    , pal=pal
    , values = ke@data[,"commitment"]
    , opacity = 1
    , title="Sum of commitments (USD)"
    ,labFormat = labelFormat(prefix="$")
    ,na.label = "0/no data"
  )