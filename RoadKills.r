options( warn = -1, quarto.hunk.opts = list( context = 2, color = TRUE, collapse = TRUE, style = "github" ) )

library(pxR) library(tidyverse) library(BFS) library(janitor) library(plotly)

library(readr) RoadTrafficAccidentLocations <- read_csv("data/RoadTrafficAccidentLocations.csv", col_types = cols(AccidentType_de = col_skip(), AccidentType_fr = col_skip(), AccidentType_it = col_skip(), AccidentSeverityCategory_de = col_skip(), AccidentSeverityCategory_fr = col_skip(), AccidentSeverityCategory_it = col_skip(), AccidentSeverityCategory_en = col_character(), RoadType_de = col_skip(), RoadType_fr = col_skip(), RoadType_it = col_skip(), AccidentMonth_de = col_skip(), AccidentMonth_fr = col_skip(), AccidentMonth_it = col_skip(), AccidentWeekDay_de = col_skip(), AccidentWeekDay_fr = col_skip(), AccidentWeekDay_it = col_skip()), na = "NA")

library(rgdal)

myConvert_LV95_to_WGS84 = function(df) { coordinates(df) = ~ long + lat df@proj4string = CRS("+init=epsg:2056") myCoords_WGS84 = spTransform(df, CRS("+init=epsg:4326")) myCoords_WGS84@coords }

WGS84 <- RoadTrafficAccidentLocations |> select(long=AccidentLocation_CHLV95_E, lat=AccidentLocation_CHLV95_N) |> myConvert_LV95_to_WGS84()

RoadTrafficAccidentLocations |> select(-AccidentLocation_CHLV95_E, -AccidentLocation_CHLV95_N) |> bind_cols(WGS84) |> write_csv("data/RoadTrafficAccidentLocations_GPS.csv")

RoadTrafficAccidentLocations_GPS <- readr::read_csv("data/RoadTrafficAccidentLocations_GPS.csv")

df_chx_chy <- data.frame(chx=c(2500845, 675500), chy=c(1117371, 271500))

test <- df_chx_chy

coords_LV95 = matrix( c(2500845,1117371, 2726148,1202001, 2726148,1201995), nrow=3, ncol=2, byrow = TRUE )

library(leaflet)

data <- RoadTrafficAccidentLocations_GPS

geneva_lat <- 46.2044 geneva_long <- 6.1432

range <- 0.1

filtered_data <- RoadTrafficAccidentLocations_GPS %>% filter(between(lat, geneva_lat - range, geneva_lat + range) & between(long, geneva_long - range, geneva_long + range))

filtered_data |> as.data.frame() |> filter(AccidentYear == 2022) |> leaflet() %>% addTiles() %>% addMarkers(long, ~lat, label=as.character(AccidentType))

RoadTrafficAccidentLocations_GPS |> as.data.frame() |> filter(AccidentYear == 2022) |> ggplot(aes(x=long, y=lat)) + geom_point() + coord_quickmap(xlim = c(6.1432 - 0.1, 6.1432 + 0.1), ylim = c(46.2044 - 0.1, 46.2044 + 0.1)) + theme_bw() + labs(x="Longitude", y="Latitude") + ggtitle("Road Traffic Accident Locations") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(axis.text.y = element_text(angle = 0, hjust = 1)) + theme(axis.title.x = element_text(size = 12)) + theme(axis.title.y = element_text(size = 12)) + theme(axis.text = element_text(size = 10)) + theme(axis.title = element_text(size = 12)) + theme(plot.title = element_text(size = 14)) + theme(legend.title = element_text(size = 12)) + theme(legend.text = element_text(size = 10)) + theme(legend.position = "none") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.border = element_blank()) + theme(panel.background = element_blank()) + theme(plot.background = element_blank()) + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) + theme(legend.key = element_blank()) + theme(legend.key.size = unit(0.5, "cm")) + theme(legend.key.height = unit(0.5, "cm")) + theme(legend.key.width = unit(0.5, "cm")) + theme(legend.key.height = unit(0.5, "cm")) + theme(legend.key.width = unit(0.5, "cm")) + theme(legend.key.size = unit(0.5, "cm")) + theme(legend.key.size = unit(0.5, "cm"))

data_accidents_pers_acc <- pxR::read.px(filename = "pers_accidents.px") |> pxR::as.data.frame.px(language="fr") |> janitor::clean_names()

pers_accidents_clean <- data_accidents_pers_acc |> mutate(type_usager = case_when( moyen_de_transport_utilise == "Vélo électrique rapide" ~ "Vélo", moyen_de_transport_utilise == "Vélo électrique lent" ~ "Vélo", moyen_de_transport_utilise == "Cycle" ~ "Vélo", TRUE ~ moyen_de_transport_utilise ) ) |> mutate(type_usager = case_when( moyen_de_transport_utilise == "Aucun (piéton)" ~ "Pieton + EAV", moyen_de_transport_utilise == "Engin assimilé à un véhicule (EAV)" ~ "Pieton + EAV", TRUE ~ type_usager ) ) |> mutate(type_usager = case_when( moyen_de_transport_utilise == "Motocycle" ~ "Motocycle", moyen_de_transport_utilise == "Cyclomoteur" ~ "Motocycle", TRUE ~ type_usager ) ) |> mutate(type_usager = case_when( moyen_de_transport_utilise == "Voiture de tourisme" ~ "Voiture", moyen_de_transport_utilise == "Véhicule de transport de personnes" ~ "

