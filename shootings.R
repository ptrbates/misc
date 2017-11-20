library(readr)
library(lubridate)
library(stringr)
library(forcats)
library(ggplot2)

# Download the csv and save it locally, then read the file into a data_frame
download.file("https://docs.google.com/spreadsheet/pub?key=0AswaDV9q95oZdG5fVGJTS25GQXhSTDFpZXE0RHhUdkE&output=csv", destfile = "shootings.csv")

shootings <- read_csv("shootings.csv", col_types = "cccccccccccccccccccccc")

# A function to remove non-digit characters from a vector so that it can be converted to numeric
fix_plus <- function(x) {
    as.numeric(str_replace_all(x, "[^0-9]", ""))
}

# Clean up the columns for analysis
shootings$Date <- mdy(shootings$Date)
shootings$Fatalities <- fix_plus(shootings$Fatalities)
shootings$Injured <- fix_plus(shootings$Injured)
shootings$`Total victims` <- fix_plus(shootings$`Total victims`)
shootings$`Prior signs of mental health issues` <- fct_collapse(shootings$`Prior signs of mental health issues`, Unknown = c("TBD", "Unclear", "unknown", "Unknown"))
shootings$`Weapons obtained legally` <- str_replace(shootings$`Weapons obtained legally`, ".*Yes.*", "Yes")
shootings$`Weapons obtained legally` <- fct_collapse(shootings$`Weapons obtained legally`, Unknown = c("TBD", "Unknown"), Yes = c("\nYes", "Yes"))
shootings$Race <- fct_collapse(shootings$Race, Black = c("black", "Black"), Unclear = c("unclear"), White = c("white", "White"))
shootings$Gender <- fct_collapse(shootings$Gender, Male = c("M", "Male"))
shootings$Type <- as_factor(shootings$Type)

# Some initial explorations
# Time Series of Fatalities
ggplot(shootings, aes(x = Date, y = Fatalities)) +
    geom_point() +
    scale_y_continuous(limits = c(0, 60))

# Draw Leaflet map (latitude and longitude information appears to be inaccurate)
library(leaflet)
shootings[,c("longitude", "latitude")] %>%
    leaflet() %>%
    addTiles() %>%
    addMarkers(popup = shootings$Case)
 