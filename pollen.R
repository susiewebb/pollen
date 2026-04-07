##Pollen.com data scrape

#Loading in the libraries
library(httr)
library(jsonlite)
library(purrr)
library(DatawRappr)
library(tidyverse)

#Loading API key, chart keys
api_key <- Sys.getenv("API_KEY")
pollen_chart <- Sys.getenv("POLLEN_KEY")

datawrapper_auth(api_key =  api_key, overwrite=TRUE)

#Create a session
session <- handle("https://www.pollen.com")


#Load data
page_resp <- GET(
  url = "https://www.pollen.com/map",
  handle = session,
  timeout(30)
)

stop_for_status(page_resp)

#Call the map API
api_resp <- GET(
  url = "https://www.pollen.com/api/map",
  add_headers(
    Accept = "application/json, text/javascript, */*; q=0.01",
    Referer = "https://www.pollen.com/map",
    `X-Requested-With` = "XMLHttpRequest"
  ),
  handle = session,
  timeout(30)
)

stop_for_status(api_resp)

#Parse the JSON data
data <- content(api_resp, as = "text", encoding = "UTF-8") |>
  fromJSON(simplifyVector = FALSE)
# Inspect structure
cat("Type:", class(data), "\n")

if (is.list(data) && !is.data.frame(data)) {
  cat("Top-level keys:", names(data)[1:min(20, length(names(data)))], "\n")
} else if (is.data.frame(data)) {
  cat("Number of records:", nrow(data), "\n")
  print(head(data, 1))
}

#Save as a CSV
rows <- lapply(names(data$Locations), function(code) {
  loc <- data$Locations[[code]]
  period1 <- loc$periods[[1]]
  
  data.frame(
    code = code,
    key = loc$key,
    City = loc$City,
    State = loc$State,
    DisplayLocation = loc$DisplayLocation,
    Period = period1$Period,
    Type = period1$Type,
    Index = period1$Index,
    stringsAsFactors = FALSE
  )
})

df <- do.call(rbind, rows) %>%
  mutate(level = case_when(
      Index >= 0   & Index < 2.4  ~ "Low",
      Index >= 2.4 & Index < 4.8  ~ "Medium-low",
      Index >= 4.8 & Index < 7.2  ~ "Medium",
      Index >= 7.2 & Index < 9.6  ~ "Medium-high",
      Index >= 9.6               ~ "High",
  ))

#Updating the datawrapper
today <- format(as.POSIXct(Sys.time(), tz = "America/New_York"), "%B %d")
today <- sub(" 0", " ", today)


#Editing the chart
dw_edit_chart(
  chart_id = pollen_chart,
  title = paste('Allergy levels as of', today)
)

dw_data_to_chart(df, chart_id = pollen_chart)

dw_publish_chart(pollen_chart)
