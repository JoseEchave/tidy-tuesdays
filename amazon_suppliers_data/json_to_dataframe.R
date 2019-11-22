#Load libraries
library(purrr) #Needed for iterations to get coordinates from list
library(readr) #To write csv file
library(jsonlite) #To read json file

#Read json file from where the data is provided to the map
amazon_json <- jsonlite::fromJSON("https://api.mapbox.com/datasets/v1/aboutamazon/ck05nwql608t12tmykhg4re4p/features?access_token=pk.eyJ1IjoiYWJvdXRhbWF6b24iLCJhIjoiY2p6MnFkZHRzMDNwYjNvbzUwZHRoZXc4NCJ9.o1G4hAqBdrylT32oYFOp7g")

#Get Properties from the JSON, is inside a list, but is already in data.frame form.
site_data <- amazon_json$features[["properties"]]
#Get geometry data, which comes as a list of vectors
geometry_data <- amazon_json$features[["geometry"]]$coordinates
#extract the first element of the vectors (X coordinate) by iterating the geometry data list
site_data$x <- map_dbl(geometry_data,~.x[1])
#do the same with the second element (Y coordinate
site_data$y <- map_dbl(geometry_data,~.x[2])

write_csv(site_data,"data/amazon_suppliers_info.csv")
