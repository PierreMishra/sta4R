library(httr)
library(jsonlite)

api <- "http://sta-demo.internetofwater.dev/api/v1.1"

# get things (by iot_id & name)
get_things <- function(api, iot_id = NULL, name = NULL) {
  
  if (is.null(iot_id) & is.null(name)){
    x <- GET(url = paste0(api, "/Things"))
    df <- fromJSON(as.character(x))
    return (df)
  } else if (is.null(iot_id)) {
    x <- GET (url = paste0(api, "/Things?$filter=name eq ", name))
    df <- fromJSON(as.character(x))
    return (df)
  } else if (is.null(name)) {
    x <- GET (url = paste0(api, "/Things?$filter=@iot.id eq ", iot_id))
    df <- fromJSON(as.character(x))
    return (df)
  }
  
}

#iot_id <- 2
#name <- 'TV-157'

abc <- get_things(api=api, iot_id = 2)




# get 

things <- get_things(api = api)

# get locations
get_location <- function (api){
  x <- GET(url = paste0(api, "/Locations"))
  return (x)
}

L <- get_location(api)
L

#### Entities [$expand, $select]

# THINGS
# Expand - Locations, historical locations, datastreams

# LOCATIONS
# Expand - Things, historical locations 
# Filter - spatial query by specifying coordinates of a polygon

# DATASTREAMS
# Expand - Things, Sensor, ObservedProperty, Observations
# Collection of Observations, having same ObservedProperty, measured by same Sensor

# SENSORS
# Expand - Datastreams

# OBSERVEDPROPERTIES
# Expand - Datastreams

# OBSERVATIONS
# Expand - Datastream, featureofinterest

# FEATUREOFINTEREST (location of Things)
# Expand - Observations

#### Query [$orderby, $top, $skip, $count, and $filter]




