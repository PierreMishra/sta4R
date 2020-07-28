library(httr)
library(jsonlite)

api <- "http://sta-demo.internetofwater.dev/api/v1.1"

# get things (by iot_id & name)
get_things <- function(api, iot_id = NULL, name = NULL) {
  
  if (is.null(iot_id) & is.null(name)){
    x <- GET(url = paste0(api, "/Things"))
    df <- fromJSON(as.character(x))
    return (df$value)
  } 
  
  else if (is.null(iot_id)) {
    x <- GET (url = paste0(api, "/Things?$filter=name%20eq%20%27", name, "%27"))
    df <- fromJSON(as.character(x))
    return (df$value)
  } 
  
  else if (is.null(name)) {
    x <- GET (url = paste0(api, "/Things?$filter=@iot.id%20eq%20", iot_id))
    df <- fromJSON(as.character(x))
    return (df$value)
  }
}

#iot_id <- 2
#name <- 'TV-157'
#abc <- get_things(api=api, name = "TV-157")


# get location (iot_id, location.coordinates)
get_location <- function (api, iot_id=NULL, location.coordinates=NULL, location.type=NULL){
  
  if (is.null(iot_id) & is.null(location.coordinates) & is.null(location.type)){
    x <- GET(url = paste0(api, "/Locations"))
    df <- fromJSON(as.character(x))
    return (df$value)
  }
  
  else if (is.null(location.coordinates)){
    x <- GET(url = paste0(api, "/Locations?$filter=@iot.id%20eq%20", iot_id))
    df <- fromJSON(as.character(x))
    return (df$value)
  }
  
  else if (is.null(iot_id)){
    x <- GET(url = paste0(api, "/Locations?$filter=st_equals(location,geography%27", 
                          toupper(location.type), "(", paste(c(location.coordinates), collapse = "%20"), ")%27)" ))
    df <- fromJSON(as.character(x))
    return (df$value)
  }
  
  
}
# type <- "point"
# lc <- c(-105.795, 32.8542)
# L <- get_location(api, location.coordinates = lc, location.type = type)

# get datastreams




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




