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
abc <- get_things(api=api,iot_id <- 2)


# get location (iot_id, location.coordinates, name) description too ????
get_locations <- function (api, iot_id=NULL, name=NULL, location.coordinates=NULL, location.type=NULL){
  
  if (is.null(iot_id) & is.null(name) & is.null(location.coordinates) & is.null(location.type)){
    x <- GET(url = paste0(api, "/Locations"))
    df <- fromJSON(as.character(x))
    return (df$value)
  }
  
  else if (is.null(iot_id) & is.null(location.coordinates) & is.null(location.type)){
    name <- gsub(" ","%20",name)
    x <- GET(url = paste0(api, "/Locations?$filter=name%20eq%20%27", name, "%27"))
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
name <- "MBOWN-208 - 29S.02E.13.113A"
# type <- "point"
# lc <- c(-105.795, 32.8542)
# L <- get_location(api, location.coordinates = lc, location.type = type)
abc <- get_locations(api=api, name=name)

# query by general location area (doesnt have to be specific)
# search by area, http://localhost/v1.0/Locations?$filter=st_intersects(location, geography'POLYGON ((-180 -90, -180 90, 180 90, 180 -90, -180 -90))') 

# get datastreams
get_datastreams <- function (api, iot_id=NULL, name=NULL){
  x <- GET(url = paste0(api, "/Datastreams"))
  df <- fromJSON(as.character(x))
  return (df$value) 
}



# they probably are interested in datastreeams, (by location, time, observed property)
# look at sls funtions made by the other person, phenomenon is observedProperty, sites=locations
# pagination


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




