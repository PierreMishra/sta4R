library(httr)
library(jsonlite)
library(sf)
library(tidyverse)

api <- "http://sta-demo.internetofwater.dev/api/v1.1"
id <- 201
name <- '354017108445201'
prop.name <- "ThingDataProvider"
prop.value <- "U.S. Geological Survey"
longitude <- -106.6997
latitude <- 31.7893

# colorado shapefile
unzip("Colorado_State_Boundary-shp.zip", overwrite = TRUE)
colorado_shapefile <- st_read("Colorado_State_Boundary.shp")
polygon <- colorado_shapefile$geometry[[1]][[1]] #but not everyone's sf object will be like this right?
polygon <- as.matrix(polygon)
polygon_vector <- as.vector(t(polygon))
# insert comma at even indices
# for (n in polygon[1,]){
#    paste
#   }
a <- as.vector(polygon[1,])
paste0(a, collapse=", ")

############################# THINGS ###############################

# add multiple things by id or names
# add an argument that lets you choose to display "expand" columns or not
# Look at logical operators in Sensorup website

# Get all things
get_things <- function(api){
  x <- GET(url = paste0(api, "/Things"))
  df <- fromJSON(as.character(x))
  return (df$value)
}

# Get things by IoT ID
get_things_by_id <- function(api, id){
  x <- GET(url = paste0(api, "/Things?$filter=@iot.id%20eq%20", id))
  df <- fromJSON(as.character(x))
  return (df$value)
}

# Get things by thing's name
get_things_by_name <- function(api, name){
  x <- GET(url = paste0(api, "/Things?$filter=name%20eq%20%27", name, "%27"))
  df <- fromJSON(as.character(x))
  return (df$value)
}

# Get things by optional properties
# prop.name and prop.value should be character values
get_things_by_optional_property <- function(api, prop.name, prop.value){
  prop.value <- gsub(" ","%20",prop.value)
  x <- GET(url = paste0(api, "/Things?$filter=properties/", prop.name,
                        "%20eq%20%27", prop.value, "%27"))
  df <- fromJSON(as.character(x))
  return (df$value)
}

# Get things by coordinates (longitude, latitude)
# use this format https://sta-demo.internetofwater.dev/api/v1.1/Things?$expand=Datastreams($filter=phenomenonTime%20le%202019-12-13T21:26:00.000Z)
# it is only filtering the entity immediately before, so if you write wrong coordinates, it will still show you all the things
# use this https://sta-demo.internetofwater.dev/api/v1.1/Things?$filter=st_equals(Locations/location,geography%27POINT(-106.6997%2031.7893)%27)
get_things_by_coordinates <- function(api, longitude, latitude){
  x <- GET(url = paste0(api, "/Things?$filter=st_equals(Locations/location,geography%27POINT(",
                        longitude, "%20", latitude, ")%27)"))
  df <- fromJSON(as.character(x))
  return (df$value)
}

# Get things by area (sf_object)
#/v1.0/Locations?$expand=Things&$filter=st_within(location, geography'POLYGON ((-79.39 43.651,-79.371 43.651,-79.371 43.641,-79.39 43.641,-79.39 43.651))')


# Get things by phenomenonTime


## allow numerical input(id, results) to have logical operators
# get_things
# get_things_by_id
# get_things_by_name
# get_things_by_add_property
# get_things_by_coordinates
# get_things_by_area
# ###get_things_by_time (INVALID)(Because it depends on datastreams which has a range of phenomenonTime)
# 
# get_datastreams
# get_datastreams_by_things ##name of things
# get_datastreams_by_id ##id of things
# get_datastreams_by_name ##name of datastreams
# get_datastreams_by_sensor
# get_datastreams_by_observed_property
# get_datastreams_by_location_name
# get_datastreams_by_latlong ##lat long, only for points
# get_datastreams_by_area ##sf object polygon
# get_datastreams_by_unit
# get_datastreams_by_time ##provde begin, end time, see sos4r example
# 
# get_observations
# get_observations_by_datastream
# get_observations_by_id
# get_observations_by_observed_property
# get_observations_by_location_name
# get_observations_by_latlong
# get_observations_by_area
# get_observations_by_time ## provde begin, end time, see sos4r example
# get_observations_by_result ## logical operations, less than, eqaul to greater than specified value
# get_observations_by_feature_of_interest
# get_observations_by_sensor
# 
# get_locations
# get_locations_by_id
# get_locations_by_name
# get_locations_by_area
# get_locations_by_description

# add get sensor, featureofinterest, observedproperty later 

# add attributes to functions based on following filter options:
# https://fraunhoferiosb.github.io/FROST-Server/sensorthingsapi/STA-Example-Queries.html
# https://fraunhoferiosb.github.io/FROST-Server/sensorthingsapi/STA-Filtering.html 

# helpful example of an application of SensorThingsAPI:
# https://developers.sensorup.com/examples/









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
# look at sos funtions made by the other person, phenomenon is observedProperty, sites=locations
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




