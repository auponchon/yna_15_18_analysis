load("Data/NewlyCreatedData/ForRestLocFail2015.RData")

sf_locs.1 <- sf::st_as_sf(loc.2015.fail, coords = c("Longitude","Latitude")) %>% 
    sf::st_set_crs(4326)

st_write(sf_locs.1,layer="loc_for_fail_2015",dsn="Maxent modelling/Maxent 2015/Occurence/RarefyOccurence",
         driver="ESRI Shapefile", delete_layer = TRUE,append=T)


load("Data/NewlyCreatedData/ForRestLocSuccess2015.RData")

sf_locs.2 <- sf::st_as_sf(loc.2015.suc, coords = c("Longitude","Latitude")) %>% 
    sf::st_set_crs(4326)

st_write(sf_locs.2,layer="loc_for_suc_2015",dsn="Maxent modelling/Maxent 2015/Occurence/RarefyOccurence",
         driver="ESRI Shapefile", delete_layer = TRUE,append=T)

load("Data/NewlyCreatedData/ForRestLocFail2018.RData")

sf_locs.3 <- sf::st_as_sf(loc.2018.fail, coords = c("Longitude","Latitude")) %>% 
    sf::st_set_crs(4326)

st_write(sf_locs.3,layer="loc_for_fail_2018",dsn="Maxent modelling/Maxent 2018/Occurence/RarefyOccurence",
         driver="ESRI Shapefile", delete_layer = TRUE,append=T)


load("Data/NewlyCreatedData/ForRestLocSuccess2018.RData")

sf_locs.4 <- sf::st_as_sf(loc.2018.suc, coords = c("Longitude","Latitude")) %>% 
    sf::st_set_crs(4326)

st_write(sf_locs.4,layer="loc_for_suc_2018",dsn="Maxent modelling/Maxent 2018/Occurence/RarefyOccurence",
         driver="ESRI Shapefile", delete_layer = TRUE,append=T)
