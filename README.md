# RM2

Obligāti jāveic izmaiņas:
1. Jāiekopē ‘LVM_NOGABALI” shp slāņi (pieejami www.lvmgeo.lv)
2. Jāiekopē 'LVM_TK' shp ceļa slānis no krautuves līdz nogabalam
3. Failā ‘ui.R’ jāsamaina ‘workdir’ norādot darba mapi ‘RM2_R_kods’
4. Failā ‘forvardera_rises_update.R’ jānomaina pievešanas ceļu shp faila atrašanās
jānomain - 
 linija <- st_read("......./LVM_TK/Pievesanas_cels.shp") 
5. jāuzstāda nepieciešamās bibliotēkas
 install.packages(c('shiny', 'sf', 'plyr', 'purrr', 'data.table', 'tidyr', 'raster', 'sp', 'DBI', 'RPostgres', 'tidyverse', 'writexl', 'akima'))
6. jāuzstāda "maptools" bibliotēka manuāli (pielikā)

Mandatory changes to be made:

1. Copy/import the LVM_NOGABALI shp layers (available at www.lvmgeo.lv).
2. Copy/import the LVM_TK shp road layer covering the route from the landing/storage area (“krautuve”) to the forest compartment (“nogabals”).
3. In the file ui.R, change workdir to point to the working folder RM2_R_kods.
4. In the file forvardera_rises_update.R, change the location/path of the forwarding road shapefile (“pievešanas ceļu shp fails”). Replace/update the line:
linija <- st_read("......./LVM_TK/Pievesanas_cels.shp")

5. Install the required R libraries:
install.packages(c('shiny', 'sf', 'plyr', 'purrr', 'data.table', 'tidyr', 'raster', 'sp', 'DBI', 'RPostgres', 'tidyverse', 'writexl', 'akima'))

6. Install the maptools library.
