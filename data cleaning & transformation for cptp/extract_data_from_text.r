# RScript
#
# Par Youssef de Madeen Amadou
#
# Look into text data and extract desired information, using stringr package

require(stringr)

bcgp_ship <- do.call(tbl_opal, list('BCGP_R2_SAMPLE.ShipmentNotes','Shipment_Comments'))

t = bcgp_ship
t[,Shipment_Comments:=tolower(Shipment_Comments)]

t[,maxtemp_ShipmentNotes:=str_extract(Shipment_Comments,"highest[[:punct:]a-zA-Z[:space:]]++[0-9.]++") %>%
     str_extract(.,"[0-9.]++")]
t[,mintemp_ShipmentNotes:=str_extract(Shipment_Comments,"lowest[[:punct:]a-zA-Z[:space:]]++[0-9.]++") %>%
     str_extract(.,"[0-9.]++")]
t[,avgtemp_ShipmentNotes:=str_extract(Shipment_Comments,"average[[:punct:]a-zA-Z[:space:]]++[0-9.]++") %>%
     str_extract(.,"[0-9.]++")]

sapply(t[,list(maxtemp_ShipmentNotes,mintemp_ShipmentNotes,avgtemp_ShipmentNotes)],function(x) levels(factor(x)))