# R Script
#
# Par Youssef de Madeen Amadou
#
# Importer une table/fichier de donnees d'un Projet Opal dans R
#


fl_opal = function(x,y){
  # x : chemin du fichier de donnees dans la memoire interne Opal
  # y : liste des variables a retenir
  x %>%  file.path(.) %>% opal.file(o,.) %>% read.csv(text=.) %>%
    data.table %>% subset(.,select=y)}

tbl_opal = function(x,y){
  # x : chemin vers le dataset sur Opal (<Projet>.<Table de donnees>)
  # y : liste des variables a retenir
  opal.assign.table(o,'cptp',value = x,variables = as.list(y),id.name = 'entity_id')
  return (opal.execute(o,'cptp') %>% data.table)}
