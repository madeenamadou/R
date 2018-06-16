# R Script
#
# Par Youssef de Madeen Amadou
#
# Exporter un dataset de R dans un Projet Opal
#


push_opal = function(x,y){
  
  # Pousser un dataset dans Opal
  # Parameters : 
  # - x : une liste de 2
  #   - x[[1]] : le dataset a pousser
  #   - x[[2]] : le nom destine au dataset sur Opal
  # - y : une liste de 3
  #   - y[1] : le projet de destination du dataset sur Opal
  #   - y[2] : le nom de la colonne d'identifiants
  #   - y[3] : le type de donnees du dataset ; ex: "Biosample", "Participant"
  # Note : Necessite d'etre connecte a Opal, dans l'environnement intitule 'o'
  
  opal.assign.data(o,x[[2]],tibble::as.tibble(x[[1]]))
  opal.symbol_import(o,x[[2]],y[1],id.name = y[2],type = y[3])
  
}

#do.call(push_opal,list(list(tbl,'CPTP_OHS_Biosample_Primary_By_SampleID_20170502'),
#                       c('OHS_R2_SAMPLE','CPAC_Sample_ID','Biosample')))