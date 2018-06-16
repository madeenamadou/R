# JavaScript Script
#
# Par Youssef de Madeen Amadou
#
# Generer le SPREC CODE d'echantillons biomedicaux
#


var t = [
  //Type of sample
  $this('SAMPLE_TYPE').map({
  1:'BLD',2:'DWB',3:'SER',4:'RBC',5:'PL1',6:'ZZZ',7:'BFF',8:'CEL',9:'SAL',10:'URN'}),
  //Type of primary container
  $this('SAMPLE_COLLECT_CONTAINER').map({
  1:'ACD',2:'SST',3:'PED',4:'CAT',5:'ORG',6:'ORG',7:'PPS'},'ZZZ','XXX'),
  //Pre-centrifugation temp & delay
  function(x,y){
      return y.map({
        '1': x.map({'<2h':'A','2-4h':'C','4-8h':'E','8-12h':'G','12-24h':'I','24-48h':'K','>48h':'M'},'Z','X').value(),
        '2':x.map({'<2h':'B','2-4h':'D','4-8h':'F','8-12h':'H','12-24h':'J','24-48h':'L','>48h':'N'},'Z','X').value(),
        '3': 'O',
        '4': 'X',
        '9': 'Z'},'X')
      }.call(null,$this('SAMPLE_CENTRI1_PRE_DELAY'),$this('SAMPLE_PRE_STAB_TEMP')),
  //Centrifugation
  function(a,b,c,d){
    var bb = b.map({'10':'10-15','15':'10-15'},b)
      return a.map({'-7':'N','9':'X','3':'Z',
      '1': bb.map({'10-15': c.group([3000,6000,10000]).map({
        '-3000': d.map({'1':'B','0':'A'},'Z'),
        '3000-6000':'E','6000-10000':'G','10000+':'I'},'Z'),
        '30': 'M'},'Z'),
      '2': bb.map({'10-15': c.group([3000,6000,10000]).map({
        '-3000': d.map({'1':'D','0':'C'},'Z'),
        '3000-6000':'F','6000-10000':'H','10000+':'J'},'Z')})
      },'X')}.call(null,$this('SAMPLE_CENTRI1_TEMP'),$this('SAMPLE_CENTRI1_DURATION_SOP'),
                  $this('SAMPLE_CENTRI1_GFORCE'),$this('SAMPLE_CENTRI1_BRAKE')),
  //Second centrifugation    
  function(a,b,c,d,e){
    var bb = b.map({'10':'10-15','15':'10-15'},b)
      return e.map({'-7':'N'},
      a.map({'-7':'N','9':'X','3':'Z',
      '1': bb.map({'10-15': c.group([3000,6000,10000]).map({
        '-3000': d.map({'1':'B','0':'A'},'Z'),
        '3000-6000':'E','6000-10000':'G','10000+':'I'},'Z')},'Z'),
      '2': bb.map({'10-15': c.group([3000,6000,10000]).map({
        '-3000': d.map({'1':'D','0':'C'},'Z'),
        '3000-6000':'F','6000-10000':'H','10000+':'J'},'Z')})
      },'X'))}.call(null,$this('SAMPLE_CENTRI1_TEMP'),$this('SAMPLE_CENTRI1_DURATION_SOP'),
                  $this('SAMPLE_CENTRI1_GFORCE'),$this('SAMPLE_CENTRI1_BRAKE'),$this('SAMPLE_CENTRI2_NA')),
                  
  //Post centrifugation  delay
  function(a,b){
      return b.map({'-7':'N','9':'X','3':'Z',
      '1': a.map({'<1h':'B','1-2h':'D','2-8h':'F','8-24h':'H','>48h':'J'},'N','X').value(),
      '2': a.map({'<1h':'A','1-2h':'C','2-8h':'E','8-24h':'G','>48h':'I'},'N','X').value()
      },'X')}.call(null,$this('SAMPLE_CENTR1_POST_DELAY'),$this('SAMPLE_CENTRI1_POST_TEMP')),
  
  //Lomg-term storage
  function(a,b){
      return a.map({'7':'Y','8':'Z',
      '1': b.map({'1':'A','2':'B','3':'V'},'Z','X').value(),
      '2': b.map({'1':'D','4':'C','5':'E'},'Z','X').value(),
      '3': b.map({'1':'G','2':'H','5':'I'},'Z','X').value(),
      '4': b.map({'1':'J','2':'K'},'Z','X').value(),
      '5': b.map({'1':'L','2':'M'},'Z','X').value(),
      '6': b.map({'1':'S','2':'T','3':'W'},'Z','X').value()
      },'X')}.call(null,$this('SAMPLE_LT_STORAGE_CONTAINER'),$this('SAMPLE_LT_STORAGE_TEMP1'))

]

t.reduce(function(x,y){return x+'-'+y})

#END