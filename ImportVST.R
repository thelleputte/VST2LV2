# Import Cubase EQ presets from a .pxml VST file

#================================================================================
#  "THE BEER-WARE LICENSE" (Revision 42):
#  <Thib@LesEnroules.Rocks> wrote this file, helped by <ben@LesEnroules.Rocks>.  
#  As long as you retain this notice you can do whatever you want with this stuff. 
#  If we meet some day, and you think this stuff is worth it, you can buy us a beer 
#  in return. Thibault Helleputte
# ================================================================================

# ================================================================================
#  CITATION : Give credits to 'Les Enroules' by complying to the rules of BEERWARE 
#  License of this code, and also by liking their music/video/material on one of 
#  the following media :
#  Web: http://LesEnroules.Rocks
#  Facebook: https://www.facebook.com/LesEnroules/
#  Youtube: https://www.youtube.com/channel/UCl6Q5lQFa6EZphm7TIEN-ew
#  Soundcloud: https://soundcloud.com/thib-hell/albums
#================================================================================

library(XML)

# input: path to a Cubase EQ Presets ".pxml" file in VST format.
readCubasePresets<-function(file){
  xmlFile=xmlTreeParse(file)  
  topXml <- xmlRoot(xmlFile)
  topXml <- topXml[[1]][[1]][[3]]
  ListOfPresets=list()
  Names=c()
  for(i in 1:length(topXml)){
    Presets=topXml[[i]]
    Name=getCubaseEQName(Presets[[1]])
    Values=getCubaseEQValues(Presets[[2]][[2]])
    ListOfPresets[[Name]]=Values
  }
  return(ListOfPresets)
}

#================================================================================
# Helper functions

getCubaseEQName<-function(input){
  Attrs<-xmlAttrs(input)
  return(Attrs["value"])
}

getCubaseEQValues<-function(input){
  EQValues=data.frame(t(c(0.0,0.0,0.0,0.0,0.0)))
  names(EQValues)=c("Enable","Type","Gain","Freq","Q")
  for(i in 1:4){
    EQValues[i,]<-c( xmlAttrs(input[[i]][[1]])["value"], xmlAttrs(input[[i]][[2]])["value"], xmlAttrs(input[[i]][[3]])["value"], xmlAttrs(input[[i]][[4]])["value"], xmlAttrs(input[[i]][[5]])["value"] )
  }
  row.names(EQValues)=c("Band1","Band2","Band3","Band4")
  return(EQValues)
}
