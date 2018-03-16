# Import Cubase EQ presets from a .pxml VST file
library(XML)

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