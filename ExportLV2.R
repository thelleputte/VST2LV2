# Export Cubase EQ presets as Ardour EQ preset files.

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

# Main function
# lestEQPresets : return value obtained from readCubasePresets() function.
# destDir       : path to the directory where LV2 EQ presets will be stored.
# 
# The function takes VST EQ presets and transform them into LV2 .ttl files.
# It also generates a 'manifest.ttl' file announcing all these new LV2 files.
# It also creates CITATION and LICENSE files.
# EQ presets, manifest.ttl, CITATION and LICENSE are all stored into destDir.

writeArdourEQPresets<-function(listEQPresets,destDir){
  cat("Generating Credits file.\n")
  generateCredits(destDir)
  cat("Initiating manifest.ttl\n")
  initiateManifest(destDir)
  cat("Writing EQ Presets\n")
  for(i in 1:length(listEQPresets)){
    cat("Progress: ",round(100*i/length(listEQPresets)),"%\r",sep="")
    writeArdourEQPreset(names(listEQPresets)[i],listEQPresets[[i]],destDir)
    addToManifest(names(listEQPresets)[i],destDir)
  }
}


writeArdourEQPreset<-function(EQName,EQParameters,destDir){
  fileName=paste(gsub(pattern=" ",replacement="_",EQName),".ttl",sep="")
  fullPath=paste(destDir,"/",fileName,sep="")
  
  #########################################
  # HEADERS
  #########################################
  
  cat(file=fullPath,append=FALSE,"@prefix atom: <http://lv2plug.in/ns/ext/atom#> .
      @prefix lv2: <http://lv2plug.in/ns/lv2core#> .
      @prefix pset: <http://lv2plug.in/ns/ext/presets#> .
      @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
      @prefix state: <http://lv2plug.in/ns/ext/state#> .
      @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
      
      <>
      a pset:Preset ;
      lv2:appliesTo <http://calf.sourceforge.net/plugins/Equalizer8Band> ;
      rdfs:label \"",EQName,"\" ;
      lv2:port [\n")
  
  #########################################
  # GENERAL PARAMETERS
  #########################################
  
  cat(file=fullPath,append=TRUE,"		lv2:symbol \"analyzer\" ;\n")
  cat(file=fullPath,append=TRUE,"		pset:value 0.0\n")
  cat(file=fullPath,append=TRUE,"	] , [\n")
  cat(file=fullPath,append=TRUE,"		lv2:symbol \"analyzer_mode\" ;\n")
  cat(file=fullPath,append=TRUE,"		pset:value 1.0\n")
  cat(file=fullPath,append=TRUE,"	] , [\n")
  cat(file=fullPath,append=TRUE,"		lv2:symbol \"bypass\" ;\n")
  cat(file=fullPath,append=TRUE,"		pset:value 0.0\n")
  cat(file=fullPath,append=TRUE,"	] , [\n")
  
  #########################################
  # HIGH PASS FILTER
  #########################################
  
  if(format(as.double(EQParameters["Band1","Enable"]),nsmall=1)=="1.0" 
      & format(as.double(EQParameters["Band1","Type"]),nsmall=1)%in%c("2.0","3.0"))
  {
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"hp_active\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value 1.0\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"hp_freq\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value ", EQParameters["Band1","Freq"] ,"\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"hp_mode\" ;\n")
    # 0->12dB/Oct; 1->24dB/Oct; 2->36dB/Oct. Unspecified in Cubase, so 1 by default.
    cat(file=fullPath,append=TRUE,"		pset:value 1.0\n")  
  }
  else
  {
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"hp_active\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value 0.0\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"hp_freq\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value 30.0\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"hp_mode\" ;\n")
    # 0->12dB/Oct; 1->24dB/Oct; 2->36dB/Oct. Unspecified in Cubase, so 1 by default.
    cat(file=fullPath,append=TRUE,"		pset:value 1.0\n")
  }
  
  #########################################
  # HIGH SHELF FILTER
  #########################################
  
  if(format(as.double(EQParameters["Band4","Enable"]),nsmall=1)=="1.0" 
     & format(as.double(EQParameters["Band4","Type"]),nsmall=1)%in%c("1.0","5.0","6.0","7.0"))
  {
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"hs_active\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value 1.0\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"hs_freq\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value ", EQParameters["Band4","Freq"] ,"\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"hs_level\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value ", 10^(as.double(EQParameters["Band4","Gain"])/20) ,"\n")
    cat(file=fullPath,append=TRUE,"	] , [\n") 
  }
  else
  {
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"hs_active\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value 0.0\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"hs_freq\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value 5000.0\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"hs_level\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value 1.0\n")
    cat(file=fullPath,append=TRUE,"	] , [\n") 
  }

  #########################################
  # OTHER GENERAL PARAMETERS
  #########################################
  
  cat(file=fullPath,append=TRUE,"		lv2:symbol \"individuals\" ;\n")
  cat(file=fullPath,append=TRUE,"		pset:value 1.0\n")
  cat(file=fullPath,append=TRUE,"	] , [\n")
  cat(file=fullPath,append=TRUE,"		lv2:symbol \"level_in\" ;\n")
  cat(file=fullPath,append=TRUE,"		pset:value 1.0\n")
  cat(file=fullPath,append=TRUE,"	] , [\n")
  cat(file=fullPath,append=TRUE,"		lv2:symbol \"level_out\" ;\n")
  cat(file=fullPath,append=TRUE,"		pset:value 1.0\n")
  cat(file=fullPath,append=TRUE,"	] , [\n")
  
  #########################################
  # LOW PASS FILTER
  #########################################
  
  if(format(as.double(EQParameters["Band4","Enable"]),nsmall=1)=="1.0" 
     & format(as.double(EQParameters["Band4","Type"]),nsmall=1)%in%c("2.0","3.0"))
  {
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"lp_active\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value 1.0\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"lp_freq\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value ", EQParameters["Band4","Freq"] ,"\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"lp_mode\" ;\n")
    # 0->12dB/Oct; 1->24dB/Oct; 2->36dB/Oct. Unspecified in Cubase, so 1 by default.
    cat(file=fullPath,append=TRUE,"		pset:value 1.0\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")        
  }
  else
  {
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"lp_active\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value 0.0\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"lp_freq\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value 18000.0\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"lp_mode\" ;\n")
    # 0->12dB/Oct; 1->24dB/Oct; 2->36dB/Oct. Unspecified in Cubase, so 1 by default.
    cat(file=fullPath,append=TRUE,"		pset:value 1.0\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")    
  }

  
  #########################################
  # LOW SHELF FILTER
  #########################################
  
  if(format(as.double(EQParameters["Band1","Enable"]),nsmall=1)=="1.0" 
     & format(as.double(EQParameters["Band1","Type"]),nsmall=1)%in%c("1.0","5.0","6.0","7.0"))
  {
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"ls_active\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value 1.0\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"ls_freq\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value ", EQParameters["Band1","Freq"] ,"\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"ls_level\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value ", 10^(as.double(EQParameters["Band1","Gain"])/20) ,"\n")
    cat(file=fullPath,append=TRUE,"	] , [\n") 
  }
  else
  {
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"ls_active\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value 0.0\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"ls_freq\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value 100.0\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"ls_level\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value 1.0\n")
    cat(file=fullPath,append=TRUE,"	] , [\n") 
  }
  
  #########################################
  # BAND FILTER 1
  #########################################
  
  if(format(as.double(EQParameters["Band1","Enable"]),nsmall=1)=="1.0" 
     & format(as.double(EQParameters["Band1","Type"]),nsmall=1)%in%c("0.0","4.0"))
  {
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"p1_active\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value 1.0\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"p1_freq\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value ", EQParameters["Band1","Freq"] ,"\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"p1_level\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value ", 10^(as.double(EQParameters["Band1","Gain"])/20) ,"\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"p1_q\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value ", ArdourQValue(as.double(EQParameters["Band1","Q"])) ,"\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
  }
  else
  {
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"p1_active\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value 0.0\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"p1_freq\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value 100.0\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"p1_level\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value 1.0\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"p1_q\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value 0.1\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
  }

  
  #########################################
  # BAND FILTER 2
  #########################################
  
  cat(file=fullPath,append=TRUE,"		lv2:symbol \"p2_active\" ;\n")
  cat(file=fullPath,append=TRUE,"		pset:value ", format(as.double(EQParameters["Band2","Enable"]),nsmall=1) ,"\n",sep="")
  cat(file=fullPath,append=TRUE,"	] , [\n")
  cat(file=fullPath,append=TRUE,"		lv2:symbol \"p2_freq\" ;\n")
  cat(file=fullPath,append=TRUE,"		pset:value ", EQParameters["Band2","Freq"] ,"\n",sep="")
  cat(file=fullPath,append=TRUE,"	] , [\n")
  cat(file=fullPath,append=TRUE,"		lv2:symbol \"p2_level\" ;\n")
  cat(file=fullPath,append=TRUE,"		pset:value ", 10^(as.double(EQParameters["Band2","Gain"])/20) ,"\n",sep="")
  cat(file=fullPath,append=TRUE,"	] , [\n")
  cat(file=fullPath,append=TRUE,"		lv2:symbol \"p2_q\" ;\n")
  cat(file=fullPath,append=TRUE,"		pset:value ", ArdourQValue(as.double(EQParameters["Band2","Q"])) ,"\n",sep="")
  cat(file=fullPath,append=TRUE,"	] , [\n")
  
  #########################################
  # BAND FILTER 3
  #########################################
  
  cat(file=fullPath,append=TRUE,"		lv2:symbol \"p3_active\" ;\n")
  cat(file=fullPath,append=TRUE,"		pset:value ", format(as.double(EQParameters["Band3","Enable"]),nsmall=1) ,"\n",sep="")
  cat(file=fullPath,append=TRUE,"	] , [\n")
  cat(file=fullPath,append=TRUE,"		lv2:symbol \"p3_freq\" ;\n")
  cat(file=fullPath,append=TRUE,"		pset:value ", EQParameters["Band3","Freq"] ,"\n",sep="")
  cat(file=fullPath,append=TRUE,"	] , [\n")
  cat(file=fullPath,append=TRUE,"		lv2:symbol \"p3_level\" ;\n")
  cat(file=fullPath,append=TRUE,"		pset:value ", 10^(as.double(EQParameters["Band3","Gain"])/20) ,"\n",sep="")
  cat(file=fullPath,append=TRUE,"	] , [\n")
  cat(file=fullPath,append=TRUE,"		lv2:symbol \"p3_q\" ;\n")
  cat(file=fullPath,append=TRUE,"		pset:value ", ArdourQValue(as.double(EQParameters["Band3","Q"])) ,"\n",sep="")
  cat(file=fullPath,append=TRUE,"	] , [\n")
  
  #########################################
  # BAND FILTER 4
  #########################################
  
  if(format(as.double(EQParameters["Band4","Enable"]),nsmall=1)=="1.0" 
     & format(as.double(EQParameters["Band4","Type"]),nsmall=1)%in%c("0.0","4.0"))
  {
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"p4_active\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value 1.0\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"p4_freq\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value ", EQParameters["Band4","Freq"] ,"\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"p4_level\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value ", 10^(as.double(EQParameters["Band4","Gain"])/20) ,"\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"p4_q\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value ", ArdourQValue(as.double(EQParameters["Band4","Q"])) ,"\n")
    cat(file=fullPath,append=TRUE,"	] , [\n") 
  }
  else
  {
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"p4_active\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value 0.0\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"p4_freq\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value 5000.0\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"p4_level\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value 1.0\n")
    cat(file=fullPath,append=TRUE,"	] , [\n")
    cat(file=fullPath,append=TRUE,"		lv2:symbol \"p4_q\" ;\n")
    cat(file=fullPath,append=TRUE,"		pset:value 0.1\n")
    cat(file=fullPath,append=TRUE,"	] , [\n") 
  }
  
  #########################################
  # OTHER GENERAL PARAMETER
  #########################################
  
  cat(file=fullPath,append=TRUE,"		lv2:symbol \"zoom\" ;\n")
  cat(file=fullPath,append=TRUE,"		pset:value 0.25\n")
  cat(file=fullPath,append=TRUE,"	] .\n")
  cat(file=fullPath,append=TRUE,"\n")
}

#================================================================================
# Helper functions

ArdourQValue<-function(CubaseValue){
  # This is an approximation only.
  # Cubase stores values in its VST preset files that are different from what is displayed in Cubase GUI.
  # The following formula generates, from internal Cubase/VST values, Ardour/LV2 values that are close to 
  # what is displayed in Cubase.
  # Improvements welcome.
  return(12*CubaseValue^.95)
}

initiateManifest<-function(destDir){
  fullPath=paste(destDir,"/manifest.ttl",sep="")
  cat(file=fullPath,append=FALSE,"
      @prefix atom: <http://lv2plug.in/ns/ext/atom#> .
      @prefix lv2: <http://lv2plug.in/ns/lv2core#> .
      @prefix pset: <http://lv2plug.in/ns/ext/presets#> .
      @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
      @prefix state: <http://lv2plug.in/ns/ext/state#> .
      @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
      
      ",sep="")
}

addToManifest<-function(EQNames,destDir){
  fullPath=paste(destDir,"/manifest.ttl",sep="")
  for(i in EQNames){
    cat(file=fullPath,append=TRUE,"<",gsub(pattern=" ",replacement="_",i),".ttl>
        lv2:appliesTo <http://calf.sourceforge.net/plugins/Equalizer8Band> ;
        a pset:Preset ;
        rdfs:seeAlso <",gsub(pattern=" ",replacement="_",i),".ttl> .
    
        ",sep="")
  }
}

generateCredits<-function(destDir){
  fullPath=paste(destDir,"/LICENSE",sep="")
  cat(file=fullPath,append=FALSE,"
      ================================================================================
      \"THE BEER-WARE LICENSE\" (Revision 42):
      <Thib@LesEnroules.Rocks> wrote this file, helped by <ben@LesEnroules.Rocks>.  
      As long as you retain this notice you can do whatever you want with this stuff. 
      If we meet some day, and you think this stuff is worth it, you can buy us a beer 
      in return. Thibault Helleputte
      ================================================================================
      \n",sep="")
  
  fullPath=paste(destDir,"/CITATION",sep="")
  cat(file=fullPath,append=FALSE,"
      ================================================================================
      CITATION : Give credits to 'Les Enroules' by complying to the rules of BEERWARE 
      License of this code, and also by liking their music/video/material on one of 
      the following media :
      Web: http://LesEnroules.Rocks
      Facebook: https://www.facebook.com/LesEnroules/
      Youtube: https://www.youtube.com/channel/UCl6Q5lQFa6EZphm7TIEN-ew
      Soundcloud: https://soundcloud.com/thib-hell/albums
      ================================================================================
      \n",sep="")
}