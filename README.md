# VST2LV2
Translation of VST plugins into LV2 standard

### Use case

The aim is to translate preset files from VST to LV2 format, in the context of Digital Audio Workstations (DAW).

At this stage, the code in R language allows to import Cubase EQ plugins presets in VST format, and to translate them into LV2 8 Band EQ presets for Ardour.

Typical use:

CubasePresets<-readCubasePresets("VstEqPreset.pxml")

writeArdourEQPresets(CubasePresets,destDir = "~/MyDestinationDirectory/")

