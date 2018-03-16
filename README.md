# VST2LV2
Translation of VST plugins into LV2 standard


### AUTHOR: 

Code written by Thibault Helleputte, drummer of the band 'Les Enroules', Belgium, EU, with help from Ben Caby, guitarist of 'Les Enroules'.

### Contact

thib at LesEnroules dot Rocks

### CITATION: 

Give credits to 'Les Enroules' by liking their music/video/material on one of the following media :
* Web: http://LesEnroules.Rocks
* Facebook: https://www.facebook.com/LesEnroules/
* Youtube: https://www.youtube.com/channel/UCl6Q5lQFa6EZphm7TIEN-ew
* Soundcloud: https://soundcloud.com/thib-hell/albums

### LICENSE

GNU LGPL V3

### Use case

The aim is to translate preset files from VST to LV2 format, in the context of Digital Audio Workstations (DAW).

At this stage, the code allows to import Cubase EQ plugins presets in VST format, and to translate them into LV2 8 Band EQ presets for Ardour.

Typical use:

CubasePresets=readCubasePresets("VstEqPreset.pxml")

writeArdourEQPresets(CubasePresets,destDir = "~/MyDestinationDirectory/")

