storm <-  function(){
        
        bzFilename <- "stomdata.bz2"
        
        fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
        
        #download.file(fileUrl,destfile=bzFilename, method="curl")
        
        data <- read.csv(bzfile(bzFilename),sep=",",quote="\"")
        
        m <- subset(data, FATALITIES >0 | INJURIES >0 | PROPDMG > 0 | CROPDMG > 0)      
              
       
        
        m[,c("EVTYPE")] <- toupper(m[,c("EVTYPE")])
        
        m[m$EVTYPE=="AVALANCE",c("EVTYPE")] <- "AVALANCHE"
        
        m[grep("BLIZZARD*",m$EVTYPE),c("EVTYPE")] <- "BLIZZARD"
        
        m[grep("HAIL*",m$EVTYPE),c("EVTYPE")] <- "HAIL"
        
        m[grep("HEAVY RAIN*",m$EVTYPE),c("EVTYPE")] <- "HEAVY RAIN"
        
        m[grep("WATERSPOUT*",m$EVTYPE),c("EVTYPE")] <-"WATERSPOUT"
        
        m[grep("HURRICANE*",m$EVTYPE),c("EVTYPE")] <-"HURRICANE"
        
        m[grep("THUNDERSTORM*|TUNDERSTORM WIND*|TSTM WIND*|THUDERSTORM WINDS*",m$EVTYPE),c("EVTYPE")] <-"THUNDERSTORM WIND"
                
        m[grep("THUNDEERSTORM WINDS*",m$EVTYPE),c("EVTYPE")] <-"THUNDERSTORM WIND"
        
        m[grep("THUNDERESTORM WINDS*",m$EVTYPE),c("EVTYPE")] <-"THUNDERSTORM WIND"
        
        m[grep("THUNDERTORM WINDS*",m$EVTYPE),c("EVTYPE")] <-"THUNDERSTORM WIND"
        
        m[grep("THUNERSTORM WINDS*",m$EVTYPE),c("EVTYPE")] <-"THUNDERSTORM WIND"
        
        m[grep("THUNDERSTROM WIND*",m$EVTYPE),c("EVTYPE")] <-"THUNDERSTORM WIND"
        
        m[grep("THUNDERSTROM WIND*",m$EVTYPE),c("EVTYPE")] <-"THUNDERSTORM WIND"
        
        m[grep("TSTMW*",m$EVTYPE),c("EVTYPE")] <-"THUNDERSTORM WIND"
        
        m[grep("TORNADO*",m$EVTYPE),c("EVTYPE")] <-"TORNADO"
        
        m[grep("TORNDAO*",m$EVTYPE),c("EVTYPE")] <-"TORNADO"
        
        m[grep("RIP CURRENT*",m$EVTYPE),c("EVTYPE")] <-"RIP CURRENT"
        
        m[grep("STRONG WIND*",m$EVTYPE),c("EVTYPE")] <-"STRONG WIND"
        
        m[grep("LIGHTNING*",m$EVTYPE),c("EVTYPE")] <-"LIGHTNING"
        
        m[grep("LIGHTING*|LIGNTNING*",m$EVTYPE),c("EVTYPE")] <-"LIGHTNING"       
      
        
        m[grep("FLASH FLOOD*",m$EVTYPE),c("EVTYPE")] <-"FLASH FLOOD" 
        
        m[grep("WINTER WEATHER*",m$EVTYPE),c("EVTYPE")] <-"WINTER WEATHER"
        
        m[grep("WINTER STORM*",m$EVTYPE),c("EVTYPE")] <-"WINTER STORM"
        
        m[grep("TROPICAL STORM*",m$EVTYPE),c("EVTYPE")] <-"TROPICAL STORM"
        
       
        
        m[grep("HEAVY SNOW*",m$EVTYPE),c("EVTYPE")] <-"HEAVY SNOW"
        
        m[grep("HEAVY RAIN*|HVY RAIN*",m$EVTYPE),c("EVTYPE")] <-"HEAVY RAIN"
        
        m[grep("FLOOD/FLASH*|FLOOD FLASH*",m$EVTYPE),c("EVTYPE")] <-"FLASH FLOOD"
        
        m[grep("FLOODING|FLOOD/RIVER FLOOD|FLOODS|FLOOD/RAIN/WINDS",m$EVTYPE),c("EVTYPE")] <-"FLOOD"
        
        m[grep("WILDFIRES*|WILD FIRES*|WILDFIRE*|WILD/FOREST*",m$EVTYPE),c("EVTYPE")] <-"WILDFIRE"
        
        m[grep("HURRICANE*|TYPHOON*",m$EVTYPE),c("EVTYPE")] <-"HURRICANE (TYPHOON)"
        
       mag <- c(0,1,2,3,4,5,6,7,8,9,"k","K","m","M","b","B","h","H")
       
       magv <- c(1e-6,1e-5,1e-4,1e-3,1e-2,1e-1,1,1e1,1e2,1e3,1e-3,1e-3,1,1,1e3,1e3,1e-4,1e-4)
       
       magdf <- data.frame(mag=mag,magv=magv)
       cb <- subset(m, m$PROPDMGEXP %in% magdf$mag | m$CROPDMGEXP %in% magdf$mag)
      cb$CROPDMGEXPV <- sapply(cb$CROPDMGEXP, function(x){if(x %in% magdf$mag)magdf[mag==x,2] else 0})
       cb$PROPDMGEXPV <- sapply(cb$PROPDMGEXP, function(x){if(x %in% magdf$mag)magdf[mag==x,2] else 0})
        #barplot(dddf$f,names.arg=dddf$n)
}