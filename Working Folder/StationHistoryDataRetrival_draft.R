#PURPOSE:  RETRIEVE PERIOD OF RECORD DATA FROM HYDSTRA FOR STATION HISTORIES
#CREDIT:   DAVID COLVIN (data retrieval), AMANDA MAGUIRE (no longer employee) and REED HOSHOVSKY (data formatting)
#UPDATES:  1/29/2025 

#-------------------------------------------------------------------------------

#STAY UP TO DATE ON PACKAGE DEVELOPMENT
install.packages("//pcdistfs1/share/Water Quality Evaluation Section/Tools/Rstuff/packages/Dtoolbag", repos=NULL, type="source")
library(Dtoolbag)

#HAVE YOU UPDATED THE WY INDEX FILE IN YOUR WORKING DIRECTORY??
setwd("C:\\Users\\rhoshovs\\Documents\\RStuff\\Projects\\Station History Template")

#IMPORTANT!! n(STATIONS) * n(PARAMETERS) =< 10 (e.g. c("I80", "LIS", "STTD") * c("SpCond", "Turb", "pH", "DOsat", "DOconc",) = 10)
#INSERT STATION CODES:
station = "LIS"
#INSERT DESIRED PARAMETERS THAT THE STATION MONITORS
# ALL PARAMETER CODES FOR HYDSTRA: 
#c("SpCond", "Turb", "pH", "DOsat", "DOconc", "Chlor", "BGA", "Depth", "Stage", "Velocity", "Flow", "Sal", "fDom", "TidalFlow", "Elevation")
parameters = c("Temp","DOconc","pH","SpCond","Turb","Chlor","Depth", "fDom")


#PULLING POR DATA FROM HYDSTRA, WILL NEED TO OPEN REMOTE DESKTOP AND HYCSV AS PART OF THIS 
#ENSURE DAVID HAS ADDED YOU TO THE Dtoolbag PACKAGE Hystra_funcs_1.1.R FILE 
#ENSURE HYCSV.PRM FILE IS PRESENT IN THE FOLLOWING REMOTE DESKTOP FILE PATH "Users/(YOUR CPU LOGIN USERNAME)//10.158.193.55/Hydstra/USERS/(YOUR CPU LOGIN USERNAME)/TEMP/PRM"
#OPEN BELOW TO SEE SPECIFIC INSTRUCTION FOR WHAT TO DO IN HYCSV FOR FIRST TIME USERS
if(TRUE){
  #Download data from Hydstra
  #use set_hycsv to set download criteria
  set_hycsv(station, parameters, range = MkDate(c(200001010000,202212312345)),type = "MEAN", interval="Daily")
  #use open in excel
  
  # Display a message to the user
  cat("Open HYCSV in Hydstra, press F8 and run HYCSV. Then Press Enter to continue...") 
  
  # Pause until the user presses Enter
  readline(prompt = "")
  
  
  #file will save to users temp on the hydstra server
  #get_HYCSV will find latest file in hydstra temp folder
  #save continuous data
  dt <- Dmelt(Dread(get_HYCSV()))
  dt<-dt[!is.na(value)]
  names(dt)[1]<-"Date"
  dt<-Dcast(dt)
  
  Dwrite(dt,paste0("Data/",station,"_HydstraPOR.csv")) #saves to (example) "file: C:/Users/dcolvin/Documents/Rstuff/projects/StationHistories/Data/ORX_HydstraPOR.csv"
  rm(dt)
  print("check")
  #copyfile to Z drive
  file.copy(from = paste0("C:/Users/dcolvin/Documents/Rstuff/projects/StationHistories/Data/",station,"_HydstraPOR.csv"), 
            to = paste0("Z:/Water Quality Evaluation Section/Stations/Station Information/Station Histories/",station,"/Data/",station,"_HydstraPOR.csv"), overwrite = TRUE)
}
