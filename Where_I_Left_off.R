
#STITCHER - Written by Dillon Brownell
#This program is intended to organize data in preparation for statistical testing


#PROGRAM REQUIREMENTS
# 1. You must specify the locations of all mobile/sessile files AND the SINGLE physical data file. Also, specify whether each file is mobile or sessile with "s" or "m".
# 2. For EVERY mobile/sessile file and phys data file, change the cell format of the DATE column to NUMBER (5 digit code) and save.
# 3. Keep input file format for mobile/sessile as consistent as possible. MINIMIZE typos and extra columns. This allows greater merging power.
# 4. KEEP mobile/sessile file naming structure CONSISTENT. This is how the site AND subsite is interpreted.
# 5. Provide as much daily physical data as possible. Rows without physical data are less useful for analysis.
# 6. Assure that the file path for each file will not confuse the program's pattern recognition. Example: "file:///C:/Users/sebens/DBfiles/HROVDL.csv" will confuse the program. Keep your paths clear, and double check that sites are accurate.



#Please report questions, bugs, or suggested changes to me at dillonbrownell214@gmail.com
#Things I plan to add:
#Species count per row in new column.
#Automate date conversion and sessile/mobile determination (MOST DIFFICULT).  

#I will send a new copy of the program with these changes as soon as I make them. 






#Below is where you will specify the paths to each file, and also specify whether each file is mobile or sessile. 
# for example, mobile_and_sessile_fauna = list(list("file:///C:/Users/dillo/Desktop/HRIH3 MF.csv", "m"), list("file:///C:/Users/dillo/Desktop/HRIHC %C.csv", "s")) would be a valid input on my computer. 

#mobile_and_sessile_fauna = list(list(), list(), list())
mobile_and_sessile_fauna = list(list("file:///C:/Users/dillo/Desktop/SESSILE_1_BUT_DIFFERENT_SHOVC.csv", "s"),list("file:///C:/Users/dillo/Downloads/MOBILE_FAUNA.csv", "m"), list("file:///C:/Users/dillo/Desktop/SESSILE_FAUNA_2.5_SHIVC.csv", "s"), list("file:///C:/Users/dillo/Desktop/MOBILE_FAUNA_2.5.csv", "m"), list("file:///C:/Users/dillo/Desktop/HRIHC %C.csv", "s"), list("file:///C:/Users/dillo/Desktop/HRIHS %C.csv", "s"), list("file:///C:/Users/dillo/Desktop/HROVDL %C.csv", "s"), list("file:///C:/Users/dillo/Desktop/HROVCB %C.csv", "s"), list("file:///C:/Users/dillo/Desktop/HRIHC MF.csv", "m"), list("file:///C:/Users/dillo/Desktop/HRIH3 MF.csv", "m"), list("file:///C:/Users/dillo/Desktop/HROVCB MF.csv", "m"))

#Here the path to the physical data is specified. No extra rules apply. 
Some_Seasonal_Data = read.csv("file:///C:/Users/dillo/Desktop/SEASONAL_DATA.csv")


#Now that you have specified your file paths, go ahead and run the program. Make sure to double check the outputs for accuracy.



masterlist = list()
all_sessiles = list()
all_mobiles = list()



sites = list("SHO", "SHI", "DB", "HRI", "HRO")
subsites = list("HC", "WA", "VCB", "VDL", "VDR", "VS", "H3", "H2", "HS", "VC", "VCT")







#as.numeric(format.Date(Trimmed_File$`as.Date(Trimmed_File$DATE, origin = "1904-01-01")`, format = "%m"))










#Below are the functions called in the main loop.


#Season teller uses the $`as.Date(Trimmed_File$DATE, origin = "1904-01-01") column to determine season and year, and places them into a new column.
#This is a leftover from when seasonal averages were used instead of monthly averages, but left in because why not?
season_teller = function(date) {
  
  if (as.numeric(substr(date, 6,7)) > 0 && as.numeric(substr(date, 6,7)) <= 3){
    return (paste("Win", substr(date, 3,4), sep = ""))
  }else if(as.numeric(substr(date, 6,7)) > 3 && as.numeric(substr(date, 6,7)) <= 6){
    return (paste("Spr", substr(date, 3,4), sep = ""))
  }else if(as.numeric(substr(date, 6,7)) > 6 && as.numeric(substr(date, 6,7)) <= 9){
    return (paste("Sum", substr(date, 3,4), sep = ""))
  }else {
    return (paste("Fall", substr(date, 3,4), sep = ""))
  }
}

#season_teller_2 = function()


season_detector = function(month) {
  
  if (month > 0 && month <= 3) {
    return("Winter")
  }else if(month > 3 && month <= 6) {
    return("Spring")
  }else if(month > 6 && month <= 9) {
    return("Summer")
  }else {return("Fall")}
}




#Attatches seasons column. Not very efficient, but not worth changing.
season_attacher = function(dataset) {
  dates = dataset[[length(dataset)]]
  return(lapply(dates, season_teller))
}

#Generates averages of physical data from dates in fauna excels. Margins can be altered. (Spotminus, spot, etc.)
#Uses targets, established by the user. It is important that the physical data file has a column with 5 digit dates.
averagemaker4 = function(sebensdate, standate, target) {
  if (sebensdate %in% standate$X) {
    selected_column = standate[[target]]
    spot = match(sebensdate,standate$X)
    spotminus = spot - 30
    return(mean(na.omit(as.numeric(as.character(selected_column[spotminus:spot])))))
  }else {
    return(NaN)
  }
}


#Searches the library of site and subsite names for matches in the file path string.
#KEEP PATH UNCONFUSING
sitenamer = function(list) {
  for (name in list) {
    if (grepl(name, x[1])) {
      return(name)
    }
    
  }
  return("ERROR")
}

#Merges processed files into "master" files. Produces the master mobile and master sessile.

merger = function(input) {
  if (length(input) == 0) {
    return("No files of this type were provided")
  }
  output = input[[1]]
  if (length(input) > 1) {
    for (f in 2:length(input)) {
      output = merge.data.frame(output, input[[f]], all.x = TRUE, all.y = TRUE)
    }
  }
  
  return(output)
}

#Names the unlabeled "X.n" columns accurately. Eases merging process.

X.replacer = function(position) {
  if (grepl("X", trimmednames[position]) && grepl("X", trimmednames[position + 1]) && grepl("X", trimmednames[position - 1]) == FALSE) {
    
    newtrimmednames[position] <<- paste(trimmednames[position - 1], "_+1STD", sep = "")
    newtrimmednames[position + 1] <<- paste(trimmednames[position - 1], "_-1STD", sep = "")
    
  }
}






















#Below is the main for loop, which iterates over the file paths. Above functions are called below.
#Each file is processed individually.


#On the outputs:
#masterlist will contain all processed files in an unmerged form
#all_mobiles will contain all processed mobile files in an unmerged form
#all_sessiles will contain all processed sessile files in an unmerged form
#master_mobile will contain all processed mobile files in a merged form
#master_sessile will contain all processed sessile files in a merged form



for (x in mobile_and_sessile_fauna) {
  
  
  targets = c(4,7,10,13,16,19,22,25)
  Site = ""
  Subsite = ""
  
  
  Rawest_File = read.csv(x[[1]])
  
  Raw_File = subset(Rawest_File, duplicated(Rawest_File$DATE) == FALSE)
  
  Raw_File = subset(Raw_File, Raw_File$DATE > 0)
  
  
  #Treatment of the files deviates here briefly, depending on whether they are sessile or mobile.
  #For mobile, extra "DATE" columns are removed.
  #For sessile, NA's are turned into zero, at professor Sebens' request. This change is completely indiscriminate, all NA's become zeroes.
  if (x[2] == "m") {
    Trimmed_File <<- Raw_File[-grep("DATE.", names(Raw_File))]
  }else {
    
    Trimmed_File <<- Raw_File
    Trimmed_File[is.na(Trimmed_File)] <- 0
    
  }
  
  #Below is some code for naming the "X.n"s.
  trimmednames = names(Trimmed_File)
  newtrimmednames = trimmednames
  lapply(1:length(trimmednames), X.replacer)
  names(Trimmed_File) = newtrimmednames
  
  
  
  
  
  
  #Below any extra "X.n" files that escaped naming are removed.
  if (TRUE %in% grepl("X.", names(Trimmed_File))) {
    Trimmed_File <<- Trimmed_File[-grep("X.", names(Trimmed_File))]
  }
  
  #A conventional date column is added here.
  Trimmed_File <<- cbind(Trimmed_File, as.Date(Trimmed_File$DATE, origin = "1904-01-01"))
  
  
  
  
  
  
  
  
  
  
  
  #Attaching of the season column.
  #Trimmed_File = cbind(Trimmed_File, as.character(season_attacher(Trimmed_File)))
  
  Season = sapply(as.numeric(format.Date(Trimmed_File$`as.Date(Trimmed_File$DATE, origin = "1904-01-01")`, format = "%m")), FUN = season_detector)
  Trimmed_File = cbind(Trimmed_File, Season)
  
  Years = format.Date(Trimmed_File$`as.Date(Trimmed_File$DATE, origin = "1904-01-01")`, format = "%Y")
  Trimmed_File = cbind(Trimmed_File, Years)
  #Trimmed_File = cbind(Trimmed_File, sapply(as.numeric(format.Date(Trimmed_File$`as.Date(Trimmed_File$DATE, origin = "1904-01-01")`, format = "%m")), FUN = season_detector))
  #stinky = lapply(as.numeric(format.Date(Trimmed_File$`as.Date(Trimmed_File$DATE, origin = "1904-01-01")`, format = "%m")), FUN = season_detector)
  #Ordering the data frame's rows by DATE, ascending.  
  Trimmed_File = Trimmed_File[order(Trimmed_File$DATE, decreasing=FALSE), ]
  
  
  
  
  #Below is leftover code, but could be reused.
  #file_plus_phys = merge.data.frame(Trimmed_File, Some_Trimmed_Seasonal_Data, by.x = "as.character(season_attacher(Trimmed_File))", by.y = "Year.Season")
  
  
  #file_plus_phys = file_plus_phys[order(file_plus_phys$DATE, decreasing=FALSE), ]
  
  
  #file_plus_king = merge.data.frame(Trimmed_File, King_Data, by.x = "DATE", by.y = "Date")
  
  
  
  
  
  
  
  
  
  
  #Calling sitenamer to make some variables.  
  Site = sitenamer(sites)
  Subsite = sitenamer(subsites)
  
  
  #Making a new variable which is simply the site and subsite codes pasted together.
  Site_and_Subsite = paste(Site, Subsite, sep = "")
  
  
  #Attaching these variables to the data frame as new columns. Can be used to compare values between sites.
  Trimmed_File = cbind(Site_and_Subsite, Trimmed_File)
  Trimmed_File = cbind(Site, Trimmed_File)
  Trimmed_File = cbind(Subsite, Trimmed_File)
  
  
  
  #Creating some name lists.
  Trimmed_File_names = names(Trimmed_File)
  names_of_monthly_averages = paste(names(Some_Seasonal_Data[targets]), "_monthly_average", sep = "")
  
  
  
  
  
  #Setting up "SITE SPECIFIC"
  for (tar in 1:length(targets)) {
    if (grepl(Site, names_of_monthly_averages[tar])) {
      print("MATCH!")
      targets <<- append(targets, targets[tar])
      names_of_monthly_averages <<- append(names_of_monthly_averages, sub(Site, "SITE_SPECIFIC", names_of_monthly_averages[tar]))
    }else {
      print("NO MATCH!")
    }
  }
  
  
  
  
  
  
  #Joining name lists.
  Trimmed_File_names = append(Trimmed_File_names, names_of_monthly_averages)
  
  
  
  
  
  #Calling averagemaker, outputs put into a new list.
  prev_month_averages = list()
  
  for(t in targets) {
    prev_month_averages <<- append(prev_month_averages, list(lapply(Trimmed_File$DATE, FUN = averagemaker4, Some_Seasonal_Data, t)))
    
  }
  
  #Naming the list.
  names(prev_month_averages) = names_of_monthly_averages
  
  #Attaching the list to Trimmed_Files. Pretty inefficient code in retrospect, could be changed.
  for (p in prev_month_averages) {
    Trimmed_File <<- cbind(Trimmed_File, as.numeric(p))
    
  }
  
  #New names
  names(Trimmed_File) = Trimmed_File_names
  #At this stage, processing is finished.
  
  #Sorting the processed files into proper output locations.
  masterlist <<- append(masterlist, list(Trimmed_File))
  if (x[2] == "s") {
    all_sessiles <<- append(all_sessiles, list(Trimmed_File))
  } else {
    all_mobiles <<- append(all_mobiles, list(Trimmed_File))
  }
  
}

#Naming masterlist
names(masterlist) = mobile_and_sessile_fauna




#Calling merger.
#Here is where most warnings arise, due to discrepencies in the data names.
#Poorly merged columns appear at the end.
master_sessile = merger(all_sessiles)
master_mobile = merger(all_mobiles)


#Here the merged data frames are ordered by some column value. Can be ordered by DATE, Site, Temp, etc.
if (typeof(master_sessile) == "list") {
  master_sessile = master_sessile[order(master_sessile$DATE, decreasing=FALSE), ]
}
if (typeof(master_mobile) == "list") {
  master_mobile = master_mobile[order(master_mobile$DATE, decreasing=FALSE), ]
}



zerohunter = function(row) {
  if (row == 0) {
    return(NA)  
    }else(return(row))
}
zerotoNA = function(column) {

  return(sapply(column, FUN = zerohunter))
}



#Try some plots out here!

plot(master_mobile$Season, master_mobile$Max.Daily.Wave.Height.44013..m._monthly_average)
plot(master_sessile$Season, master_sessile$SITE_SPECIFIC.Temp.All.Dates_monthly_average)
plot(master_sessile$Season, master_sessile$Lithothamnion.spp.)
plot(master_mobile$Season, zerotoNA(master_mobile$Coryphella.verrucosa))
