#' Get the FY period number and year for given dates
#'
#' It takes character values
#' @param startDate is a character / Date
#' @param endDate accepts character / Date
#' @param periodType may be week,month,year
#' @return Return data frame with period numbers for the give date ranges with Year
#' @export

getPeriodNumbers <- function(startDate,endDate,periodType){
  reqdate = seq(as.Date(startDate),as.Date(endDate),"days")
  Period=c()
  Year=c()
  count=1
  for(date in reqdate){
    date = as.Date(date,origin = "1970-01-01")
    if(month(date) %in% 1:3){
      year = year(date)-1
    }else{
      year=year(date)
    }
    start = paste0(year,"-04-01")
    if(periodType=="week"){
      if(date==start){
        week=1
      }else if(date!=start & count==1){
        week=1
        tempdate = start
        tempdate=as.Date(tempdate, origin="1970-01-01")
        while(tempdate<date){
          if(weekdays(tempdate)=="Monday"){
            week=week+1
          }
          tempdate=tempdate+1
        }
      }else if(date!=start & weekdays(date)=="Monday"){
        week = week+1
      }
      Period= append(Period,week)
    }else if(periodType=="month"){
      Period=append(Period,month(date))
    }else if(periodType=="year"){
      Period=append(Period,year)
    }
    Year=c(Year, paste0(year,"-",year+1))
    count = count+1
  }
  periodNumberDF = data.frame(Date=reqdate, Period=Period, Year=Year)
  return(periodNumberDF)
}

#' Get week-1 soh using sales happened in that week and soh of next week
#'
#' @param soh_period accepts data frame (soh with edited period numbers for given date ranges by increasing the period number of data
#' @param sales_with_period is a dataframe sales data merge with period
#' @return soh is dataframe i.e soh for week1
#' @export

getWeekOneSoh <- function(soh_period,sales_with_period){
  soh = subset(soh_period,soh_period$Period==2)
  if(nrow(soh)!=0){
    soh$Period=1
    print("Merging week-1 sales and week-2 soh, to get soh for week-1")
    sales = subset(sales_with_period,sales_with_period$Period==1)
    soh = rbind(sales,soh)
  }else{
    print("week1 is not exists in the given dates range")
    soh=NULL
  }
  return(soh)
}
#' Get soh for the starting period for the given dates if minimum of period is not equal to 2
#'
#' @param soh_period soh data for given date ranges by increasing the period number of data
#' @param sales_with_period sales data merge with period
#' @param minperiod as integer starting period other than 2
#' @param minYear accepts character data (i.e starting year of the data)
#' @return soh is dataframe having soh for the starting period of given dates
#' @export

getSohForStartingWeekOfDate <- function(soh_period,sales_with_period,minperiod,minYear){
  if(minperiod!=2){
    soh = subset(soh_period,soh_period$Period==minperiod & soh_period$Year==minYear)
    soh$Period=minperiod-1
    sales = subset(sales_with_period,sales_with_period$Period==minperiod-1)
    soh = rbind(sales,soh)
  }else{
    cat("minimum of week is: ",minperiod,"It's already covered in week1 soh \n")
    soh =NULL
  }
  return(soh)
}

#' Add unsold items from soh data into sales data
#'
#' @param salesDF sales dataframe
#' @param sohDF soh dataframe
#' @param i is integer takes 1 & 3 for i to differeniate training and periods
#' @return finally return a data frame by combining sales and soh.
#' @export

addUnsoldItemsIntoSales <- function(salesDF,sohDF,i){
  sohDF=sohDF[sohDF$Sales_Qty>0,]
  salesDF=salesDF[salesDF$Sales_Qty>0,]
  print("Removed negatives from soh data")
  if(nrow(salesDF)!=0){
    unsolditems= anti_join(sohDF,salesDF, by=c("Sku","Period","Year","Sales_Qty","Store_Name"))
    print(nrow(unsolditems))
    onlysales=anti_join(salesDF,sohDF, by=c("Sku","Period","Year","Sales_Qty","Store_Name"))
    sohdata= rbind(onlysales,sohDF)
    sohdata = as.data.frame(sohdata)
    #Adding soh column to the data
    sohdata =sohdata[colnames(sohdata) %in% c("Sku","Period","Year","Sales_Qty","Store_Name")]
    names(sohdata)[names(sohdata)=="Sales_Qty"]="Soh_Qty"
    print("changes column name to soh_Qty")
    unsolditems$Sales_Qty=0
    unsolditemsIntoSales = rbind(salesDF,unsolditems)
    unsolditemsIntoSales=merge(unsolditemsIntoSales,sohdata,by=c("Sku","Period","Year","Store_Name"),all.y=T)
    #update soh_Qty column with sales_Qty where sales_Qty is grater than soh_Qty
    index= which(unsolditemsIntoSales$Sales_Qty >unsolditemsIntoSales$Soh_Qty)
    unsolditemsIntoSales$Soh_Qty[index]=unsolditemsIntoSales$Sales_Qty[index]
  }else if(nrow(salesDF)==0 & i==1){
    print("No sales data availble for the given training period")
    break
  }else if(nrow(salesDF)==0 & i==3){
    unsolditemsIntoSales=sohDF
    unsolditemsIntoSales$Soh_Qty=sohDF$Sales_Qty
  }
  return(unsolditemsIntoSales)
}

#' RGB values for given color hexcode
#' Get the color hexcodes with their RGB values into a dataframe
#' @param datawithcolorcodes accepts dataframe
#' @return tempDF data frame which have a rgb values of hex code with their color codes
#' @export
addRGBValuesToColor<- function(datawithcolorcodes){
  colorsInGivenPeriod = c(levels(as.factor(datawithcolorcodes$`COLOR-1`)),levels(as.factor(datawithcolorcodes$`COLOR-3`)),levels(as.factor(datawithcolorcodes$`COLOR-2`)))
  trim_attribute = substr(colorsInGivenPeriod,1, nchar(colorsInGivenPeriod)-2)
  LegacyColors = c("DENIM 01","DENIM 02","DENIM 04","DENIM 05","DENIM 06","DENIM 07","DENIM 08","105-1","105-2","475571-1","475571-2","475571-7","475571-9","49120-3","951236-4","2471-5","82103-6","82103-8","82103-10","Dec-")
  colordf = data.frame(TrimmedDBColorCode=trim_attribute,DBColorCode=colorsInGivenPeriod)
  revisedColorsFromDB = subset(colordf,!colordf$TrimmedDBColorCode %in%  LegacyColors)
  colorNames = read.csv("/home/anjali/Rscripts/Inferneon-Scripts/ProlineColorsWithHexCodes.csv", strip.white = T)
  newcolors = setdiff(trimws(revisedColorsFromDB$TrimmedDBColorCode,which="both"),colorNames$Revised.color.code.Reqd)
  if(length(newcolors) !=0){
    print("We are seeing new color codes in DB")
    newColorDF = subset(revisedColorsFromDB,revisedColorsFromDB$TrimmedDBColorCode %in% newcolors)
    write.csv(newColorDF,file="/home/anjali/newColorsInDB.csv", row.names = F)
  }else{
    ColorCodeWithColorNames = merge(revisedColorsFromDB,colorNames, by.x="TrimmedDBColorCode",by.y="Revised.color.code.Reqd")
    ColorCodeWithColorNames=ColorCodeWithColorNames[!duplicated(ColorCodeWithColorNames),]
    #finalcolorDF= dcast(mergeColorCodeWithColorNames,TrimmedDBColorCode+HexCode~DBColumnName, value.var='DBColorCode')
    rgbDF = col2rgb(ColorCodeWithColorNames$HexCode)
    rgbDF=t(rgbDF)
    print("Adding RGB values to the color code")
    tempcolorDF=cbind(ColorCodeWithColorNames,Red=rgbDF[,"red"],Green=rgbDF[,"green"],Blue=rgbDF[,"blue"])
    colorWithRgbValues = subset(tempcolorDF,select =-c(TrimmedDBColorCode,Color.Name,HexCode))
  }
  tempDF=datawithcolorcodes
  for(colorcolum in c("COLOR-1","COLOR-2","COLOR-3")){
    index = substr(colorcolum,nchar(colorcolum),nchar(colorcolum))
    data = merge(tempDF,colorWithRgbValues, by.x=colorcolum,by.y="DBColorCode")
    setnames(data,c("Red","Green","Blue"),c(paste0("Color",index,".Red"),paste0("Color",index,".Green"),paste0("Color",index,".Blue")))
    tempDF=data
  }
  return(tempDF)
}

#' Close DB connections

closeDbConnection<- function(){
  cons = dbListConnections(MySQL())
  for(con in cons){
    dbDisconnect(con)
  }
}

#' Create csv files from given data frame
#' It takes a all stores data with all attributes and save the data into csv files by store/producucttype.
#' @param finalDF It takes dataframe as input
#' @return create a csv in the selected path
#' @export

dataSavingIntoCsvFiles = function(finalDF){
  drops = c("Category..L5.","Store_Name","seasonName","Style")
  for(store in Stores){
    setwd("/home/anjali")
    if(dir.exists(store)){
      unlink(store, recursive = T)
    }
    dir.create(store)
    setwd(paste0("/home/anjali/",store))
    print("folder is created with storeName")
    for(attributeValue in Products$Attribute_Value){
      attributeDF = subset(finalDF,finaldata$Category..L5.==attributeValue)
      if(nrow(attributeDF)!=0){
        attributeDF[drops] = NULL
        write.csv(attributeDF,file=paste0(getwd(),"/",attributeValue,".csv"), row.names=F)
      }
    }
    cat("Finally data saved into files in local for store: ",store)
  }
}