
AddNewSystem = function(Name, 
                        data, 
                        MLS.description = NULL,
                        MLS.recovery.description = NULL,
                        MLS.days = 1,
                        MLS.recovery = "Good",
                        WCS.description = NULL,
                        WCS.recovery.description = NULL,
                        WCS.days = 1,
                        WCS.recovery = "Good", 
                        SHIP.code = "C21"
                        ){
  
  if(file.exists("master.Rdata")){
    load("master.Rdata")
  }else{
    master = list()
  }  
  
  new.list = list(
    "Data" = data, 
    "MLS.description" = MLS.description,
    "MLS.recovery.description" = MLS.recovery.description,
    "MLS.days" = MLS.days,
    "MLS.recovery" = MLS.recovery,
    "WCS.description" = WCS.description,
    "WCS.recovery.description" = WCS.recovery.description,
    "WCS.days" = WCS.days,
    "WCS.recovery" = WCS.recovery,
    "SHIP.code" = SHIP.code
  )
  
  
  master[[Name]] = new.list
  
 
  save(master, file = "master.Rdata")
  
   
}



#AddNewSystem("Digester 3", data[, c(1, 3,4,5,6,12, 13, 14, 15)])
#AddNewSystem("Plant - Global", data[, c(1, 2, 7, 8, 9, 10, 11, 16, 17, 18, 19)])

# expected system/master elements
# MLS.description
# MLS.recovery.description
# MLS.days
# MLS.recovery
# WCS.description
# WCS.recovery.description
# WCS.days
# WCS.recovery


# AddNewSystem(
#   Name = "Digester 3",
#    data = data[, c(1, 3,4,5,6,12, 13, 14, 15)], 
#    MLS.description = "test",
#    MLS.recovery.description = "test",
#    MLS.days = 14,
#    MLS.recovery = "Fair",
#    WCS.description = "test",
#    WCS.recovery.description = "test",
#    WCS.days = 56,
#    WCS.recovery = "Good"
# )
# 
# 
# AddNewSystem(
#   Name = "Plant - Global",
#   data = data[, c(1, 2, 7, 8, 9, 10, 11, 16, 17, 18, 19)], 
#   MLS.description = "test",
#   MLS.recovery.description = "test",
#   MLS.days = 66,
#   MLS.recovery = "Limited",
#   WCS.description = "test",
#   WCS.recovery.description = "test",
#   WCS.days = 75,
#   WCS.recovery = "Poor"
# )



# Ui helper functtions ####

CreateTab = function(i){
  index = which(names(master) %in% i)
  menuItem(paste(i), tabName = paste0("tab", index))
}
