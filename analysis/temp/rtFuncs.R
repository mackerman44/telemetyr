# Author: Kevin See
# Purpose: Read in radio telemetry data
# Created: 10/02/2017
# Last Modified: 10/18/2017
# Notes:

#-----------------------------------------------------------------
# this function generates data.frame of all files within each site's folder
getFileNms = function(path = '.') {
  # list the folders
  site_nms = list.files(path)
  # keep only folders with 3 characters, to get hid of any hidden folders we don't want to read
  site_nms = site_nms[nchar(site_nms) == 3]

  if(length(site_nms) == 0) stop('No folders found')

  folders = as.list(site_nms)
  names(folders) = site_nms

  file_df = folders %>%
    map(.f = function(x) {
      list.files(paste(path, x[1], sep = "/"))
    }) %>%
    stack() %>%
    select(Site = ind,
           nm = values) %>%
    tbl_df() %>%
    mutate(fileName = paste(Site, nm, sep = "/"))

  return(file_df)
}

# This function reads in all the data from all the sites in .csv format
read.csv.data = function(path = '.') {
  
  # list all files
  file_df = getFileNms(path) %>%
    filter(grepl('.csv$', nm))
  
  file_df %>%
    select(fileName) %>%
    as.matrix() %>%
    as.character() %>%
    as.list() %>%
    map_df(.f = function(x) {
      tmp = try(read_csv(paste(path, x[1], sep = "/"),
                         col_types = c('cccccc'),
                         col_names = c('receiver',
                                       'valid',
                                       'tag_id',
                                       'start',
                                       'end',
                                       'n')))
      
      if(ncol(tmp) == 0 | class(tmp)[1] == 'try-error') return(NULL)
      
      tmp = tmp %>%
        mutate_at(vars(start, end),
                  list(~ if_else(str_count(., '\\:') == 1,
                               paste(., '00', sep = ':'),
                               .))) %>%
        mutate_at(vars(start, end),
                  list(mdy_hms)) %>%
        mutate_at(vars(valid, tag_id, n),
                  list(as.integer))
      
      return(tmp)
    })
  
} 

# this function reads in the "raw" text files
read.txt.data = function(path = '.') {
  
  # list all files
  file_df = getFileNms(path) %>%
    filter(grepl('.txt$', nm),
           !grepl('\\$', nm))
  
  
  file_df = file_df %>%
    mutate(nums = str_extract(nm, '[:digit:]+'),
           jday = if_else(nchar(nums) == 3,
                          nums,
                          str_sub(nums, 3, 5)),
           jday = as.integer(jday),
           yr = if_else(nchar(nums) == 5,
                        str_sub(nums, 1, 2),
                        if_else(jday > 200,
                                '17',
                                '18')),
           date = ymd(paste0('20', yr, '0101')) + days(jday - 1)) %>%
    arrange(Site, date)
  
  rawFile = file_df %>%
    split(list(.$fileName)) %>%
    map(.f = function(x) {
      
      tmp = try(read_table2(paste(path, x$fileName, sep = '/'),
                            skip = 3,
                            col_types = c('ctciiii'),
                            col_names = c('Date', 'Time', 'receiver', 'valid', 'freq', 'tagCode', 'sigStrgth')))
      
      if(nrow(tmp) == 0 | class(tmp)[1] == 'try-error' | tmp[1,1] == '<END>') {
        cat(paste('Problem reading in site', x$Site, ', file', x$nm))
        return(NULL)
      }
      
      tmp = tmp %>%
        filter(!is.na(valid)) %>%
        rename(orgDate = Date) %>%
        filter(orgDate != '00/00/00') %>%
        mutate(Date = dmy(orgDate)) %>%
        bind_rows(tmp %>%
                    filter(!is.na(valid)) %>%
                    rename(orgDate = Date) %>%
                    filter(orgDate == '00/00/00') %>%
                    mutate(Date = x$date)) %>%
        filter(!is.na(Time)) %>%
        arrange(Date, Time) %>%
        mutate(receiver = if_else(receiver == '000',
                                  as.character(x$Site),
                                  receiver)) %>%
        mutate(DateTime = ymd_hms(paste(year(Date), month(Date), day(Date), Time)),
               tmp = paste0(freq, if_else(nchar(tagCode) == 2,
                                          paste0(0, tagCode),
                                          as.character(tagCode))),
               tag_id = if_else(str_sub(tmp, -1) %in% c(0, 1, 2),
                                as.integer(paste0(str_sub(tmp, 1, -2), 0)),
                                if_else(str_sub(tmp, -1) %in% c(8, 9),
                                        as.integer(ceiling(as.numeric(tmp) / 10) * 10),
                                        as.integer(paste0(str_sub(tmp, 1, -2), 5))))) %>%
        select(receiver, valid, tag_id, DateTime, freq, sigStrgth) %>%
        arrange(tag_id, DateTime)
      
      return(tmp)
      
    })
  
  rawFile = rawFile[which(!sapply(rawFile, is.null))]
  
  rawDf = map_df(rawFile, .f = identity)
  
  return(rawDf)
}

# this function reads in the "raw" text files and concatenates them into the same format as the csv files. 
compress.txt.to.csv = function(path = '.') {
  
  # list all files
  file_df = getFileNms(path) %>%
    filter(grepl('.txt$', nm),
           !grepl('\\$', nm))
  
  
  file_df = file_df %>%
    mutate(nums = str_extract(nm, '[:digit:]+'),
           jday = if_else(nchar(nums) == 3,
                          nums,
                          str_sub(nums, 3, 5)),
           jday = as.integer(jday),
           yr = if_else(nchar(nums) == 5,
                        str_sub(nums, 1, 2),
                        if_else(jday > 200,
                                '17',
                                '18')),
           date = ymd(paste0('20', yr, '0101')) + days(jday - 1)) %>%
    arrange(Site, date)
  
  csvFile = file_df %>%
    split(list(.$fileName)) %>%
    map(.f = function(x) {
      
      tmp = try(read_table2(paste(path, x$fileName, sep = '/'),
                            skip = 3,
                            col_types = c('ctciiii'),
                            col_names = c('Date', 'Time', 'receiver', 'valid', 'freq', 'tagCode', 'sigStrgth')))
      
      if(nrow(tmp) == 0 | class(tmp)[1] == 'try-error') {
        cat(paste('Problem reading in site', x$Site, ', file', x$nm))
        return(NULL)
      }
      
      tmp = tmp %>%
        filter(!is.na(valid)) %>%
        rename(orgDate = Date) %>%
        filter(orgDate != '00/00/00') %>%
        mutate(Date = dmy(orgDate)) %>%
        bind_rows(tmp %>%
                    filter(!is.na(valid)) %>%
                    rename(orgDate = Date) %>%
                    filter(orgDate == '00/00/00') %>%
                    mutate(Date = x$date)) %>%
        filter(!is.na(Time)) %>%
        arrange(Date, Time) %>%
        mutate(receiver = if_else(receiver == '000',
                                  as.character(x$Site),
                                  receiver)) %>%
        mutate(DateTime = ymd_hms(paste(year(Date), month(Date), day(Date), Time)),
               tmp = paste0(freq, if_else(nchar(tagCode) == 2,
                                          paste0(0, tagCode),
                                          as.character(tagCode))),
               tag_id = if_else(str_sub(tmp, -1) %in% c(0, 1, 2),
                               as.integer(paste0(str_sub(tmp, 1, -2), 0)),
                               if_else(str_sub(tmp, -1) %in% c(8, 9),
                                       as.integer(ceiling(as.numeric(tmp) / 10) * 10),
                                       as.integer(paste0(str_sub(tmp, 1, -2), 5))))) %>%
        select(receiver, valid, tag_id, DateTime) %>%
        arrange(tag_id, DateTime) %>%
        group_by(tag_id) %>%
        mutate(lastTime = lag(DateTime),
               diff = as.numeric(difftime(DateTime, lastTime, units = 'mins')),
               sameGrp = if_else(diff > 5 | is.na(diff),
                                 F, T)) %>%
        select(receiver:tag_id, lastTime,
               ObsTime = DateTime,
               diff, sameGrp) %>%
        mutate(nxtGrp = lead(sameGrp),
               grp = NA) %>%
        ungroup()
      
      tmp$grp[1] = 1
      for(i in 2:nrow(tmp)) {
        if(tmp$tag_id[i] != tmp$tag_id[i - 1] ) {
          tmp$grp[i] = 1
        }
        
        if(tmp$tag_id[i] == tmp$tag_id[i - 1] ) {
          tmp$grp[i] = if_else(tmp$sameGrp[i],
                               tmp$grp[i - 1],
                               tmp$grp[i - 1] + 1)
          
        }
        
      }
      
      myCsv = tmp %>%
        group_by(receiver, tag_id, grp) %>%
        summarise(start = min(ObsTime),
                  end = max(ObsTime),
                  n = n()) %>%
        ungroup() %>%
        select(-grp) %>%
        mutate(valid = as.integer(1)) %>%
        select(receiver, valid, tag_id, start:n)
      
      return(myCsv)
      
    })
  
  csvFile = csvFile[which(!sapply(csvFile, is.null))]
  
  return(csvFile)
}


# this function reads all the on/off data
readOnOffData = function(path = '.') {
  # list all files
  file_df = getFileNms(path) %>%
    filter(grepl('\\$', fileName),
           !grepl('\\$\\$', fileName)) %>%
    select(Site, fileName)

  my_df = NULL
  for(i in 1:nrow(file_df)) {
    my_df = my_df %>%
      bind_rows(read_table(paste(path, file_df$fileName[i], sep = "/"),
                           col_names = F) %>%
                  mutate(Site = file_df$Site[i]))
  }
  my_df = my_df %>%
    select(Site,
           onOff = X1,
           date = X2,
           time = X3)

  return(my_df)
}

# this function reads all the voltage and temperature data
readVoltTempData = function(path = '.') {
  # list all files
  file_df = getFileNms(path) %>%
    filter(grepl('\\$\\$', fileName)) %>%
    select(Site, fileName)

  my_df = NULL
  for(i in 1:nrow(file_df)) {
    my_df = my_df %>%
      bind_rows(read_table(paste(path, file_df$fileName[i], sep = '/'),
                           col_names = T,
                           skip = 1) %>%
                  slice(-1) %>%
                  rename(VoltAvg = AVG,
                         VoltMin = MIN,
                         VoltMax = MAX,
                         TempAvg = AVG_1,
                         TempMin = MIN_1,
                         TempMax = MAX_1) %>%
                  mutate(Site = file_df$Site[i]) %>%
                  select(Site, everything()))
  }

  return(my_df)
}

#-----------------------------------------------------------------

# group sequential detections on the same site
addGrpSite = function(detectDF) {
  j = 1
  for(i in 1:(nrow(detectDF))) {
    if(i == 1) {
      detectDF$grp[i] = j
      next
    }
    if(detectDF$tag_id[i] == detectDF$tag_id[i - 1] &
       detectDF$site[i] == detectDF$site[i - 1]) {
      detectDF$grp[i] = j
    }
    if(detectDF$tag_id[i] != detectDF$tag_id[i - 1] |
       detectDF$site[i] != detectDF$site[i - 1]) {
      j = j + 1
      detectDF$grp[i] = j
    }
  }
  res = detectDF %>%
    group_by(tag_id) %>%
    mutate(grp = grp - min(grp) + 1) %>%
    group_by(tag_id, site, grp) %>%
    summarise(start = min(start, na.rm = T),
              end = max(end, na.rm = T),
              n = sum(n)) %>%
    ungroup() %>%
    arrange(tag_id, grp)
  
  # make it recursive, to keep adding detections together and filtering until complete
  if(nrow(res) == nrow(detectDF)) return(res)
  if(nrow(res) < nrow(detectDF)) return(addGrpSite(res))
}

# group sequential detections on the same receiver
addGrpRec = function(detectDF) {
  j = 1
  for(i in 1:(nrow(detectDF))) {
    if(i == 1) {
      detectDF$grp[i] = j
      next
    }
    if(detectDF$tag_id[i] == detectDF$tag_id[i - 1] &
       detectDF$site[i] == detectDF$site[i - 1]) {
      detectDF$grp[i] = j
    }
    if(detectDF$tag_id[i] != detectDF$tag_id[i - 1] |
       detectDF$site[i] != detectDF$site[i - 1]) {
      j = j + 1
      detectDF$grp[i] = j
    }
  }
  res = detectDF %>%
    group_by(tag_id) %>%
    mutate(grp = grp - min(grp) + 1) %>%
    group_by(tag_id, site, receiver, grp) %>%
    summarise(start = min(start, na.rm = T),
              end = max(end, na.rm = T),
              n = sum(n)) %>%
    ungroup() %>%
    arrange(tag_id, grp)
  
  # make it recursive, to keep adding detections together and filtering until complete
  if(nrow(res) == nrow(detectDF)) return(res)
  if(nrow(res) < nrow(detectDF)) return(addGrpRec(res))
}

#-----------------------------------------------------------------

# drop upstream detections
dropUpStrm = function(detectDF, siteOrder) {
  # detectDF is the detection dataframe
  # siteOrder is a data.frame containing site code and site_order. The colname of site code in detectDF should match the colname of site code in siteOrder
  detectDF %>%
    left_join(siteOrder) %>%
    split(list(.$tag_id)) %>%
    map_df(.f = function(x) {
      x %<>%
        mutate(nextSite = lead(site_order))
      
      while(sum(x$nextSite < x$site_order, na.rm = T) > 0) {
        probRow = which(x$nextSite < x$site_order)[1] + 1
        
        x = x %>%
          slice(-probRow) %>%
          mutate(nextSite = lead(site_order))
      }
      
      x %>%
        select(-nextSite)
    })
}

#-----------------------------------------------------------------

# filter tag locations by tags and time steps for selected tags
compileTagDataFrame = function(tagLocAll,
                               tags) {
  tagLoc = tagLocAll %>%
    filter(tag_id %in% tags)
  
  # drop some time steps after tags have stopped moving
  tagMaxDist = tagLoc %>%
    group_by(tag_id) %>%
    summarise_at(vars(maxDist = distKm),
                 list(max),
                 na.rm = T) %>%
    ungroup()
  
  maxTimeStep = tagLoc %>%
    left_join(tagMaxDist) %>%
    filter(distKm == maxDist) %>%
    group_by(tag_id) %>%
    filter(timeStepNum == min(timeStepNum)) %>%
    ungroup() %>%
    select(timeStepNum) %>%
    filter(timeStepNum == max(timeStepNum)) %>%
    as.matrix() %>%
    as.integer()
  
  minTimeStep = tagLoc %>%
    filter(site1 == 'Rel') %>%
    filter(timeStepNum == min(timeStepNum)) %>%
    select(timeStepNum) %>%
    distinct() %>%
    as.matrix() %>%
    as.integer()
  
  tagLoc %<>%
    filter(timeStepNum >= max(1, minTimeStep - 10),
           timeStepNum <= min(max(tagLoc$timeStepNum), maxTimeStep + 10))
  
  return(tagLoc)
}

