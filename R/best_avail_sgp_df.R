best_avail_sgp_df <- function (
  df
 ,prefer_period = 'Spring to Spring'
 ,backup_period_1 = 'Fall to Spring' 
 ,backup_period_2 = 'Winter to Spring') {

 #make 3 dfs, and then merge into 1 best 
 
 #first turn the data frame into a data table
 #data table methods are fast!
 dt <- data.table(df)
 
 #index PERIOD_STRING to speed things up
 setkey(dt, PERIOD_STRING)
 
 #subset into 3 data tables
 preferred <- copy(dt[prefer_period])
 backup_1 <- copy(dt[backup_period_1])
 backup_2 <- copy(dt[backup_period_2])
  
 #get uniques in df
 #record is defined by studentid, (ending) year, measuremenscale
 unq_key <- c('STUDENTID', 'YEAR', 'MEASUREMENTSCALE')
 uniques <- unique(df[, unq_key])
 
 #set key
 setkey(preferred,STUDENTID, YEAR, MEASUREMENTSCALE)
 setkey(backup_1,STUDENTID, YEAR, MEASUREMENTSCALE)
 setkey(backup_2,STUDENTID, YEAR, MEASUREMENTSCALE)

 #enter these as a vector of strings
 cols_to_get <- c(
   'STUDENTID', 'LASTFIRST', 'STUDENT_NAME'
  ,'YEAR', 'MEASUREMENTSCALE', 'SCH_ABBREV'
  ,'START_TERM_VERIF', 'END_TERM_VERIF'
  ,'START_RIT', 'END_RIT'
  ,'START_NPR', 'END_NPR'
  ,'RIT_CHANGE'
  ,'START_TERM_STRING', 'END_TERM_STRING'
  ,'START_GRADE_VERIF', 'END_GRADE_VERIF'
  ,'REPORTED_GROWTH_PROJECTION'
  ,'MET_TYPICAL_GROWTH_TARGET'
  ,'GROWTH_PERCENTILE'
  ,'VALID_OBSERVATION')
 
 #merge preferred
 #nb - with = FALSE is essential to geto cols to behave...
 preferred_merged <- preferred[uniques, cols_to_get, with=FALSE]
 
 setkey(preferred_merged, VALID_OBSERVATION)
 
 #matched and unmatched rows...
 pref_matched <- preferred_merged[VALID_OBSERVATION == 1,]
 unmatched <- preferred_merged[VALID_OBSERVATION == 0,]
 unmatched_key <- unmatched[,list(STUDENTID, YEAR, MEASUREMENTSCALE)]
 nrow(unmatched) + nrow(pref_matched)
 
 #backup 1
 backup_1_merge <- backup_1[unmatched_key, cols_to_get, with=FALSE]
 
 #matched and unmatched rows
 backup_1_matched <- backup_1_merge[VALID_OBSERVATION == 1,]
 unmatched <- backup_1_merge[VALID_OBSERVATION == 0,]
 unmatched_key <- unmatched[,list(STUDENTID, YEAR, MEASUREMENTSCALE)]
 nrow(unmatched) + nrow(backup_1_matched) + nrow(pref_matched)
  
 #backup 2
 backup_2_merge <- backup_2[unmatched_key, cols_to_get, with=FALSE]
 backup_2_matched <- backup_2_merge[VALID_OBSERVATION == 1,]
 unmatched <- backup_2_merge[VALID_OBSERVATION == 0,]
 nrow(unmatched) + nrow(backup_1_matched) + nrow(backup_2_matched) + nrow(pref_matched)
 
 ##COMBINE
 final <- rbind(pref_matched, backup_1_matched, backup_2_matched, unmatched)
 
 return(final)  
}
