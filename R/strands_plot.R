#' @title Plot MAP Goal Strand Results
#'
#'
#' @description Plots a group of students' goal strand RIT scores, with each 
#' student ranked by overall RIT score.  Colors of points indicating goal strand RIT 
#' scores indicate deviation from overall RIT score; colors of overall RIT mark (|)
#' indicate national percentile rank of the overall RIT score
#'
#' @details Creates and prints a ggplot2 object showing both overall and goal strand RIT
#' scores for each student in a subets.  Subsetting students is very imporant for
#' making this visualization useful.  Original data from a \code{mapvizier} object 
#' can be subsetted by by passing unquoted field names to the \code{...} argument, a 
#' al \code{\link[dplyr]{filter}}, to which these arguments are passed. See the example 
#' below.
#' 
#' @param x a \code{mapvizier} object or data.frame with the following columns:
#' \itemize{
#'  \item  StudentID,
#'  \item StudentFirstname,
#'  \item StudentLastname,
#'  \item SchoolInitials,
#'  \item Grade,
#'  \item MeasurementScale,
#'  \item TestRITScore,
#'  \item TestPercentile,
#'  \item TestQuartile,
#'  \item Classroom,
#'  \item Year2,
#'  \item Season
#'  \item columns with of Goal Strand RIT score of interest named Goal1RitScore, Goal2RitScore, etc.
#'  \item columns with of Goal Strand Names of interest named Goal1Name, Goal2Name, etc.
#' }
#' @param ... arguments passed to \code{\link[dplyr]{filter}} use to subset \code{x}.
#' @return a ggplot2 object
#' @examples 
#' data(nweamap)
#' x<-mapvizier(nweamap)
#' strands_plot(x, 
#'              Grade==8, 
#'              Classroom=="UChicago", 
#'              MeasurementScale=="Mathematics", 
#'              Year2==2014, 
#'              Season=="Spring")
#'
#'@export
strands_plot<-function(.data, ...) {
  
  # capture filter arguments
  arg<-dots(...) # not used in this function yet    
  # validation
  if(is.mapvizier(.data)){
    .data<-dplyr::filter(.data$mapData, ...)
  } else {
    # validation
    required_cols<-c("StudentID",
                      "StudentFirstname",
                      "StudentLastname",
                      "SchoolInitials",
                      "Grade",
                      "MeasurementScale",
                      "TestRITScore",
                      "TestPercentile",
                      "TestQuartile",
                      "Classroom",
                      "Year2",
                      "Season")
    
    lapply(required_cols, function(x) { 
      if(!x %in% colnames(.data)) {
        stop(paste("Your data frame is missing the required column named:\n", x))
      }
    }
    )
    
    if(!grepl("(Goal)[0-9]RitScore", paste(colnames(.data), collapse = " "))){
      stop(paste("This function requires at least one goal strand RIT score column.\n",
                 "These columns must be named similar to Goal1RitScore./n",
                 "See ?strands_plot for more info."
                 )
           )
    }
    
    if(!grepl("(Goal)[0-9]Name", paste(colnames(.data), collapse = " "))){
      stop(paste("This function requires at least one goal strand Name column.\n",
                 "These columns must be named similar to Goal1Name",
                 "See ?strands_plot for more info."
                 )
           )
    }
    
    
    .data<-dplyr::filter(.data, ...)
  }

  
  # Munging, need to get scores and names into long format
  
  
  m.sub.scores<-dplyr::select(.data, 
                               StudentID,
                               StudentFirstname,
                               StudentLastname,
                               SchoolInitials,
                               Grade,
                               MeasurementScale,
                               TestRITScore,
                               TestPercentile,
                               TestQuartile,
                               matches("(Goal)[0-9]RitScore")
                               )
                               
   
  
  
  m.sub.names<-dplyr::select(.data, 
                              StudentID,
                              StudentFirstname,
                              StudentLastname,
                              SchoolInitials,
                              Grade,
                              MeasurementScale,
                              TestRITScore,
                              TestPercentile,
                              TestQuartile,
                              matches("(Goal)[0-9]Name")
                              )
  
  
  # melt scores
  m.melt.scores<-reshape::melt(m.sub.scores, 
                      id.vars=names(m.sub.scores)[1:9], 
                      measure.vars = names(m.sub.scores)[-c(1:9)]
  )
  
  m.melt.names<-reshape::melt(m.sub.names, 
                     id.vars=names(m.sub.names)[1:9],
                     measure.vars = names(m.sub.names)[-c(1:9)]
  )
  
  assert_that(nrow(m.melt.scores)==nrow(m.melt.names))
  
  
 # m.melt.scores2<-left_join(m.melt.scores, homerooms, by="StudentID")
#  assert_that(nrow(m.melt.scores)==nrow(m.melt.scores2))
  
  m.long<-m.melt.scores
  
  m.long$Goal_Name<-m.melt.names$value
  
  assert_that(nrow(m.long)==nrow(m.melt.names))
  
  m.long.2<-filter(m.long, !is.na(Goal_Name)) %>%
    filter(!is.na(value))

  assert_that(nrow(m.long)>=nrow(m.long.2))  
  
  m.plot<-m.long.2 %>% 
    mutate(Rank=rank(TestRITScore, ties.method="first")) %>%
             group_by(SchoolInitials, 
                      Grade,
                      MeasurementScale) %>%
             mutate(
                    StudentFullName=paste(StudentFirstname, StudentLastname),
                    StudentDisplayName=factor(StudentFullName, 
                                               levels=unique(StudentFullName)[order(Rank,decreasing = TRUE)])  
                    )
                 
  p <- ggplot(data=m.plot, 
         aes(y=Goal_Name, 
             x=value
             )
         ) +
    geom_point(aes(fill=value-TestRITScore), 
               shape=21,
               color=NA
               ) + 
    geom_vline(aes(xintercept=mean(TestRITScore)), color="gray") + 
    geom_vline(aes(xintercept=TestRITScore, color=TestQuartile), size=1.5, show_guide=T) + 
    scale_fill_gradient("Deviation from\nOverall RIT",low="red", high="green") +
    scale_color_discrete("Overall RIT Score\nQuartile") +
    xlab("RIT Score") + 
    ylab("Strand Name") +
    facet_grid(StudentDisplayName~.) + 
    theme_bw() + # xlim(150,300) +
    theme(strip.text.y=element_text(angle=0), 
          axis.text.y=element_text(size=5)#,
          #legend.position="none"
    )
  
  # return
  p
  
}