#' @title plot method for \code{mapvizierSummary} class
#'
#' @description
#'  Plots a \code{ggplot2} object from a \code{mapvizierSummary} object
#'
#' @details Creates and prints a \code{ggplot2} object from 
#' a \code{mapvizierSummary} object.  User can choose subjects, growth season, 
#' grades, and years to subest data prior to plotting
#' @param x a \code{mapvizierSummary} object 
#' @param growthseason a character string denoting test term name (i.e. growth seaons):
#' \itemize{
#' \item "Spring - Spring"
#' \item "Fall - Spring"
#' \item "Fall - Winter"
#' \item "Winter - Spring"
#' \item "Fall - Fall"
#' }
#' @param subjects character string to subset subjects/measurement scales by:
#' \itemize{
#' \item "Mathematics"
#' \item "Reading"
#' \item "General Science"
#' \item "Language Usage"
#' }
#' @param grades integer vector to subset grades by (NB: Kindergarten should be denoted 0)
#' @param years integer vector with years of second test year in term names (i.e., 
#' to plot results from SY2010-2011, SY 2011-2012, and SY 2012-2013, you should pass the 
#' vector \code(c(2011, 2012, 2013)) or \code(c(2011:2013))).
#' @param metric character value indicating which MAP measurement to plot. Choices are:
#' \itemize{
#' \item "Typical": % Meets/Exceeds\nTypical Growth,
#' \item "CR": % Meets/Exceeds\nCollege Ready Growth,
#' \item "50th" = % Greater than\n50th Percentile, Season 2,
#' \item "75th" = % Greater than\75th Percentile, Season 2.
#' }
#' 
#' @return a \code{gglot2} object
#' @rdname plot.mapvizierSummary
#' @export
#' @examples 
#' data(nweamap)
#' x<-mapvizier(nweamap)
#' x.sum<-summary(x)
#' plot(x.summ)

plot.mapvizierSummary <-  function(x, 
                                   growthseason="Fall - Spring", 
                                   subjects=c("Reading", 
                                              "Mathematics", 
                                              "General Science"),
                                   grades=c(2:8),
                                   years=NULL,
                                   metric="Typical"){
  require(ggplot2)
  x<-as.data.frame(x)
  plot_data <- filter(x, 
                      GrowthSeason == growthseason,
                      Subject %in% subjects,
                      Grade %in% grades
  )
  
  if(!is.null(years)) {
    plot_data<-mutate(plot_data, Year2=
                      as.integer(gsub("(\\d)+\\-(\\d+)", 
                                      "\\2", 
                                      SY))) %>%
      dplyr::filter(Year2 %in% years)
  }
  
  
  
  y<-switch(metric,
            "Typical" = "Pct_Typical",
            "CR" = "Pct_CollegeReady",
            "50th" = "Pct_50th_Pctl_S2",
            "75th" = "Pct_75th_Pctl_S2"
            )
  y_call<-paste0(y, "*100")

 
  y_axis_label <- switch(metric,
                         "Typical" = "% Meets/Exceeds\nTypical Growth",
                         "CR" = "% Meets/Exceeds\nCollege Ready Growth",
                         "50th" = "% Greater than\n50th Percentile, Season 2" ,
                         "75th" = "% Greater than\75th Percentile, Season 2"
  )
  
  p.long<-ggplot(plot_data, 
                 aes_string(x='gsub("20","",SY)', 
                     y=y_call
                 )
  ) + 
    geom_line(aes(group=School, color=School)) +
    geom_point(color="white", size=8.75) +
    geom_hline(aes(yintercept=80), color="lightgray") +
    geom_text(aes_string(label=paste('paste(',y_call,',"%",sep="")'), 
                  color="School"),
              size=3) +
    #scale_color_manual(values = c("#439539", "purple", "#60A2D7", "#C49A6C")) +
    facet_grid(Subject~Grade) +
    theme_bw() + 
    theme(legend.position="bottom") +
    xlab("School Year") +
    ylab(y_axis_label)  
  
  # return
  p.long
}