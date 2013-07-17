network_typ_growth_distro <- function (
  nat_results_df
 ,measurementscale
 ,academic_year
 ,grade_level
 ,start_season
 ,end_season
 ,comparison_name
 ,comparison_pct_typ_growth
  #search the nat_results_df and drop/replace any matching
  #entry for this school/term/year
 ,replace_nat_results_match = FALSE
 ,de_kippify_names = TRUE
 ,de_schoolify_names = TRUE) {
  
  #subset the df
  nat <- nat_results_df[nat_results_df$Sub_Test_Name == measurementscale
                  & nat_results_df$Growth_Academic_Year == academic_year
                  & nat_results_df$Growth_Grade_Level == grade_level
                  & nat_results_df$Start_Season == start_season
                  & nat_results_df$End_Season == end_season, ]
  

  #strip KIPP from name?
  if (de_kippify_names == TRUE) {
    #corner case, TEAM
    nat$School_Display_Name <- gsub(', a KIPP school', '', nat$School_Display_Name)
    nat$School_Display_Name <- gsub('KIPP ', '', nat$School_Display_Name)
  }

  #strip school, academy, etc from name?
  if (de_schoolify_names == TRUE) {
    nat$School_Display_Name <- gsub(' Academy for Girls', '', nat$School_Display_Name)
    nat$School_Display_Name <- gsub(' Academy for Boys', '', nat$School_Display_Name)
    nat$School_Display_Name <- gsub(' Academy of', '', nat$School_Display_Name)
    nat$School_Display_Name <- gsub(' Academy', '', nat$School_Display_Name)
    nat$School_Display_Name <- gsub(' Middle School', '', nat$School_Display_Name)
    nat$School_Display_Name <- gsub(' College Preparatory', '', nat$School_Display_Name)
    nat$School_Display_Name <- gsub(' College Prep', '', nat$School_Display_Name)
    nat$School_Display_Name <- gsub(' Charter School', '', nat$School_Display_Name)
    nat$School_Display_Name <- gsub(' School', '', nat$School_Display_Name)
    nat$School_Display_Name <- gsub(' Preparatory', ' Prep', nat$School_Display_Name)
  }
  
  #add a row for comparison school
  nat$highlight_me <- 0
  new_row <- c(comparison_name, NA, NA, comparison_pct_typ_growth, NA, grade_level
              ,measurementscale, start_season, end_season, 2012, 1)
  nat_plus <- rbind(nat, new_row)

  head(nat)
  tail(nat_plus)
  #dummy x, sort order
  nat_plus$dummy_x <- rank(as.numeric(nat_plus$Perc_Growth), ties.method = 'random')
  
  nat_plus$Perc_Growth <- as.numeric(nat_plus$Perc_Growth) * 100
  
  #how do I avoid doing this awful global assignment
  kipp_rank <<- sum(comparison_pct_typ_growth * 100 <= nat_plus$Perc_Growth)
  kipp_denom <<- length(nat_plus$Perc_Growth)
  
  p <- ggplot(
    data = nat_plus
   ,aes(
      x = dummy_x
     ,y = as.numeric(Perc_Growth)
     ,fill = factor(highlight_me)
     ,label = paste(School_Display_Name, ' (', round(Perc_Growth,0) , '%)',sep = '')
    ) 
  ) + 
  geom_bar(
    stat="identity"
  ) +  
  geom_text(
    y=0.25
   ,hjust = 0
   ,vjust = 0.3
   #,angle = 90
   ,size= 1
   ,color='floralwhite'
  ) + 
  coord_flip() +  
  guides(
    fill=FALSE
  ) +
  labs(
    y = 'Percent Making Typical [Keep Up] Growth'
   ,title = 'KIPP Network Comparison'
  ) +
  scale_x_continuous(
    limits = c(0, max(nat_plus$dummy_x))
  ) +
  theme(
    #zero out cetain formatting
    panel.background = element_blank()
   ,plot.background = element_blank()
    
    #grid
   ,panel.grid.major = element_blank()
    #title and axis sizes
   ,title = element_text(size = rel(0.7))

   ,axis.title.y = element_blank()
   ,axis.text.y = element_blank()
   ,axis.ticks.y = element_blank()
    
   #,axis.title.y = element_text(size = rel(0.7))   
   ,panel.margin = unit(0,"null")
   ,plot.margin = rep(unit(0,"null"),4)
   ,axis.ticks.margin = unit(0,"null")
  ) +
  scale_fill_manual(
    values = c('gray30', 'gold1')
  ) + 
  annotate(
    geom = 'text'
   ,x = 1
   #,y = .85 * nat_plus[nat_plus$dummy_x == 1,'Perc_Growth']
   ,y = .85 *nat_plus[nat_plus$dummy_x == max(nat_plus$dummy_x),'Perc_Growth']
   ,label = paste(kipp_rank, ordinal_me(kipp_rank), ' of ', kipp_denom, sep = '')
   ,color = 'gray20'
   ,alpha = .8
   ,size = 9
   ,vjust = 1
   ,angle = 0
  )
  return(p)
}
