#' @title Chris Haid's Waterfall-Rainbow-Arrow Chart
#'
#' @description
#' \code{haid_plot} returns a ggplot object showing per student MAP performance 
#'
#' @details 
#' This function builds and prints aa graphich that plots MAP performance over one
#' or two seasons. RIT scores are color coded by percentile. If a second season is passed to the function (in \code{end_col_name}),
#' then arrows between estimated RIT scores for the two seasons are added to plot and are color coded
#' based on the amount of growth attained (postive, typical, college ready, and negative)
#' 
#' @param df data.frame with columns for  RIT scores (start and/or end season), predicted typical and college ready
#' growth, growth status, etc.
#' @param student_col_name string identifying colummn in \code{df} of student names.
#' @param start_col_name string identifying colummn in \code{df} of first term's RIT score.
#' @param end_col_name string identifying colummn in \code{df} of second term's RIT score.
#' @param start_percentile_col_name string identifying colummn in \code{df} with first terms NPR.
#' @param end_percentile_col_name string identifying colummn in \code{df} with second terms NPR.
#' 
#' @param keep_up_col_name string identifying colummn in \code{df} typical (or project) term 1 to term 2 growth.
#' @param col_ready_col_name string identifying colummn in \code{df} college ready term 1 to term 2 growth
#season names.
#' @param start_season_abbrev string indicating term 1's name (e.g. Fall).
#' @param end_season_abbrev string indicating term 2's name (e.g. Spring).
#' 
#' @param growth_status_col_name string identifying column in \code{df} indicating students growth status.
#' @param name_status_col_name ??? Thoughts Andrew???
#' @param y_sort_column string identifying column in \code{df} used to sort students vertically.
#' 
#' @param p_title = '' string used for title.
#' @param p_arrow_colors vector of colors passed to ggplot.
#' @param p_arrow_tiers vector of colors passed to ggplot.
#' @param p_name_colors vector of colors passed to ggplot.
#' @param p_name_color_tiers vector of colors passed to ggplot.
#' @param p_quartile_colors vector of colors passed to ggplot.
#' @param p_name_size sets point size of student names.
#' @param p_alpha sets level fo transparency for goals. 
#' 
#' @return prints a ggplot object
#' @export
#' 
#' @examples 
#' 
#' haid_plot(...)

haid_plot <- function(
  df
  #column identifiers
  ,student_col_name
  #RIT
  ,start_col_name
  ,end_col_name
  #percentile 
  ,start_percentile_col_name
  ,end_percentile_col_name
  #goals
  ,keep_up_col_name
  ,col_ready_col_name
  #season names
  ,start_season_abbrev
  ,end_season_abbrev
  #other
  ,growth_status_col_name
  ,name_status_col_name
  ,y_sort_column
  
  #titles and axis
  ,p_title = ''
  
  #look and feel
  #arrows
  ,p_arrow_colors
  ,p_arrow_tiers
  #names
  ,p_name_colors
  ,p_name_color_tiers
  #quartiles
  ,p_quartile_colors = c('#f3716b', '#79ac41', '#1ebdc2', '#a57eb8')
  ,p_name_size = 3
  ,p_alpha = 1
) {
  require(ggplot2)
  require(stringr)
  require(data.table)
  require(scales)
  #thematic stuff
  pointsize <- 2.5
  segsize <- 1
  annotate_size <- 5
  
  #copy the sort column to a new column
  df$sort_column <- df[ , y_sort_column]
  #data transformations
  #haid plot is self-munging!
  #the theory here is that it is easier to work inside of ggplot's calling environment
  #IF YOU KNOW WHAT THINGS ARE CALLED.  so let's have the user pass a series of identifiers
  #that indicate which column in their df has key data.  then we'll rename the headers of
  #the passed df so that our transformations and ggplot calls can be as simple as possible. 
  colnames(df)[colnames(df) == student_col_name] <- 'student_name'
  #RIT
  colnames(df)[colnames(df) == start_col_name] <- 'base_rit'
  colnames(df)[colnames(df) == end_col_name] <- 'end_rit'
  #percentile
  colnames(df)[colnames(df) == start_percentile_col_name] <- 'baseline_percentile'
  colnames(df)[colnames(df) == end_percentile_col_name] <- 'endpoint_percentile'
  #goals
  colnames(df)[colnames(df) == keep_up_col_name] <- 'keep_up_goal'
  colnames(df)[colnames(df) == col_ready_col_name] <- 'college_ready_goal'
  #other
  colnames(df)[colnames(df) == growth_status_col_name] <- 'growth_status'
  colnames(df)[colnames(df) == name_status_col_name] <- 'name_status'
  
  #if a student doesn't have a base rit, plot will break
  ommitted_count <- sum(is.na(df$base_rit))
  df <- df[!is.na(df$base_rit), ]
  
  num_stu <- nrow(df)
  
  stopifnot(
    length(df$base_rit) > 0
   ,length(df$end_rit) > 0
  )
  
  #make a psuedo-axis by ordering based on one variable
  df$y_order <- rank(
    x = df[ , 'sort_column']
    ,ties.method = "first"
    ,na.last = FALSE
  )
  
  df$growth_status = factor(
    x = df$growth_status
    ,levels = p_arrow_tiers
    ,ordered = TRUE
  )
  
  #quartile conversions    
  df$baseline_quartile <- 1 + floor(df$baseline_percentile / 25)
  df$endpoint_quartile <- 1 + floor(df$endpoint_percentile / 25)
  
  #tag rows pos / neg change
  df$neg_flag <- ifelse(df$end_rit <= df$base_rit, 1, 0)
  
  #tag names
  df$student_name_format <- ifelse(df$neg_flag == 1, df$student_name, paste(df$student_name, df$base_rit))
  #NAs
  df$student_name_format <- ifelse(is.na(df$student_name_format), df$student_name, df$student_name_format)    
  
  #composite name position vector - if growth is NEGATIVE, use the endpoint
  df$name_x <- ifelse(df$neg_flag == 1, df$end_rit - 1.5, df$base_rit - 0.25)
  #NAs
  df$name_x <- ifelse(is.na(df$name_x), df$base_rit - 0.25, df$name_x)
  
  df$rit_xoffset <- ifelse(df$neg_flag == 1, -.25, .25)
  df$rit_hjust <- ifelse(df$neg_flag == 1, 1, 0)
  
  #colors for identity!
  arrow_colors <- data.frame(
    status = p_arrow_tiers
    ,color = p_arrow_colors
    ,stringsAsFactors = FALSE
  )
  #cribbing off of 'subscripting' http://rwiki.sciviews.org/doku.php?id=tips:data-frames:merge
  df$arrow_color_identity <- arrow_colors$color[match(df$growth_status, arrow_colors$status)]
  
  #start/end quartile colors
  quartile_colors <- data.frame(
    quartile = c(1,2,3,4)
    ,color = p_quartile_colors
    ,stringsAsFactors = FALSE
  )
  
  df$baseline_color <- quartile_colors$color[match(df$baseline_quartile, quartile_colors$quartile)]
  df$endpoint_color <- quartile_colors$color[match(df$endpoint_quartile, quartile_colors$quartile)]
  
  #name colors
  name_colors <- data.frame(
    tier = p_name_color_tiers
    ,color = p_name_colors
    ,stringsAsFactors = FALSE
  )
  
  df$name_color <- name_colors$color[match(df$name_status, name_colors$tier)]
  
  df$base_quartile_format <- paste('Quartile', as.factor(df$baseline_quartile))
  
  #base ggplot object
  p <- ggplot(
    data = df
    ,aes(
      x = base_rit
      ,y = y_order
    )
  )
  
  #typical and college ready goal lines (want these behind segments)
  p <- p + 
    geom_point(
      aes(
        x = base_rit + keep_up_goal
      )
      ,size = pointsize - 0.5
      ,shape = '|'
      ,color = '#CFCCC1'
      ,alpha=p_alpha
    ) + 
    geom_point(
      aes(
        x = base_rit + college_ready_goal
      )
      ,size = pointsize - 0.5
      ,shape = '|'
      ,color = '#FEBC11'
      ,alpha=p_alpha
    )  
  
  #typical and college ready goal labels
  p <- p +
    geom_text(
      aes(
        x = base_rit + keep_up_goal
        ,label = base_rit + keep_up_goal
      )  
      ,color = "#CFCCC1"
      ,size = pointsize - 0.5 
      ,hjust = 0.5
      ,vjust = 0
      ,alpha=p_alpha
    ) + 
    geom_text(
      aes(
        x = base_rit + college_ready_goal
        ,label = base_rit + college_ready_goal
      )  
      ,color = "#FEBC11"
      ,size = pointsize - 0.5 
      ,hjust = 0.5
      ,vjust = 0
      ,alpha=p_alpha
    )
  
  #only do the following if there is data in end rit
  if (sum(!is.na(df$end_rit)) > 0) {
    #add segments
    p <- p +
      geom_segment(
        data = df[!is.na(df$end_rit), ]
        ,aes(
          xend = end_rit
          ,yend = y_order
          ,group = arrow_color_identity
          ,color = arrow_color_identity
        )
        ,arrow = arrow(length = unit(0.1,"cm"))
      ) + 
      scale_color_identity()  

    #add RIT text
    p <- p +
      geom_text(
        data = df[!is.na(df$end_rit), ]
        ,aes(
          x = end_rit + rit_xoffset
          ,group = endpoint_color
          ,color = endpoint_color
          ,label = end_rit
          ,hjust = rit_hjust
        )
        ,size =p_name_size
      )
  }
  
  
  #add name labels
  p <- p +
    geom_text(
      aes(
        x = name_x
        ,label = student_name_format
        ,group = name_color
        ,color = name_color
      )
      ,size = p_name_size
      ,vjust = 0.5
      ,hjust = 1
    )
  
  #negative students start rit is not part of name string.  print to right of baseline
  if (nrow(df[df$neg_flag == 1 & !is.na(df$neg_flag), ]) > 0) {
    p <- p + geom_text(
      data = df[df$neg_flag == 1 & !is.na(df$neg_flag), ] 
      ,aes(
        x = base_rit + 1
        ,label = base_rit
        ,group = baseline_color
        ,color = baseline_color
      )
      ,size =p_name_size
    )
  }  
  
  #add season 1 start point
  p <- p +
    geom_point(
      aes(
        group = baseline_color
        ,color = baseline_color
      )
      ,size = pointsize
    )
  
  #theme stuff
  p <- p + 
    theme(
      panel.background = element_rect(
        fill = "transparent"
        ,colour = NA
      )
      ,plot.background = element_rect(
        fill = "transparent"
        ,colour = NA
      )
      ,axis.text.x = element_text(size = 15)
      ,axis.text.y = element_blank()
      ,axis.ticks = element_blank()
      ,strip.text.x = element_text(size = 15)
      ,strip.text.y = element_text(size = 15)
      ,strip.background = element_rect(
        fill = "#F4EFEB"
        ,colour = NA)
      ,plot.title = element_text(size = 18)
      ,legend.position = "none"
    )
  
  #faceting
  p <- p + 
    facet_grid(
      formula(base_quartile_format ~ .)
      ,scale="free_y"
      ,space = "free_y"
      ,shrink = FALSE
      ,as.table = FALSE
    ) 
  
  #scale stuff
  p <- p +
    scale_y_continuous(
      name = " "
      ,breaks = seq(1:max(df$y_order)-1)
      ,expand = c(0,0.5)
    )
  
  #titles etc
  p <- p +
    ggtitle(p_title) +
    xlab('RIT Score')
  
  #summary labels
  start_labels <- get_group_stats(
    df = df[!is.na(df$base_rit), ]
    ,grp = 'base_quartile_format'
    ,RIT = 'base_rit'
    ,dummy_y =  'y_order'
  )
  
  #force to data table
  start_labels <- as.data.table(start_labels)
  
  #turn stats into printable label
  start_labels[ ,count_label := paste0(
    start_season_abbrev, ': ', start_labels$count_students, " students (", round(start_labels$pct_of_total * 100), "%)")]
  
  #repeat for end quartile
  end_labels <- get_group_stats(
    df = df[!is.na(df$end_rit), ]
    ,grp = 'endpoint_quartile'
    ,RIT = 'end_rit'
    ,dummy_y =  'y_order'
  )
  
  #force to data table
  end_labels <- as.data.table(end_labels)
  
  #turn stats into printable label
  end_labels[ ,count_label := paste0(
    end_season_abbrev, ': ', end_labels$count_students, " students (", round(end_labels$pct_of_total * 100), "%)")]
  
  #calculate x position
  calc_df <- df[!is.na(df$base_rit) & !is.na(df$end_rit), ]
  quartile_label_pos <- round_any(min(c(calc_df$base_rit, calc_df$end_rit)) - 10, 10, floor) + 10
  
  #add x position to summary dfs
  start_labels[ ,quartile_label_pos := rep(quartile_label_pos, nrow(start_labels))]
  end_labels[ ,quartile_label_pos := rep(quartile_label_pos, nrow(end_labels))]
  
  #names have to be the same for the facet to behave properly - 
  #end_labels will have quartile name of 'base_quartile_format' which strictly speaking
  #is wrong, but if you don't do that ggplot won't facet properly.
  #reset name of col 1
  setnames(end_labels, old=1, new='base_quartile_format')
  #ensures that faceting behaves
  end_labels$base_quartile_format <- paste('Quartile', end_labels$base_quartile_format)
  
  #season 2 is below season 1
  #end_labels[ ,avg_y_dummy := start_labels[ ,avg_y_dummy] - 3]
  #first match
  #end_labels$avg_y_dummy <- end_labels$avg_y_dummy - (1 +  floor(num_stu / 30))
  
  #start_labels$color_identity <- annotate_colors$color[match(start_labels$base_quartile_format, annotate_colors$quartile)]
  
  #make annotation lables so that season 2 is after season 1
  #god this is the absolute worst.
  #begin by flipping back to data frame  
  start_labels <- as.data.frame(start_labels, stringsAsFactors = FALSE)
  end_labels <- as.data.frame(end_labels, stringsAsFactors = FALSE)
  #grab everything in the start that matches the end
  #this is necessary when there are quartiles present in the end data not present in the start
  matched_label = start_labels[start_labels$base_quartile_format == end_labels$base_quartile_format, 'base_quartile_format']
  matched_ypos = start_labels[start_labels$base_quartile_format == end_labels$base_quartile_format, 'avg_y_dummy']
  
  #make it a df
  label_match_df <- data.frame(
    label = matched_label
    #offset lower; if n is small, only offset by 1.
    ,ypos = matched_ypos - (1 +  floor(num_stu / 30))
    ,stringsAsFactors = FALSE
  )
  
  #for the ones you can match, replace with the adjusted start, so they print below
  #unmatched will remain in the avg/middle position
  end_labels[end_labels$base_quartile_format == label_match_df$label, 'avg_y_dummy'] <- label_match_df$ypos
  
  #lookup colors
  annotate_colors <- data.frame(
    quartile = c('Quartile 1', 'Quartile 2', 'Quartile 3', 'Quartile 4')
    ,color = p_quartile_colors
    ,stringsAsFactors = FALSE
  )
  start_labels$color_identity <- annotate_colors$color[match(start_labels$base_quartile_format, annotate_colors$quartile)]
  end_labels$color_identity <- annotate_colors$color[match(end_labels$base_quartile_format, annotate_colors$quartile)]
  
  #add to plot
  #base students
  p <- p +
    geom_text(
      data = start_labels
      ,aes(
        x = quartile_label_pos
        ,y = avg_y_dummy
        ,label = count_label
        ,group = base_quartile_format
        ,color = color_identity
      )
      ,vjust = 0.5
      ,hjust = 0
      ,size = annotate_size
    ) + 
    #end students      
    geom_text(
      data = end_labels
      ,aes(
        x = quartile_label_pos
        ,y = avg_y_dummy
        ,label = count_label
        ,group = base_quartile_format  
        ,color = color_identity
      )
      ,vjust = 0.5
      ,hjust = 0
      ,size = annotate_size
    )
  
  return(p) 
  
}