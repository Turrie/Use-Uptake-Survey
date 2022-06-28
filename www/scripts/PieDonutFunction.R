#'The source code was obtained from https://rdrr.io/cran/ggiraphExtra/src/R/ggPieDonut.R
#'Edited By : Fridah Wanjala 
#'Edits made: colours and label font sizes sizes
#'Draw a Pie and Donut plot
#'@param data A data.frame
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@param addPieLabel A logical value. If TRUE,  labels are added to the Pies
#'@param addDonutLabel A logical value. If TRUE,  labels are added to the Donuts
#'@param showRatioDonut A logical value. If TRUE,  Ratios are added to the DonutLabels
#'@param showRatioPie A logical value. If TRUE,  Ratios are added to the PieLabels
#'@param showRatioPieAbove10 A logical value. If TRUE,  labels are added to the Pies with ratio above 10.
#'@param title Plot title
#'@param labelposition A number indicating the label position
#'@param polar A logical value. If TRUE,  coord_polar() function will be added
#'@param use.label Logical. Whether or not use column label in case of labelled data
#'@param use.labels Logical. Whether or not use value labels in case of labelled data
#'@param interactive A logical value. If TRUE,  an interactive plot will be returned
#'@importFrom grDevices rainbow
#'@importFrom ggplot2 geom_segment
#'@export
#'@return An interactive Pie and Donut plot
#'@examples
#'require(ggplot2)
#'require(ggiraph)
#'require(plyr)
#'require(moonBook)
#'ggPieDonut(acs, aes(pies = Dx, donuts = smoking))
#'ggPieDonut(acs, aes(pies = smoking))
#'ggPieDonut(browsers, aes(pies = browser, donuts = version, count = share))
#'ggPieDonut(browsers, aes(x = c(browser, version), y = share), interactive = TRUE)
ggPieDonutMine = function(data, mapping, 
                    #pies = "Dx", donuts = "smoking", count = NULL, 
                    addPieLabel = TRUE, addDonutLabel = TRUE, 
                    showRatioDonut = TRUE, showRatioPie = TRUE, 
                    showRatioPieAbove10 = TRUE, title = "", 
                    labelposition = 1,  polar = TRUE, 
                    use.label = TRUE, use.labels = TRUE, 
                    interactive = FALSE, 
                    colors = NULL,  size.pie  =  4.5,  size.donut  =  4){
  
  #data = browsers;mapping = aes(pies = browser, donuts = version, count = share)
  # data = acs;mapping = aes(donuts = Dx)
  # addPieLabel = TRUE;addDonutLabel = TRUE
  # showRatioDonut = TRUE;showRatioPie = TRUE
  # showRatioPieAbove10 = TRUE;title = ""
  # labelposition = 1; polar = TRUE;interactive = FALSE
  (cols = colnames(data))
  if(use.labels) data = addLabelDf(data, mapping)
  
  count <- NULL
  if("y" %in% names(mapping)){
    count <- getMapping(mapping, "y")
  } else {
    if("count" %in% names(mapping)) count <- getMapping(mapping, "count")
  }
  count
  (pies = getMapping(mapping, "pies"))
  
  (donuts = getMapping(mapping, "donuts"))
  
  if((length(pies) + length(donuts)) != 2){
    (xvar = getMapping(mapping, "x"))
    
    #if(length(xvar)<2) warning("Two variables are required")
    if(length(xvar) > 1) {
      # xvar <- xvar[-1]
      pies = xvar[1]
      donuts = xvar[2]
    }
  }
  if((length(pies) + length(donuts)) == 1) {
    if(is.null(pies)) {
      p <- ggDonut(data, mapping, addDonutLabel = addDonutLabel, 
                 showRatio = showRatioDonut, title = title, 
                 labelposition = labelposition,  polar = polar, interactive = interactive)
    } else {
      p <- ggPie(data, mapping, title = title, 
               addPieLabel = addPieLabel, showRatioPie = showRatioPie, 
               showRatioPieAbove10 = showRatioPieAbove10, 
               labelposition = labelposition,  polar = polar, interactive = interactive)
    }
  } else {
    if(is.null(count)){
      dat1 = plyr::ddply(data, c(pies, donuts), nrow)
      colnames(dat1)[3] = "n"
      
      dat1$ymax = cumsum(dat1$n)
      dat1$ymin = cumsum(dat1$n) - dat1$n
      dat1$ypos = dat1$ymin + dat1$n/2
      dat1$ratio = dat1$n*100/sum(dat1$n)
      dat1$cumratio = dat1$ypos*100/sum(dat1$n)
      dat1$hjust = ifelse((dat1$cumratio > 25 & dat1$cumratio < 75), 0, 1)
      dat1$label = paste0(dat1[[pies]], '<br>', dat1[[donuts]], "<br>", dat1$n, "(", round(dat1$ratio, 1), "%)")
      
      #print(dat1)
      
      data2 = plyr::ddply(data, pies, nrow)
      colnames(data2)[2] = "sum"
      #data2 = data2[order(data2$sum, decreasing = TRUE), ]
      data2$cumsum = cumsum(data2$sum)
      data2$pos = data2$cumsum - data2$sum/2
      data2$ymin = data2$cumsum - data2$sum
      data2$ratio = data2$sum*100/sum(data2$sum)
      data2$label = ifelse(data2$ratio > 10, 
                         paste0(data2[[pies]], "<br>", data2$sum, "(", round(data2$ratio, 1), "%)"), 
                         paste0(data2[[pies]]))
      data2$tooltip = paste0(data2[[pies]], "<br>", data2$sum, "(", round(data2$ratio, 1), "%)")
      #print(data2)
      
      
    } else{
      dat1 = data
      colnames(dat1)[colnames(dat1) == count] = "n"
      dat1$ymax = cumsum(dat1$n)
      dat1$ymin = cumsum(dat1$n) - dat1$n
      dat1$ypos = dat1$ymin + dat1$n/2
      dat1$ratio = dat1$n*100/sum(dat1$n)
      dat1$cumratio = dat1$ypos*100/sum(dat1$n)
      dat1$hjust = ifelse((dat1$cumratio > 25 & dat1$cumratio < 75), 0, 1)
      dat1$label = paste0(dat1[[pies]], "<br>", dat1[[donuts]], "<br>", dat1$n, "(", round(dat1$ratio, 1), "%)")
      
      dat1
      #print(dat1)
      pies
      
      data2 = eval(parse(text = "plyr::ddply(dat1, pies, summarize, sum(n))"))
      data2
      colnames(data2)[2] = "sum"
      data2 = data2[order(data2$sum, decreasing = TRUE), ]
      data2$cumsum = cumsum(data2$sum)
      data2$pos = data2$cumsum - data2$sum/2
      data2$ymin = data2$cumsum - data2$sum
      data2$ratio = data2$sum*100/sum(data2$sum)
      data2$label = ifelse(data2$ratio > 10, 
                         paste0(data2[[pies]], "<br>", data2$sum, "(", round(data2$ratio, 1), "%)"), 
                         paste0(data2[[pies]]))
      data2$tooltip = paste0(data2[[pies]], "<br>", data2$sum, "(", round(data2$ratio, 1), "%)")
      #print(data2)
      
      
    }
    mainCol =  colors #rainbow(nrow(data2))
    subCol = subcolors(dat1, pies, mainCol)
    #subCol
    p <- ggplot(dat1)  + 
      geom_rect_interactive(aes_string( ymax = "ymax",  ymin = "ymin",  xmax = "4",  xmin = "3", 
                                        tooltip = "label", data_id = donuts), fill = subCol, colour = "white")
    
    p <- p +      geom_rect_interactive(aes_string(ymax = "cumsum",  ymin = "ymin",  xmax = "3",  xmin = "0", 
                                               tooltip = "tooltip", data_id = pies), data = data2, 
                                    fill = mainCol, colour = "white", alpha = 0.7)
    p <- p +     theme_clean()
    
    if(addDonutLabel) {
      label2 = dat1[[donuts]]
      if(showRatioDonut)
        label2 = paste0(label2, "\n(", round(dat1$ratio, 0), "%)")
      if(polar){
        if(labelposition == 1) {
          p <-  p +  geom_text(aes_string(label = "label2", x = "4.3", y = "ypos", hjust = "hjust"), size = size.donut) + 
            geom_segment(aes_string(x = "4", xend = "4.2", y = "ypos", yend = "ypos"))
        }  else{
          p <-  p +  geom_text(aes_string(label = "label2", x = "3.5", y = "ypos"), size = size.donut)
        }
      } else{
        p <- p +  geom_text(aes_string(label = "label2", x = "3.5", y = "ypos"), size = size.donut)
        
      }
      
    }
    if(addPieLabel) {
      Pielabel = data2[[pies]]
      if(showRatioPie) {
        if(showRatioPieAbove10) {
          Pielabel = ifelse(data2$ratio > 10, 
                          paste0(data2[[pies]], "\n(", round(data2$ratio, 0), "%)"), 
                          paste0(data2[[pies]]))
        }
        else Pielabel = paste0(Pielabel, "\n(", round(data2$ratio, 0), "%)")
      }
      p <- p + geom_text(data = data2, aes_string(label = "Pielabel", x = "1.5", y = "pos"), size = size.pie)
    }
    if(polar) p <- p + coord_polar(theta = "y", start = 3*pi/2)
    if(title != "") p <- p + ggtitle(title)
    if(use.label){
      labels = c()
      for(i in 1:length(cols)) {
        temp = get_label(data[[cols[i]]])
        labels = c(labels, ifelse(is.null(temp), cols[i], temp))
      }
      labels
      # angles = (90-(0:(length(labels)-1))*(360/length(labels)))-5
      # angles
      # angles[which(angles < -90)] = angles[which(angles < -90)] + 180
      p <- p + scale_x_discrete(labels = labels)
      #theme(axis.text.x = element_text(angle = angles))
      
    }
    
    if(interactive) {
      tooltip_css  <-  "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
      p <- girafe(ggobj = p)
      p <- girafe_options(p, 
                        opts_tooltip(css = tooltip_css, opacity = .75), 
                        opts_zoom(min = 1, max = 10))
      
    }
    p
    
  }
  p
  
}
