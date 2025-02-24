server <- function(input, output, session) {

  custom_size <- reactiveValues(width = 300, height = 300)
  observeEvent(input$reflect_size, {
    if (!button_click_state())
      button_click_state(!button_click_state())
    custom_size$width <- input$width_box
    custom_size$height <- input$height_box
  })
  button_click_state <- reactiveVal(FALSE)

  
  observeEvent(input$datafile, {
    if (stringr::str_detect(input$datafile$datapath,".xlsx")){
      output$colname3 = renderUI({ 
        selectInput("sheet", "Sheet:",getSheetNames(input$datafile$datapath))
      })
      
      data_file = reactive(read.xlsx(input$datafile$datapath,sheet = input$sheet))
      data_type = "xl"
      
      
      
    }else {
      data_file =reactive(read.csv(input$datafile$datapath,fileEncoding="utf-8"))

    }
    
    output$table = renderDataTable(data_file(), editable = 'cell',filter = "top",
                                   options = list(pagelength = 100))
    
    output$colname1 = renderUI({ 
      selectInput("x", "x axis:", colnames(data_file()))
    })
    output$colname2 = renderUI({ 
      selectInput("y", "y axis:", colnames(data_file()))
    })
    output$soubetuka = renderUI({ 
      selectInput("group", "Group:",colnames(data_file()))
    })
    output$colname4 = renderUI({ 
      selectInput("vert", "vertical:",colnames(data_file()))
    })
    output$colname5 = renderUI({ 
      selectInput("hori", "horizontal:",colnames(data_file()))
    })
    output$colname6 = renderUI({ 
      selectInput("line_iden", "line:",colnames(data_file()))
    })
    output$colname7 = renderUI({ 
      selectInput("line_pair", "selcet data:",colnames(data_file()))
    })
    
    
    
    
    

  })
  
  
  
    #### keypressed .js ####
  txt <- reactiveVal()
  observeEvent(input[["keyPressed"]], {
    txt(input[["col_text"]])
  })
  txt2 <- reactiveVal()
  observeEvent(input[["keyPressed"]], {
    txt2(input[["col_enter"]])
  })
  
  observeEvent(input$group, {
    name = names(input$datafile)
    if (str_detect(input$datafile$datapath,".xlsx")){
      data_file = reactive(openxlsx::read.xlsx(input$datafile$datapath, sheet = input$sheet))
    }else{
      data_file = reactive(read.csv(input$datafile$datapath))
    }
    grp = data_file()[input$group]
    
    if (is.character(grp[,1]))
      updateCheckboxInput(session, "factify_g", value = T)
    else {
      updateCheckboxInput(session, "factify_g", value = F)
    }

    })
  
  observeEvent(input$x, {
    name = names(input$datafile)
    if (str_detect(input$datafile$datapath,".xlsx")){
      data_file = reactive(openxlsx::read.xlsx(input$datafile$datapath, sheet = input$sheet))
    }else{
      data_file = reactive(read.csv(input$datafile$datapath))
    }
    x = data_file()[input$x]
    
    if (is.character(x[,1]))
      updateCheckboxInput(session, "factify", value = T)
    else {
      updateCheckboxInput(session, "factify", value = F)
    }
    
  })
  

  
  
  
  
    #### ggplot ####
  observeEvent(input$submit, {
    plotinput = reactive({
    name = names(input$datafile)
    if (str_detect(input$datafile$datapath,".xlsx")){
      data_file = reactive(openxlsx::read.xlsx(input$datafile$datapath, sheet = input$sheet))
      data_type = "xl"
    }else{
      data_file = reactive(read.csv(input$datafile$datapath,fileEncoding="utf-8",header = T))
      data_type = "csv"
    }
    write.csv(data_file(),"www/export.csv",row.names = FALSE)
    
    options(dplyr.summarise.inform = FALSE)  
      
    x = data_file()[input$x]
    y = data_file()[input$y]
    grp = data_file()[input$group]
    pair_dat = data_file()[input$line_pair]

    #### keeping data order ####
    
    x2 <- x[,1]
    
    if (input$factify == F)
      x2 <- as.numeric(x[,1])
    else {
      x2 <- factor(x[,1], levels=unique(x2))
    }
    
    
    if (input$factify_g == F){
      grp[,1] <- as.numeric(grp[,1])
      grp_chara = F
    } else {
      grp[,1] <- factor(grp[,1],levels=unique(grp[,1]))
      num <- length(table(grp[,1]))
      grp_chara = T  
    }
      
    
      ### facet ###
      if (input$facet == "vertical" || input$facet == "both")
        v = data_file()[input$vert]
      if (input$facet == "horizontal" || input$facet == "both")
        h = data_file()[input$hori]
      
      if (input$line_type != "none")
        n = data_file()[input$line_iden]
      
      if (input$facet == "horizontal" || input$facet == "both"){
        if (is.character(h[,1]))
          head(h[,1])
          h[,1] <- factor(h[,1], levels = unique(h[,1]))}
      
      if (input$facet == "vertical" || input$facet == "both"){
        if (is.character(v[,1]))
          v[,1] <- factor(v[,1], levels = unique(v[,1]))}
     
    
      

      
      
      if (input$factify == T)
        g<-ggplot(data_file()) + aes(x=factor(x2), y=y[,1]) 
      else{
        g<-ggplot(data_file()) + aes(x=x2, y=y[,1])}
      
      if (input$col_enter_tf == T){
        enter_col <- trimws(unlist(strsplit(txt2(), ",")))
      }else{
        enter_col <- input$fill_one}

      #dodge  <- position_dodge(width=input$basic_inter)
      #dodge2 <- position_dodge2(width=input$basic_inter)
      #dodge3 <- position_dodge2(width=input$basic_inter_e)
      dodge4 <- position_dodge(width=input$basic_lap)
      dodge5 <- position_dodge2(width=input$basic_lap)
      
      if (input$graph =="lineplot"){
        data_change <- dplyr::select(data_file(), grp_rename = input$group, x_rename =  input$x, y_rename=input$y,dplyr::everything())
        mean_SE <- data_change %>% group_by(grp_rename, x_rename) %>% summarize(mean_cal = mean(y_rename), se_cal = sd(y_rename)/sqrt(length(y_rename)))
        if (input$factify == F)
          mean_SE <- mean_SE %>% mutate(x_rename = as.numeric(x_rename))
        else {
          mean_SE <- mean_SE %>% mutate(x_rename = factor(x_rename, levels = unique(x_rename)))
        }
        if (input$factify_g == F){
          mean_SE <- mean_SE %>% mutate(grp_rename = as.numeric(grp_rename))
          grp_chara = F
        } else {
          mean_SE <- mean_SE %>% mutate(grp_rename = factor(grp_rename, levels = unique(grp_rename)))
          grp[,1] <- factor(grp[,1],levels=unique(grp[,1]))
          num <- length(table(grp[,1]))
          grp_chara = T  
        }
      }
      
      
      if ((input$divide == T) && (input$dot_shape == F)){
        div <- aes(color = grp[,1] ,fill = grp[,1])
        if ((input$fill_l_b == T)&&(input$graph == "boxplot"))
          div <- aes(fill = grp[,1])
        
        div2 <- aes(group = grp[,1])
        div3 <- aes(fill = grp[,1])
        div4 <- aes(color = grp[,1])
        div5 <- aes(group = grp[,1], color = grp[,1])
        
      
      }else if ((input$divide == T) && (input$dot_shape == T)){    
        div <- aes(color = grp[,1] ,fill = grp[,1],shape = grp[,1])
        div2 <- aes(group = grp[,1],shape = grp[,1])
        div3 <- aes(fill = grp[,1],shape = grp[,1])
        div4 <- aes(color = grp[,1],shape = grp[,1])
        div5 <- aes(group = grp[,1], color = grp[,1],shape = grp[,1])
      }
      
      if (input$col_dot== F){
        dot_line <- NA
      }else if (input$col_dot== T){
         dot_line <- input$col_dot_line
      }
      
        
      if (input$fomul == "1"){
        type_formu <- y ~ x
          
      }else if (input$fomul == "2"){
        type_formu <- y ~ x + I(x^2)
      
      }else if (input$fomul == "3"){
        type_formu <- y ~ x + I(x^2) + I(x^3)
      }
      
      
      

      
    ###### kind of plot ######
                                              ###### box plot for staplewex ######
      
    # Set up for jitter
    #if (input$jit != "ramdom")
    if (input$half_plot == T){
      boxplot <- geom_half_boxplot}
    else {
      boxplot <- geom_boxplot}
    
    if (input$half_dot == T){
      side_half <- 1}
    else {
      side_half <- 0}
    
    if (input$staplewex == "ON")
      wid <- 0.5
    else {
      wid <- 0
    }

                                                ###### dot plot ######
    #ggplot deal dotplot with color, and box with fill
    
      if ((input$graph == "dotplot")&&(input$jit == "random")&&(input$divide == T )&&(is.character(dot_line) == F))
        g<-g + geom_jitter(shape= 16,div4, position = position_jitterdodge(dodge.width=input$basic_lap, jitter.width = input$ran_var, jitter.height = 0), 
                   alpha=input$line_trans, size = input$size)
      
      else if ((input$graph == "dotplot")&&(input$jit == "random")&&(input$divide == T))
        g<-g + geom_jitter(shape= 21,div3, position = position_jitterdodge(dodge.width=input$basic_lap, jitter.width = input$ran_var, jitter.height = 0), 
                           alpha=input$line_trans, size = input$size , color = dot_line)
      
                                                ###### boxplot ##########
      
      else if ((input$graph == "boxplot")&&(input$jit == "random")&&(input$divide == T)&&(is.character(dot_line) == F))
        g<-g + boxplot(div,position = dodge4,alpha=input$fill_trans,outlier.shape = NA,notch = input$notch_tf,width = input$basic_inter, errorbar.draw = wid, staplewidth= wid) +  
               geom_jitter(shape= 16,div4, position = position_jitterdodge(dodge.width=input$basic_lap, jitter.width = input$ran_var, jitter.height = 0), 
                           alpha=input$line_trans, size = input$size)

      
      else if ((input$graph == "boxplot")&&(input$jit == "random")&&(input$divide == T))
        g<-g + boxplot(div,position = dodge4,alpha=input$fill_trans,outlier.shape = NA,notch = input$notch_tf,width = input$basic_inter, errorbar.draw = wid, staplewidth= wid) +  
               geom_jitter(shape= 21,div3, position = position_jitterdodge(dodge.width=input$basic_lap, jitter.width = input$ran_var, jitter.height = 0), 
                           alpha=input$line_trans, size = input$size , color = dot_line)
      
      else if ((input$graph == "boxplot")&&(input$jit == "swarm")&&(input$divide == T)&&(is.character(dot_line) == F))
        g<-g + boxplot(div,position = dodge4,alpha=input$fill_trans,outlier.shape = NA,notch = input$notch_tf,width = input$basic_inter, errorbar.draw = wid, staplewidth= wid) +  
               geom_beeswarm(side= side_half,method = "swarm",shape= 16,div4,dodge.width=input$basic_lap, alpha=input$line_trans, size = input$size,cex = input$cex_ui)
      
      
      else if ((input$graph == "boxplot")&&(input$jit == "swarm")&&(input$divide == T))
        g<-g + boxplot(div,position = dodge4,alpha=input$fill_trans,outlier.shape = NA,notch = input$notch_tf,width = input$basic_inter, errorbar.draw = wid, staplewidth= wid) +  
              geom_beeswarm(side= side_half,method = "swarm",shape= 21,div3,dodge.width=input$basic_lap, alpha=input$line_trans, size = input$size , color = dot_line,cex = input$cex_ui)
      
        
      #else if ((input$graph == "boxplot")&&(input$jit == "random"))
        #g<-g + boxplot(position = dodge4,alpha=input$fill_trans,outlier.shape = NA,notch = input$notch_tf,width = input$basic_inter,
        #                    fill = enter_col,color=dot_line, errorbar.draw = wid, staplewidth= wid)

      
      
      
        
                                                ### lineplot ###

         
        else if ((input$graph == "lineplot")&&(input$jit == "random")&&(input$divide == T)&&(is.character(dot_line) == F)){
          g<- g + geom_jitter(shape= 16,div4, position = position_jitterdodge(dodge.width=input$basic_lap, jitter.width = input$ran_var, jitter.height = 0), 
                                  alpha=input$line_trans, size = input$size) +
                geom_point(div4, position=dodge4,stat = 'summary', fun = 'mean') + 
                geom_errorbar(div4,position=dodge4, fun.args = list(mult = as.numeric(input$SE)),
                              stat = 'summary', fun.data = 'mean_se',size =input$line_size-.5,width=input$hige_w)
        if ((input$dot_shape == T)&&(input$line_select == T))
          g<- g + geom_line(data = mean_SE,aes(x=x_rename,y=mean_cal,group=grp_rename, color = grp_rename,linetype=grp_rename, shape = grp_rename),position=dodge4,size=input$line_size)
        else if (input$line_select == T)
          g<- g + geom_line(data = mean_SE,aes(x=x_rename,y=mean_cal,group=grp_rename, color = grp_rename,linetype=grp_rename),position=dodge4,size=input$line_size)
        else if (input$dot_shape)
          g<- g + geom_line(data = mean_SE,aes(x=x_rename,y=mean_cal,group=grp_rename, color = grp_rename,shape=grp_rename),position=dodge4,size=input$line_size)
        else {
          g<- g + geom_line(data = mean_SE,aes(x=x_rename,y=mean_cal,group=grp_rename, color = grp_rename),position=dodge4,size=input$line_size)
        }
        shokiti_summ <- "no"
        
      }else if ((input$graph == "lineplot")&&(input$jit == "random")&&(input$divide == T)){
        g<- g + geom_jitter(shape= 21,div3, position = position_jitterdodge(dodge.width=input$basic_lap, jitter.width = input$ran_var, jitter.height = 0), 
                                  alpha=input$line_trans, size = input$size , color = dot_line) +
                geom_point(div4, position=dodge4,stat = 'summary', fun = 'mean') + 
                geom_errorbar(div4,position=dodge4, fun.args = list(mult = as.numeric(input$SE)),
                              stat = 'summary', fun.data = 'mean_se',size =input$line_size-.5,width=input$hige_w)
        if ((input$dot_shape == T)&&(input$line_select == T))
          g<- g + geom_line(data = mean_SE,aes(x=x_rename,y=mean_cal,group=grp_rename, color = grp_rename,linetype=grp_rename, shape = grp_rename),position=dodge4,size=input$line_size)
        else if (input$line_select == T)
          g<- g + geom_line(data = mean_SE,aes(x=x_rename,y=mean_cal,group=grp_rename, color = grp_rename,linetype=grp_rename),position=dodge4,size=input$line_size)
        else if (input$dot_shape)
          g<- g + geom_line(data = mean_SE,aes(x=x_rename,y=mean_cal,group=grp_rename, color = grp_rename,shape=grp_rename),position=dodge4,size=input$line_size)
        else {
          g<- g + geom_line(data = mean_SE,aes(x=x_rename,y=mean_cal,group=grp_rename, color = grp_rename),position=dodge4,size=input$line_size)
        }        
        
        
        shokiti_summ <- "no"
      }else if ((input$graph == "lineplot")&&(input$jit == "random")&&(input$divide == 1)&&(is.character(dot_line) == F)){
        g<- g + geom_jitter(shape= 16,div4, position = position_jitterdodge(dodge.width=input$basic_lap, jitter.width = input$ran_var, jitter.height = 0), 
                                  alpha=input$line_trans, size = input$size) +
                geom_point(div4, position=dodge4,stat = 'summary', fun = 'mean') + 
                geom_errorbar(div4,position=dodge4, fun.args = list(mult = as.numeric(input$SE)),
                              stat = 'summary', fun.data = 'mean_se',size =input$line_size-.5,width=input$hige_w)
        if ((input$dot_shape == T)&&(input$line_select == T))
          g<- g + geom_line(data = mean_SE,aes(x=x_rename,y=mean_cal,group=grp_rename, color = grp_rename,linetype=grp_rename, shape = grp_rename),position=dodge4,size=input$line_size)
        else if (input$line_select == T)
          g<- g + geom_line(data = mean_SE,aes(x=x_rename,y=mean_cal,group=grp_rename, color = grp_rename,linetype=grp_rename),position=dodge4,size=input$line_size)
        else if (input$dot_shape)
          g<- g + geom_line(data = mean_SE,aes(x=x_rename,y=mean_cal,group=grp_rename, color = grp_rename,shape=grp_rename),position=dodge4,size=input$line_size)
        else {
          g<- g + geom_line(data = mean_SE,aes(x=x_rename,y=mean_cal,group=grp_rename, color = grp_rename),position=dodge4,size=input$line_size)
            }
        shokiti_summ <- "no"
        
      }else if ((input$graph == "lineplot")&&(input$jit == "no")&&(input$divide == T)){
        g<- g + geom_point(div4, position=dodge4,stat = 'summary', fun = 'mean') + 
                geom_errorbar(div4,position=dodge4, fun.args = list(mult = as.numeric(input$SE)),
                              stat = 'summary', fun.data = 'mean_se',size =input$line_size-.5,width=input$hige_w) 
        if ((input$dot_shape == T)&&(input$line_select == T))
          g<- g + geom_line(data = mean_SE,aes(x=x_rename,y=mean_cal,group=grp_rename, color = grp_rename,linetype=grp_rename, shape = grp_renamev),position=dodge4,size=input$line_size)
        else if (input$line_select == T)
          g<- g + geom_line(data = mean_SE,aes(x=x_rename,y=mean_cal,group=grp_rename, color = grp_rename,linetype=grp_rename),position=dodge4,size=input$line_size)
        else if (input$dot_shape)
          g<- g + geom_line(data = mean_SE,aes(x=x_rename,y=mean_cal,group=grp_rename, color = grp_rename,shape=grp_rename),position=dodge4,size=input$line_size)
        else {
          g<- g + geom_line(data = mean_SE,aes(x=x_rename,y=mean_cal,group=grp_rename, color = grp_rename),position=dodge4,size=input$line_size)
        }
        shokiti_summ <- "no"
      }
      
      if (input$par_d == T){
        g<- g + geom_line(aes(group = pair_dat[,1]), linetype = input$line_type,size=input$line_size)
      }
      
      
      
                                               ########### scatter #########

        #fill sinnrakukan col sen, color=col_dot_line
        
        
      else if ((input$graph == "scatter")&&(input$divide == 1) &&(input$fomul == "1")){
          shokiti_summ = "no"
          
          if (input$col_dot== F) {
            g <- g + geom_point(shape=19, aes(color=grp[,1]), size = input$size, alpha = input$line_trans)
          } else {
            g <- g + geom_point(shape=21, aes(fill=grp[,1]),  size = input$size, color = dot_line, alpha = input$line_trans)
          }
          g<-g + geom_smooth(div,se = input$CI, method = input$type_sm,formula = type_formu)
      
      }else if ((input$graph == "scatter")&&(input$divide == 1) &&(input$fomul == "2")){
        shokiti_summ <- "no"
        if (input$col_dot== F) {
          g <- g + geom_point(shape=19, aes(color=grp[,1]), size = input$size, alpha = input$line_trans)
        } else {
          g <- g + geom_point(shape=21, aes(fill=grp[,1]),  size = input$size, color = dot_line, alpha = input$line_trans)
        }
        g<-g  + geom_smooth(div,se = input$CI, method = input$type_sm,formula = y ~ x + I(x^2))  
          
      }else if ((input$graph == "scatter")&&(input$divide == 1) &&(input$fomul == "3")){
        shokiti_summ <- "no"
        if (input$col_dot== F) {
          g <- g + geom_point(shape=19, aes(color=grp[,1]), size = input$size, alpha = input$line_trans)
        } else {
          g <- g + geom_point(shape=21, aes(fill=grp[,1]),  size = input$size, color = dot_line, alpha = input$line_trans)
        }
        g<-g + geom_smooth(div,se = input$CI, method = input$type_sm,formula = y ~ x + I(x^2) + I(x^3))  
        
      }else if ((input$graph == "scatter")&&(input$divide == 1)){
        shokiti_summ = "no"
        if (input$col_dot== F) { # Check if point color is selected
          g <- g + geom_point(shape=19, aes(color=grp[,1]), size = input$size, alpha = input$line_trans)
        } else {
          g <- g + geom_point(shape=21, aes(fill=grp[,1]),  size = input$size, color = dot_line, alpha = input$line_trans)
        }
      }else if (input$graph == "scatter"){
        if (input$col_dot== F) { # Check if point color is selected
          g <- g + geom_point(shape=19, size = input$size, alpha = input$line_trans)
        } else {
          g <- g + geom_point(shape=21,  size = input$size, color = dot_line, alpha = input$line_trans)
        }
      }
          

        
    
    ##### stat #####
      if (input$col_stat == F)
        div2 = div4
      
      if(shokiti_summ != "no"){
      
        if ((input$summ == "mean") && (input$graph == "boxplot") && (input$divide == 1))
          g<-g+stat_summary(div2, position = dodge5,fun = "mean", geom = "point",shape=4,size = input$stat_size)
        
        else if ((input$summ == "mean")&&(input$graph == "boxplot")&& (input$divide == 0))
          g<-g+stat_summary(fun = "mean", geom = "point",size = input$stat_size,shape=4)
        
        else if ((input$summ == "mean") && (input$graph == "dotplot") && (input$divide == 1))
          g<-g+stat_summary(div2,position = dodge4,
                            fun = "mean", geom = "crossbar",width= input$basic_s * 1.4)
        else if (input$summ == "mean")
          g<-g+stat_summary(fun = "mean", geom = "crossbar",width= input$basic_s *1.4)
        
        else if ((input$summ == "SE") && (input$divide == 1))
          g<-g+stat_summary(div2,position = dodge4,fun = "mean", geom = "crossbar",width= input$basic_s *1.4) + 
               stat_summary(div2,position = dodge4,fun.data = "mean_se", geom = "errorbar",width= input$basic_s )
        else if (input$summ == "SE")
          g<-g+stat_summary(fun = "mean", geom = "crossbar",width= input$basic_s *1.4) + 
               stat_summary(fun.data = "mean_se", geom = "errorbar",width= input$basic_s)
      
      }
 
                                                        #### color ####
      if ((input$col_select == T) && (input$fill_fill %in% palet$wesanderson)){
        txt_chara <- as.numeric(unlist(strsplit(txt(),",")))
        pale_w_s <- wes_palette(input$fill_fill)[txt_chara]
        
      }else if (input$col_select == T){
        txt_chara <- as.numeric(unlist(strsplit(txt(),",")))
        max_num <- brewer.pal.info[input$fill_fill,]$maxcolors
        pale_r_s <- brewer.pal(max_num, input$fill_fill)[txt_chara]
        
      }
        

      if (input$one_or_two == T || input$col_enter_tf == T){
        if (grp_chara == T){
            pale_s <- colorRampPalette(enter_col)(num) 
            g<-g + scale_fill_manual(values =pale_s) + scale_color_manual(values = pale_s)
        } else if (grp_chara == F){
            pale_s <- colorRampPalette(enter_col)(10)
            g<-g + scale_fill_gradientn(colors =pale_s) + scale_color_gradientn(colors = pale_s)
        
        }}
      
                               ### weanderson & make outline black & select any color ###
        

        
      else if ((input$col_select == T) && (input$fill_fill %in% palet$wesanderson)){
        g<-g + scale_fill_manual(values =pale_w_s) + scale_color_manual(values = pale_w_s)
        

        
      }else if (input$fill_fill %in% palet$wesanderson){
        if (grp_chara == T){
            pale_w <- wes_palette(input$fill_fill, n = num)
            g <- g + scale_fill_manual(values =pale_w) + scale_color_manual(values = pale_w)
       } else if (grp_chara == F){
            
            pale_w <- wes_palette(input$fill_fill, n = 4,type = "continuous")
            g<-g + scale_fill_gradientn(colors =pale_w) + scale_color_gradientn(colors = pale_w)
       }}
      
      
      
      
                             #### Rcolorbrewer & makeoutline black & darken color(1st,2nd condition) & select any color(3rd,4th condition) ####
        
    
      else if (input$col_check == T){
        num2 <- num + input$col_num-1
        col_pal <- brewer.pal(9, input$fill_fill)[(input$col_num:num2)]
        g <- g + scale_fill_manual(values=col_pal) +scale_color_manual(values = col_pal)
      
      
      }else if ((input$col_select == T) && (input$divide == T)){
        g <- g + scale_fill_manual(values = pale_r_s)  + scale_color_manual(values = pale_r_s)
        
      }  

        
            
      else if (grp_chara == T){
           g <- g + scale_fill_brewer(palette = input$fill_fill) + scale_color_brewer(palette = input$fill_fill)
      
      }else if (grp_chara == F){
           num3 <- brewer.pal.info[input$fill_fill,]$maxcolors
           pale_s <- brewer.pal(num3, input$fill_fill)
           g<-g + scale_fill_gradientn(colors =pale_s) + scale_color_gradientn(colors = pale_s)
      }
      
      
      
    #     else if (is.numeric(grp[,1]) == T) 
     #       g <- g + scale_fill_distiller(palette = input$fill_fill) + scale_color_distiller(palette = input$fill_fill)
          
      
      
      
    #### axis option ####      
      if (input$lab_x_a == 0){
        lab_x_axis <- element_text(size = input$font_t_s, colour = "black")
      } else {
        lab_x_axis <- element_text(size = input$font_t_s, colour = "black", angle=input$lab_x_a,hjust=1,vjust=.5)}
      
      
      g <- g + labs(title = input$lab_t,x =input$lab_x, y = input$lab_y,fill = input$lab_l,colour =input$lab_l)
      
      
      if (input$not_x == 1)
        g <- g + theme(axis.text.x = element_text(colour =NA))
      
      
    #### linetype ####
      

      if ((input$graph =="lineplot") &&(input$divide == T)&&(input$line_select == T)){
        line_text_num <- as.numeric(unlist(strsplit(input$line_text_plt,",")))
        g <- g + scale_linetype_manual(values = line_text_num)
      }
       ## scale & log ##
      
      
      
      if (input$change_y == 1){
        g <- g + scale_y_continuous(expand= c(0,0),breaks = seq(from = input$min_y, to = max(y[,1])*2, by = input$by_y), limits = c(input$min_y,max(y[,1])*2)) 
        g <- g + coord_cartesian(ylim=(c(input$min_y,input$max_y)))
      }else{
        g <- g + scale_y_continuous(expand= c(0,0))
      }
      
      
      if ((input$log == T) &&(input$select_x == T)){
        
        sel_x_txt_num <- as.numeric(unlist(strsplit(input$sel_x_txt,",")))
        g <- g + scale_x_continuous(trans = "log10",breaks = c(sel_x_txt_num))}
      
      else if ((input$log2 == T) &&(input$select_x == T)){
        
        sel_x_txt_num <- as.numeric(unlist(strsplit(input$sel_x_txt,",")))
        g <- g + scale_x_continuous(trans = "log2",breaks = c(sel_x_txt_num))}
      
      else if ((input$log == T) &&(input$change_x == T))
        g <- g + scale_x_continuous(trans ="log10",expand= c(0,0),breaks = seq(from = input$min_x, to = input$max_x, by = input$by_x), limits = c(input$min_x,input$max_x))

      else if ((input$log2 == T) &&(input$change_x == T))
        g <- g + scale_x_continuous(trans ="log2",expand= c(0,0),breaks = seq(from = input$min_x, to = input$max_x, by = input$by_x), limits = c(input$min_x,input$max_x))
      
      else if (input$log == T)
        g <- g + scale_x_continuous(trans = "log10")
      
      else if (input$log == T)
        g <- g + scale_x_continuous(trans = "log2")
      
      else if (input$select_x == T){
        sel_x_txt_num <- as.numeric(unlist(strsplit(input$sel_x_txt,",")))
        g <- g + scale_x_continuous(breaks = c(sel_x_txt_num))}
      
      else if (input$change_x == T)
        g <- g + scale_x_continuous(expand= c(0,0),breaks = seq(from = input$min_x, to = input$max_x, by = input$by_x), limits = c(input$min_x,input$max_x))
      
      #####
      if (input$flip == T )
        g <- g + coord_flip(ylim=(c(input$min_y,input$max_y)))
      
      
      
      

      #### theme ####

      showtext.begin()
      
      if (input$facet == "vertical"){
        g <- g + facet_grid(v[,1] ~ .) 
        #+theme(strip.background = element_blank(), 
          #    strip.text.y = element_text(size = input$fac_s*2,angle = input$fac_v_a,family = input$font_f))
      
      }else if (input$facet == "horizontal"){ 
        g <- g  + facet_grid(. ~ h[,1]) 
        #+  theme(strip.background = element_blank(),
         #       strip.text.x  = element_text(size = input$fac_s*2,family = input$font_f))
        
        
      }else if (input$facet == "both"){
        g <- g  + facet_grid(v[,1] ~ h[,1]) +
        theme(strip.background = element_blank(), 
              strip.text.x = element_text(size = input$fac_s*2,family = input$font_f),
              strip.text.y = element_text(size = input$fac_s*2,angle = input$fac_v_a,family = input$font_f))}
      
      if ((input$line_type != "none") && (input$facet != "none"))
        g <- g +stat_summary(fun="identity",aes(group = NO),geom = "line",linetype = input$line_type)
      
      
      g <- g  + theme(axis.text = lab_x_axis, axis.title = element_text(colour = "black"))
      g <- g + theme_cowplot(font_size = input$font_t_s,
                             font_family = input$font_t,
                             line_size = 0.5,
                             rel_small = 14/input$font_t_s,
                             rel_tiny = 11/input$font_t_s,
                             rel_large = 16/input$font_t_s)

      g_legend = get_legend(g)
      g <- g  + theme(legend.position = "none",
                      axis.title.y = element_text(margin = margin(r = input$y_posi))) # Must be written after cowplot
      
      ##############
      

      if (input$legend_off ==T){
        plot_grid(g)
      }else{
        plot_grid(g_legend)
      }
         })
    
    

    output$plot_a <- renderPlot({
      plotinput()
    },bg = "transparent")
  
    output$plot_b <- renderPlot({
      plotinput()},
      bg="transparent",
      width = reactive(custom_size$width),
      height = reactive(custom_size$height)
      )
    
    output$dynamic_plot <- renderUI({
      if (button_click_state()) {
        plotOutput("plot_b")
      } else {
        jqui_resizable(plotOutput("plot_a"),operation = "enable")
      }
    })
    
    output$plot_dimensions <- renderText({
      if (button_click_state()){
        paste("Width:", custom_size$width, "Height:", custom_size$height)
        }
      else{
        w <- input$plot_a_size
        paste("Width:", w$width, "Height:", w$height)
      }
    })
    

    
    
    # can't output file by plot_a
    output$downloadPlot <- downloadHandler(
      filename = function(){paste("downloadPlot",input$kakuchou,sep='')},
      content = function(file){
        w <- input$plot_size
        if (button_click_state()) {
          w$width <- custom_size$width
          w$height <- custom_size$height
        }
        input_w <- w$width/100
        input_h <- w$height/100
        
        ggsave2(file,plotinput(),height=input_h, width=input_w, dpi = 600)

      }
    )
  })
  
  
  observe({
    plot_height <- if (button_click_state()) custom_size$height else input$plot_a_size$height
    adjusted_plot_height <- plot_height - 400  # You can adjust the value '20' to increase or decrease the space.
    runjs(sprintf("$('#plot_space').css('height', '%dpx');", adjusted_plot_height))
  })
  
  #### book marking ####
  
  setBookmarkExclude(c("submit","x","y","group","data_file","datafile","x2","name","grp","x2","g","table","colname1","colname2","plot","Plot","Table"))
  reactive(onRestore(input$rds))
  latestBookmarkURL <- reactiveVal()
  
  onBookmarked(
    fun = function(url) {
      latestBookmarkURL(parseQueryString(url))
    }
  )
  
  onRestored(function(state) {
    showNotification(paste("Restored session:", basename(state$dir)), duration = 10, type = "message")
  })
  
  observeEvent(input$save_inputs, {
    showModal(modalDialog(
      title = "Session Name",
      textInput("session_name", "Please enter a session name (optional):"),
      footer = tagList(
        modalButton("Cancel"),
        downloadButton("download_inputs", "OK")
      )
    ))
  }, ignoreInit = TRUE)
  
  # SAVE SESSION
  output$download_inputs <- downloadHandler(
    filename = function() {
      removeModal()
      session$doBookmark()
      if (input$session_name != "") {
        
        tmp_session_name <- sub("\\.rds$", "", input$session_name)
        
        # "Error: Invalid state id" when using special characters - removing them:
        tmp_session_name <- stri_replace_all(tmp_session_name, "", regex = "[^[:alnum:]]")
        # TODO: check if a valid filename is provided (e.g. via library(shinyvalidate)) for better user feedback
        
        tmp_session_name <- paste0(tmp_session_name, ".rds")
        
      } else {
        paste(req(latestBookmarkURL()), "rds", sep = ".")
      }
    },
    content = function(file) {
      file.copy(from = file.path(
        ".",
        "shiny_bookmarks",
        req(latestBookmarkURL()),
        "input.rds"
      ),
      to = file)
    }
  )
  
  # LOAD SESSION
  observeEvent(input$restore_bookmark, {
    
    sessionName <- file_path_sans_ext(input$restore_bookmark$name)
    targetPath <- file.path(".", "shiny_bookmarks", sessionName, "input.rds")
    
    if (!dir.exists(dirname(targetPath))) {
      dir.create(dirname(targetPath), recursive = TRUE)
    }
    
    file.copy(
      from = input$restore_bookmark$datapath,
      to = targetPath,
      overwrite = TRUE
    )
    
    restoreURL <- paste0(session$clientData$url_protocol, "//", session$clientData$url_hostname, ":", session$clientData$url_port, "/?_state_id_=", sessionName)
    
    # redirect user to restoreURL
    runjs(sprintf("window.location = '%s';", restoreURL))
    
    # showModal instead of redirecting the user
    # showModal(modalDialog(
    #     title = "Restore Session",
    #     "The session data was uploaded to the server. Please visit:",
    #     tags$a(restoreURL),
    #     "to restore the session"
    # ))
  })
  
  ####  save ####
  

  
  
  
  
}