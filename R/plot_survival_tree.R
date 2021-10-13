plot_survival_tree<-function(survival_tree){
  
  a_table<-tree_to_table(survival_tree)
  a_table<-dplyr::arrange(a_table,plot_order)
  
  variable_names<-survival_tree$variable_names
  
  n_node<-nrow(a_table)
  n_layer<-max(a_table$layer,na.rm=T)
  
  node_width<-1
  node_height<-1
  fig_height<-2
  gap_size<-0.1
  pad_bottom<-1.0
  pad_left<-0.5
  pad_right<-0.1
  
  total_width<-pad_left+node_width*(n_node+1)+pad_right
  total_height<-0.5*node_height+(n_layer-1)*node_height+gap_size+fig_height+pad_bottom
  
  xx_node<-pad_left+(1:n_node)*node_width
  yy_node<-total_height-
    ifelse(a_table$terminal,
           0.5*node_height+(n_layer-1)*node_height,
           0.5*node_height+(a_table$layer-1)*node_height)
  
  gap_size<-node_height*0.2
  
  
  grid.newpage()
  vp<-viewport(xscale=c(0,total_width),yscale=c(0,total_height))
  pushViewport(vp)
  
  for(ii in 1:nrow(a_table)){
    if(is.na(a_table$left_id[ii]))next
    if(is.na(a_table$right_id[ii]))next
    xx_node_parent<-xx_node[ii]
    yy_node_parent<-yy_node[ii]-gap_size
    xx_node_left<-xx_node[which(a_table$id==a_table$left_id[ii])]
    yy_node_left<-yy_node[which(a_table$id==a_table$left_id[ii])]+gap_size
    xx_node_right<-xx_node[which(a_table$id==a_table$right_id[ii])]
    yy_node_right<-yy_node[which(a_table$id==a_table$right_id[ii])]+gap_size
    grid.lines(x=unit(c(xx_node_parent,xx_node_left),"native"),y=unit(c(yy_node_parent,yy_node_left),"native"))
    grid.lines(x=unit(c(xx_node_parent,xx_node_right),"native"),y=unit(c(yy_node_parent,yy_node_right),"native"))
    
    if(a_table$type[ii]=="numeric"){
      split_text<-num_to_str_g(a_table$split_numeric[[ii]],3)
      left_text<-paste0("&le;",split_text)
      right_text<-paste0("&gt;",split_text)
    }else{
      split_text<-paste0(num_to_str_g(unlist(a_table$split_factor[[ii]]),3),collapse=",")
      left_text<-paste0(" is ",split_text)
      right_text<-paste0(" not ",split_text)
    }
    gp<-gpar(fontsize=8)
    box_gp<-gpar(lty=0,fill="white")
    grid.draw(richtext_grob(
      left_text,
      x=unit((xx_node_parent+xx_node_left)/2,"native"),y=unit((yy_node_parent+yy_node_left)/2,"native"),
      gp=gp,box_gp=box_gp))
    grid.draw(richtext_grob(
      right_text,
      x=unit((xx_node_parent+xx_node_right)/2,"native"),y=unit((yy_node_parent+yy_node_right)/2,"native"),
      gp=gp,box_gp=box_gp))
  }
  
  
  for(ii in 1:nrow(a_table)){
    if(!a_table$terminal[ii]){
      line1<-paste0("**Node ",a_table$id[ii],"** (n=",num_to_str_f(a_table$w[ii],1),")")
      line2<-variable_names[a_table$j[ii]]
      line3<-paste0("z=",num_to_str_f(a_table$layer[ii],1),", ",num_to_pvalue(a_table$p[ii],3))
      a_text<-paste0(line1,"<br>",line2,"<br>",line3)
      gp<-gpar(fontsize=7)
      box_gp<-gpar(lty=1,fill="white")
      grid.draw(richtext_grob(
        a_text,x=unit(xx_node[ii],"native"),y=unit(yy_node[ii],"native"),padding=unit(2,"pt"),
        r=unit(4,"pt"),gp=gp,box_gp=box_gp))
    }else{
      line1<-paste0("**Node ",a_table$id[ii],"**")
      line2<-paste0("(n=",num_to_str_f(a_table$w[ii],1),")")
      a_text<-paste0(line1,"<br>",line2)
      gp<-gpar(fontsize=7)
      box_gp<-gpar(lty=1,fill="white")
      grid.draw(richtext_grob(
        a_text,x=unit(xx_node[ii],"native"),y=unit(yy_node[ii]+gap_size,"native"),padding=unit(2,"pt"),
        r=unit(4,"pt"),gp=gp,box_gp=box_gp))
      ot_sub<-a_table$survival[[ii]]$time
      delta_sub<-a_table$survival[[ii]]$event
      weight_sub<-a_table$survival[[ii]]$weight
      a_survfit<-survfit(Surv(ot_sub,delta_sub)~1,weights=weight_sub)
      a_dostep<-dostep(a_survfit$time,a_survfit$surv)
      xscale<-c(0,max(ot))
      yscale<-c(0,1)
      vp<-viewport(
        x=unit(xx_node[ii],"native"),y=unit(0.5*fig_height+pad_bottom,"native"),
        width=unit(node_width*2,"native"),height=unit(fig_height,"native"),
        xscale=xscale+c(-0.025,0.025)*(xscale[2]-xscale[1]),
        yscale=yscale+c(-0.025,0.025)*(yscale[2]-yscale[1]))
      pushViewport(vp)
      grid.rect()
      grid.lines(
        x=unit(c(0,a_dostep$x),"native"),
        y=unit(c(1,a_dostep$y),"native"))
      if(ii==1)grid.yaxis(gp=gpar(cex=0.25))
      grid.xaxis(gp=gpar(cex=0.25))
      popViewport(1)
    }
  }
  
  grid.text("Time (years)",y=unit(0.5*pad_bottom,"native"),x=unit(0.5,"npc"),gp=gpar(fontsize=8))
}
