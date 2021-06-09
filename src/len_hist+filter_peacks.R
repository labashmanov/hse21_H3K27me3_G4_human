
ffa_data <- ffa_names
for (variant in c(1))
{
  if (variant==1)
  {
    #fn<-"ffa_plot_avr_age.pdf"
    ffa_data_to_plot<-ffa_data
    #nrow(ffa_data_to_plot)
  } else  {
    #fn<-"ffa_plot_avr_age_normal_w.pdf"
    ffa_data_to_plot<-ffa_data[which(ffa_data$BMI<=25 | is.na(ffa_data$BMI)),] # нормальный вес - для известных >BMI  & ffa_data$bmi>0)
    #nrow(ffa_data_to_plot)
  } 
  # 
  # x<-ffa_data_to_plot$LBDTT3SI
  # x[x==1 | x==2 | x==3]<-NA
  # ffa_data_to_plot$LBDTT3SI<-x
  
  # pdf(fn,title = fn)
  if (!file.exists("graphs/")) 
  {
    dir.create("graphs/")
  }
  
  
  i<-1
  
  jj<-1
  
  for (v in inter_cols) # цикл по переменным
  {
    #v<-as.character(ffa_names$Name[1])
    
    # пропускаем переменные - атрибуты -to_exclude
    if (v %in% to_exclude) next
    if (v %in% names(clone_vector_names)) 
    {
      cat("Skip clone: ",v,"\n") 
      next
    }
    
    folder<-paste0("graphs/",v,"_",gsub("/", "_", ffa_names_[v]))
    folder<-gsub("%", "proc.", folder)  
    
    if (!file.exists(folder)) 
    {
      dir.create(folder)
      folder_hm<-paste0(folder,"/hm_all")
      if (!file.exists(folder_hm)) dir.create(folder_hm)
    }
    
    fn<- paste0(folder,"/",v,"_",gsub("/", "_", ffa_names_[v]),"_mean_type=",mean_type,".pdf")
    fn<-gsub("%", "proc.", fn)
    
    ############################################################
    pdf(fn,title = fn)
    
    #dev.off()
    
    ffa_data3<-ffa_data_to_plot[!is.na(ffa_data_to_plot[,v]),]
    ffa_data4<-ffa_data3[,c(v,"age")]
    # dim(ffa_data4)
    
    x<-as.numeric(ffa_data3[,v])
    qq<-quantile(x,probs = c(qq1,qq2))
    
    if (manual_qq_enabled)
    {
      if (!is.na(qq2_man[v])) qq[2]<-qq2_man[v]
      if (!is.na(qq1_man[v])) qq[1]<-qq1_man[v]
    }
    
    #if (v=="LBXCAP") qq[1]<-1.2
    
    if (show_density)
    {
      plot(density(x),main=paste0("Density plot for ",v, " before filter"))
      abline(v=qq,col="blue")
      plot(density(ffa_data3$age),main=paste0("Density plot for age"))
      
      x2<-x[x>=qq[1] & x<=qq[2]]
      if (qq1qq2_enabled) mm<-"" else mm<-"FILTER DESABLED"
      plot(density(x2),main=paste0("Density plot for ",v, " after filter ",mm))
      
      age_child<-25
      
      ffa_data3_old<-ffa_data3[ffa_data3$age>age_child,]
      x3<-as.numeric(ffa_data3_old[,v])
      plot(density(x3),main=paste0("Density plot for ",v, ", age>",age_child," ",mm))
      abline(v=qq)
      
      x4<-x3[x3>=qq[1] & x3<=qq[2]]
      plot(density(x4),main=paste0("Density plot for ",v, ", age>",age_child," with filter",mm))
      
      plot(density(ffa_data3_old$age),main=paste0("Density plot for age, age>",age_child," ",mm))
      
      
    }
    #abline(v=qq)
    
    if (!is.na(ffa_names[which(ffa_names$Name==v),]$A3_conc_aug2020)) a3_v<-ffa_names[which(ffa_names$Name==v),]$A3_conc_aug2020 else a3_v<-NA
    
    # if (!is.na(a3_v))
    # {
    #   v_min<-min(ffa_data4[,1],a3_v)
    #   v_max<-max(ffa_data4[,1],a3_v)
    # } else {
    #   v_min<-min(ffa_data4[,1])
    #   v_max<-max(ffa_data4[,1])
    # }
    
    
    ffa_data3M<-ffa_data_to_plot[!is.na(ffa_data_to_plot[,v]) & ffa_data_to_plot$sex=="M",]
    ffa_data4M<-ffa_data3M[,c(v,"age")]
    names(ffa_data4M)<-c("v","age")
    #if (!mean2_enabled) 
    if (qq1qq2_enabled) ffa_data4M<-ffa_data4M[which(ffa_data4M$v>=qq[1] & ffa_data4M$v<=qq[2]),] # фильтр по квантилям
    
    #ffa_data3M<-ffa_data3M[which(ffa_data3M[,v]>=qq[1] & ffa_data3M[,v]<=qq[2]),]
    
    ffa_data3F<-ffa_data_to_plot[!is.na(ffa_data_to_plot[,v]) & ffa_data_to_plot$sex=="F",]
    ffa_data4F<-ffa_data3F[,c(v,"age")]
    names(ffa_data4F)<-c("v","age")
    #if (!mean2_enabled) 
    if (qq1qq2_enabled) ffa_data4F<-ffa_data4F[which(ffa_data4F$v>=qq[1] & ffa_data4F$v<=qq[2]),] # фильтр по квантилям
    
    
    #ffa_data3F<-ffa_data3F[which(ffa_data3F[,v]>=qq[1] & ffa_data3F[,v]<=qq[2]),]
    
    n_M<-nrow(ffa_data4M)
    n_F<-nrow(ffa_data4F)
    rmw_M<-k_rmw*n_M
    rmw_F<-k_rmw*n_F
    
    
    cat(paste("\n\nCalc&plot ",v,n_M,n_F,rmw_M,rmw_F,paste0(i,"/",length(inter_cols)))," ",ffa_names_[v],"\n")
    
    if_m<-(nrow(ffa_data4M)!=0)
    if_f<-(nrow(ffa_data4F)!=0)
    
    if (if_m) pp_M<-aggregate(v ~ age, ffa_data4M, mean ) else pp_M<-data.frame(v=NULL,age=NULL)
    if (if_f) pp_F<-aggregate(v ~ age, ffa_data4F, mean ) else pp_F<-data.frame(v=NULL,age=NULL)
    
    pp_all<-rbind(pp_M,pp_F)
    
    # head(ffa_data4M)
    # table(ffa_data4M$age)
    # head(pp_M)
    # head(pp_F)
    
    
    #qq_pp_all<-quantile(pp_all$v,probs = c(0.01,0.95))
    #pp_all<-pp_all[which(pp_all$v>=qq_pp_all[1] & pp_all$v<=qq_pp_all[2]),] # убирам выбросы при определения масштабирования графика
    
    tit<-paste0(v, " n(M)=",n_M," rmw_M=",rmw_M, " n(F)=",n_F," rmw_F=",rmw_F," Mean_type=",mean_type)
    
    logxy=""
    if (v %in% log_y_a) logxy<-"y"
    
    # начинаем строить график (пустой пока)
    plot(pp_all,ylab = ffa_names_[v],col=col,main=tit, xlab="Age, years",log=logxy,type="n", cex.main=0.8)
    legend("topleft", legend=c("Male", "Female"),col=c("red", "blue"), lty=1,lwd=2, cex=0.8)
    
    
    ffa_data4_list<-list()
    
    for(sex in c("M","F"))
    {
      #ffa_data3<-ffa_data_to_plot[!is.na(ffa_data_to_plot[,v]) & ffa_data_to_plot$sex==sex,]
      if (sex=="M") 
      {
        ffa_data3<-ffa_data3M
        rmw<-rmw_M
      }
      if (sex=="F") 
      {
        ffa_data3<-ffa_data3F
        rmw<-rmw_F
      }
      
      
      ffa_data4<-ffa_data3[,c(v,"age")]
      names(ffa_data4)<-c("v","age")
      
      
      if (nrow(ffa_data4)==0) next
      
      #if (!mean2_enabled) 
      ffa_data4<-ffa_data4[which(ffa_data4$v>=qq[1] & ffa_data4$v<=qq[2]),] # фильтр по квантилям
      
      pp<-aggregate(v ~ age, ffa_data4, mean )
      
      # сохраняем для дальнейшего использования
      ffa_data4_list[[sex]]<-ffa_data4
      ages<-ffa_data4$age
      
      if (sex=="M") col="red" else col="blue"
      if (sex=="M") col2="deeppink3" else col2="slateblue1"
      
      #points(pp,ylab = ffa_names_[v],col=col)
      tit<-paste0(v, " n(M)=",n_M," rmw_M=",rmw_M, " n(F)=",n_F," rmw_F=",rmw_F," Mean_type=",mean_type)
      
      # мои графики
      #plot(pp,ylab = ffa_names_[v],col=col,main=tit, xlab="Age, years",log=logxy,type="n", cex.main=0.8)
      if (sex=="M" & !is.null(a3_v) & all(is.na(a3_v))) points(34,a3_v,col=col,pch=8) # мои данные
      
      ffa_data4<-ffa_data4[order(ffa_data4$age),]
      d4<-data.frame(age=ffa_data4$age,v=ffa_data4$v)
      
      n<-nrow(d4)
      
      d4$age<-d4$age+runif(nrow(d4)) # рандомизация age
      
      #rmw<-rmw*1.1
      
      #mean_type<-1
      
      rmw0<-rmw
      
      iii<- -0.1
      
      koef_<-c(0.01, 0.04, 0.07, 0.105, 0.137, 0.168, 0.2)
      
      for (koef_i in koef_)
      {
        rmw<-rmw0*koef_i
        # align = c("center", "left", "right")
        if (mean_type==1)  rm1<-rollmean(d4,rmw) 
        #if (mean_type==2)  rm1<-rollapply(d4, rmw, mean2cmp)
        #if (mean_type==3)  rm1<-rollapply(d4, rmw, mean3)
        #if (mean_type==4)  rm1<-roll_mean(d4,rmw) 
        
        tit<-paste0(v, " koef=", koef_i, " n(M)=", ", Sex=", sex," Mean_type=",mean_type)
        plot(rm1[,1],rm1[,2]+iii,col=col,lwd=1,type="l",main=tit, xlab="Age, years",log=logxy) #darkgreen
        
        rsave<-data.frame(v=v,sex=sex,rmw=rmw,rmw0=rmw0,koef_i=koef_i,rm1)
        if (jj==1) rm_save_df<-rsave else rm_save_df<-rbind(rm_save_df,rsave)
        jj<-jj+1
        
        iii<-iii+0.03
      }
      # plot(d4)
      
      #rsave<-data.frame(v=v,sex=sex,rmw=rmw,rm1)
      
      
      
      #abline(v=c(25,34,45,50),lty=2)
      abline(v=tt1,lty=2,col="brown3")
      #  abline(v=tt2,lty=2,col="darkgreen")
      abline(v=tt3,lty=2,col="slateblue4")
      
      if (v=="LBDGLUSI") abline(h=glu_crit,lty=2,col="brown3")
      if (v=="LBDGLTSI") abline(h=glu_crit,lty=2,col="brown3")
      if (v=="LBDTCSI") abline(h=chol_crit,lty=2,col="brown3")
      
      
    } # loop by sex
    
    #rmw_M<-rmw_M*2
    #rmw_F<-rmw_F*2
    
    dev.off()
    
    
    for(sex in c("M","F"))
    {
      d3_0<-ffa_data4_list[[sex]]$v
      ages_0<-ffa_data4_list[[sex]]$age
      
      if (length(d3_0)==0) 
      {
        cat("No data ",v,"on sex=",sex,"\n")
        next
      }
      
      # plot(ages_0,d3_0)
      #plot(density(d3_0))
      #dev.off()
      #sex<-"M"
      # if (sex=="M")
      # {
      #   d3_0<-ffa_data4M$v
      #   ages_0<-ffa_data4M$age
      #   
      # } else
      # {
      #   d3_0<-ffa_data4F$v
      #   ages_0<-ffa_data4F$age
      # }
      # 
      #write.csv(ffa_data4M,file="ffa_data4M.csv",row.names = F)
      #write.csv(ffa_data4F,file="ffa_data4F.csv",row.names = F)
      
      #d3<-ffa_data4$v
      #ages<-ffa_data4F$age
      
      #plot(density(d3))
      
      #d3_virtual<-rnorm(n = length(d3),mean = mean(d3),sd = sd(d3))
      
      
      #gplist<-list()
      frac_sample<-0.8
      max_delta<-0.05 #0.05
      bs_n<-50
      top_quantile_filter<-0.9
      
      
      #myplot_g <- function(variant)
      fns<-list()
      ptm <- proc.time()
      for (variant in 1:bs_n)
      {
        sam<-sample(1:length(d3_0),round(length(d3_0)*frac_sample), replace = F)
        
        d3<-d3_0[sam]
        ages<-ages_0[sam]
        
        #table(d3)
        
        plot(ages,d3)
        #sample(data.frame(ages=ages,d3=d3),round(length(d3)*0.6)) #-d3_virtual
        
        #plot(density(d3_virtual))
        
        vmin<-min(d3)
        vmax<-quantile(d3,top_quantile_filter) #max(d3)#0.4 #0.3 #max(d3)
        vmax
        setka<-seq(from=vmin,to=vmax,length.out = 150) #500 # by=0.1 100 - для 9го!
        #setka<-round(setka,4)
        #val_min<-quantile(d3,c(0.05,0.95))
        lst<-list()
        w<-2 #2 #3 # 8 для 9го!!!
        ages_<-ages#0.5*runif(length(ages),-1,1) #0,8
        
        is_dens_plot<-F
        
        diap<-12:80
        for (a in diap)
        {
          amin<-a-w #-w
          amax<-a+w
          
          to_use<-((ages_>=(amin)) & (ages_<=(amax)))
          
          delta_w<-w
          
          #is.na(table(to_use)["TRUE"])
          
          # расширяем, пока не найдём точки
          while (is.na(table(to_use)["TRUE"]) | (table(to_use)["TRUE"] < 3)) {
            delta_w<-delta_w+1
            to_use<-((ages_>=(amin-delta_w)) & (ages_<=(amax+delta_w)))
            #if (!is.na(table(to_use)["TRUE"]))
            # {
            #   if (table(to_use)["TRUE"] < 3) # чтобы было больше больше 2 точек!
            #   {
            #     delta_w<-delta_w+1
            #     to_use<-((ages_>=(amin-delta_w)) & (ages_<=(amax+delta_w)))
            #   }
            # }
          }
          if (delta_w!=w) cat("delta_w=",delta_w,"\n")
          
          
          #to_use<-((ages_>=(amin-delta_w)) & (ages_<=(amax+delta_w)))
          
          d4<-d3[to_use]
          dd<-density(d4)
          
          # $dd$x
          # par(mfrow=c(2,1)) 
          # plot(10^(dd$y/max(dd$y)))
          # plot(dd$y/max(dd$y))
          #ss<-spline(dd$x, dd$y,xout = setka,method = "natural")
          #points(ss,col="red")
          
          
          #plot(density(dd))
          
          #ss<-spline(dd$x, dd$y,xout = setka,method = "natural") # , cv = TRUE cv = TRUE,xmin=28,xmax=70
          #ss<-spline(dd$x, dd$y/max(dd$y),xout = setka,method = "fmm") # , cv = TRUE cv = TRUE,xmin=28,xmax=70
          ss<-spline(dd$x, 10^(dd$y/max(dd$y)),xout = setka,method = "natural") # , cv = TRUE cv = TRUE,xmin=28,xmax=70  
          #ss<-spline(dd$x, dd$y,xout = setka,method = "natural") # , cv = TRUE cv = TRUE,xmin=28,xmax=70  
          #points(ss,col="red")
          if (is_dens_plot)
          {
            par(mfrow=c(2,1)) 
            plot(dd)
            plot(ss)
          }
          lst[[a]]<-ss$y
          
        } # a
        tab_res<-do.call("rbind", lst)
        dim(tab_res)
        rownames(tab_res)<- diap
        colnames(tab_res)<- setka
        
        require(pheatmap)
        m<-t(as.matrix(tab_res))
        
        
        
        rnew<-sort(as.numeric(rownames(m)),decreasing = T)
        m2<-m[as.character(rnew),]
        
        
        #
        #pheatmap(m2,cluster_cols = F,cluster_rows = F)
        #dev.off()
        
        fn_hm_i<- paste0(folder,"/hm_all/",v,"_",gsub("/", "_", ffa_names_[v]),"_",sex,"_v_",variant,"_hm.pdf")
        fn_hm_i<-gsub("%", "proc.", fn_hm_i)
        fns[[variant]]<-fn_hm_i
        #m2[120,1]<-11
        #m2[120,2]<-11
        #m2[120,3]<-11
        
        max_p_id<-(abs(m2-10)<max_delta)
        
        #m2[,"42"]
        #summary(m2)
        #dim(m2)
        #dim(max_p_id)
        
        #max_p_id[,"12"]
        
        for (a in diap)
        {
          #length(names(m2[,a]))
          #length(max_p_id[,a])
          #max_p_id[,a]
          vvv<-as.numeric(names(m2[,as.character(a)])[max_p_id[,as.character(a)]])
          k_max_delta<-max_delta #0.05
          while(length(vvv)<2) # хак для малого количества точек
          {
            max_p_id<-(abs(m2-10)<(max_delta+k_max_delta))
            vvv<-as.numeric(names(m2[,as.character(a)])[max_p_id[,as.character(a)]])
            k_max_delta<-k_max_delta+max_delta #0.05
          }
          
          rvvv<-data.frame(age=a,maxv=vvv,variant=variant)
          if (variant==1 & a==diap[1]) res_max<-rvvv else res_max<-rbind(res_max,rvvv)
          #max_p_id[,a]
          #table(max_p_id[,a])
          
        }
        
        
        m2[max_p_id]<-11
        lr<-round(as.numeric(rownames(m2)),2)
        
        
        
        #mf<-mean(m2)
        # (max(m2)-min(m2))/(max(m2)+min(m2)) # масштабный фактор
        
        main_str<-paste(v,ffa_names_[v],sex,paste0("vmax=",vmax," w=",w," variant=",variant," BS_frac=",frac_sample))
        
        pheatmap(m2,cluster_rows = F, cluster_cols =F, main = main_str ,
                 fontsize = 4,angle_col = 0, display_numbers = T,labels_row =lr,filename = fn_hm_i) #labels_row = "" ,filename = fn,
        cat(variant," ")
        #gplist[[variant]]<-pp
      } # variant
      proc.time() - ptm
      
      fn_hm<- paste0(folder,"/",v,"_",gsub("/", "_", ffa_names_[v]),"_",sex,"_hm.pdf") # имя файла
      fn_hm<-gsub("%", "proc.", fn_hm)
      
      ptm <- proc.time()
      concatenate_pdfs(fns,fn_hm)
      proc.time() - ptm
      
      fn_bs<-paste0(folder,"/",v,"_",gsub("/", "_", ffa_names_[v]),"_",sex,"_BS_frac_sample=",frac_sample,"_max_delta=",max_delta,"_bs_result.pdf")
      fn_bs<-gsub("%", "proc.", fn_bs)
      
      titl<-paste0("Bootstrap plot for ",ffa_names_[v]," ",sex," frac_sample=",frac_sample," max_delta=",max_delta," N(BS_samples)=",bs_n)
      
      #cols<-rep("red",length(diap))
      #names(cols)<-diap
      #cols["12"]<-"blue"
      #res_max$mycolors<-cols[res_max$age]
      #res_max$mycolors[res_max$age==12]<-"blue"
      #use_wtest<-F
      # использовать ли тест Вилконсона на значимость разницы
      if (use_wtest)
      {
        if (!exists("res_max_list")) res_max_list<-list()
        rm_key<-paste0(v,"-",sex)
        res_max_list[[rm_key]]<-res_max
        
        names(res_max_list)
        
        wres<-wres_f(res_max,diff_type = 2,diap = diap )
        
        # M -19
        #F -40
        wresi<-wres[wres$sex==sex & wres$var==v,]
        
        wresi<-wresi[order(wresi$w_pvalue_ms),]
        
        if (!exists("wresi_all")) 
        {
          wresi_all<-wresi[1:10,]
        } else wresi_all<-rbind(wresi_all,wresi[1:10,])
        
        a_kr<-wresi$age[1:10]
        
        
        res_max$trans<-"Non significant change"
        res_max$trans[res_max$age %in% (a_kr)]<-"Significant change"
        
        res_max$trans<-as.factor(res_max$trans)
        colList = c("gray30","red") #scales::hue_pal()(5)
        gp<-ggplot(res_max, aes(x = age, y = maxv, group=age,col=trans)) +  geom_boxplot()  + xlab("Age, years") + ylab(ffa_names_[v])+ggtitle(titl) + 
          theme(plot.title = element_text(size=10)) +  scale_colour_manual(values = colList) + scale_x_continuous(breaks=diap)
        gp
      }
      else {
        gp<-ggplot(res_max, aes(x = age, y = maxv, group=age)) +  geom_boxplot()  + xlab("Age, years") + ylab(ffa_names_[v])+ggtitle(titl) + 
          theme(plot.title = element_text(size=10)) + scale_x_continuous(breaks=diap)  
      }
      
      # сохраняем данные в лист
      ag_<-aggregate(maxv ~ age, data=res_max, median)
      
      # сохраняем данные в "лист"
      df_to_plot[[paste0(v,"-",sex)]]<-ag_
      
      #gp
      
      fn_bs_data<-paste0("bs_data/res_max_",v,"_",sex,".bin")
      save(res_max,file = fn_bs_data)
      
      #+ scale_x_discrete(breaks=diap,labels=as.character(diap))
      
      # color="red" , face="bold.italic"
      
      ggsave(filename = fn_bs, plot = gp, width = 15,height = 10) 
    } #loop by sex
    
    i<-i+1
    
    
  } # loop by v
  #dev.off()
}