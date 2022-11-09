#codes for figure 1 to 6
library(tidyverse)
library(randomForest)
library(Hmisc)

###===========================Fig1==============================================
##for yield & density of CN and USA
ggplot(data1, aes(x=year,y=values))+
  geom_bar(aes(fill = set),color='#C7C7C7', stat="identity",  alpha=1, width = 0.8, position = "identity")+
  geom_smooth(method = lm, aes(group=set, color=set, fill=set), linetype=1,formula = y ~ x, alpha=1, level=0)+
  scale_fill_manual(values =c('#00B050','#81C688'), breaks =c('China','USA'))+
  scale_color_manual(values =c('#C55A11','#F4B183'), breaks =c('China','USA'))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.ticks.length=unit(-0.5,"lines"),
        axis.title = element_text(size=14),
        legend.background = element_blank(),
        legend.text = element_text(size=14),
        legend.position = c(0.15, 0.88))


###===============================Fig2==========================================
##Data distributions of OPDs and Yield_opt
ggplot(data2, (aes(val, fill=sets)))+
  geom_histogram(binwidth=.5, alpha=.5, position="identity")+
  geom_vline(aes(xintercept= m1), color="red", linetype="dashed", size=1)+
  geom_vline(aes(xintercept= m2), color="blue", linetype="dashed", size=1)+
  coord_cartesian(xlim = c(2, 22), ylim = c(5, 110))+
  labs(x=quote(AOPD~("×"~10^4~plants~ha^-1)~or~Yield~(Mg~ha^-1)), y="Count")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color="black", size=1, linetype=1),
        legend.title = element_blank(),
        legend.position = c(0.15, 0.85),
        legend.background = element_blank())

##Comparisons of the estimated OPD by RF model with observed OPD in 448 observations
ggplot(data3)+
  geom_point(data= out1, aes(x=obs, y=pred), alpha=0.4, size= 2, color= "#1E90FF")+
  geom_abline(intercept=0,slope=1,colour="DimGrey",size=0.6,linetype="solid")+
  geom_abline(intercept=er,slope=1,colour="brown",size=0.6,linetype="dashed")+
  geom_abline(intercept=-er,slope=1,colour="brown",size=0.6,linetype="dashed")+
  scale_x_continuous(expand=c(0,0), limits = c(0, 16), breaks = seq(5,15,5))+
  scale_y_continuous(expand=c(0,0), limits = c(0, 16), breaks = seq(5,15,5))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size=16,vjust=-0.2),
        axis.text = element_text(size=16,color = 'black'),
        legend.position =c(2, 0.89))

##Linear-model for the relationship between OPD and Yield_OPD
ggplot(data4, aes(x=optd, y=opty))+
  geom_point(size=2, color='#BEBEBE', shape=1, alpha=1)+
  geom_smooth(method = 'lm', formula = y ~ x, level=0.95,size=1,alpha=0.8, color='#00A277', fill='#DDF1EB')+
  scale_y_continuous(limits = c(0,22), breaks = seq(0,22,5))+
  theme_bw()+
  labs(x=quote(AOPD~("×"~10^4~plants~ha^-1)), y=quote(Yield~(Mg~ha^-1)))+
  theme(panel.grid = element_blank())


###=============Fig3============================================================
##relative importance (regionally)
#--data analysis--
regions <- c('NE', 'NCP', 'NW', 'SW')
outfile <- data.frame()
for (r in regions){
  print(r)
  df1 <- filter(df0, region == r)
  #1:relative importance
  data1 <-  select(df1, SOM, GDD, Radn,  Tmax, Tmin, Prec, optd) #pH,
  set.seed(1234)
  fit <- randomForest(optd~., data1, importance=TRUE, mtry = 3, ntree=500)
  a1 <- fit$importance
  b1 <- rownames(a1)
  out <- data.frame(fact = b1, IncMSE=a1[,1])
  relat <- out$IncMSE/sum(out$IncMSE)
  out$relat <- relat
  #2: correlation
  data2 <-  select(df1, SOM,  GDD, Radn,  optd, Tmax, Tmin, Prec, optd)#pH,
  cor <- rcorr(as.matrix(data2))
  cor_r <- data.frame(cor$r, fact = rownames(cor$r))
  rr <- select(cor_r, fact, optd)
  colnames(rr)[2] <- 'r'
  out <- merge(out, rr, by= 'fact')
  cor_p <- data.frame(cor$P, fact = rownames(cor$P))
  pp <- select(cor_p, fact, optd)
  colnames(pp)[2] <- 'p'
  out <- merge(out, pp, by= 'fact')
  out$region <- r
  outfile <- rbind(outfile, out)
}
df <- outfile
df <- mutate(df, rr=ifelse(r>=0, '+', '-'))
df <- mutate(df, pp=ifelse(p < 0.001, '***', ifelse(p<0.01, '**', ifelse(p<0.05,'*', ''))))
#figure
ggplot(df)+
  geom_point(aes(y=region, x=fact, size=relat, fill=region, color = region),alpha=0.8, shape=21)+
  scale_size(range = c(5, 25), limits = c(0, 0.5),breaks = c(0.03, 0.15, 0.3), name=NULL,guide = "legend")+
  scale_fill_manual(values =c("#7CAE00","#F8766D","#00BFC4","#C77CFF"), breaks =c('NE','NCP','NW','SW'),guide ="none")+
  scale_color_manual(values =c("#7CAE00","#F8766D","#00BFC4","#C77CFF"), breaks =c('NE','NCP','NW','SW'),guide ="none")+
  geom_text(aes(y=region,x=fact), label = paste(df$rr), size=6)+
  geom_text(aes(y=region,x=fact), label = paste(df$pp), size=6, vjust = 1.5)+
  theme_light()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color="black", size=1, linetype=1),
        legend.position = 'right')

##relationship between indicators and OPDs##
library(easynls)
#SOM===
#def linear-plateau curve 
f1 <- function(x){a + b * (x - c) * (x <= c)}
#def quadratic curve
f2 <- function(x){c*x*x + b*x + a}
#def linear curve
f3 <- function(x){b*x + a}
#NE
df1 <- filter(df, region == 'NE')
data <- select(df1, SOM, optd)
fit <- nlsfit(data, model = 3, start = c(8, 1,15))
nlsfit(data, model = 2)

fit$Parameters$optd
a <- fit$Parameters$optd[1]
b <- fit$Parameters$optd[2]
c <- fit$Parameters$optd[3]
xx <- seq(0, 70, 0.5)
yy <- f1(xx)
df.p1 <- data.frame(SOM=xx, optd=yy,region='NE')

#NCP
df1 <- filter(df, region == 'NCP')
data <- select(df1, SOM, optd)
fit <- nlsfit(data, model = 2)
fit$Parameters$optd
a <- fit$Parameters$optd[1]
b <- fit$Parameters$optd[2]
c <- fit$Parameters$optd[3]
xx <- seq(0, 30, 0.5)
yy <- f2(xx)
df.p2 <- data.frame(SOM=xx, optd=yy,region='NCP')

#NW
df1 <- filter(df, region == 'NW')
data <- select(df1, SOM, optd)
fit <- nlsfit(data, model = 1)
fit$Parameters$optd
a <- fit$Parameters$optd[1]
b <- fit$Parameters$optd[2]
xx <- seq(0, 20, 0.5)
yy <- f3(xx)
df.p3 <- data.frame(SOM=xx, optd=yy,region='NW')

#SW
df1 <- filter(df, region == 'SW')
data <- select(df1, SOM, optd)
fit <- nlsfit(data, model = 2)
fit$Parameters$optd
a <- fit$Parameters$optd[1]
b <- fit$Parameters$optd[2]
xx <- seq(0, 30, 0.5)
yy <- f3(xx)
df.p4 <- data.frame(SOM=xx, optd=yy,region='SW')
df.p <- rbind(df.p1, df.p2, df.p3)
dff <- filter(df, region!='SW')
ggplot()+
  geom_point(data=dff, aes(x=SOM, y=optd, shape = region), size=1.5, color= '#BEBEBE',alpha=0.8)+
  scale_shape_manual(values = c(3,4,2,1), breaks =c('NE','NCP','NW','SW'))+
  geom_line(data = df.p, aes(SOM, optd, color=region), size=1.25)+
  geom_vline(xintercept = 20, linetype=2, color='#8B8682', size = 1)+
  scale_y_continuous(expand=c(0,0), limits = c(0, 16), breaks = seq(0,15,5))+
  scale_color_manual(values = c( "#7CAE00","#F8766D","#00BFC4","#C77CFF"), breaks =c('NE','NCP','NW','SW'))+
  theme_bw()+
  theme(panel.grid = element_blank())
        
#T_min==
ggplot(df,aes(x=Tmin, y=optd))+
  geom_point(aes(shape=region), size=1.5, color= '#BEBEBE',alpha=0.8)+
  geom_smooth(aes(color=region),fill='#FFFFFF', method = 'lm', formula = y~x, level = 0,size=1.25,alpha=0)+
  scale_x_continuous(limits = c(10, 25), breaks = seq(12, 25, 4))+
  scale_y_continuous(expand=c(0,0), limits = c(0, 16), breaks = seq(0,15,5))+
  scale_shape_manual(values = c(3,4,2,1), breaks =c('NE','NCP','NW','SW'))+
  scale_color_manual(values = c( "#7CAE00","#F8766D","#00BFC4","#C77CFF"), breaks =c('NE','NCP','NW','SW'))+
  theme_bw()+
  theme(panel.grid = element_blank())

#T_max==
#def quadratic curve function 
f2 <- function(x){c*x*x + b*x + a}
fit <- nlsfit(data, model = 2)
fit$Parameters$optd
a <- fit$Parameters$optd[1]
b <- fit$Parameters$optd[2]
c <- fit$Parameters$optd[3]
xx <- seq(25, 32, 0.1)
yy <- f2(xx)
ggplot()+
  geom_point(data=df1, aes(x=Tmax, y=optd,shape=region), size=1.5, color= '#BEBEBE',alpha=0.8)+
  geom_line(data=df2, aes(x=Tmax, y=optd, color= region), size=1.25)+
  scale_x_continuous(limits = c(25, 32), breaks = seq(25, 35, 2))+
  scale_y_continuous(expand=c(0,0), limits = c(0, 16), breaks = seq(0,15,5))+
  scale_shape_manual(values = c(4,1), breaks =c('NCP','SW'))+
  scale_color_manual(values = c("#F8766D","#C77CFF"), breaks =c('NCP','SW'))+
  theme_bw()+
  theme(panel.grid = element_blank())

#Radn==
ggplot(df1, aes(x=Radn, y=optd))+
  geom_point(aes(shape=region), size=1.5, color= '#BEBEBE',alpha=0.8)+
  geom_smooth(aes(color=region, fill=region), method = 'lm', formula = y~x,alpha=0.2,size=1.25,level=0)+
  scale_shape_manual(values = c(3,2,1), breaks =c('NE','NW','SW'))+
  scale_color_manual(values = c("#7CAE00","#00BFC4","#C77CFF"), breaks =c('NE','NW','SW'))+
  scale_fill_manual(values = c("#7CAE00","#00BFC4","#C77CFF"), breaks =c('NE','NW','SW'))+
  scale_x_continuous(limits = c(1400, 4000), breaks = seq(1500, 4000, 1000))+
  scale_y_continuous(expand=c(0,0), limits = c(0, 16), breaks = seq(0,15,5))+
  theme_bw()+
  theme(panel.grid = element_blank())

#Prec==
#def quadratic function
f2 <- function(x){c*x*x + b*x + a}
#NE
data <- select(ne, Prec, optd)
fit <- nlsfit(data, model = 2)
fit$Parameters$optd
a <- fit$Parameters$optd[1]
b <- fit$Parameters$optd[2]
c <- fit$Parameters$optd[3]
xx <- seq(200, 1200, 1)
yy <- f2(xx)
df.p1 <- data.frame(Prec=xx, optd=yy,region='NE')
#NW
data <- select(nw, Prec, optd)
fit <- nlsfit(data, model = 2)
fit$Parameters$optd
a <- fit$Parameters$optd[1]
b <- fit$Parameters$optd[2]
c <- fit$Parameters$optd[3]
xx <- seq(200, 700, 1)
yy <- f2(xx)
df.p2 <- data.frame(Prec=xx, optd=yy,region='NW')
df1 <- rbind(df.p1, df.p2)

ggplot()+
  geom_point(data=df1, aes(x=Prec, y=optd,shape=region), size=1.5, color= '#BEBEBE',alpha=0.8)+
  geom_line(data=df1, aes(x=Prec, y=optd, color= region), size=1.25)+
  scale_x_continuous(limits = c(150, 1300), breaks = seq(200, 1300, 400))+
  scale_y_continuous(expand=c(0,0), limits = c(0, 16), breaks = seq(0,15,5))+
  scale_shape_manual(values = c(3,2), breaks =c('NE','NW'))+
  scale_color_manual(values = c( "#7CAE00","#00BFC4"), breaks =c('NE','NW'))+
  scale_fill_manual(values = c( "#7CAE00","#00BFC4"), breaks =c('NE','NW'))+
  theme_bw()+
  theme(panel.grid = element_blank())

#GDD==
ggplot(data=df,aes(x=GDD, y=optd))+
  geom_point(color = '#BEBEBE',alpha=0.8, shape =4)+
  geom_smooth(method = 'lm',formula = y~x, size=1.25, color = '#F8766D', fill = '#F8766D',level = 0,alpha=0.2)+
  scale_x_continuous(limits = c(1000, 2200), breaks = seq(1000, 2200,400))+
  scale_y_continuous(expand=c(0,0), limits = c(0, 16), breaks = seq(0,15,5))+
  theme_bw()+
  theme(panel.grid = element_blank())


###==========================Fig4===============================================
#Comparation of maize density and yield under different methods.
ggplot(df1, mapping = aes(x=set, y=density, fill=set, group=factor(1)))+#or y= yield
  geom_bar(stat= "identity", width = 0.5)+
  scale_fill_manual(values =c('#D0D0D0','#64C5A6','#64C5A6','#64C5A6'), breaks =c('Actual','M1','M2','M3'))+
  scale_y_continuous(expand=c(0,0), limits=c(0, 13), breaks = seq(0, 12, 3))+
  labs(x='Method', y='Density')+#or yield
  theme_bw()+
  theme(panel.grid = element_blank())

###==============================Fig5===========================================
ggplot(data, aes(x=region, y=values))+
  geom_bar(aes(fill=set),stat= "identity", position = 'dodge', width = 0.5)+
  scale_fill_manual(values = c('#8DA0CB','#F4B183','#64C5A6'), breaks = c('ck','opt','rf'), 
                    labels = c("Control density",'OPD (field trials)','OPD (RF)'))+
  scale_y_continuous(expand=c(0,0), limits=c(0, 12.5), breaks = seq(0, 12, 3))+
  theme_bw()+
  theme(panel.grid = element_blank())


###=============================Fig6============================================
##ab
ggplot(fig, aes(x=region, y=AOPD))+
  geom_boxplot(aes(fill= set), alpha=0.7, varwidth = FALSE, 
               outlier.colour="#D3D3D3", outlier.size=1.5, position=position_dodge(.9))+
  stat_summary(fun ='mean', geom = 'point', aes(group=set), color='black', position=position_dodge(0.9), pch=10, size=2)+
  scale_fill_manual(values =c('#008000','#00FF00','#FFBF66','#FF8000'), breaks =c('Baseline','S1','S2','S3'),
                    labels =c('2010s','2010s (soil improvement)','2030s','2030s (soil improvement)'))+
  scale_y_continuous(limits=c(3, 14), breaks = seq(3, 12, 3))+
  theme_bw()+
  theme(panel.grid = element_blank())

##cd
#OPD
ggplot()+
  geom_segment(aes(x = 2030, y = 8, xend = -Inf, yend = 8), size=0.5, linetype = 2, color='#D0D0D0') +
  geom_ribbon(aes(x = x1, ymin = d2, ymax = d3), alpha = 1, fill = "#FFDDCE")+  #"#C4E0F8"
  geom_ribbon(aes(x = x1, ymin = d1, ymax = d2), alpha = 1, fill = "#C4E0F8")+  #"#FFDDCE"
  geom_line(data=df1, aes(x=year, y=density, color = set), size=1, linetype = 1)+
  geom_line(data=df2, aes(x=year, y=density, color = set), size = 0.75, linetype = 1)+
  scale_color_manual(values =c('#000000','#FF0000','#4F95D0','#DB6B4E'), breaks =c('history','trend','AOPD','SOM'),
                     labels =c('Historical density','Historical trend','OPD','OPD (soil improvement)'))+
  scale_x_continuous(limits=c(1990,2032), breaks = seq(1950, 2030, 20))+
  scale_y_continuous(limits=c(4, 9.8), breaks = seq(4, 9, 2))+
  theme_bw()+
  labs(x='Year', y =quote(Plant~density~('×'~10^4~plants~ha^'-1')))+
  theme(panel.grid = element_blank())

#Yield
ggplot()+
  geom_segment(aes(x = 2030, y = 10.14, xend = -Inf, yend = 10.14), size=0.5, linetype = 2, color='#D0D0D0') +
  geom_ribbon(aes(x = x1, ymin = y2, ymax = y3), alpha = 1, fill = "#FFDDCE")+  #"#C4E0F8"
  geom_ribbon(aes(x = x1, ymin = y1, ymax = y2), alpha = 1, fill = "#C4E0F8")+  #"#FFDDCE"
  geom_line(data=df1, aes(x=year, y=yield, color = set), size=1, linetype = 1)+
  geom_line(data=df2, aes(x=year, y=yield, color = set), size = 0.75, linetype = 1)+
  scale_color_manual(values =c('#000000','#FF0000','#4F95D0','#DB6B4E'), breaks =c('history','trend','AOPD','SOM'),
                     labels =c('Historical yield','Historical trend', quote("Yield"["OPD"]), quote('Yield'['OPD']~'(soil improvement)')))+
  scale_x_continuous(limits=c(1990,2032), breaks = seq(1990, 2030, 20))+
  scale_y_continuous(limits=c(3, 14), breaks = seq(4, 16, 4))+
  theme_bw()+
  theme(panel.grid = element_blank())
