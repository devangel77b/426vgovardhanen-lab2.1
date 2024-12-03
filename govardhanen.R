library(ggplot2)
raw <- read.csv("govardhanen.csv",header=TRUE)

library(dplyr)
data <- mutate(
 raw,
 ameas.ms2 = 2*x.m/t.s^2
 )

apredicted.ms2 <- function(m2,m1,mc){
  9.81*m2/(m2+m1+mc)
}

fig <- ggplot(data)+geom_point(aes(x=m2.kg,y=ameas.ms2))+
    ylim(0,1.0)+xlim(0.020,0.1)+
    geom_function(fun=apredicted.ms2,args=list(m1=0.,mc=0.5),color='blue')+
    geom_function(fun=apredicted.ms2,args=list(m1=2.5,mc=0.5),color='red')+
    xlab('$m_2$ (\\unit{\\kilo\\gram})')+
    ylab('$a$ (\\unit{\\meter\\per\\second\\squared})')+
    theme_bw(base_size=8)

library(svglite)
svglite('fig1.svg',width=3,height=2,pointsize=8)
print(fig)
dev.off()

library(xtable)
results <- summarize(
	mean.t = mean(t.s),
	sd.t = sd(t.s),
	mean.a = mean(ameas.ms2),
	sd.a = sd(ameas.ms2),
	group_by(data,m2.kg)
	)
print(xtable(results),include.rownames=FALSE,file='table1raw.tex')

