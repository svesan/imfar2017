# Creating sex specific figures for IMFAR

library(ggplot2)
require(gridExtra)

imffig = function(data, xc, x, est, lc, uc, cols="#56B4E9") {
  ggplot(data, aes(x, size=10), colour=cols) + theme_bw(base_size=10) + geom_rect(aes(xmin=x-0.2, xmax=x+0.2, ymin=lc, ymax=uc),color=NA,size=0.1,fill=cols) + theme_bw(base_size=10) + scale_y_log10(limits = c(0.3, 400), breaks = c(0.3, 1, 3, 10, 100, 400)) + ylab("Relative Risk") +
    xlab("Disorder") + geom_hline(yintercept = 1) + theme(
      axis.text.x = element_text(
        face = "plain",
        color = "#993333",
        size = 18,
        angle = 0
      ),
      axis.title.y = element_blank(),
      axis.text.y = element_text(
        face = "plain",
        color = "#993333",
        size = 18,
        angle = 0
      )
    ) + theme(axis.title.x=element_text(face="bold", color="#993333", size=18)) + theme(legend.position="none") + theme(axis.ticks.y = element_blank())  + coord_flip()
  
}


ss=read.csv("C:/psync/mssm/Presentations/2017_05_10_IMFAR/sex_sex.csv", colClasses = c("character","character",rep("numeric",6)))
ss$xc = trimws(ss$xc)
ss$x  = rep(seq(8,18,by=2),4)

ss$xmin=ss$x-0.2; ss$xmax=ss$x+0.2
ss$ymin=ss$lc;    ss$ymax=ss$uc

mm=subset(ss, subset=sexsex=="Male-Male");  mf=subset(ss, subset=sexsex=="Male-Female")
fm=subset(ss, subset=sexsex=="Female-Male");  ff=subset(ss, subset=sexsex=="Female-Female")

ps1=imffig(mm, mm$xc, mm$x, mm$est.crude, mm$lc.crude, mm$uc.crude)+scale_x_discrete(limits=mm$x, labels=mm$xc) + annotate("text", label = "Male -> Male", x=19, y=400,size=5,colour="#993333",hjust=1,fontface="bold")

ps2=imffig(mf, mf$xc, mf$x, mf$est.crude, mf$lc.crude, mf$uc.crude)+scale_x_discrete(limits=mf$x, labels=mf$xc) + annotate("text", label = "Male -> Female", x=19, y=400,size=5,colour="#993333",hjust=1,fontface="bold")

ps3=imffig(fm, fm$xc, fm$x, fm$est.crude, fm$lc.crude, fm$uc.crude)+scale_x_discrete(limits=fm$x, labels=fm$xc) + annotate("text", label = "Female -> Male", x=19, y=400,size=5,colour="#993333",hjust=1,fontface="bold")

ps4=imffig(ff, ff$xc, ff$x, ff$est.crude, ff$lc.crude, ff$uc.crude)+scale_x_discrete(limits=ff$x, labels=ff$xc) + annotate("text", label = "Female -> Female", x=19, y=400,size=5,colour="#993333",hjust=1,fontface="bold")

grid.arrange(ps1,ps2,ps3,ps4)

ps4=imffig(ff, ff$xc, ff$x, ff$est.crude, ff$lc.crude, ff$uc.crude)+scale_x_discrete(limits=ff$x, labels=ff$xc) + annotate("text", label = "Female -> Female", x=19, y=400,size=5,colour="#993333",hjust=1,fontface="bold")
