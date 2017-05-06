# Creating figures for IMFAR

imffig = function(data, xc, x, est, lc, uc, cols="#56B4E9") {
  ggplot(data, aes(x, size=10), colour=cols) + theme_bw(base_size=10) + geom_rect(aes(xmin=x-0.2, xmax=x+0.2, ymin=lc, ymax=uc),color=NA,size=0.1,fill=cols) + theme_bw(base_size=10) + scale_y_log10(limits = c(0.25, 24), breaks = c(0.25, 0.5, 1, 2, 4, 8, 16)) + ylab("Relative Risk") +
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


library(ggplot2)
pe=c(3.4,2.4,2.9,2.6,6.9,11.5,0.8,0.8,1.0)
lc=c(2.8,1.9,2.4,2.3,6.0,9.2,0.7,0.6,0.5)
uc=c(4,3,3.6,3,7.9,14.4,0.9,0.9,2.1)
x =1:length(pe)
xc=c("Psycotic", "Mood", "Anxiety", "Personality", "ID", "ASD", "Cognition", "Hernia", "Diabetes-I")

aa=data.frame(x,pe,lc,uc,xc)

#-- Reorder
bb=aa[c(9,8,7,1:6),]
bb$x=x

cols1=c(rep("#009E73",3), rep("#56B4E9",6))

imffig(bb, bb$xc, bb$x, bb$pe, bb$lc, bb$uc, cols=cols1)+scale_x_discrete(limits=bb$x, labels=bb$xc)


#-- Add sweden RRR ASD
cc     =rbind(bb,bb[9,])

cc$xc  =factor(cc$xc, levels=c(levels(bb$xc),"ASD Swe"))
cc[10,1:4]=c(10,10.3,9.4,11.3)
cc[10,5]="ASD Swe"
cols2=c(cols1,"#CC79A7")

imffig(cc, cc$xc, cc$x, cc$pe, cc$lc, cc$uc, cols=cols2)+scale_x_discrete(limits=cc$x, labels=cc$xc)

#-- Add Denmark RRR ASD
dd     =rbind(cc,bb[9,])

dd$xc  =factor(dd$xc, levels=c(levels(cc$xc),"ASD Dan"))
dd[11,1:4]=c(11,6.9,6.1,7.8)
dd[11,5]="ASD Dan"
cols3=c(cols2,"#E69F00")

imffig(dd, dd$xc, dd$x, dd$pe, dd$lc, dd$uc, cols=cols3)+scale_x_discrete(limits=dd$x, labels=dd$xc)

#-- Add Finland RRR ASD
ee     =rbind(dd,bb[9,])

ee$xc  =factor(ee$xc, levels=c(levels(dd$xc),"ASD Fin"))
ee[12,1:4]=c(12,10.4,5.2,21.1)
ee[12,5]="ASD Fin"
cols4=c(cols3,"#D55E00")

imffig(ee, ee$xc, ee$x, ee$pe, ee$lc, ee$uc, cols=cols4)+scale_x_discrete(limits=ee$x, labels=ee$xc)

#-- Add Finland RRR Anxiety
ff     =rbind(ee,bb[9,])

ff$xc  =factor(ff$xc, levels=c(levels(ee$xc),"Anxiety Fin"))
ff[13,1:4]=c(13,1.9,0.9,3.7)
ff[13,5]="Anxiety Fin"
cols5=c(cols4,"#D55E00")

imffig(ff, ff$xc, ff$x, ff$pe, ff$lc, ff$uc, cols=cols5)+scale_x_discrete(limits=ff$x, labels=ff$xc)
