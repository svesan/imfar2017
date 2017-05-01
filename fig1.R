library(plotrix)
pe=c(3.4,2.4,2.9,2.6,6.9,11.5,0.8,0.8,1.0)
lc=c(2.8,1.9,2.4,2.3,6.0,9.2,0.7,0.6,0.5)
uc=c(4,3,3.6,3,7.9,14.4,0.9,0.9,2.1)
x =1:length(pe)

# plotCI(x, log(pe), ui=log(uc), li=log(lc), sfrac=0.001, slty=1, lwd=2)
# abline(h=0)
# points(x, log(pe), col="blue", pc=3)




library(ggplot2)
p2 <- ggplot(hospital.estimates, aes(var,idr, size=10)) + theme_bw(base_size=10))

p2 + geom_point() +geom_errorbar(aes(x = var, ymin = lower, ymax = upper, size=2), width = 0.2) + scale_y_log10(limits=c(0.1, 50), breaks=c(0.1, 0.5, 1, 5, 10, 25, 50)) + xlab("Site") + ylab("RR")

xc=c("PD", "Mood", "Anxiety", "Personal", "ID", "ASD", "Cognition", "Hernia", "Diab")

estim=data.frame(x,pe,lc,uc,xc)


#-- Set the order of the labels
#-- http://www.cookbook-r.com/Graphs/Axes_(ggplot2)/
p2 + geom_errorbar(aes(x = xc, ymin = lc, ymax = uc, size=1), width = 0.001, color="blue") + scale_y_log10(limits=c(0.25, 15), breaks=c(0.25, 1, 5, 15))+ coord_flip()+ylab("RR")+xlab("Disorder")+scale_x_discrete(limits=c("PD", "Mood", "Anxiety", "Personal", "ID", "ASD", "Cognition", "Hernia", "Diab"))

#values=c("#999999", "#E69F00", "#56B4E9")

#-- Add reference line
#-- Font size axis text
#-- Remove yaxis label
p2 + geom_errorbar(aes(x = xc, ymin = lc, ymax = uc, size=1), width = 0.001, color="#56B4E9") + scale_y_log10(limits=c(0.5, 15), breaks=c(0.5, 1, 2, 5, 10, 15))+ylab("Relative Risk")+xlab("Disorder")+scale_x_discrete(limits=c("ASD", "ID", "PD", "Mood", "Anxiety", "Personal", "Cognition", "Hernia", "Diab"))+ geom_hline(yintercept = 1)+theme(axis.text.x = element_text(face="plain", color="#993333", size=14, angle=0), axis.title.y=element_blank(), axis.text.y = element_text(face="plain", color="#993333", size=14, angle=0))+ coord_flip()

last_plot() + theme(axis.title.x=element_text(face="bold", color="#993333", size=14))

#-- remove legend
last_plot() + theme(legend.position="none")

