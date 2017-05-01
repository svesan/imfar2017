imffig = function(data, xc, pe, lc, uc) {
ggplot(data, aes(est.crude, xc, size=10)) + theme_bw(base_size=10)  + geom_errorbar(aes(pe,
  x = xc,
  ymin = lc,
  ymax = uc,
  size = 1
),
width = 0.001,
color = "#56B4E9") + scale_y_log10(limits = c(0.25, 400),
                                   breaks = c(0.25, 1, 10, 100, 400)) + ylab("Relative Risk") +
  xlab("Disorder") + scale_x_discrete(
    limits = c(
      "ASD",
      "ID",
      "Psychotic",
      "Mood",
      "Anxiety",
      "Personality"
    )
  ) + geom_hline(yintercept = 1) + theme(
    axis.text.x = element_text(
      face = "plain",
      color = "#993333",
      size = 14,
      angle = 0
    ),
    axis.title.y = element_blank(),
    axis.text.y = element_text(
      face = "plain",
      color = "#993333",
      size = 14,
      angle = 0
    )
  )

last_plot() + theme(axis.title.x=element_text(face="bold", color="#993333", size=14))

#-- remove legend
last_plot() + theme(legend.position="none")

last_plot() + theme(axis.ticks.y = element_blank())  + coord_flip()

}

mm=subset(ss, subset=sexsex=="Male-Male");  mf=subset(ss, subset=sexsex=="Male-Female")
fm=subset(ss, subset=sexsex=="Female-Male");  ff=subset(ss, subset=sexsex=="Female-Female")
p1=imffig(mm, mm$xc, mm$est.crude, mm$lc.crude, mm$uc.crude)+annotate("text", label = "Male -> Male", x="Personality",y=400,size=5,colour="#E69F00",hjust=1)
p2=imffig(mf, mf$xc, mf$est.crude, mf$lc.crude, mf$uc.crude)+annotate("text", label = "Male -> Female", x="Personality",y=400,size=5,colour="#E69F00",hjust=1)
p3=imffig(fm, fm$xc, fm$est.crude, fm$lc.crude, fm$uc.crude)+annotate("text", label = "Female -> Male", x="Personality",y=400,size=5,colour="#E69F00",hjust=1)
p4=imffig(ff, ff$xc, ff$est.crude, ff$lc.crude, ff$uc.crude)+annotate("text", label = "Female -> Female", x="Personality",y=400,size=5,colour="#E69F00",hjust=1)


require(gridExtra)
grid.arrange(p1,p2,p3,p4)


imffig(mf, xc, pe, lc, uc)
imffig(fm, xc, pe, lc, uc)
imffig(ff, xc, pe, lc, uc)
