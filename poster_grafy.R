library(ggplot2)
library(gridExtra)

 
#--------------------------------------
#  GRAF 1
#-------------------------------------

# Zmena pH pocas ischemie

data <- read.csv("data/ph_fosfat.csv")
ph <- ggplot(data, aes(x = Time, y = ph))
pH <- ph + geom_line(colour="brown", size=1.8) + theme_bw() +
theme(text = element_text(size = 22), axis.text.y = element_text(size = 16, 
        colour = "darkblue"), axis.text.x = element_text(size = 16,colour = "darkblue"), 
          axis.title.y = element_text(vjust = 1.6), plot.margin=unit(c(1,1.5,1,1), "cm")) +
labs(x = "Time [min]", y = "pH") +
ggtitle("pH changes during ischemia")

# Zmena fosfatu pocas ischemie

Pi <- ggplot(data, aes(x = Time, y = Pi))
P <- Pi + geom_line(colour="navyblue", size=1.8) + theme_bw() +
theme(text = element_text(size = 22), axis.text.y = element_text(size = 16, colour="darkblue"), 
axis.text.x = element_text(size = 16, colour="darkblue"), axis.title.y = element_text(vjust = 1.6), plot.margin=unit(c(1,2,1,1.5), "cm")) + 
labs(x = "Time [min]", y = "Phosphate (mM)" ) +
ggtitle("Phosphate concentration during ischemia")

pH.P <- arrangeGrob(pH, P, ncol = 2)    
ggsave("fig/graf1.pdf", pH.P, width = 14, height = 5)


#------------------------------------------------
#  GRAF 2
#------------------------------------------------

# Vply zmeny pH na silu

df_ph <- read.csv("data/pH.csv")
p <- ggplot(df_ph, aes(x = Time, y = Force)) 
pp <- p + geom_line(size = 1.8, colour="brown") +  scale_x_continuous(breaks=c(-4,0,5,10)) +
theme_bw() + geom_segment(aes(x = 0, y = 0, xend = 0, yend =1.0), colour = "darkred", linetype = "dashed") +
theme(text = element_text(size = 22), axis.text.y = element_text(size = 16, colour = "darkblue"), axis.text.x = element_text(size = 16,colour = "darkblue"), 
      axis.title.y = element_text(vjust = 1.6), plot.margin=unit(c(1,1.5,1,1), "cm")) +
labs(x = "Time [min]", y = "Normalized force" ) +
ggtitle("Effect of pH on force") +
annotate("text", x = -2, y = 0.37, label = "Before \n ischemia", colour = "midnightblue", size = 6.5)

# Vply zmeny fosfatu na silu

fosfat <- read.csv("data/fosfat.csv")
fos <- ggplot(fosfat, aes(x = Time, y = Force))
# dorabky na graf
ffos <- fos + geom_line(size = 1.8, colour="navyblue") +  scale_x_continuous(breaks=c(-4,0,5,10)) +
theme_bw() + geom_segment(aes(x = 0, y = 0, xend = 0, yend =1.0), colour = "darkred", linetype = "dashed") +
theme(text = element_text(size = 22), axis.text.y = element_text(size = 16, colour = "darkblue"), 
      axis.text.x = element_text(size = 16,colour = "darkblue"), axis.title.y = element_text(vjust = 1.6), plot.margin=unit(c(1,2,1,1.5), "cm")) +
labs(x = "Time [min]", y = "Normalized force" ) +
ggtitle("Effect of phosphate on force") +
annotate("text", x = -2, y = 0.37, label = "Before \n ischemia", colour = "midnightblue", size = 6.5)

fos.ph <- arrangeGrob(pp, ffos, ncol = 2)
ggsave("fig/graf2.pdf", fos.ph, width = 14, height = 5)

#---------------------------------------
#  GRAF 3
#---------------------------------------

df <- read.csv("data/ischemia.csv")
df$Ischemic.time <- factor(df$Ischemic.time, levels=c("T = 0 min","T = 2.5 min","T = 5.0 min","T = 10.0 min"))
g <- ggplot(df, aes(x = Time, y = Force, colour = Ischemic.time, group = Ischemic.time)) 
gg <- g + geom_line(size = 1.5) + theme_bw() +
theme(legend.position=c(0.8,0.7), legend.key.size = unit(1.0,"cm"), 
      legend.key.width = unit(1.5,"cm"), axis.text.x = element_blank(),
      axis.text.y = element_text(size=16,color="darkblue"), text = element_text(size = 22), 
            axis.title.y = element_text(vjust = 1.6), axis.title.x = element_blank()) +
labs(x = "Time [ms]", y = "Normalized force") + 
guides(colour = guide_legend(override.aes = list(size=5))) + 
scale_colour_discrete(name  ="Ischemic times") +
ggtitle("Force influenced by ischemic concentration of pH and Pi")
ggsave("fig/graf3.pdf", gg, width = 10, height = 5)
