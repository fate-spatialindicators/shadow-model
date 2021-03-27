library(ggplot2)
library(INLA)
library(inlabru)

r = rgamma(n = 100000, shape = 10, scale = 1/0.01)
df = data.frame("r"=r)
g = ggplot(df, aes(r), 100) +
  geom_histogram(bins=20, col = "blue",fill="blue",alpha=0.3,size=0.1) +
  geom_freqpoly(bins=20, col="darkblue",alpha=0.7) +
  xlab("") +
  ylab("") +
  theme_void()
ggsave("panel_a.jpg", plot=g,width=1.5,height=1.5)

# make prediction cartoon
set.seed(123)
y = runif(20, min=0, max=10)
x = runif(20, min=0, max=10)
df = data.frame("x"=x,"y"=y)
df$pt = "grey30"
df$pt[which(df$x > 1 & df$x < 9 & df$y > 1 & df$y < 9)] = "red"
g = ggplot(df, aes(x,y)) +
  theme_void() +
  geom_segment(aes(x=1,xend=9,y=1,yend=1), col="grey30",linetype=2) +
  geom_segment(aes(x=1,xend=9,y=9,yend=9), col="grey30",linetype=2) +
  geom_segment(aes(x=1,xend=1,y=1,yend=9), col="grey30",linetype=2) +
  geom_segment(aes(x=9,xend=9,y=1,yend=9), col="grey30",linetype=2) +
  geom_point(col=df$pt,alpha=0.5, size=2)
ggsave("panel_b.jpg", plot=g,width=1.5,height=1.5)


g = ggplot(df, aes(x,y,col="white")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_segment(aes(x=0,xend=10,y=0,yend=0), col="grey") +
  geom_segment(aes(x=0,xend=10,y=2,yend=2), col="grey") +
  geom_segment(aes(x=0,xend=10,y=4,yend=4), col="grey") +
  geom_segment(aes(x=0,xend=10,y=6,yend=6), col="grey") +
  geom_segment(aes(x=0,xend=10,y=8,yend=8), col="grey") +
  geom_segment(aes(x=0,xend=10,y=10,yend=10), col="grey") +
  geom_segment(aes(x=0,xend=0,y=0,yend=10), col="grey") +
  geom_segment(aes(x=2,xend=2,y=0,yend=10), col="grey") +
  geom_segment(aes(x=4,xend=4,y=0,yend=10), col="grey") +
  geom_segment(aes(x=6,xend=6,y=0,yend=10), col="grey") +
  geom_segment(aes(x=8,xend=8,y=0,yend=10), col="grey") +
  geom_segment(aes(x=10,xend=10,y=0,yend=10), col="grey") +
  geom_point(col=df$pt,alpha=0.5, size=2)

mesh = inla.mesh.2d(df[,1:2], cutoff=1, max.edge=2)
g = ggplot() + gg(mesh) +
  geom_point(aes(x,y),col=df$pt, fill=df$pt,alpha=0.5, size=2) + theme_void()
ggsave("panel_c.jpg", plot=g,width=1.5,height=1.5)


df2 <- expand.grid(x = seq(0,10,by=2.5), y = seq(0,10,by=2.5))
df2$z <- runif(nrow(df2))
g = ggplot(df2, aes(x, y, fill = z)) +
  geom_raster(alpha=0.3) +
  scale_fill_viridis(begin=0.2) +
  theme_void() +
  geom_point(data=df, aes(x,y), col=df$pt,alpha=0.5, size=2) +
  theme(legend.position = "none")
ggsave("panel_d.jpg", plot=g,width=1.5,height=1.5)


g = ggplot(df, aes(x,y,col="white")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_segment(aes(x=0,xend=10,y=0,yend=0), col="blue",linetype=2,alpha=0.5) +
  geom_segment(aes(x=0,xend=10,y=3.33,yend=3.33), col="blue",linetype=2,alpha=0.5) +
  geom_segment(aes(x=0,xend=10,y=6.66,yend=6.66), col="blue",linetype=2,alpha=0.5) +
  geom_segment(aes(x=0,xend=10,y=10,yend=10), col="blue",linetype=2,alpha=0.5) +
  geom_segment(aes(x=0,xend=0,y=0,yend=10), col="blue",linetype=2,alpha=0.5) +
  geom_segment(aes(x=3.33,xend=3.33,y=0,yend=10), col="blue",linetype=2,alpha=0.5) +
  geom_segment(aes(x=6.66,xend=6.66,y=0,yend=10), col="blue",linetype=2,alpha=0.5) +
  geom_segment(aes(x=10,xend=10,y=0,yend=10), col="blue",linetype=2,alpha=0.5) +
  geom_point(col="red", fill="red",alpha=0.5, size=2)
ggsave("panel_d.jpg", plot=g,width=1.5,height=1.5)
