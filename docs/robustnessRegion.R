simdat <- rnorm(50, 0.25, 1)

getRobustnessRegion <- function(x,scale) {
  BF = c();
  for (s in scale) {
    BF = c(BF, ttestBF(x,rscale=s)%>%as.vector())
  }
  df <- tibble(rscale=scale,BF=BF);
  p <- ggplot(a,aes(x=rscale,y=BF))+geom_line()
  return(p)
}

df <- tibble(
  rscale = double(),
  es = double(),
  logBF = double()
)

for (s in seq(0.01,4,0.01)) {
  for (es in seq(0.01,0.5,0.005)) {
    df <- df %>% add_row(
      rscale=s,
      es=es,
      logBF =  (ttest.tstat(t=es*sqrt(106), n1=106, rscale = s)[['bf']])
    )
  }
}

df <- df %>% 
  mutate(conclusion = ifelse(
    exp(logBF)>3,
    'BF>3',
    ifelse(exp(logBF)<1/3,
           'BF<1/3',
           'inconclusive')
  ));

ggplot(df, aes(rscale, es, fill= conclusion)) +
  geom_tile() +
  geom_hline(yintercept=1.66/sqrt(106)) +
  scale_x_continuous(breaks=seq(0,4,0.4)) +
  scale_y_continuous(breaks=seq(0,0.5,0.05),name ='effect size') +
  theme_classic() +
  scale_fill_manual(values=c('#E32227','#1A7CFA','#686361')) +
  geom_vline(xintercept=0.65,linetype=2)

ggsave('figures/RR.png',height=4,width=6)