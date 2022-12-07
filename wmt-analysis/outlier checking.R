# look closer


dataGet <- function(df){
  df <- df %>%
    mutate(delta = as.numeric(df$givenResponse)  - as.numeric(df$probeAngle))
  for(i in 1:length(df$delta)){
    if (df$delta[i] < -180){
      df$deltaAngle[i] = df$delta[i] + 360
    }else if(df$delta[i] > 180){
      df$deltaAngle[i] = df$delta[i] - 360
    }else{
      df$deltaAngle[i]=df$delta[i]
    }
  }
  return(df)
}

# 5f411ec87e08c606616559cb-t2*ss6*ss2*
df5f411ec87e08c606616559cb <- read.csv('/Users/JSK/Desktop/orient_130157/orient_130157_000002.csv')

# 5f411ec87e08c606616559cb-t3*ss6*ss2*
df5f411ec87e08c606616559cb <- read.csv('/Users/JSK/Desktop/orient_130157/orient_130157_000003.csv')

# 5f411ec87e08c606616559cb-t4*ss6*ss2*
df5f411ec87e08c606616559cb <- read.csv('/Users/JSK/Desktop/orient_130157/orient_130157_000004.csv')

test <- dataGet(df5f411ec87e08c606616559cb)

ggplot(test,aes(x= deltaAngle))+geom_histogram(binwidth=30)
ggsave('/Users/JSK/Desktop/orient_130157/testAllt4',device = "png")

test2 <- test %>% filter(test$setSize==4)
ggplot(test2,aes(x= deltaAngle))+geom_histogram(binwidth=30)
ggsave('/Users/JSK/Desktop/orient_130157/testss4t2',device = "png")

test6 <- test %>% filter(test$setSize==6)
ggplot(test6,aes(x= deltaAngle))+geom_histogram(binwidth=30)
ggsave('/Users/JSK/Desktop/orient_130157/testss6t4',device = "png")

#5fb028d5e28a5c39c175eeda-t2
df5fb028d5e28a5c39c175eeda <- read.csv('/Users/JSK/Desktop/orient_130151/orient_130151_000002.csv')

test <- dataGet(df5fb028d5e28a5c39c175eeda)

ggplot(test,aes(x= deltaAngle))+geom_histogram(binwidth=30)
ggsave('/Users/JSK/Desktop/orient_130151/testAll',device = "png")

test2 <- test %>% filter(test$setSize==4)
ggplot(test2,aes(x= deltaAngle))+geom_histogram(binwidth=30)
ggsave('/Users/JSK/Desktop/orient_130151/testss4t2',device = "png")



# MLE ####
pacman::p_load(stats4)

set.seed(1001)
N <- 120
x <- rnorm(N,mean = 3,sd = 2)
mean(x)
sd(x)
LL <- function(mu,sigma){
  R=dnorm(x,mu,sigma)
  -sum(log(R))
}

mle(minuslogl = LL,start = list(mu=1,sigma=1), method = "L-BFGS-B", lower = c(-Inf, 0),upper = c(Inf, Inf))
