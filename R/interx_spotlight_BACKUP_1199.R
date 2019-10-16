<<<<<<< HEAD
spotlight <- function(x, y, w, v, spot = 1, center = TRUE, model = 1, plot = FALSE, data) {
=======
spotlight <- function(x, y, w, v, spot = 1, center = FALSE, model = 1, plot = FALSE, data) {
>>>>>>> f05d7863a082b75b1ba78a0be96038317530183b
  if(model == 1) {
    if(missing(v)){
      if(center == FALSE) {
        attach(data)
        n <- length(w)
        sd <- rep(sd(w)*spot,n)
        w1 <- w-sd
        w2 <- w+sd
        a <- summary(lm(y ~ x*w, data = data))
        b <- summary(lm(y ~ x*w2, data = data))
        c <- summary(lm(y ~ x*w1, data = data))
        moderator_w <- c(mean(w)-sd(w), mean(w), mean(w)+sd(w))
        effect_x <- c(b$coefficients[2,1], a$coefficients[2,1], c$coefficients[2,1])
        std.error <- c(b$coefficients[2,2], a$coefficients[2,2], c$coefficients[2,2])
        tstat <- c(b$coefficients[2,3], a$coefficients[2,3], c$coefficients[2,3])
        pvalue <- c(b$coefficients[2,4], a$coefficients[2,4], c$coefficients[2,4])
        results <- data.frame(moderator_w,std.error,effect_x,tstat,pvalue)
        ploty <- c((a$coefficients[1,1] + a$coefficients[2,1]*max(x) + a$coefficients[3,1]*(mean(w)-sd(w)) + a$coefficients[4,1]*max(x)*(mean(w)-sd(w))),
                   (a$coefficients[1,1] + a$coefficients[2,1]*max(x) + a$coefficients[3,1]*mean(w) + a$coefficients[4,1]*max(x)*mean(w)),
                   (a$coefficients[1,1] + a$coefficients[2,1]*max(x) + a$coefficients[3,1]*(mean(w)+sd(w)) + a$coefficients[4,1]*max(x)*(mean(w)+sd(w))),
                   (a$coefficients[1,1] + a$coefficients[2,1]*min(x) + a$coefficients[3,1]*(mean(w)-sd(w)) + a$coefficients[4,1]*min(x)*(mean(w)-sd(w))),
                   (a$coefficients[1,1] + a$coefficients[2,1]*min(x) + a$coefficients[3,1]*mean(w) + a$coefficients[4,1]*min(x)*mean(w)),
                   (a$coefficients[1,1] + a$coefficients[2,1]*min(x) + a$coefficients[3,1]*(mean(w)+sd(w)) + a$coefficients[4,1]*min(x)*(mean(w)+sd(w))))
        cond <- c("Treatment","Treatment","Treatment","Control","Control","Control")
        mod <- c(mean(w)-sd(w), mean(w), mean(w)+sd(w),mean(w)-sd(w), mean(w), mean(w)+sd(w))
        plot.data <- data.frame(ploty, cond, mod)
        graph <- ggplot(data = plot.data, aes(x = mod, y = ploty, color = cond)) +
          geom_smooth(method = "lm", se = FALSE) +
          labs(y = "DV", x = "Moderator W", color = "Effect X") +
          theme_minimal()
      }else{
        attach(data)
        n <- length(w)
        sd <- rep(sd(w)*spot,n)
        w <- scale(w, scale = FALSE)
        w1 <- w-sd
        w2 <- w+sd
        a <- summary(lm(y ~ x*w, data = data))
        b <- summary(lm(y ~ x*w2, data = data))
        c <- summary(lm(y ~ x*w1, data = data))
        moderator_w <- c(mean(w)-sd(w), mean(w), mean(w)+sd(w))
        effect_x <- c(b$coefficients[2,1], a$coefficients[2,1], c$coefficients[2,1])
        std.error <- c(b$coefficients[2,2], a$coefficients[2,2], c$coefficients[2,2])
        tstat <- c(b$coefficients[2,3], a$coefficients[2,3], c$coefficients[2,3])
        pvalue <- c(b$coefficients[2,4], a$coefficients[2,4], c$coefficients[2,4])
        results <- data.frame(moderator_w,effect_x,std.error,tstat,pvalue)}
      ploty <- c((a$coefficients[1,1] + a$coefficients[2,1]*max(x) + a$coefficients[3,1]*(mean(w)-sd(w)) + a$coefficients[4,1]*max(x)*(mean(w)-sd(w))),
                 (a$coefficients[1,1] + a$coefficients[2,1]*max(x) + a$coefficients[3,1]*mean(w) + a$coefficients[4,1]*max(x)*mean(w)),
                 (a$coefficients[1,1] + a$coefficients[2,1]*max(x) + a$coefficients[3,1]*(mean(w)+sd(w)) + a$coefficients[4,1]*max(x)*(mean(w)+sd(w))),
                 (a$coefficients[1,1] + a$coefficients[2,1]*min(x) + a$coefficients[3,1]*(mean(w)-sd(w)) + a$coefficients[4,1]*min(x)*(mean(w)-sd(w))),
                 (a$coefficients[1,1] + a$coefficients[2,1]*min(x) + a$coefficients[3,1]*mean(w) + a$coefficients[4,1]*min(x)*mean(w)),
                 (a$coefficients[1,1] + a$coefficients[2,1]*min(x) + a$coefficients[3,1]*(mean(w)+sd(w)) + a$coefficients[4,1]*min(x)*(mean(w)+sd(w))))
      cond <- c("Treatment","Treatment","Treatment","Control","Control","Control")
      mod <- c(mean(w)-sd(w), mean(w), mean(w)+sd(w),mean(w)-sd(w), mean(w), mean(w)+sd(w))
      plot.data <- data.frame(ploty, cond, mod)
      graph <- ggplot(data = plot.data, aes(x = mod, y = ploty, color = cond)) +
        geom_smooth(method = "lm", se = FALSE) +
        labs(y = "DV", x = "Moderator W", color = "Effect X") +
        theme_minimal()
      if(plot==TRUE){return(list(a, results, plot.data, graph))}else{return(list(a, results, plot.data))}}
    else{
      if(center == FALSE) {
        attach(data)
        n <- length(w)
        sd <- rep(sd(w)*spot,n)
        w1 <- w-sd
        w2 <- w+sd
        a <- summary(lm(y ~ x*w + v, data = data))
        b <- summary(lm(y ~ x*w2 + v, data = data))
        c <- summary(lm(y ~ x*w1 + v, data = data))
        moderator <- c(mean(w)-sd(w), mean(w), mean(w)+sd(w))
        effect <- c(b$coefficients[2,1], a$coefficients[2,1], c$coefficients[2,1])
        std.error <- c(b$coefficients[2,2], a$coefficients[2,2], c$coefficients[2,2])
        tstat <- c(b$coefficients[2,3], a$coefficients[2,3], c$coefficients[2,3])
        pvalue <- c(b$coefficients[2,4], a$coefficients[2,4], c$coefficients[2,4])
        results <- data.frame(moderator,std.error,effect,tstat,pvalue)
        ploty <- c((a$coefficients[1,1] + a$coefficients[2,1]*max(x) + a$coefficients[3,1]*(mean(w)-sd(w)) + a$coefficients[5,1]*max(x)*(mean(w)-sd(w))),
                   (a$coefficients[1,1] + a$coefficients[2,1]*max(x) + a$coefficients[3,1]*mean(w) + a$coefficients[5,1]*max(x)*mean(w)),
                   (a$coefficients[1,1] + a$coefficients[2,1]*max(x) + a$coefficients[3,1]*(mean(w)+sd(w)) + a$coefficients[5,1]*max(x)*(mean(w)+sd(w))),
                   (a$coefficients[1,1] + a$coefficients[2,1]*min(x) + a$coefficients[3,1]*(mean(w)-sd(w)) + a$coefficients[5,1]*min(x)*(mean(w)-sd(w))),
                   (a$coefficients[1,1] + a$coefficients[2,1]*min(x) + a$coefficients[3,1]*mean(w) + a$coefficients[5,1]*min(x)*mean(w)),
                   (a$coefficients[1,1] + a$coefficients[2,1]*min(x) + a$coefficients[3,1]*(mean(w)+sd(w)) + a$coefficients[5,1]*min(x)*(mean(w)+sd(w))))
        cond <- c("Treatment","Treatment","Treatment","Control","Control","Control")
        mod <- c(mean(w)-sd(w), mean(w), mean(w)+sd(w),mean(w)-sd(w), mean(w), mean(w)+sd(w))
        plot.data <- data.frame(ploty, cond, mod)
        graph <- ggplot(data = plot.data, aes(x = mod, y = ploty, color = cond)) +
          geom_smooth(method = "lm", se = FALSE) +
          labs(y = "DV", x = "Moderator W", color = "Effect X") +
          theme_minimal()
      }else{
        attach(data)
        n <- length(w)
        sd <- rep(sd(w)*spot,n)
        w <- scale(w, scale = FALSE)
        w1 <- w-sd
        w2 <- w+sd
        a <- summary(lm(y ~ x*w + v, data = data))
        b <- summary(lm(y ~ x*w2 + v, data = data))
        c <- summary(lm(y ~ x*w1 + v, data = data))
        moderator <- c(mean(w)-sd(w), mean(w), mean(w)+sd(w))
        effect <- c(b$coefficients[2,1], a$coefficients[2,1], c$coefficients[2,1])
        std.error <- c(b$coefficients[2,2], a$coefficients[2,2], c$coefficients[2,2])
        tstat <- c(b$coefficients[2,3], a$coefficients[2,3], c$coefficients[2,3])
        pvalue <- c(b$coefficients[2,4], a$coefficients[2,4], c$coefficients[2,4])
        results <- data.frame(moderator,effect,std.error,tstat,pvalue)}
      ploty <- c((a$coefficients[1,1] + a$coefficients[2,1]*max(x) + a$coefficients[3,1]*(mean(w)-sd(w)) + a$coefficients[5,1]*max(x)*(mean(w)-sd(w))),
                 (a$coefficients[1,1] + a$coefficients[2,1]*max(x) + a$coefficients[3,1]*mean(w) + a$coefficients[5,1]*max(x)*mean(w)),
                 (a$coefficients[1,1] + a$coefficients[2,1]*max(x) + a$coefficients[3,1]*(mean(w)+sd(w)) + a$coefficients[5,1]*max(x)*(mean(w)+sd(w))),
                 (a$coefficients[1,1] + a$coefficients[2,1]*min(x) + a$coefficients[3,1]*(mean(w)-sd(w)) + a$coefficients[5,1]*min(x)*(mean(w)-sd(w))),
                 (a$coefficients[1,1] + a$coefficients[2,1]*min(x) + a$coefficients[3,1]*mean(w) + a$coefficients[5,1]*min(x)*mean(w)),
                 (a$coefficients[1,1] + a$coefficients[2,1]*min(x) + a$coefficients[3,1]*(mean(w)+sd(w)) + a$coefficients[5,1]*min(x)*(mean(w)+sd(w))))
      cond <- c("Treatment","Treatment","Treatment","Control","Control","Control")
      mod <- c(mean(w)-sd(w), mean(w), mean(w)+sd(w),mean(w)-sd(w), mean(w), mean(w)+sd(w))
      plot.data <- data.frame(ploty, cond, mod)
      graph <- ggplot(data = plot.data, aes(x = mod, y = ploty, color = cond)) +
        geom_smooth(method = "lm", se = FALSE) +
        labs(y = "DV", x = "Moderator W", color = "Effect X") +
        theme_minimal()
      if(plot==TRUE){return(list(a, results, plot.data, graph))}else{return(list(a, results, plot.data))}}}
}
