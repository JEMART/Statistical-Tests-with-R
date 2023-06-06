############# ANOVA ###########

#1. Import the data

  setwd("C:\\Parametric tests\\ANOVA")
  data2<-read.csv("ANOVA_data.csv",dec=",",sep=";")


#2. Do the ANOVA
  m<-lm(Sales~Area,data=data2)
  anova(m)
  summary(m)
  #3 Visualize
  ##SST
  par(mfcol=c(1,3))
  plot(data2$Sales ~rep(c(1,2,3,4),each=12),xaxt="n",main="SST",xlab="Area",ylab="Sales (million $)",las=1)
  axis(side=1,at=c(1,2,3,4),labels=c(1,2,3,4))
  abline(h=mean(data2$Sales),col="blue",lty="dashed")

  segments(rep(c(1,2,3,4),each=12),data2$Sales[seq(1,max(length(Sales)),1)],
  rep(c(1,2,3,4),each=12),mean(data2$Sales),col="red")

  Area1<-round(tapply(data2$Sales,data2$Area,mean),digits=2)[1]
  Area2<-round(tapply(data2$Sales,data2$Area,mean),digits=2)[2]
  Area3<-round(tapply(data2$Sales,data2$Area,mean),digits=2)[3]
  Area4<-round(tapply(data2$Sales,data2$Area,mean),digits=2)[4]

#SSB
  plot(data2$Sales ~rep(c(1,2,3,4),each=12),xaxt="n",main="SSB",xlab="Area",ylab="Sales (million $)",las=1)
  axis(side=1,at=c(1,2,3,4),labels=c(1,2,3,4))
  abline(h=mean(data2$Sales),col="blue",lty="dashed")

  segments(0.9,Area1,1.1,Area1,lwd=2)
  segments(1.9,Area2,2.1,Area2,lwd=2)
  segments(2.9,Area3,3.1,Area3,lwd=2)
  segments(3.9,Area4,4.1,Area4,lwd=2)

  segments(c(1,2,3,4),round(tapply(data2$Sales,data2$Area,mean),digits=2),
  c(1,2,3,4),mean(data2$Sales),col="red")

#SSW
  plot(data2$Sales ~rep(c(1,2,3,4),each=12),xaxt="n",main="SSW",xlab="Area",ylab="Sales (million $)",las=1)
  axis(side=1,at=c(1,2,3,4),labels=c(1,2,3,4))
  abline(h=mean(Sales),col="blue",lty="dashed")
  segments(0.9,Area1,1.1,Area1,lwd=2)
  segments(1.9,Area2,2.1,Area2,lwd=2)
  segments(2.9,Area3,3.1,Area3,lwd=2)
  segments(3.9,Area4,4.1,Area4,lwd=2)

  segments(rep(c(1,2,3,4),each=12),data2$Sales[seq(1,max(length(data2$Sales)),1)],
  rep(c(1,2,3,4),each=12),rep(round(tapply(data2$Sales,data2$Area,mean),digits=2),each=12),col="red")



#3. Check the assumptions

  #3.1 Normality
    #QQ plot
      st.res<-rstandard(m)
      qqnorm(st.res,ylab="Standardized Residuals",xlab="Theoretical",las=1,bty="l") 
      qqline(st.res)

    # Histogram
      x11()
      par(mfcol=c(2,2))
      tapply(data1$Sales,data1$Area,hist,col="skyblue",las=1,yaxt="n",xaxt="n",xlab="Sales",main="Histogram")

  #4.1 Equal variances
    #Compute the variances of each Area
      d<-data.frame(data1[which(data1$Area=="Area 1"),],data1[which(data1$Area=="Area 2"),],
      data1[which(data1$Area=="Area 3"),], data1[which(data1$Area=="Area 4"),])

      std<-tapply(data1$Sales,data1$Area,sd)
      var<-std^2

#F-test
  var.test(d$Sales,d$Sales.1)

