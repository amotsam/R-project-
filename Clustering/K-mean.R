
teens<-read.csv("SNSdata.csv")


interests<- teens[,5:40]# ניקח רק את המשתנים שאנחנו צריכים לקיימינס
interests_z<-scale(interests)# מתרגמת את הערכים לציוני תקן, כלומר המרחק של כל משתנה מהממוצע
interests<-as.data.frame((interests_z))# האלגוריתם קיימינס עובר על דאטהפריים לא על מטריצה, לכן נהפוך לדאטהפריים
set.seed(2345)# אם לא נרשום את זה, כל פעם שנריץ את האלגוריתם נקבל תוצאות שונות אך דומות
teen_clusters<-kmeans(interests_z,5)
# עכשיו נריץ 4 מתוך 9 קומפוננטות שאפשר להריץ מהאלגוריתם הזה
teen_clusters$size
teen_clusters$cluster# נקבל וקטור עם 30000 ערכים ולאיפה משוייך כל
teen_clusters$centers# האדם הממוצע של כל אשכול, בכל מילה יופיע הממוצע של כל הערכים ספציפית למילה הזאת בקלסטר הזףה 
teen_clusters$iter# 

#profiling- לתת אינטרפרטציה לכל קלאסטר
#צנטרואיד  זה הסנטר של הקלסטר

teens$cluster<- teen_clusters$cluster
class(teens$cluster)
teens$cluster<- factor(teens$cluster,levels=1:5, labels=c("clus1", "clus2", "clus3", "clus4", "clus5"))# מגדיר את המשתנה קלאסטר בתור פאקטור ולא אינטג'ר
friendsmean<- tapply(teens$friends,teens$cluster,mean)        
  #האם קיימים בין הבדלים בין מס' החברים של האנשים בקלסטרים השונים
barplot(friendsmean,main="friennds by cluster", xlab="", ylab="")