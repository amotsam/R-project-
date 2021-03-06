
teens<-read.csv("SNSdata.csv")


interests<- teens[,5:40]# ���� �� �� ������� ������ ������ ��������
interests_z<-scale(interests)# ������ �� ������ ������ ���, ����� ����� �� �� ����� �������
interests<-as.data.frame((interests_z))# ��������� ������� ���� �� ��������� �� �� ������, ��� ����� ����������
set.seed(2345)# �� �� ����� �� ��, �� ��� ����� �� ��������� ���� ������ ����� �� �����
teen_clusters<-kmeans(interests_z,5)
# ����� ���� 4 ���� 9 ���������� ����� ����� ���������� ���
teen_clusters$size
teen_clusters$cluster# ���� ����� �� 30000 ����� ������ ������ ��
teen_clusters$centers# ���� ������ �� �� �����, ��� ���� ����� ������ �� �� ������ ������� ����� ���� ������ ���� 
teen_clusters$iter# 

#profiling- ��� ����������� ��� ������
#��������  �� ����� �� ������

teens$cluster<- teen_clusters$cluster
class(teens$cluster)
teens$cluster<- factor(teens$cluster,levels=1:5, labels=c("clus1", "clus2", "clus3", "clus4", "clus5"))# ����� �� ������ ������ ���� ������ ��� �����'�
friendsmean<- tapply(teens$friends,teens$cluster,mean)        
  #��� ������ ��� ������ ��� ��' ������ �� ������ �������� ������
barplot(friendsmean,main="friennds by cluster", xlab="", ylab="")