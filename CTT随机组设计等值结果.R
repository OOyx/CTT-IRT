rm(list=ls())

setwd("C:/Users/oyxen/Desktop/等值项目/11.27模拟数据/CTT测试数据/02 随机组设计")

# 调用函数包
library("equate")
library(tmvtnorm)
#读入数据
library(openxlsx)
library(dplyr)

data = read.xlsx("02 随机组设计.xlsx")
data = as.data.frame(data)

# 数据预处理
sub1_data = list()
sub2_data =list()

attach(data)
for(i in 1:5){
  paper=paste0("试卷",i)
  subject=paste0("科目",1)
  sub_data=data[Subject==subject&Paper==paper,]
  sub1_data[[i]]= sub_data
}
detach(data)

attach(data)
for(i in 1:5){
  paper=paste0("试卷",i)
  subject=paste0("科目",2)
  sub_data=data[Subject==subject&Paper==paper,]
  sub2_data[[i]] = sub_data
}
detach(data)

linear1 = list()
equi1 = data.frame(X.Score = 0:120)
equi1.1_1 = list()
equi1.2_2 = list()
equi1.4_3 = list()
equi1.6_3 = list()
equi1.8_3 = list()
equi1.12_3 = list()

cau_equi = function(x,max_x =120,max_y=120){
  
  # 计算连续型等百分位数值
  if(x<(-0.5)){
    P.x = 0
  } else if(x>=(max_x+0.5)){
    P.x = 1
  } else{
    x_ = round(x)
    if(x_ == 0){
      P.x = x_feq$Px[x_feq$Score==x_]
    } else{
      a = x_feq$Fx[x_feq$Score==x_]- x_feq$Fx[x_feq$Score== x_-1]
      b = x-(x_-0.5)
      c = x_feq$Fx[x_feq$Score== x_-1] + a*b
      P.x = c
    }
  }
  
  # 计算对应的y*
  y_ = max(y_feq$Score[y_feq$Fy < P.x])
  if(y_ == -Inf){
    eq_x = min(y_feq$Score)
  } else {
    a = P.x - y_feq$Fy[y_feq$Score== y_]
    b = y_feq$Fy[y_feq$Score== y_+1] - y_feq$Fy[y_feq$Score== y_]
    eq_x =  (a / b) + (y_+0.5)
  }
  return(eq_x)
}

# 计算科目1下试卷2-5与试卷1的等值结果

for(i in 2:5){
  # 统计2-5试卷和试卷1的频数
  test_frq = freqtab(sub1_data[[i]][4],scales = 0:120)
  test.1_frq = freqtab(sub1_data[[1]][4],,scales = 0:120)
  
  # 线性等值
  linear1[[i]]= equate(test_frq, test.1_frq, type = "linear")
  
  # 等百分位等值
  x_feq = as.data.frame(freqtab(test_frq,scales = 0:120))
  y_feq = as.data.frame(freqtab(test.1_frq,scales = 0:120))
  
  colnames(x_feq) = c("Score","frequency")
  colnames(y_feq) = c("Score","frequency")
  
  # 读取数据并计算频率，修正频率，累计频率，等百分位数值
  
  ##  引入adj修正频率
  
  # x频率分布
  x_feq = mutate(x_feq , fx = frequency / sum(x_feq$frequency))
  ##  adj fx
  adj = 0.000006
  adj_freq = 1 + (adj*121)
  x_feq = mutate(x_feq,adj.fx = (fx + adj)/adj_freq)
  x_feq = mutate(x_feq , Fx = cumsum(adj.fx))
  x_feq = mutate(x_feq, Px = lag(Fx ,n = 1) + (adj.fx / 2))
  x_feq[1,6] = x_feq[1,4]
  
  # y频率分布
  y_feq = mutate(y_feq , fy = frequency / sum(y_feq$frequency))
  ##  adj fy
  y_feq = mutate(y_feq,adj.fy = (fy + adj)/adj_freq)
  y_feq = mutate(y_feq , Fy = cumsum(adj.fy))
  y_feq = mutate(y_feq, Py = lag(Fy ,n = 1) + (adj.fy / 2))
  y_feq[1,6] = y_feq[1,4]
  
  equi1 = cbind(equi1, setNames(data.frame(a = sapply(0:120,cau_equi)), i))
  
  # 等百分位等值前平滑
  
  ## C= (1,1)
  equi1.1_1[[i]] = equate(loglinear(test_frq,degrees = 1),
                          loglinear(test.1_frq,degrees = 1),
                          ,type = "equipercentile")
  ## C= (2,2)
  equi1.2_2[[i]] = equate(loglinear(test_frq,degrees = 2),
                          loglinear(test.1_frq,degrees = 2),
                          ,type = "equipercentile")
  ## C= (4,3)
  equi1.4_3[[i]] = equate(loglinear(test_frq,degrees = 4),
                          loglinear(test.1_frq,degrees = 3),
                          ,type = "equipercentile")
  ## C= (6,3)
  equi1.6_3[[i]] = equate(loglinear(test_frq,degrees = 6),
                          loglinear(test.1_frq,degrees = 3),
                          ,type = "equipercentile")
  ## C= (8,3)
  equi1.8_3[[i]] = equate(loglinear(test_frq,degrees = 8),
                          loglinear(test.1_frq,degrees = 3),
                          ,type = "equipercentile")
  ## C= (12,3)
  equi1.12_3[[i]] = equate(loglinear(test_frq,degrees = 12),
                           loglinear(test.1_frq,degrees = 3),
                           ,type = "equipercentile")
  
  # 表格整理
  
  ## 等值结果
  result = data.frame(c(linear1[[i]]$concordance[1]),                        
                      c(linear1[[i]]$concordance[2]),                        
                      c(equi1[ ,i]),                        
                      c(equi1.1_1[[i]]$concordance[2]),                        
                      c(equi1.2_2[[i]]$concordance[2]),                        
                      c(equi1.4_3[[i]]$concordance[2]),                        
                      c(equi1.6_3[[i]]$concordance[2]),                        
                      c(equi1.8_3[[i]]$concordance[2]),                        
                      c(equi1.12_3[[i]]$concordance[2])) 
  colnames(result) = c("SCORE","Linear","NoSmooth",                         
                       "C(1,1)","C(2,2)","C(4,3)",                         
                       "C(6,3)","C(8,3)","C(12,3)")
  
  file_name = paste0("科目1试卷",i,"-试卷1.csv")
  write.csv(result,file_name)
}

linear2 = list()
equi2 = data.frame(X.Score = 0:120)
equi2.1_1 = list()
equi2.2_2 = list()
equi2.4_3 = list()
equi2.6_3 = list()
equi2.8_3 = list()
equi2.12_3 = list()

# 计算科目2下试卷2-5与试卷1的等值结果
for(i in 2:5){
  # 统计试卷和标准卷的频数
  test_frq = freqtab(sub2_data[[i]][4],scales = 0:120)
  test.1_frq = freqtab(sub2_data[[1]][4],,scales = 0:120)
  
  # 线性等值
  linear2[[i]]= equate(test_frq, test.1_frq, type = "linear")
  
  # 等百分位等值
  x_feq = as.data.frame(freqtab(test_frq,scales = 0:120))
  y_feq = as.data.frame(freqtab(test.1_frq,scales = 0:120))
  
  colnames(x_feq) = c("Score","frequency")
  colnames(y_feq) = c("Score","frequency")
  
  # 读取数据并计算频率，修正频率，累计频率，等百分位数值
  
  ##  引入adj修正频率
  
  # x频率分布
  x_feq = mutate(x_feq , fx = frequency / sum(x_feq$frequency))
  ##  adj fx
  adj = 0.000006
  adj_freq = 1 + (adj*121)
  x_feq = mutate(x_feq,adj.fx = (fx + adj)/adj_freq)
  x_feq = mutate(x_feq , Fx = cumsum(adj.fx))
  x_feq = mutate(x_feq, Px = lag(Fx ,n = 1) + (adj.fx / 2))
  x_feq[1,6] = x_feq[1,4]
  
  # y频率分布
  y_feq = mutate(y_feq , fy = frequency / sum(y_feq$frequency))
  ##  adj fy
  y_feq = mutate(y_feq,adj.fy = (fy + adj)/adj_freq)
  y_feq = mutate(y_feq , Fy = cumsum(adj.fy))
  y_feq = mutate(y_feq, Py = lag(Fy ,n = 1) + (adj.fy / 2))
  y_feq[1,6] = y_feq[1,4]
  
  equi2 = cbind(equi2, setNames(data.frame(a = sapply(0:120,cau_equi)), i))
  
  # 等百分位等值前平滑
  
  ## C= (1,1)
  equi2.1_1[[i]] = equate(loglinear(test_frq,degrees = 1),
                          loglinear(test.1_frq,degrees = 1),
                          ,type = "equipercentile")
  ## C= (2,2)
  equi2.2_2[[i]] = equate(loglinear(test_frq,degrees = 2),
                          loglinear(test.1_frq,degrees = 2),
                          ,type = "equipercentile")
  ## C= (4,3)
  equi2.4_3[[i]] = equate(loglinear(test_frq,degrees = 4),
                          loglinear(test.1_frq,degrees = 3),
                          ,type = "equipercentile")
  ## C= (6,3)
  equi2.6_3[[i]] = equate(loglinear(test_frq,degrees = 6),
                          loglinear(test.1_frq,degrees = 3),
                          ,type = "equipercentile")
  ## C= (8,3)
  equi2.8_3[[i]] = equate(loglinear(test_frq,degrees = 8),
                          loglinear(test.1_frq,degrees = 3),
                          ,type = "equipercentile")
  ## C= (12,3)
  equi2.12_3[[i]] = equate(loglinear(test_frq,degrees = 12),
                           loglinear(test.1_frq,degrees = 3),
                           ,type = "equipercentile")
  
  # 表格整理
  result = data.frame(c(linear2[[i]]$concordance[1]),                        
                      c(linear2[[i]]$concordance[2]),                        
                      c(equi2[ ,i]),                        
                      c(equi2.1_1[[i]]$concordance[2]),                        
                      c(equi2.2_2[[i]]$concordance[2]),                        
                      c(equi2.4_3[[i]]$concordance[2]),                        
                      c(equi2.6_3[[i]]$concordance[2]),                        
                      c(equi2.8_3[[i]]$concordance[2]),                        
                      c(equi2.12_3[[i]]$concordance[2])) 
  colnames(result) = c("SCORE","Linear","NoSmooth",                         
                       "C(1,1)","C(2,2)","C(4,3)",                         
                       "C(6,3)","C(8,3)","C(12,3)")
  
  file_name = paste0("科目2试卷",i,"-试卷1.csv")
  write.csv(result,file_name)
}

# 爱德华等值结果(仅选取科目1试卷2-试卷1)

test_frq = as.data.frame(freqtab(sub1_data[[2]]$Score,scales = 0:120))
test.1_frq = as.data.frame(freqtab(sub1_data[[1]]$Score,,scales = 0:120))
iowa_data = data.frame(c(test_frq),
                       c(test.1_frq$count),
                       c(equi1[,2]))
write.csv(iowa_data,"iowa_data.csv",row.names = F)

