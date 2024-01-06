rm(list=ls())

setwd("C:/Users/oyxen/Desktop/等值项目/11.27模拟数据/CTT测试数据/03 锚题组设计")

# 调用函数包
library("equate")
library(tmvtnorm)
#读入数据
library(openxlsx)

data = read.xlsx("03 锚题组设计.xlsx")
data = as.data.frame(data)

paper.A = list()
paper.B = list()
# 数据预处理
attach(data)
for(i in 1:20){
  # 筛选出专业1-20，试卷A的分数
  major=paste0("专业",i)
  sub_data=data[Major==major&Paper=="试卷A",]
  paper.A[[i]]= sub_data
}
detach(data)

attach(data)
for(i in 1:20){
  # 筛选出专业1-20，试卷B的分数
  major=paste0("专业",i)
  sub_data=data[Major==major&Paper=="试卷B",]
  paper.B[[i]]= sub_data
}
detach(data)

# 计算专业1-20等值结果
tucker =list()
levine =list()
braun =list()
equi =list()

# 此处应该是总分和锚题分！
for(i in 1:20){
  # 统计试卷A和试卷B的频数
  paper.a = freqtab(paper.A[[i]][, c("Other", "Anchor")], 
                    scales = list(0:90, 0:30))
  paper.b = freqtab(paper.B[[i]][, c("Other", "Anchor")], 
                    scales = list(0:90, 0:30)) # 存在疑问
  
  # 等百分位等值
  equi[[i]] = equate(paper.a, paper.b, type = "equipercentile")
  
  #TUCKER线性等值
  tucker[[i]] = equate(paper.a, paper.b, type = "linear", method = "tucker") 
  
  #LEVINE线性等值
  levine[[i]] = equate(paper.a, paper.b, type = "linear", method = "levine") 
  
  #BRAUN-HOLLAND
  braun[[i]] = equate(paper.a, paper.b, type = "linear", method = "braun") 
  
  # 表格整理
  
  ## 等值结果
  result = data.frame(c(equi[[i]]$concordance[1]),                        
                      c(equi[[i]]$concordance[2]),                        
                      c(tucker[[i]]$concordance[2]),                        
                      c(levine[[i]]$concordance[2]),                        
                      c(braun[[i]]$concordance[2]))                        
  colnames(result) = c("paper.a","UNSM","TLIN","LLIN","BLIN")
  file_name = paste0("专业",i,"试卷A-试卷B.csv")
  write.csv(result,file_name)
}



# 搞错了
# 爱德华等值结果(仅选取专业1试卷A-试卷B)
iowa.a = paper.A[[1]][, c("Other", "Anchor")]
iowa.b = paper.B[[1]][, c("Other", "Anchor")]
write.csv(iowa.a,"iowa.a.csv",row.names = F)
write.csv(iowa.b,"iowa.b.csv",row.names = F)


# 爱德华等值结果(仅选取专业1试卷A-试卷B)
iowa.a = paper.A[[1]][, c("Score", "Anchor")]
iowa.b = paper.B[[1]][, c("Score", "Anchor")]
write.csv(iowa.a,"total_iowa.a.csv",row.names = F)
write.csv(iowa.b,"total_iowa.b.csv",row.names = F)

