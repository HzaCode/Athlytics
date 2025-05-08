# 首先确保包是最新的
devtools::load_all()

# 然后运行我们修改过的脚本
# (确保您的 R 工作目录是项目的根目录)
source("run_coverage.R")
devtools::test()

setwd("C:/Users/Ang/Documents/GitHub/Athlytics")
getwd() # 确认输出是您的项目路径


library(covr)
cov <- covr::package_coverage(quiet = FALSE)

print(cov)
covr::report(cov) # 可选，生成HTML报告


devtools::test()


setwd("C:/Users/Ang/Documents/GitHub/Athlytics")
library(covr)
# 确保 tests/testthat.R 文件存在且内容是 test_check("Athlytics")
cov <- covr::package_coverage(quiet = FALSE)


print(cov)


