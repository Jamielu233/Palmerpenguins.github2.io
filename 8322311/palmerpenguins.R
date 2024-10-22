ggplot(data=penguins)+geom_smooth(mapping=aes(x=flipper_length_mm,y=body_mass_g))

ggplot(data=penguins)+geom_smooth(mapping=aes(x=flipper_length_mm,y=body_mass_g))+
  geom_point(mapping=aes(x=flipper_length_mm,y=body_mass_g))

ggplot(data=penguins)+geom_smooth(mapping=aes(x=flipper_length_mm,y=body_mass_g,linetype=species))
                      
ggplot(data=penguins)+geom_bar(mapping=aes(x=species))
ggplot(data=penguins)+geom_bar(mapping=aes(x=species,color=species))
ggplot(data=penguins)+geom_bar(mapping=aes(x=species,fill=species))
ggplot(data=penguins)+geom_bar(mapping=aes(x=species,fill=sex))+facet_wrap(~species)

ggplot(data=penguins)+geom_point(mapping=aes(x=flipper_length_mm,y=body_mass_g,shape=species,color=species,size=species))+
  facet_wrap(~species)

ggplot(data=penguins)+geom_point(mapping=aes(x=flipper_length_mm,y=body_mass_g,shape=species,color=species,size=species))+
  facet_grid(sex~species)


head(penguins)


install.packages("rmarkdown")

ggplot(penguins, aes(x = species, y = bill_length_mm)) + geom_violin()





ggplot(data=penguins)+geom_violin(mapping=aes(x=flipper_length_mm,y=))


ggplot(data=penguins, aes(x = species, y = bill_length_mm, fill = species)) +
  geom_violin(trim = FALSE) +
  theme_minimal() +  # 使用简洁主题
  labs(x = "Species", y = "Bill Length (mm)", 
       title = "Bill Length Distribution by Species",
       fill = "Species") +  # 添加图例标题
  scale_fill_brewer(palette = "Set1")  # 使用颜色方案


penguins %>%
  group_by(species) %>%
  summarise(median_bill_length = median(bill_length_mm, na.rm = TRUE))





# 使用ggplot2绘制箱型图，并添加美化选项
ggplot(penguins, aes(x = species, y = bill_length_mm)) +
  geom_boxplot(outlier.shape = 19, fill = "skyblue") +  # 设置异常点形状和填充颜色
  scale_color_brewer(palette = "Set1") +  # 使用颜色方案
  theme_minimal() +  # 使用简洁主题
  labs(x = "Species", y = "Bill Length (mm)",
       title = "Boxplot of Bill Length by Species") +
  ggtitle("Bill Length Variation Among Penguin Species")  # 添加图形标题





# 不同物种的体重分布
ggplot(penguins, aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot() +
  labs(x = "Species", y = "Body Mass (g)", title = "Body Mass by Species")


ggplot(data=penguins, aes(x = species, y = bill_length_mm，fill = species)) +
  geom_boxplot() +  # 设置异常点形状和填充颜色
  scale_color_brewer(palette = "Set1") +  # 使用颜色方案
  theme_minimal() +  # 使用简洁主题
  labs(x = "Species", y = "Bill Length (mm)",
       title = "Boxplot of Bill Length by Species") +
  ggtitle("Bill Length Variation Among Penguin Species")  # 添加图形标题



# 使用ANOVA检验不同物种的体重是否有显著差异
anova_result <- aov(body_mass_g ~ species, data = penguins_clean)
summary(anova_result)

View()



# 线性模型预测体重与喙长度的关系
lm_model <- lm(body_mass_g ~ bill_length_mm, data = penguins)
summary(lm_model)


# 线性模型预测体重与喙长度的关系
lm_model <- lm(body_mass_g ~ bill_length_mm, data = penguins)
summary(lm_model)




# 使用ggplot2绘制线性模型的图形
ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g)) +
  geom_point() +  # 添加散点图
  geom_smooth(method = "lm", color = "blue") +  # 添加线性拟合线
  labs(x = "Bill Length (mm)",
       y = "Body Mass (g)",
       title = "Relationship between Bill Length and Body Mass") +
  theme_minimal()  # 使用简洁主题


# 安装并加载broom包
install.packages("broom")
library(broom)

# 获取模型的预测值和残差
penguins_augmented <- augment(lm_model)

# 绘制散点图和拟合线
ggplot(penguins_augmented, aes(x = bill_length_mm, y = body_mass_g)) +
  geom_point() +
  geom_abline(intercept = lm_model$coefficients[1], 
              slope = lm_model$coefficients[2], 
              color = "red") +
  labs(x = "Bill Length (mm)",
       y = "Body Mass (g)",
       title = "Linear Regression Model of Body Mass on Bill Length")


# 线性模型预测体重与喙长度的关系
lm_model <- lm(body_mass_g ~ bill_length_mm, data = penguins)
summary(lm_model)















