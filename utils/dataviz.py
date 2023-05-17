import seaborn as sns
import matplotlib.pyplot as plt
plt.style.use('ggplot')

def hist_box_plot(dataset, var_name):
    '''histogram and boxplot for some variable in dataset'''
    plt.figure(figsize = (8,5))
    plt.subplot(121)
    sns.histplot(x = var_name, data = dataset, bins = 100)
    plt.subplot(122)
    sns.boxplot(y = var_name, data = dataset)
    plt.show()