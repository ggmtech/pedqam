
import seaborn as sns
import pandas as pd
anscombe = sns.load_dataset('anscombe')
anscombe.head()
# Pretty simple dataset...
anscombe.shape
anscombe.groupby('dataset').describe()

sns.scatterplot(data=anscombe, x='x', y='y', hue='dataset')
