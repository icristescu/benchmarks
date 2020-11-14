import matplotlib.pyplot as plt
import pandas as pd

df1 = pd.read_csv("./completed_times_single4", names=['a', 'b'])
df2 = pd.read_csv("./block_validated_single4", names=['a', 'c'])

df1 = df1.head(906)
df2 = df2.head(6)

df1['a'] = df1['a'].astype(int)
df2['a'] = df2['a'].astype(int)

df = df1.merge(df2, on='a', how='outer')

df.set_index('a',inplace=True)

ax = df['b'].plot(kind="line")

plt.show()
