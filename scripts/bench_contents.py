import matplotlib.pyplot as plt
import pandas as pd

df = pd.read_csv("./contents", names=['a', 'b'])

print(df.shape)
print(df)
print(df.b.max())
print(df['b'].sum())

x = df['b'].sum()
#print(df.a.diff().sort_values())
#print(df.iloc[1452295:1452305])
#exit()
df.set_index('a',inplace=True)

ax = df['b'].plot(color="green", label='total timer for copying 1_743_928 contents=' + str(x) + 's', style=".")

plt.xlabel("execution time in us")
plt.ylabel("copy_contents in s")

plt.legend()

plt.show()
