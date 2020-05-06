import matplotlib.pyplot as plt
import pandas as pd

df1 = pd.read_csv("./completed_times_patch")
df1 = pd.read_csv("./completed_times_master")

#df1 = df1.head(906)
#df2 = df2.head(6)

df = pd.concat([df1, df2])

df.plot(kind="line")

plt.show()
