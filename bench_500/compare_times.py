import matplotlib.pyplot as plt
import pandas as pd


df0 = pd.read_csv("./completed_times_master")
#thread yields and pauses
df1 = pd.read_csv("./completed_times_24")
#auto yields
df2 = pd.read_csv("./completed_times_io_3")
df3 = pd.read_csv("./completed_times_io_4")
df4 = pd.read_csv("./completed_times_io_6")
df5 = pd.read_csv("./completed_times_io_7")
df6 = pd.read_csv("./completed_times_logs2")


df0 = df0.head(10000)
df1 = df1.head(10000)
df2 = df2.head(10000)
df3 = df3.head(10000)
df4 = df4.head(10000)
df5 = df5.head(10000)
df6 = df6.head(10000)

df = pd.concat([df4, df5, df6])

df.plot(kind="line")

plt.show()
