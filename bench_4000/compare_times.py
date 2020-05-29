import matplotlib.pyplot as plt
import pandas as pd


df0 = pd.read_csv("./completed_times_master")
df1 = pd.read_csv("./completed_times_io_8")


df0 = df0.head(16000)
df1 = df1.head(16000)

df = pd.concat([df0, df1])

df.plot(kind="line")

plt.show()
