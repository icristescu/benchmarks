import matplotlib.pyplot as plt
import pandas as pd

df2 = pd.read_csv("./bench_no_freeze_w_pause")
df3 = pd.read_csv("./bench_no_freeze_wo")

df1 = pd.read_csv("./bench_3")
df4 = pd.read_csv("./bench_wo_3")

#unsafe_add instead of add
df5 = pd.read_csv("./bench_4")
df6 = pd.read_csv("./bench_wo_4")

df7 = pd.read_csv("./bench_7")


df = pd.concat([df5, df6, df7])

df.plot(kind="line")

plt.show()
