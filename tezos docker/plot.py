import matplotlib.pyplot as plt
import pandas as pd

dbv = pd.read_csv("./block_validator")
plt.plot(dbv, label='block validator')

plt.xlabel("Blocks validated")
plt.ylabel("Seconds")
plt.legend()


plt.savefig('foo.png')
