import re
import matplotlib.pyplot as plt
import matplotlib.ticker as plticker
import numpy as np
import argparse
import os

def remove_freeze (object_nums ):
    new_object_num = []
    for num in object_nums:
        if num != 0:
            new_object_num = new_object_num + [num]
    return new_object_num 


def main(log_file):
    object_str = 'created by commit'
    object_num = []
    with open(log_file, 'r') as reader:
        file_contents = reader.readlines()
        for line in file_contents:
            if object_str in line:
                l = line.split(" ")
                object_num = object_num + [int(l[-2])]

        object_num= remove_freeze(object_num)

    with open('object_time_output','w') as f:
        f.write('num_of_objects\ttime_to_create'+os.linesep)
        for  num in object_num:
            content = str(num)
            f.write(content + os.linesep)

    object_num = np.array(object_num)

    bins = [i for i in range(1, len(object_num)+1)]

    plt.xlim(min(bins), max(bins))
    plt.ylim(min(object_num), max(object_num))
    plt.grid(axis='y', alpha=0.75)
    plt.xlabel('Number of commits', fontsize=15)
    plt.ylabel('Number of objects',fontsize=15)
    plt.xticks(fontsize=15)
    plt.yticks(fontsize=15)
    plt.title('Objects created by each commit',fontsize=15)
    
    plt.xticks(np.arange(min(bins), max(bins)+1, 100))

    plt.step(x=bins, y=object_num)

    plt.show()


if __name__ == "__main__":
    parser = argparse.ArgumentParser("Plot histogram for the number of objects created by a commit")
    parser.add_argument('-l', help='path for the log file')
    args = parser.parse_args()
    main(args.l)