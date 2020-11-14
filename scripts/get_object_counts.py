import re
import matplotlib.pyplot as plt
import numpy as np
import argparse
import os

def remove_freeze (object_nums, block_times):
    new_object_num = []
    new_block_times = []
    for (num, time) in zip(object_nums, block_times):
        if num != 0:
            new_object_num = new_object_num + [num]
            new_block_times = new_block_times + [time]
    return new_object_num, new_block_times


def main(log_file, block_validation_times_file):
    object_str = 'created by commit'
    object_num = []
    with open(log_file, 'r') as reader:
        file_contents = reader.readlines()
        for line in file_contents:
            if object_str in line:
                l = line.split(" ")
                object_num = object_num + [int(l[-2])]

    
    block_times = []
    with open(block_validation_times_file, 'r') as reader:
        file_contents = reader.read().splitlines()
        block_times = [float(time) for time in file_contents]

    object_num, block_times = remove_freeze(object_num, block_times)

    with open('object_time_output','w') as f:
        f.write('num_of_objects\ttime_to_create'+os.linesep)
        for  (num, time) in zip(object_num, block_times):
            content = str(num)+'\t'+str(time)
            f.write(content + os.linesep)

    cumulative_block_times = np.cumsum(block_times)
    bins = np.array(cumulative_block_times)

    object_num = np.array(object_num)


    plt.xlim(min(bins), max(bins))
    plt.ylim(min(object_num), max(object_num))
    plt.grid(axis='y', alpha=0.75)
    plt.xlabel('Time taken',fontsize=15)
    plt.ylabel('Number of objects',fontsize=15)
    plt.xticks(fontsize=15)
    plt.yticks(fontsize=15)
    plt.title('Objects created by each commit',fontsize=15)
    
    plt.step(x=bins, y=object_num)

    plt.show()


if __name__ == "__main__":
    parser = argparse.ArgumentParser("Plot histogram for the number of objects created by a commit")
    parser.add_argument('-l', help='path for the log file')
    parser.add_argument('-b', help='path for the time taken by block file')
    args = parser.parse_args()
    main(args.l, args.b)