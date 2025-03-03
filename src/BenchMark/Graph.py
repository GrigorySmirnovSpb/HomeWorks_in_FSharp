import matplotlib.pyplot as plt
import numpy as np
import csv

x = [ ]
y = [ ]
def FileRead (name, numLine):
    MergeRes = [ ]
    QuickRes = [ ]
    BubbleRes = [ ]
    SysRes = [ ]
    with open(name, 'r', encoding='utf-8') as file:
        for i in range(numLine - 1):
            next(file)
        for i in range (40):
            line = next(file)
            parts = line.strip().split('|')
            timeStr = parts[5].strip().replace('μs', '')

            if i % 4 == 0:
                MergeRes.append(timeStr)
            elif i % 4 == 1:
                QuickRes.append(timeStr)
            elif i % 4 == 2:
                BubbleRes.append(timeStr)
            else: 
                SysRes.append(timeStr)
        file.close
        return MergeRes, QuickRes, BubbleRes, SysRes

ListArray = FileRead('/home/gregory/demo_2024/FsharpProj/src/BenchMark/BenchmarkDotNet.Artifacts/Benchmarks.SortsBenchmark-20250302-211140.log', 5702)
plt.plot(x, y)
plt.show()