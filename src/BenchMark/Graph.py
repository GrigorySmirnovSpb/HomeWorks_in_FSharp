import matplotlib.pyplot as plt
import numpy as np
import csv

def FileRead (name, numLine):
    MergeRes = [ ]
    QuickRes = [ ]
    BubbleRes = [ ]
    SysRes = [ ]
    with open(name, 'r', encoding='utf-8') as file:
        for i in range(numLine):
            next(file)
        for i in range (40):
            line = next(file)
            parts = line.strip().split('|')
            timeStr = parts[6].strip().replace('μs', '')

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
    
def FileListRead (name, numLine):
    MergeRes = [ ]
    QuickRes = [ ]
    BubbleRes = [ ]
    SysRes = [ ]
    with open(name, 'r', encoding='utf-8') as file:
        for i in range(numLine):
            next(file)
        for i in range (28):
            line = next(file)
            parts = line.strip().split('|')
            timeStr = parts[6].strip().replace('μs', '')

            if i % 4 == 0:
                MergeRes.append(timeStr)
            elif i % 4 == 1:
                QuickRes.append(timeStr)
            elif i % 4 == 2:
                if timeStr == 'NA':
                    timeStr = '0'
                BubbleRes.append(timeStr)
            else:
                SysRes.append(timeStr)
        file.close
        return MergeRes, QuickRes, BubbleRes, SysRes
    
ListofArray = FileRead('/home/gregory/demo_2024/FsharpProj/src/BenchMark/BenchmarkDotNet.Artifacts/Benchmarks.ArrayBenchmark-20250305-135526.log', 5596)
ListofList = FileListRead('/home/gregory/demo_2024/FsharpProj/src/BenchMark/BenchmarkDotNet.Artifacts/BenchmarkRun-20250304-211313.log', 10937)

size = [10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000]
listsize = [10000, 20000, 30000, 40000, 50000, 60000, 70000]
smallsizes = [10000, 20000]

AMergeGraph = [float(value.replace(',', '')) / 1000 for value in ListofArray[0]]
AQuickGraph = [float(value.replace(',', '')) / 1000 for value in ListofArray[1]]
ABubbleGraph = [float(value.replace(',', '')) / 1000 for value in ListofArray[2]] [:2]
ASysGraph = [float(value.replace(',', '')) / 1000 for value in ListofArray[3]]

LMergeGraph = [float(value.replace(',', '')) / 1000 for value in ListofList[0]] [:7]
LQuickGraph = [float(value.replace(',', '')) / 1000 for value in ListofList[1]] [:7]
LBubbleGraph = [float(value.replace(',', '')) / 1000 for value in ListofList[2]] [:2]
LSysGraph = [float(value.replace(',', '')) / 1000 for value in ListofList[3]] [:7]

DifMergeLvsA = [LMergeGraph[i] / AQuickGraph[i] for i in range(7)]
DifQuickLvsA = [value / value for value in AQuickGraph]

DifQuickAvsS = [AQuickGraph[i] / ASysGraph[i] for i in range(10)]
DifSysA = [value / value for value in ASysGraph]

DifMergeLvsS = [LMergeGraph[i] / LSysGraph[i] for i in range(7)]
DifSysL = [value / value for value in LSysGraph]

plt.figure(figsize=(9, 6)) 

#plt.plot(listsize, LMergeGraph, color='r', label = 'Mergesort на списках')
#plt.plot(listsize, AQuickGraph[:7], color='g', label = 'Quicksort на массивах')

#plt.plot(listsize, LMergeGraph, color='r', label = 'Mergesort на списках')
#plt.plot(listsize, LSysGraph, color='g', label = 'System sort на списках')

plt.plot(size, DifQuickAvsS, color='r', label = 'Quicksort на массивах')
plt.plot(size, DifSysA, color='g', label = 'System sort на массивах')

#plt.plot(listsize, DifMergeLvsS, color='r', label = 'Mergesort на списках')
#plt.plot(listsize, DifSysL, color='g', label = 'System sort на списках')

#plt.plot(listsize, DifMergeLvsA, color='r', label = 'Mergesort на списках')
#plt.plot(listsize, DifQuickLvsA[:7], color='g', label = 'Quicksort на массивах')

#plt.plot(size, AMergeGraph, color='r', label = 'Mergesort на массивах ')
#plt.plot(size, AQuickGraph, color='g', label = 'Quicksort на массивах')
#plt.plot(smallsizes, ABubbleGraph, color='b')
#plt.plot(size, ASysGraph, color='y', label = 'System sort на массивах')

#plt.plot(listsize, LMergeGraph, color='r', label = 'Mergesort на списках')
#plt.plot(listsize, LQuickGraph, color='g', label = 'Quicksort на списках')
#plt.plot(smallsizes, LBubbleGraph, color='b')
#plt.plot(listsize, LSysGraph, color='y', label = 'System sort на списках')

plt.title('Сравнение между лучшей сортировкой на массиве и системной')
#plt.title('Сравнение между лучшей сортировкой на списке и системной')
#plt.title('Сравнение между лучшими сортировками на массивах и на списках')
#plt.title('Производительность лучших сортировок на массивах и на списках')
#plt.title('Производительность сортировок на массивах')
#plt.title('Производительность сортировок на списках')
plt.xlabel('Кол-во элементов')
plt.ylabel('Время выполнения, мс')
plt.ylabel('Во сколько раз')
plt.legend()

plt.show()
