import matplotlib.pyplot as plt
import numpy as np

def FileRead (name):
    MergeRes = [ ]
    QuickRes = [ ]
    BubbleRes = [ ]
    SysRes = [ ]
    with open(name, 'r', encoding='utf-8') as file:
        for i in range(5596):
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
    
def FileListRead (name):
    MergeRes = [ ]
    QuickRes = [ ]
    BubbleRes = [ ]
    SysRes = [ ]
    with open(name, 'r', encoding='utf-8') as file:
        for i in range(10937):
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
    
ListofArray = FileRead('src/BenchMark/BenchmarkDotNet.Artifacts/Benchmarks.ArrayBenchmark-20250305-135526.log')
ListofList = FileListRead('src/BenchMark/BenchmarkDotNet.Artifacts/BenchmarkRun-20250304-211313.log')

size = [10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000]
listsize = [10000, 20000, 30000, 40000, 50000, 60000, 70000]
smallsizes = [10000, 20000, 30000, 40000]

AMergeGraph = [float(value.replace(',', '')) / 1000 for value in ListofArray[0]]
AQuickGraph = [float(value.replace(',', '')) / 1000 for value in ListofArray[1]]
ABubbleGraph = [float(value.replace(',', '')) / 1000 for value in ListofArray[2]]
ASysGraph = [float(value.replace(',', '')) / 1000 for value in ListofArray[3]]

LMergeGraph = [float(value.replace(',', '')) / 1000 for value in ListofList[0]] [:7]
LQuickGraph = [float(value.replace(',', '')) / 1000 for value in ListofList[1]] [:7]
LBubbleGraph = [float(value.replace(',', '')) / 1000 for value in ListofList[2]] [:4]
LSysGraph = [float(value.replace(',', '')) / 1000 for value in ListofList[3]] [:7]

DifMergeLvsA = [LMergeGraph[i] / AQuickGraph[i] for i in range(7)]
DifQuickLvsA = [value / value for value in AQuickGraph]

DifQuickAvsS = [AQuickGraph[i] / ASysGraph[i] for i in range(10)]
DifSysA = [value / value for value in ASysGraph]

DifMergeLvsS = [LMergeGraph[i] / LSysGraph[i] for i in range(7)]
DifSysL = [value / value for value in LSysGraph]
  
plt.figure(figsize=(10, 6)) 

#plt.plot(listsize, LMergeGraph, color='r', label = 'Mergesort на списках')
#plt.plot(listsize, AQuickGraph[:7], color='g', label = 'Quicksort на массивах')

#plt.plot(listsize, LMergeGraph, color='r', label = 'Mergesort на списках')
#plt.plot(listsize, LSysGraph, color='g', label = 'System sort на списках')

#plt.plot(size, DifQuickAvsS, color='r', label = 'Quicksort на массивах')
#plt.plot(size, DifSysA, color='g', label = 'System sort на массивах')

#plt.plot(listsize, DifMergeLvsS, color='r', label = 'Mergesort на списках')
#plt.plot(listsize, DifSysL, color='g', label = 'System sort на списках')

plt.plot(listsize, DifMergeLvsA, color='r', label = 'Mergesort на списках')
plt.plot(listsize, DifQuickLvsA[:7], color='g', label = 'Quicksort на массивах')

#plt.plot(size, AMergeGraph, color='r', label = 'Mergesort на массивах ')
#plt.plot(size, AQuickGraph, color='g', label = 'Quicksort на массивах')
#plt.plot(size, ABubbleGraph, color='b', label = 'Bubblesort на массивах')
#plt.plot(size, ASysGraph, color='y', label = 'System sort на массивах')

#plt.plot(listsize, LMergeGraph, color='r', label = 'Mergesort на списках')
#plt.plot(listsize, LQuickGraph, color='g', label = 'Quicksort на списках')
#plt.plot(smallsizes, LBubbleGraph, color='b',label = 'Bubblesort на списках')
#plt.plot(listsize, LSysGraph, color='y', label = 'System sort на списках')

#plt.title('Производительность Bubblesot-а на списках', fontsize = 15)
#plt.title('Сравнение между лучшей сортировкой на массиве и системной', fontsize = 15)
#plt.title('Сравнение между лучшей сортировкой на списке и системной', fontsize = 15)
plt.title('Сравнение между лучшими сортировками на массивах и на списках', fontsize = 15)
#plt.title('Производительность лучших сортировок на массивах и на списках', fontsize = 15)
#plt.title('Производительность сортировок на массивах', fontsize = 15)
#plt.title('Производительность сортировок на списках', fontsize = 15)
#plt.yscale('log')
plt.xlabel('Кол-во элементов', fontsize = 15)
plt.ylabel('Время выполнения, мс', fontsize = 15)
plt.ylabel('Во сколько раз', fontsize = 15)
plt.legend(fontsize = 15)

plt.show()