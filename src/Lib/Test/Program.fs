open System
open Lists

module prog =

    [<EntryPoint>]
    let main args =
        // Считываем строку из консоли
        let input = Console.ReadLine()

        // Преобразуем строку в список чисел
        let arr1 =
            input.Split(' ') // Предполагаем, что числа разделены пробелами
            |> Array.toList // Преобразуем массив строк в список строк
            |> List.map float // Преобразуем строки в целые числа

        // Создаем экземпляр MyList
        let arr: MyList.MyList<float> = MyList.fromList arr1 // Предполагаем, что MyList принимает список

        // Сортируем массив
        let arr1 = arr.QuickSort compare
        let arr2 = MyList.toList arr1
        printf ("%A\n") arr2
        // Возвращаем 0 для завершения программы
        0
