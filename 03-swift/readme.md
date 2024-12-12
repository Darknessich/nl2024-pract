## Задача
**Тема:** Ввод-вывод\
**Язык выполнения:** Swift\
**Задание:**\
Написать программу, которая считывает текст из файла ищет в нём строки, соответствующие заданному регулярному выражению. Найденные строки сохраняются в файл `output.txt`.

## Сборка и запуск
### Linux | Windows
```bash
swift main.swift <regex> <input_file> <output_file>
```

### Docker
```bash
sudo docker run --rm -it -w /app -v $(pwd):/app swift swift main.swift <regex> <input_file> <output_file>
```