## Задача
**Тема:** Конвертация данных из одного формата в другой\
**Язык выполнения:** Lua\
**Задание:**\
	Перевести массив простых объектов (без вложенных объектов) из формата `json` в табличный формат `csv`.

**Пример:**
- **Входной файл:**
```json
[
  {"name": "Alice", "age": 30, "city": "New York"},
  {"name": "Bob", "age": 25, "city": "Los Angeles"},
  {"name": "Charlie", "age": 35, "city": "Chicago"}
]
```
- **Выходной файл:**
```csv
name,age,city
Alice,30,New York
Bob,25,Los Angeles
Charlie,35,Chicago
```

## Сборка и запуск

### Linux | Windows
```sh
lua main.lua <input_json_file> <output_csv_file>
```