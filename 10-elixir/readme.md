## Задача
**Тема:** Конвертация данных из одного формата в другой\
**Язык выполнения:** Elixir\
**Задание:**\
	Конвертировать данные в формате `json` в данные формата `yaml`.

**Пример:**
- **Входной файл:**
    ```json
    [
        {
            "name": "Acme Corp",
            "employees": 150,
            "established": 663408000,
            "age": 34
        },
        {
            "name": "Beta Ltd",
            "employees": 200,
            "established": 1126368000,
            "age": 19
        }
    ]
    ```
- **Выходной файл:**
    ```yaml
    - name: Acme Corp
      employees: 150
      established: 663408000
      age: 34
    - name: Beta Ltd
      employees: 200
      established: 1126368000
      age: 19
    ```

## Запуск

### Linux | Windows
```sh
elixir main.exs <input_json_file> <output_yaml_file>
```

### Docker
```sh
sudo docker run --rm -it -w /app -v $(pwd):/app elixir elixir main.exs <input_json_file> <output_yaml_file>
```