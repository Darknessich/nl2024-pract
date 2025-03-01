## Задача
**Тема:** Алгоритмы\
**Язык выполнения:** Rust\
**Задание:**\
На доске `2 x 3` есть пять плиток, помеченных от `1` до `5`, и пустой квадрат, представленный как `0`. **Ход** состоит из выбора одной из плиток, соседней с `0` (4 направления), и её дальнейшего перемещения на место `0`. Иными словами, необходимо решить головоломку по типу "Пятнашки" на доске `2 x 3`.
Головоломка решена тогда и только тогда, когда доска равна `[[1,2,3],[4,5,0]]`.
Нужно вернуть _наименьшее количество ходов, необходимое для того, чтобы решить головоломку_. Если это невозможно, возвращается `-1`. ([leetcode#773](https://leetcode.com/problems/sliding-puzzle/))
- **Входные данные:**
    - `board` - массив, описывающий текущее состояние доски.
- **Результат:** минимальное количество ходов, необходимое для решения головоломки, или `-1`, если это невозможно.

## Сборка и запуск
### Linux | Windows
```bash
cargo run
```

### Docker
```bash
sudo docker run --rm -it -w /app -v $(pwd):/app rust cargo run
```