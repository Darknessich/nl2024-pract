-- Парсинг JSON
local function parse_json(str)
    local pos = 1

    -- Предварительное объявление всех функций
    local parse_value, parse_object, parse_array, parse_string, parse_number

    local function skip_whitespace()
        while str:sub(pos, pos):match("%s") do
            pos = pos + 1
        end
    end

    function parse_string()
        pos = pos + 1 -- Пропускаем начальную кавычку
        local start_pos = pos
        while str:sub(pos, pos) ~= '"' do
            if str:sub(pos, pos) == "\\" then
                pos = pos + 1 -- Пропускаем экранированный символ
            end
            pos = pos + 1
        end
        local s = str:sub(start_pos, pos - 1)
        pos = pos + 1 -- Пропускаем закрывающую кавычку
        return s:gsub("\\(.)", {
            ['"'] = '"',
            ["\\"] = "\\",
            ["/"] = "/",
            ["b"] = "\b",
            ["f"] = "\f",
            ["n"] = "\n",
            ["r"] = "\r",
            ["t"] = "\t"
        })
    end

    function parse_number()
        local start_pos = pos
        while str:sub(pos, pos):match("[%d%.%-eE+]") do
            pos = pos + 1
        end
        return tonumber(str:sub(start_pos, pos - 1))
    end

    function parse_value()
        skip_whitespace()
        local char = str:sub(pos, pos)

        if char == "{" then
            return parse_object()
        elseif char == "[" then
            return parse_array()
        elseif char == '"' then
            return parse_string()
        elseif char:match("[%d%-]") then
            return parse_number()
        elseif str:sub(pos, pos + 3) == "true" then
            pos = pos + 4
            return true
        elseif str:sub(pos, pos + 4) == "false" then
            pos = pos + 5
            return false
        elseif str:sub(pos, pos + 3) == "null" then
            pos = pos + 4
            return nil
        else
            error("Invalid JSON value at position " .. pos)
        end
    end

    function parse_object()
        pos = pos + 1 -- Пропускаем '{'
        local obj = {}
        skip_whitespace()
        if str:sub(pos, pos) == "}" then
            pos = pos + 1 -- Пропускаем '}'
            return obj
        end
        while true do
            skip_whitespace()
            local key = parse_string()
            skip_whitespace()
            if str:sub(pos, pos) ~= ":" then
                error("Expected ':' at position " .. pos)
            end
            pos = pos + 1 -- Пропускаем ':'
            skip_whitespace()
            local value = parse_value()
            obj[key] = value
            skip_whitespace()
            if str:sub(pos, pos) == "}" then
                pos = pos + 1 -- Пропускаем '}'
                break
            elseif str:sub(pos, pos) ~= "," then
                error("Expected ',' or '}' at position " .. pos)
            end
            pos = pos + 1 -- Пропускаем ','
        end
        return obj
    end

    function parse_array()
        pos = pos + 1 -- Пропускаем '['
        local arr = {}
        skip_whitespace()
        if str:sub(pos, pos) == "]" then
            pos = pos + 1 -- Пропускаем ']'
            return arr
        end
        while true do
            skip_whitespace()
            arr[#arr + 1] = parse_value()
            skip_whitespace()
            if str:sub(pos, pos) == "]" then
                pos = pos + 1 -- Пропускаем ']'
                break
            elseif str:sub(pos, pos) ~= "," then
                error("Expected ',' or ']' at position " .. pos)
            end
            pos = pos + 1 -- Пропускаем ','
        end
        return arr
    end

    return parse_value()
end

-- Конвертация JSON в CSV
local function json_to_csv(json_str)
    local data = parse_json(json_str)

    if type(data) ~= "table" or #data == 0 then
        error("Expected JSON array of objects")
    end

    -- Собираем все уникальные ключи из объектов
    local all_keys = {}
    for _, obj in ipairs(data) do
        if type(obj) ~= "table" then
            error("Expected each element of the array to be an object")
        end
        for key, value in pairs(obj) do
            if type(value) == "table" then
                error("Values must be primitive types (string, number, boolean, or null)")
            end
            if not all_keys[key] then
                all_keys[key] = true
            end
        end
    end

    -- Преобразуем таблицу ключей в упорядоченный список
    local headers = {}
    for key in pairs(all_keys) do
        headers[#headers + 1] = key
    end
    table.sort(headers) -- Для единообразного порядка ключей

    -- Создаём CSV строки
    local csv_lines = {}
    csv_lines[1] = table.concat(headers, ";") -- Заголовок

    for _, obj in ipairs(data) do
        local row = {}
        for _, header in ipairs(headers) do
            local value = obj[header]
            row[#row + 1] = value ~= nil and tostring(value) or "-"
        end
        csv_lines[#csv_lines + 1] = table.concat(row, ";")
    end

    return table.concat(csv_lines, "\n")
end

-- Главная программа
local function main()
    -- Проверяем наличие аргументов
    if #arg < 2 then
        print("Usage: lua script.lua <input_json_file> <output_csv_file>")
        return
    end

    local input_file = arg[1]
    local output_file = arg[2]

    -- Чтение JSON из файла
    local json_data
    local input_handle = io.open(input_file, "r")
    if input_handle then
        json_data = input_handle:read("*a")
        input_handle:close()
    else
        print("Error: Unable to open input file: " .. input_file)
        return
    end

    -- Конвертация JSON в CSV
    local csv_data
    local success, err = pcall(function()
        csv_data = json_to_csv(json_data)
    end)
    if not success then
        print("Error: " .. err)
        return
    end

    -- Запись CSV в файл
    local output_handle = io.open(output_file, "w")
    if output_handle then
        output_handle:write(csv_data)
        output_handle:close()
        print("CSV file successfully written to: " .. output_file)
    else
        print("Error: Unable to write to output file: " .. output_file)
    end
end

main()
