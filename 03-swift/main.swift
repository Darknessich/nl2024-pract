import Foundation

func getLine(from inputStream: InputStream, buffer: inout [UInt8], partialLine: inout String) -> String? {
    let bufferSize = buffer.count

    // Проверяем наличие завершенных строк в partialLine
    var lines = partialLine.components(separatedBy: .newlines)
    if lines.count > 1 {
        partialLine = lines.dropFirst().joined(separator: "\n")
        return lines.first
    }

    // Считываем новые данные только если нет завершенных строк
    while inputStream.hasBytesAvailable {
        let bytesRead = inputStream.read(&buffer, maxLength: bufferSize)
        if bytesRead > 0 {
            if let chunk = String(bytes: buffer[0..<bytesRead], encoding: .utf8) {
                partialLine += chunk
                lines = partialLine.components(separatedBy: .newlines)
                if lines.count > 1 {
                    partialLine = lines.dropFirst().joined(separator: "\n")
                    return lines.first
                }
            }
        } else {
            break
        }
    }

    // Возвращаем остаток строки, если поток завершился
    if !partialLine.isEmpty {
        let line = partialLine
        partialLine = ""
        return line
    }

    return nil
}

func filterLines(matching regexPattern: String, from inputFile: String, to outputFile: String) {
    do {
        // Компилируем регулярное выражение
        let regex = try NSRegularExpression(pattern: regexPattern)

        // Открываем входной файл для построчного чтения
        guard let inputStream = InputStream(fileAtPath: inputFile) else {
            print("Error: Failed to open input file.")
            return
        }
        inputStream.open()
        defer { inputStream.close() }

        // Открываем выходной файл для записи
        guard let outputStream = OutputStream(toFileAtPath: outputFile, append: false) else {
            print("Error: Failed to open output file.")
            return
        }
        outputStream.open()
        defer { outputStream.close() }

        var buffer = [UInt8](repeating: 0, count: 1024)
        var partialLine = ""

        var i = 0
        while let line = getLine(from: inputStream, buffer: &buffer, partialLine: &partialLine) {
            let range = NSRange(location: 0, length: line.utf8.count)
            print("Line #\(i): \(line)");
            if regex.firstMatch(in: line, options: [], range: range) != nil {
                let lineData = (line + "\n").data(using: .utf8)!
                _ = lineData.withUnsafeBytes { outputStream.write($0.bindMemory(to: UInt8.self).baseAddress!, maxLength: lineData.count) }
            }
            i += 1
        }

        print("Filtration is complete. The results are written to \(outputFile)")
    } catch {
        print("Error: \(error.localizedDescription)")
    }
}

// Проверяем аргументы командной строки
let arguments = CommandLine.arguments
if arguments.count < 4 {
    print("Using: swift main.swift <regex> <input_file> <output_file>")
    exit(1)
}

let regexPattern = arguments[1]
let inputFile = arguments[2]
let outputFile = arguments[3]

// Запускаем фильтрацию
filterLines(matching: regexPattern, from: inputFile, to: outputFile)
