import Network.Socket
import System.IO
import Control.Concurrent (forkFinally)
import Control.Monad (forever)

main :: IO ()
main = withSocketsDo $ do
    -- Создаем сокет
    serverSock <- socket AF_INET Stream defaultProtocol

    -- Привязываем сокет к адресу и порту
    bind serverSock (SockAddrInet 8080 (tupleToHostAddress (127, 0, 0, 1)))
    listen serverSock 5
    putStrLn "Server is listening on port 8080"

    -- Запускаем цикл ожидания клиентов
    forever $ do
        -- Принимаем подключение
        (connSock, clientAddr) <- accept serverSock
        putStrLn $ "Client connected: " ++ show clientAddr

        -- Обрабатываем клиента в отдельном потоке
        handle <- socketToHandle connSock ReadWriteMode
        hSetBuffering handle LineBuffering
        _ <- forkFinally (handleClient handle) (\_ -> hClose handle)
        return ()

-- Обработка клиента
handleClient :: Handle -> IO ()
handleClient handle = do
    putStrLn "Handling client..."
    clientLoop handle
    putStrLn "Client disconnected."

-- Цикл обработки сообщений от клиента
clientLoop :: Handle -> IO ()
clientLoop handle = do
    input <- hGetLine handle
    if input == "exit"
        then return () -- Завершаем цикл, если получена команда "exit"
        else do
            let response = reverse input  -- Переворачиваем строку
            hPutStrLn handle response    -- Отправляем обратно клиенту
            putStrLn $ "Processed: " ++ input ++ " -> " ++ response
            clientLoop handle