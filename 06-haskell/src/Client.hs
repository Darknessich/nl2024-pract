import Network.Socket
import System.IO
import Control.Monad (unless)

main :: IO ()
main = withSocketsDo $ do
    -- Создаем сокет
    clientSock <- socket AF_INET Stream defaultProtocol

    -- Подключаемся к серверу
    connect clientSock (SockAddrInet 8080 (tupleToHostAddress (127, 0, 0, 1)))
    handle <- socketToHandle clientSock ReadWriteMode
    hSetBuffering handle LineBuffering
    putStrLn "Connected to the server. Type your messages (type 'exit' to quit)."

    -- Основной цикл ввода/вывода
    clientLoop handle

    -- Закрываем соединение
    hClose handle
    close clientSock
    putStrLn "Disconnected."

-- Основной цикл клиента
clientLoop :: Handle -> IO ()
clientLoop handle = do
    putStrLn "You: "
    input <- getLine
    hPutStrLn handle input
    unless (input == "exit") $ do
        response <- hGetLine handle
        putStrLn $ "Server: " ++ response
        clientLoop handle