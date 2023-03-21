import Control.Concurrent
import Data.Char
import Control.Monad
import Data.Bits

bsd = map (`div` 2^16) . tail . iterate (\n -> (214013 * n + 2531011) `mod` 2^31)

main = printRng (bsd 25)

filterNewline '\n' = '0'
filterNewline '\r' = '0'
filterNewline c = c

removeOne (_:xs) = xs
removeOne [] = []

printRng:: [Int] -> IO ()
printRng rng = do
    putChar $ filterNewline $ chr ((.&.) (head rng) 255)
    threadDelay 100
    printRng $ removeOne rng
