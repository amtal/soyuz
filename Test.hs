import DCPU16.Assembly.Printer
import DCPU16.Assembly.Parser
import DCPU16.Assembly.Optimizer

test = do
    (Just x) <- parseFile defaults "lower.masm"
    putStrLn $ pprint x
    putStrLn $ pprint (sizeVariant x)
