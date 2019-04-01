--  File     : haskelltest.hs
--  Author   : Peter Schachte
--  Origin   : Mon Aug 23 16:00:03 2010
--  Purpose  : Run an assessment test suite
--
-- |This code has a similar purpose to hunit, namely to run a suite of test
--  cases.  However, the intended application is for student project
--  assessment.  As such, it has a few extra features:  it produces output
--  for every test.  It prints messages as it goes along, so if the program
--  crashes or hangs during a test, you can see which test it was.  It also
--  supports quality tests, which compute a numeric quality assessment for
--  each test.  Finally, it supports timeouts, so individual tests or lists
--  of tests can be given a limited time to complete, and it catches
--  exceptions and reports them as failures (a test intended to fail should
--  catch the exception itself and report it as a success).

module HaskellTest (TestCase(..), TestResult(..), expect, quality, 
                    test, testStdout, testVerbose) where

import Prelude hiding (catch)
import Text.Printf
import System.Timeout
import Data.Time
-- import Data.Time.Clock
import System.CPUTime
import System.IO
import Control.Monad
import Control.Exception

-- | The possible results from running a single test.
data TestResult
    -- | Straightforward test success, including a (often empty) string to
    -- show for brief output and another for verbose output.
    = Succeed String String
    -- | Straightforward test failure, including a string explaining how or
    -- why the test failed, and another string for verbose output.
    | Fail String String
    -- | Test threw an exception.
    | Exception SomeException
    -- | Test quality result, as a number between 0.0 and 1.0, including a
    -- string explaining why the test received the score it did, and another
    -- string for verbose output.
    | Quality Double String String
    -- | The test did not complete in the allowed time.
    | Timeout


-- | The TestCase represents an individual test case or a collection of
-- tests.
data TestCase
    -- | The 'Label' test case attaches a string as a label to an individual
    -- test or a collection.  The label will be printed when the test is
    -- started to show what is being tested; a counter is also printed to
    -- distinguish among multiple individual tests with the same label.
    = Label String TestCase
    -- | The value of the specified test case(s) is multiplied by the 
    -- specified factor 
    | Scaled Double TestCase
    -- | The value of the specified test case is the quotient of the 
    -- number of passed tests by the total number of tests.
    | Ratio TestCase
    -- | The specified number of seconds is given as a time limit for the
    -- execution of each individual test in the constituent test case.
    | TimeLimit Double TestCase
    -- | The total score for the included test(s), after any scaling, 
    -- is written to the specified file
    | ResultFile FilePath TestCase
    -- | An individual correctness test.  The specified test is
    -- evaluated, and a message is printed indicating whether it passed or
    -- failed, or the quality of the result.
    | Test TestResult
    -- | A collection of test cases.
    | Suite [TestCase]
    -- | A collection of test cases with its own summary information.  The
    -- first String will introduce the summary, and the percentage correct
    -- will be scaled by the double.  Finally, if the second string is
    -- not empty, the scaled value will be written to a file with the
    -- specified name.
    | Summarised String (Double -> Double -> String) TestCase


-- | Construct a correctness test case that compares the value of an
--   expression with an expected value.  Returns 'Succeed' if the actual
--   value '==' the expected value, and 'Fail' otherwise.
expect :: (Eq a, Show a) => a -> a -> TestCase
expr `expect` expected
    = Test (if expr == expected then Succeed "" ""
            else Fail "" ("Expected " ++ show expected ++ 
                          " but got " ++ show expr))

-- | Construct a quality test case that tests the value of the first
-- argument, an expression, with the second argument, a function.  If the
-- function returns 'False', then the test fails.  Otherwise the third
-- argument, another function, is applied, and its value is the quality of
-- the test result (between 0.0 and 1.0).
quality :: Show a => a -> (a -> Bool) -> (a -> Double) -> TestCase
quality expr test assessment
    = Test (if not $ test expr then Fail "invalid output" ""
            else Quality (assessment expr) "" "")


-- | Run the provided test or suite, logging the output to the named file.
test :: String -> String -> TestCase -> IO ()
test username fileName test = do
  fhandle <- openFile fileName WriteMode
  hPutStrLn fhandle ("Begin test of submission for " ++ username)
  testToHandle fhandle False test
  hClose fhandle


-- | Run the provided test or suite, logging the output to standard out.
testStdout :: TestCase -> IO ()
testStdout = testToHandle stdout False


-- | Run the provided test or suite, producing a verbose output log to
-- standard out.
testVerbose :: TestCase -> IO ()
testVerbose = testToHandle stdout True


-- | Run the provided test or suite, logging the output to the provided file
-- | handle.
testToHandle :: Handle -> Bool -> TestCase -> IO ()
testToHandle fhandle verbose test = do
  startDate <- getZonedTime
  hPutStrLn fhandle $ "Haskell test run started " ++ show startDate
  startTime <- getCPUTime
  (_,score,count) <- runTest fhandle "Test" 1 Nothing verbose test
  endTime <- getCPUTime
  endDate <- getZonedTime
  hPutStrLn fhandle $ "Haskell test run ended " ++ show endDate
  hPutStrLn fhandle ("Total CPU time used = " ++ 
                    (show $ round (realToFrac(endTime-startTime)/1000000000)) ++
                    " milliseconds"
                   )

-- ("Total score:  " ++ show score ++ 
--                     " / " ++ (show $ realToFrac count) ++
--                     " = " ++ 
--                     show (100*((realToFrac score)/(realToFrac count))) ++
--                     "%"
--                    )



-- | Run an individual test or suite, sending the output to the provided file
-- handle.  The given string and integer are the label in whose scope this
-- test is included and the ordinal position of this test in the suite of
-- tests for this label.  Returns the test number of the next test case for
-- this label, the total of the qualities and count of successful test cases,
-- and the total count of individual tests run.
runTest :: Handle -> String -> Int -> Maybe Double -> Bool -> TestCase 
           -> IO (Int,Double,Double)
-- runTest fhandle label count limit verbose testcase
runTest fhandle _ _ limit verbose (Label label tcase) = 
    runTest fhandle label 1 limit verbose tcase
runTest fhandle l n _ verbose (TimeLimit secs tcase) =
    runTest fhandle l n (Just secs) verbose tcase
runTest fhandle l n limit verbose (Scaled sc tcase) = do
    (n1,t,c) <- runTest fhandle l n limit verbose tcase
    return (n1,sc*t,sc*c)
runTest fhandle l n limit verbose (Ratio tcase) = do
    (n1,t,c) <- runTest fhandle l n limit verbose tcase
    return (n1,t/c,1)
runTest fhandle l n limit verbose (ResultFile file tcase) = do
    (n1,t,c) <- runTest fhandle l n limit verbose tcase
    writeFile file $ (show t) ++ "\n"
    return (n1,t,c)
runTest fhandle l n limit verbose (Test code) =
    actualTest fhandle l n limit verbose code 
runTest fhandle l n limit verbose (Summarised intro outrofn tcase) =
    do when (intro /= "") $ hPutStrLn fhandle intro
       (n1,t,c) <- runTest fhandle l n limit verbose tcase
       let outro = outrofn t c
       when (outro /= "") $ hPutStrLn fhandle outro
       return (n1,t,c)
runTest fhandle l n _ _ (Suite []) = return (n,0,0)
runTest fhandle l n limit verbose (Suite (t:ts)) =
    do (n1,t1,c1) <- runTest fhandle l n limit verbose t
       (n2,t2,c2) <- runTest fhandle l n1 limit verbose (Suite ts)
       return (n2,t1+t2,c1+c2)

timeoutTime :: Maybe Double -> Int
timeoutTime Nothing = (-1)      -- Negative timeout number means no timeout
timeoutTime (Just secs) = 
    if secs >= (fromIntegral (maxBound::Int)) / 1000000
    then error ("TimeLimit too large: " ++ (show secs))
    else (round (1000000.0*secs)) -- convert to microsecs

actualTest :: Handle -> String -> Int ->  Maybe Double -> Bool 
           -> TestResult -> IO (Int,Double,Double)
actualTest fhandle l n limit verbose code =
    do printLabel fhandle l n
       result0 <- timeout (timeoutTime limit) 
                  (limitedTest fhandle n verbose code)
       case result0 of
         Nothing ->             -- timed out:  haven't printed message yet
             do printTestResult fhandle verbose Timeout
                return (n+1,testResult Timeout,1)
         Just r -> return r     -- already printed message

limitedTest fhandle n verbose code =
    -- force evaluation; catch any exceptions
    catch (handleTestResult fhandle n verbose code)
              (\e -> handleTestResult fhandle n verbose
                     (Exception (e::SomeException)))

printLabel :: Handle -> String -> Int -> IO ()
printLabel fhandle label num =
    do hPrintf fhandle "%30s" label
       hPrintf fhandle " %3d" num
       hPutStr fhandle " ... "
       hFlush fhandle

handleTestResult :: Handle -> Int -> Bool -> TestResult 
                    -> IO (Int,Double,Double)
handleTestResult fhandle n verbose code =
    do printTestResult fhandle verbose code     -- forces execution of code
       return (n+1,testResult code,1)   -- doesn't re-evaluate code

printTestResult :: Handle -> Bool -> TestResult -> IO ()
printTestResult fhandle verbose result 
    = hPutStrLn fhandle (resultMessage verbose result )

resultMessage :: Bool -> TestResult -> String
resultMessage verbose (Succeed norm verb) 
    = "PASSED       "  ++ (parenthetical $ pick verbose norm norm++verb)
resultMessage verbose (Fail norm verb) 
    = "FAILED***    "  ++ (parenthetical $ pick verbose norm norm++verb)
resultMessage verbose (Exception e) 
    = "EXCEPT***    "  ++ (parenthetical (show e))
resultMessage verbose (Quality score norm verb) 
    = "PASSED " ++ (printf "%5.1f" (100*score)) ++ "% " ++ 
      (parenthetical $ pick verbose norm norm++verb)
resultMessage _       Timeout
    = "TIMEOUT**"

testResult (Succeed _ _)     = 1.0
testResult (Fail _ _)        = 0.0
testResult (Exception _)     = 0.0
testResult (Quality val _ _) = val
testResult Timeout           = 0.0

pick True  _ x = x
pick False x _ = x

parenthetical "" = ""
parenthetical text = "(" ++ text ++ ")"

printQualityResult fhandle = (hPutStrLn fhandle) . show

