-- Omar AlSughayer (1337255)
-- CSE 341 AB
-- Assignment 03, Questions 1 - 6

import Test.HUnit
import Data.Char

-- Answer for Questoin 1
-- calculates a cone's volume given its height and radus
cone_volume :: Double->Double->Double
cone_volume h r = (r^2)*(h/3)*pi

-- Answer for Questoin 2
-- checks weather a list of Integers is in strict ascending order
ascending :: Integral i => [i]->Bool
ascending [] = True
ascending [f] = True
ascending (f:ls) = (f < (head ls)) && ascending ls

-- Answer for Question 3
-- squares the elements of list of Integers
squares :: Integral i => [i] -> [i]
squares ls = map (^2) ls

-- Answer for Question 4
-- calculates the total resistance for a number of resistors
parallel_resistors :: Fractional f => [f] -> f
parallel_resistors ls = recip (sum (map recip ls))
{-	Passing an empty list to parallel_resistors (i.e no resistors) returns
	Infinity since sum defines the summation of an empty list as 0, while recip
	defines the reciprocal of 0 as infinity -}
{-	Passing a list with zero as a resistor (e.g [1,2,0,4]) to parallel_resistors
	returns 0 since recip define the reciprocal of 0 as infinity, sum defnies summing
	anything over Infinity as Infinity, and recip defines the reciprocal of Infinity as zero -}

-- Answer for Question 5
-- a pointfree version of parallel_resistors
pointfree_parallel_resistors :: Fractional f => [f] -> f
pointfree_parallel_resistors = recip . sum . map recip 

-- Answer for Question 6
-- checks if a string is a palindrome or not, taking into accoutn only letters and digits
palindrome :: [Char] -> Bool
palindrome str = (remove_special str) == (remove_special (reverse str))

-- removes all special characters from a string and transforms it to lower case
remove_special :: [Char] -> [Char]
remove_special str = (map toLower (filter or_filters str))

-- checks if a character is a letter or a digit
or_filters :: Char -> Bool
or_filters c = (isLetter c) || (isDigit c)

-- Unit tests for all the functions

-- unit tests for cone_volume function
test_cone_volume1 = TestCase (assertBool "cone with volume pi" (is_close (cone_volume 3 1) pi))
test_cone_volume2 = TestCase (assertEqual "empty cone" (cone_volume 0 0) 0)
test_cone_volume3 = TestCase (assertBool "cone with random volume" (is_close (cone_volume 7 5) ((7/3)*(5^2)*pi)))

-- unit tests for ascending function
test_ascending1 = TestCase (assertBool "empty list" (ascending []))
test_ascending2 = TestCase (assertBool "one element list" (ascending [2]))
test_ascending3 = TestCase (assertBool "an ascending list" (ascending [1, 4, 9]))
test_ascending4 = TestCase (assertBool "a list with equals" (not (ascending [1, 1, 9])))
test_ascending5 = TestCase (assertBool "a not-so-ascending list" (not (ascending [10, 4, 9])))
test_ascending6 = TestCase (assertBool "an ascending infinit list" (ascending (take 50 [1..])))

-- unit tests for the squares function 
test_squares1 = TestCase (assertEqual "empty list" [] (squares []))
test_squares2 = TestCase (assertEqual "one element list" [25] (squares [5]))
test_squares3 = TestCase (assertEqual "normal list" [1,9,100] (squares [1,3,10]))
test_squares4 = TestCase (assertEqual "infinit list" [1,4,9,16,25] (take 5 (squares [1..])))

-- unit tests for the parallel_resistors function
test_parallel_resistors1 = TestCase (assertBool "[1,1,1] equal resistors" (is_close (1/3) (parallel_resistors [1,1,1])))
test_parallel_resistors2 = TestCase (assertBool "[5,2,-6] random resistors" (is_close (1/(1/5+1/2-1/6)) (parallel_resistors [5,2,-6])))
test_parallel_resistors3 = TestCase (assertEqual "[5,0,-6] zero resistor" 0.0 (parallel_resistors [5,0,-6]))
test_parallel_resistors4 = TestCase (assertEqual "[5] single resistor" 5 (parallel_resistors [5]))

-- unit tests for the pointfree_parallel_resistors function, basically the same as parallel_resistors tests
test_pointfree_parallel_resistors1 =
    TestCase (assertBool "[1,1,1] equal resistors" (is_close (1/3) (pointfree_parallel_resistors [1,1,1])))
test_pointfree_parallel_resistors2 =
    TestCase (assertBool "[5,2,-6] random resistors" (is_close (1/(1/5+1/2-1/6)) (pointfree_parallel_resistors [5,2,-6])))
test_pointfree_parallel_resistors3 =
    TestCase (assertEqual "[5,0,-6] zero resistor" 0.0 (pointfree_parallel_resistors [5,0,-6]))
test_pointfree_parallel_resistors4 =
    TestCase (assertEqual "[5] single resistor" 5 (pointfree_parallel_resistors [5]))

-- unit tests for the palindrome function 
test_palindrome1 = TestCase (assertBool "banana palindrome" (palindrome "Yo! Banana Boy!"))
test_palindrome2 = TestCase (assertBool "carrot palindrome" (not (palindrome "Yo! Carrot Girl!")))
test_palindrome3 = TestCase (assertBool "empty palindrome" (palindrome ""))
test_palindrome4 = TestCase (assertBool "date palindrome" (palindrome "01/02/2010"))

--unit tests for the palindrome helper functions
test_or_filters1 = TestCase (assertBool "B is a letter" (or_filters 'B'))
test_or_filters2 = TestCase (assertBool "1 is a number" (or_filters '1'))
test_or_filters3 = TestCase (assertBool "- is not a number nor a digit" (not (or_filters '-')))
test_or_filters4 = TestCase (assertBool "space is a not number nor a digit" (not (or_filters ' ')))
test_remove_special1 = TestCase (assertEqual "BaNa-Na -> banana" "3bananas" (remove_special "3 BaNa-Nas"))
test_remove_special2 = TestCase (assertEqual "empty string" "" (remove_special " "))

-- all the tests
tests = TestList [TestLabel "cone_volume test 1" test_cone_volume1,
                  TestLabel "cone_volume test 2" test_cone_volume2,
                  TestLabel "cone_volume test 3" test_cone_volume3,
                  TestLabel "ascending test 1" test_ascending1,
                  TestLabel "ascending test 2" test_ascending2,
                  TestLabel "ascending test 3" test_ascending3,
                  TestLabel "ascending test 4" test_ascending4,
                  TestLabel "ascending test 5" test_ascending5,
                  TestLabel "ascending test 6" test_ascending6,
                  TestLabel "squares test 1" test_squares1,
                  TestLabel "squares test 2" test_squares2,
                  TestLabel "squares test 3" test_squares3,
                  TestLabel "squares test 4" test_squares4,
                  TestLabel "parallel_resistors test 1" test_parallel_resistors1,
                  TestLabel "parallel_resistors test 2" test_parallel_resistors2,
                  TestLabel "parallel_resistors test 3" test_parallel_resistors3,
                  TestLabel "parallel_resistors test 4" test_parallel_resistors4,
                  TestLabel "pointfree_parallel_resistors test 1" test_pointfree_parallel_resistors1,
                  TestLabel "pointfree_parallel_resistorstest 2" test_pointfree_parallel_resistors2,
                  TestLabel "pointfree_parallel_resistorstest 3" test_pointfree_parallel_resistors3,
                  TestLabel "pointfree_parallel_resistorstest 4" test_pointfree_parallel_resistors4,
                  TestLabel "palindrome test 1" test_palindrome1,
                  TestLabel "palindrome test 2" test_palindrome2,
                  TestLabel "palindrome test 3" test_palindrome3,
                  TestLabel "palindrome test 4" test_palindrome4,
                  TestLabel "or_filter test 1" test_or_filters1,
                  TestLabel "or_filter test 2" test_or_filters2,
                  TestLabel "or_filter test 3" test_or_filters3,
                  TestLabel "or_filter test 4" test_or_filters4,
                  TestLabel "remove_Special test 1" test_remove_special1,
                  TestLabel "remove_Special test 2" test_remove_special2]

-- run all tests
run = runTestTT tests

{- test whether a number is within epsilon of to another (for unit tests on
Doubles, to accomodate floating point roundoff errors). Note that this doesn’t
work for testing numbers that should be exactly 0 -- for that you should specify
your own test with an appropriate epsilon -}
is_close x y = abs (x-y) < abs x * epsilon 
   where epsilon = 1.0e-6