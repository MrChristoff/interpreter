#Simple Calculator Interpreter written in Haskell#

Takes a mathematical expression in form of a string, tokenizes, parses and evaluates to a result.

##Installation Instructions##
- Clone project into local repository
- Install Haskell
- ``cabal install``

##Run Program##
- Navigate to src folder
- Run ``ghci`` in terminal
- ``:load Main``
- Enter expression string eg `` interpret "123+4*13"``
- Program can handle integers, multiplication, division, addition, subtraction and parenthesis. Do not use spaces.


##Run Tests##
- Navigate to root folder
- Run ``stack test``
- Watch tests turn green

##Authors ##
- Rosie Gollancz
- Laura Weston
- Chris Lawrence
- Lilian Breidenbach

with a little help from [Bartosz Milewski] (https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell)
