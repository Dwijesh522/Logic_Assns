Whenever bddFromExpr function is called all hash tables get re-initialized.
Information gets lost about older robdd. 

----------------------------------------------------
So following kind of code for testing will not work.
----------------------------------------------------
x1 = bddFromExpr bexpr1 order1

x2 = bddFromExpr bexpr2 order2  // information about x1 is lost


all_sat x1                      // wrong answer bcz of information loss

all_sat x2                      // correct answer

-----------------------------
Following will work correctly
-----------------------------
x1 = bddFromExpr bexpr1 order1

all_sat x1


x2 = bddFromExpr bexpr2 order2

all_sat x2
