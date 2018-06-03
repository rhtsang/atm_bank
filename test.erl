-module(test).
-export([test/0]).

 test() ->
     % spawn bank and ATM processes
     B = spawn(fun bank:loop/0),
     A1 = spawn(fun atm:loop/0),
     A2 = spawn(fun atm:loop/0),

     % init bank with some accounts
     B ! {create, [{betty,200},{sally,300}], bank1},
     B ! {open,dave,100,bank1},
     B ! {balance, sally, bank1},
     timer:sleep(100),

     % init two ATMs
     A1 ! {start, B, bank1, 200, atm1},
     A2 ! {start, B, bank1, 500, atm2},

     % deposit money
     A1 ! {cashsupply,atm1},
     A1 ! {deposit,sally,50,atm1},
     timer:sleep(100),

     % withdraw money
     A1 ! {cashsupply,atm1},
     A1 ! {withdraw,sally,100,atm1},
     timer:sleep(100),

     A1 ! {cashsupply,atm1},
     A2 ! {balance,sally,atm2},
     A1 ! {balance,dave,atm1},
     A2 ! {withdraw,dave,50,atm2},
     A1 ! {cashsupply,atm1},
     timer:sleep(100),

     A1 ! {withdraw,dave,150,atm1},
     A1 ! {cashsupply,atm1},
     A2 ! {cashsupply,atm2},
     A2 ! {withdraw,dave,150,atm2},
     A2 ! {cashsupply,atm2},
     A2 ! {withdraw,dave,25,atm2},
     A2 ! {cashsupply,atm2},
     A1 ! {cashsupply,atm1}
.
