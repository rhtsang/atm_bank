-module(atm).
-export([loop/0]).

loop() ->
    % create database of ATMs only if the table does not yet exist
    % http://erlang.org/pipermail/erlang-questions/2008-March/033737.html
    % https://stackoverflow.com/questions/15913645/erlang-how-to-do-nothing-in-true-branch-of-if-statement
    case ets:info(atmTable) of
        undefined -> ets:new(atmTable,[named_table]);
        _ -> ok
    end,

    receive
        % start a new ATM @ATMName with @Amount dollars available and associate with bank @BankName and process @BankPID
        {start, BankPID, BankName, Amount, ATMName} ->
            ets:insert(atmTable, {ATMName, Amount, BankPID, BankName}),
            io:format("atm ~p started with ~p dollars cash available~n", [ATMName, Amount]),
            loop();

        % see how much cash ATM @ATMName has available
        {cashsupply, ATMName} ->
            {_,Amount,_,_} = hd(ets:lookup(atmTable, ATMName)),
            io:format("atm ~p has ~p dollars on hand~n", [ATMName,Amount]),
            loop();

        % deposit @Amount dollars in account @AccountName at ATM @ATMName
        {deposit, AccountName, Amount, ATMName} ->
            {BankPID, BankName, _, _} = hd(ets:lookup(atmTable, ATMName)),
            BankPID ! {deposit, BankName, AccountName, Amount},
            loop();

        % withdraw @Amount dollars from account @AccountName at ATM @ATMName
        {withdraw, AccountName, Amount, ATMName} ->
            {_, ATMAmount, BankPID, BankName} = hd(ets:lookup(atmTable, ATMName)),
            case Amount > ATMAmount of
                false -> BankPID ! {withdraw, BankName, AccountName, Amount};
                true -> io:format("sorry, insufficient cash in this atm~n")
            end,
            loop()
    end
.
