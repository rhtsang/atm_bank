-module(atm).
-export([loop/0]).

loop() ->
    % create database of ATMs only if the table does not yet exist
    % http://erlang.org/pipermail/erlang-questions/2008-March/033737.html
    % https://stackoverflow.com/questions/15913645/erlang-how-to-do-nothing-in-true-branch-of-if-statement
    case ets:info(atmTable) of
        undefined -> ets:new(atmTable,[named_table, public]);
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
            {_, _, BankPID, BankName} = hd(ets:lookup(atmTable, ATMName)),
            BankPID ! {deposit, BankName, AccountName, Amount},
            loop();

        % withdraw @Amount dollars from account @AccountName at ATM @ATMName
        {withdraw, AccountName, Amount, ATMName} ->
            {_, ATMAmount, BankPID, BankName} = hd(ets:lookup(atmTable, ATMName)),
            % if ATM @ATMName has enough cash, check if account @AccountName has enough money
            case Amount > ATMAmount of
                false -> BankPID ! {withdraw, BankName, AccountName, Amount, self(), ATMName};
                true -> io:format("sorry, insufficient cash in this atm~n")
            end,
            loop();

        % if ATM @ATMName and Account have enough money, take cash from ATM
        {withdrawn, Amount, ATMName} ->
            {_, CurrentAmount, BankPID, BankName} = hd(ets:lookup(atmTable, ATMName)),
            ets:insert(atmTable, {ATMName, CurrentAmount-Amount, BankPID, BankName}),
            loop();

        % check bank account @AccountName balance via ATM @ATMName
        {balance, AccountName, ATMName} ->
            {_, _, BankPID, BankName} = hd(ets:lookup(atmTable, ATMName)),
            BankPID ! {balance, AccountName, BankName},
            loop()
    end
.
