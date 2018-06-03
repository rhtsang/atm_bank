-module(bank).
-export([loop/0]).

loop() ->
    receive
        % create a new bank @BankName and init with accounts in @AccountsList
        {create, AccountsList, BankName} ->
            ets:new(BankName, [named_table]),
            lists:map(fun(Account) -> ets:insert(BankName, Account) end,
                      AccountsList),
            io:format("bank ~p created~n",[BankName]),
            loop();

        % open a new account @AccountName with @Amount dollars in bank @BankName
        {open, AccountName, Amount, BankName} ->
            ets:insert(BankName, {AccountName, Amount}),
            io:format("new account ~p created with ~p dollars~n", [AccountName, Amount]),
            loop();

        % look up the balance of account @AccountName in bank @BankName
        {balance, AccountName, BankName} ->
            Amount = ets:lookup(BankName, AccountName),
            io:format("account ~p has ~p dollars~n", [AccountName, Amount]),
            loop();

        % deposit @Amount dollars in account @AccountName in bank @BankName
        {deposit, BankName, AccountName, Amount} ->
            {_, CurrentAmount} = hd(ets:lookup(BankName, AccountName)),
            ets:insert(BankName, CurrentAmount+Amount),
            io:format("account ~p now has ~p dollars~n", [AccountName, CurrentAmount+Amount]),
            loop();

        % withdraw @Amount dollars from account @AccountName in bank @BankName
        {withdraw, BankName, AccountName, Amount} ->
            {_, CurrentAmount} = hd(ets:lookup(BankName, AccountName)),
            case Amount > CurrentAmount of
                false -> ets:insert(BankName, {AccountName, CurrentAmount-Amount}),
                         io:format("account ~p now has ~p dollars~n", [AccountName, CurrentAmount-Amount]);
                true -> io:format("sorry, account ~p has only ~p dollars~n", [AccountName, CurrentAmount])
            end,
            loop()
    end
.
