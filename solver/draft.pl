# FROM http://stackoverflow.com/q/11122814/2305521
:- use_module(library(clpfd)).

neighbor(X, Y) :-
    (X #= (Y - 1)) #\/ (X #= (Y + 1)).

solve([British, Swedish, Danish, Norwegian, German]) :-

    Nationalities = [British, Swedish, Danish, Norwegian, German],
    Colors = [Red, Green, Blue, White, Yellow],
    Beverages = [Tea, Coffee, Milk, Beer, Water],
    Cigarettes = [PallMall, Marlboro, Dunhill, Winfield, Rothmanns],
    Pets = [Dog, Bird, Cat, Horse, Fish],

    all_different(Nationalities),
    all_different(Colors),
    all_different(Beverages),
    all_different(Cigarettes),
    all_different(Pets),

    Nationalities ins 1..5,
    Colors ins 1..5,
    Beverages ins 1..5,
    Cigarettes ins 1..5,
    Pets ins 1..5,

    British #= Red, % Hint 1
    Swedish #= Dog, % Hint 2
    Danish #= Tea, % Hint 3

    Green #= White - 1 , % Hint 4
    Green #= Coffee, % Hint 5,
    PallMall #= Bird, % Hint 6

    Milk #= 3, % Hint 7
    Yellow #= Dunhill, % Hint 8,
    Norwegian #= 1, % Hint 9

    neighbor(Marlboro, Cat), % Hint 10
    neighbor(Horse, Dunhill), % Hint 11
    Winfield #= Beer, % Hint 12

    neighbor(Norwegian, Blue), % Hint 13
    German #= Rothmanns, % Hint 14,
    neighbor(Marlboro, Water). % Hint 15