%Rules for Iris-versicolor classification
versicolor(SepalLength, SepalWidth, PetalLength, PetalWidth):-
    (
        PetalLength > 2.45,
        PetalLength =< 4.75,
        PetalWidth =< 1.65
    );
    (
        PetalLength > 2.45,
        PetalLength > 4.75,
        PetalWidth =< 1.75,
        PetalLength =< 4.95
    );
    (
        PetalLength > 2.45,
        PetalLength > 4.75,
        PetalWidth =< 1.75,
        PetalLength > 4.95,
        PetalWidth > 1.55,
        PetalLength =< 5.45
    );
    (
        PetalLength > 2.45,
        PetalLength > 4.75,
        PetalWidth > 1.75,
        PetalLength =< 4.85,
        SepalLength =< 5.95
    ).

%Rules for Iris-setosa classification
setosa(SepalLength, SepalWidth, PetalLength, PetalWidth) :-
    (
        PetalLength =< 2.45  
    ).


%Rules for Iris-virginica classification
virginica(SepalLength, SepalWidth, PetalLength, PetalWidth):-
    (
        PetalLength > 2.45,
        PetalLength =< 4.75,
        PetalWidth > 1.65
    );
    (
        PetalLength > 2.45,
        PetalLength > 4.75,
        PetalWidth =< 1.75,
        PetalLength > 4.95,
        PetalWidth =< 1.55
    );
    (
        PetalLength > 2.45,
        PetalLength > 4.75,
        PetalWidth =< 1.75,
        PetalLength > 4.95,
        PetalWidth > 1.55,
        PetalLength > 5.45
    );
    (
        PetalLength > 2.45,
        PetalLength > 4.75,
        PetalWidth > 1.75,
        PetalLength =< 4.85,
        SepalLength > 5.95
    );
    (
        PetalLength > 2.45,
        PetalLength > 4.75,
        PetalWidth > 1.75,
        PetalLength > 4.85
    ).




classify(SepalLength, SepalWidth, PetalLength, PetalWidth) :-
    (
        versicolor(SepalLength, SepalWidth, PetalLength, PetalWidth) ->  write('Iris-versicolor');
        setosa(SepalLength, SepalWidth, PetalLength, PetalWidth) ->  write('Iris-setosa');
        virginica(SepalLength, SepalWidth, PetalLength, PetalWidth)->  write('Iris-virginica');
        write('unknown class')
    ).