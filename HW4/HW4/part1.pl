% Algorithm to traverse all the nodes up to down and find possible paths
depth_first_search(Last_location, Last_location, Path, CostOfTime, Path, CostOfTime). % desired location

depth_first_search(First_location, Last_location, Visited, CurrentTime, Path, CostOfTime) :-
    path(First_location, Next_location, WeightOfTime), % find neighbour node
    \+ member(Next_location, Visited),  %not visited
    TotalTime is CurrentTime + WeightOfTime, % calculates Total time
    depth_first_search(Next_location, Last_location, [Next_location|Visited], TotalTime, Path, CostOfTime). % continues recursively

% Rule to find the shortest path between two places
min_path(First_location, Last_location, Path, Time) :-
    findall(
        [Path, CostOfTime],
        (
            depth_first_search(First_location, Last_location, [First_location], 0, DfsPath, CostOfTime),
            reverse(DfsPath, Path)
        ), 
        Path_list), % find all Path_list and their lengths between first and last locations
    find_shortest_path_time([Path, Time], Path_list). % searchs shortest path and releated time and it returns

% To find the minimum of a list of lists
find_shortest_path_time(Smallest ,[Smallest]). % if there is only one element, it is the Smallest one
find_shortest_path_time(Smallest, [[Path, CostOfTime]|T]) :- % |T is the tail of the list 
    find_shortest_path_time([First_path, First_time], T),
    (CostOfTime < First_time -> Smallest = [Path, CostOfTime] ; Smallest = [First_path, First_time]).

% Rule for checking if an object is on delivery
check_delivery_status(ObjectID) :-
    on_delivery(ObjectID),
    current_delivery_person(ObjectID, PersonID),
    format("Object ~w is on delivery. Delivered by person ~w.~n", [ObjectID, PersonID]).

% Rule for checking if an object is not on delivery and finding available persons to deliver
check_delivery_status(ObjectID) :-
    \+ on_delivery(ObjectID),
    find_available_delivery_persons(ObjectID, Persons),
    print_available_delivery_persons(Persons).

% Rule for finding all available delivery persons and their total time
find_available_delivery_persons(ObjectID, AvailablePersons) :-
    object(ObjectID, Weight, PickUpPlace, DropOffPlace, _, _),  % Get the weight of the object
    findall(
        [PersonID, Path1, Path2, TotalTime],
        (
            person(PersonID, _, _,  0, Location),  % Person is available (fourth parameter is 0)
            min_path(Location, PickUpPlace, Path1, StartToDeliveryLocationTime), % Finds shortest path and calculates Time to go from deliver person's location to pick up location
            min_path(PickUpPlace, DropOffPlace, Path2, PickToDropTime), % Finds shortest path and calculates Time to go from pick up location to drop off location
            travel_time(TotalTime, StartToDeliveryLocationTime, PickToDropTime), % Calculates total time
            available_for_delivery(PersonID, Weight, TotalTime) % % Checks if deliver person is available to deliver this object in terms of work hours, carrying weight of object
        ),
        AvailablePersons
    ).

travel_time(TotalTime, StartToDeliveryLocationTime, PickToDropTime) :- % Finds total time to deliver specified object
    TotalTime is StartToDeliveryLocationTime + PickToDropTime.

available_for_delivery(PersonID, WeightOfObject, TotalTimeToDeliver) :- % Checks if deliver person is available to deliver this object in terms of work hours, carrying weight of object
    person(PersonID, Capacity, WorkHours, _, _),
    Capacity >= WeightOfObject,  % Person can carry the weight of the object
    WorkHours >= TotalTimeToDeliver.

% Rule for printing available delivery persons
print_available_delivery_persons([]) :- % There is not any available delivery person case
    format("No available delivery persons.~n").

print_available_delivery_persons(Persons) :- % Print all the possible deliver informations.
    print_remaining_persons(Persons).

print_remaining_persons([]).
print_remaining_persons([[PersonID, Path1, Path2, TotalTime] | Rest]) :-
    format("Person ~w can make the delivery.~n", [PersonID]),
    format("Path from current deliver person location to pick-up place: ~w~n", [Path1]),
    format("Path from pick-up place to drop-off place: ~w~n", [Path2]),
    format("Total Time: ~w~n~n", [TotalTime]),
    print_remaining_persons(Rest).

delivering(PersonID) :- person(PersonID, _, _, ObjectID, _), ObjectID \= 0. % Check if the person is delivering an object
on_delivery(ObjectID) :- object(ObjectID, _, _, _, _, ObjectStatus), ObjectStatus \= 0.  % Check if the object is on delivery

current_delivery_person(ObjectID, PersonID) :-
    delivering(PersonID),    % Check if the person is delivering an object
    person(PersonID, _, _, ObjectID, _),    % Check if the person is delivering the specified object
    ObjectID \= 0.    % object ID is not 0 (indicating no delivery)

%-----------FACTS---------------
place(adminOffice, 1).
place(cafeteria, 2).
place(engineeringBld, 3).
place(library, 4).
place(socialSciencesBld, 5).
place(instituteX, 6).
place(instituteY, 7).
place(lectureHallA, 8).

path(adminOffice, engineeringBld, 3).
path(adminOffice, cafeteria, 4).
path(adminOffice, library, 1).
path(cafeteria, library, 5).
path(engineeringBld, library, 5).
path(socialSciencesBld, library, 2).
path(socialSciencesBld, cafeteria, 2).
path(instituteY, library, 3).
path(instituteY, lectureHallA, 3).
path(engineeringBld, lectureHallA, 2).
path(instituteX, socialSciencesBld, 8).

%Reverse order
path(engineeringBld, adminOffice, 3).
path(cafeteria, adminOffice, 4).
path(library, adminOffice, 1).
path(library, cafeteria, 5).
path(library, engineeringBld, 5).
path(library, socialSciencesBld, 2).
path(cafeteria, socialSciencesBld, 2).
path(library, instituteY, 3).
path(lectureHallA, instituteY, 3).
path(lectureHallA, engineeringBld, 2).
path(socialSciencesBld, instituteX, 8).

% Rules for undirected connections
undirected_connected(X, Y, Weight) :- path(X, Y, _).
undirected_connected(X, Y, Weight) :- path(Y, X, _).

% Parameters
% A unique ID.  
% Weight in kg. 
% Pick up place. 
% Drop of place. 
% Urgency of the delivery (low, medium, high). 
% ID of the delivery person if in transit. 
object(1, 4, adminOffice, library, low, 0).
object(2, 2, instituteX, instituteY, medium, 0).
object(3, 3 , socialSciencesBld, cafeteria, medium, 0).
object(4, 4, adminOffice, lectureHallA, low, 0).
object(5, 4, cafeteria, lectureHallA, high, 1).

% Parameters
% A unique ID. 
% Capacity to carry (in kg). 
% Work hours (in 4-hour increments including the entire day). 
% Current delivery job (if any). 
% Current location.
person(1, 4, 16, 5, adminOffice).
person(2, 5, 24, 0, socialSciencesBld).
person(3, 5, 12, 0, instituteX).

% person(4, 5, 12, 5, cafeteria).
% person(5, 4, 24, 0, adminOffice).