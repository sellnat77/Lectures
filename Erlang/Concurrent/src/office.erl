-module(office).
-export([room/4, student/2, officedemo/0, busyOffice/0, first/1, putTail/2, index_of/2, index_of/3, debugList/1,checkEmpty/1]).

%Pattern matching to catch when queue is empty
room(Students, Capacity, [], Helping) ->
  io:format("~nRoom called~n, current state is:~n"),
  io:format("Students:"),
  office:debugList(Students),
  io:format("~n"),
  receive
    {From, enter, Name} when Capacity > 0 ->
          io:format("~n~s admitted no queue~n",[Name]),
          room([Name|Students], Capacity - 1,[],Helping);

  % student entering, at capacity
    {From, enter, Name} ->
      %Queue is empty but at capacity
          QueueWait = 1000,
          From ! {self(), room_full, QueueWait},
          io:format("~n~nStarted new queue"),
          room(Students, Capacity, [Name],Helping);

    {From, help_me, Name} ->
      case Helping of
        true->
          From ! {self(), busy},
          room(Students,Capacity,[],Helping);
        false->
          From ! {self(), ok},
          room(Students,Capacity,[],true)
      end;

  % student leaving
    {From, leave, Name} ->
      io:format("~n~s leaving.~n", [Name]),
      % make sure they are already in the room
      case lists:member(Name, Students) of
        true ->
          io:format("~s left.~n", [Name]),
          room(lists:delete(Name, Students), Capacity + 1,[],Helping);
        false ->
          From ! {self(), not_found},
          io:format("~s not found.~n", [Name]),
          room(Students, Capacity,[],Helping)
      end;

  % student thanks
    {From, thanks, Name} ->
      io:format("~s thanks the professor.~n", [Name]),
      room(Students,Capacity,[],false)
  end;

room(Students, Capacity, Queue, Helping) ->
  io:format("~n~nRoom called with an existing queue~n, current state is:~n"),
  io:format("Students:"),
  office:debugList(Students),
  io:format("~nQueue:"),
  office:debugList(Queue),
  io:format("~n"),
  receive
    {From, enter, Name} when Capacity > 0 ->
      IsMember = lists:member(Name, Queue),
      IsFront = Name =:= office:first(Queue),

      if
        %Non-empty queue but front of line
        IsFront ->
          io:format("~n~s admitted, removing item from queue~n",[Name]),
          room([Name|Students], Capacity - 1,lists:delete(Name, Queue),Helping);

        %Non-empty queue but somewhere already inline
        IsMember ->
          io:format("~s already in line~n",[Name]),
          QueueWait = 1000 * office:index_of(Name,Queue),
          From ! {self(), room_full, QueueWait},
          room(Students, Capacity, Queue, Helping);

        %Non-empty queue but not yet in queue
        true ->
          %need to construct the queue appropriately with reverse
          NewQueue = office:putTail(Name,Queue),
          QueueWait = 1000 * office:index_of(Name,NewQueue),
          From ! {self(), room_full, QueueWait},
          room(Students, Capacity, NewQueue, Helping)
      end;

    % student entering, at capacity
    {From, enter, Name} ->
      IsFront = Name =:= office:first(Queue),
      IsMember = lists:member(Name,Queue),
      if
      %Non-empty queue but front of line
        IsFront,Helping =:= false ->
          io:format("~n~s at front of the line but no capacity.~n",[Name]),
          %From ! {self(), ok},
          room(Students, Capacity,Queue,Helping);

      %Queue is not empty and current student is in line
        IsMember ->
          io:format("~s already in queue~n",[Name]),
          %need to construct the queue appropriately with reverse
          QueueWait = 1000 * office:index_of(Name,Queue),
          From ! {self(), room_full, QueueWait},
          room(Students, Capacity, Queue, Helping);

      %Queue is not empty but student not yet in line
        true ->
          %need to construct the queue appropriately with reverse
          NewQueue = office:putTail(Name,Queue),
          Index = office:index_of(Name,NewQueue),
          QueueWait = 1000 * Index,
          From ! {self(), room_full, QueueWait},
          room(Students, Capacity, NewQueue, Helping)
      end;

    {From, help_me, Name} ->
      case Helping of
        true->
          From ! {self(), busy},
          room(Students,Capacity,Queue,Helping);
        false->
          From ! {self(), ok},
          room(Students,Capacity,Queue,true)
      end;

    % student leaving
    {From, leave, Name} ->
      io:format("~n~s leaving.~n", [Name]),
      % make sure they are already in the room
      case lists:member(Name, Students) of
        true ->
          io:format("~s left.~n", [Name]),
          From ! {self(), ok},
          room(lists:delete(Name, Students), Capacity + 1, Queue, Helping);
        false ->
          From ! {self(), not_found},
          io:format("~s not found.~n", [Name]),
          room(Students, Capacity, Queue, Helping)
      end;

    % student thanks
    {From, thanks, Name} ->
      io:format("~s thanks the professor.~n", [Name]),
      room(Students,Capacity+1,Queue,false)
end.

studentWork(Name) ->
  SleepTime = rand:uniform(5000) + 5000,
  io:format("~p entered the Office and will work for ~B ms.~n", [Name, SleepTime]),
  timer:sleep(SleepTime).

student(Office, Name) ->
  timer:sleep(rand:uniform(3000)),
  Office ! {self(), enter, Name},
  Office ! {self(), help_me, Name},
  receive
    % Success; can enter room.
    {_, ok} ->
      studentWork(Name);

  % Success; can enter room.
    {_, ok, SleepTime} ->
      studentWork(Name),
      timer:sleep(SleepTime);

  % Instructor is busy
    {_, busy} ->
      office:busyOffice(),
      io:format("~s wanted help but the instructor was busy, asking again.~n", [Name]),
      Office ! {self(), help_me, Name};

    % Office is full; sleep and try again.
    {_, room_full, SleepTime} ->
      io:format("~s could not enter and must wait ~B ms.~n", [Name, SleepTime]),
      timer:sleep(SleepTime),
      student(Office, Name)
  end,

  Office ! {self(), leave, Name},
  Office ! {self(), thanks, Name}.

busyOffice() ->
  io:format("Instructor is busy.~n"),
  timer:sleep(1000).

first([]) -> undefined;
first([H|_]) -> H.

putTail(Element, []) -> [Element];
putTail(Element, [H]) -> lists:reverse([Element] ++ [H]);
putTail(Element, [H|T]) ->
  Rev = lists:reverse(T),
  [H] ++ lists:reverse([Element] ++ Rev).

index_of(Value, List) ->
   index_of(Value, List, 1).
 index_of(V, [V|T], N) ->
   N;
 index_of(V, [_|T], N) ->
   index_of(V, T, N+1);
 index_of(_, [], _) ->
   false.

debugList([]) -> undefined;
debugList([H]) -> io:format("~p ",[H]);
debugList([H|T]) -> io:format("~p ",[H]),
  debugList(T).

checkEmpty([]) -> true;
checkEmpty([_]) -> false;
checkEmpty([H|T]) -> false.

officedemo() ->
  R = spawn(office, room, [[], 3,[],false]), % start the room process with an empty list of students
  spawn(office, student, [R, "Ada"]),
  spawn(office, student, [R, "Barbara"]),
  spawn(office, student, [R, "Charlie"]),
  spawn(office, student, [R, "Donald"]),
  spawn(office, student, [R, "Elaine"]),
  spawn(office, student, [R, "Frank"]),
  spawn(office, student, [R, "George"]).