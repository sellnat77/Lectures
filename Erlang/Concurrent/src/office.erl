-module(office).
-export([room/4, student/2, officedemo/0, busyOffice/0, first/1, putTail/2, index_of/2, index_of/3, debugList/1,checkEmpty/1]).

%Pattern matching to catch when queue is empty
room(Students, Capacity, [], Helping) ->
  receive
    {From, enter, Name} when Capacity > 0 ->
      %Immediate entry
          room([Name|Students], Capacity - 1,[],Helping);

  % student entering, at capacity
    {From, enter, Name} ->
      %Queue is empty but at capacity
          QueueWait = 1000,
          From ! {self(), room_full, QueueWait},
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
      case lists:member(Name, Students) of
        true ->
          room(lists:delete(Name, Students), Capacity + 1,[],Helping);
        false ->
          From ! {self(), not_found},
          room(Students, Capacity,[],Helping)
      end;

  % student thanks
    {From, thanks, Name} ->
      room(Students,Capacity,[],false)
  end;

room(Students, Capacity, Queue, Helping) ->
  receive
    {From, enter, Name} when Capacity > 0 ->
      io:format("The queue is ~B",[Queue]),
      IsMember = lists:member(Name, Queue),
      IsFront = Name =:= office:first(Queue),
      if
        %Non-empty queue but front of line
        IsFront ->
          io:format("~s FRONT.~nFRONT.~nFRONT.~nFRONT.~nFRONT.~nFRONT.~n", [Name]),
          room([Name|Students], Capacity - 1,lists:delete(Name, Queue),Helping);

        %Non-empty queue but somewhere already in line
        IsMember ->
          io:format("MEMBERMEMBERMEMBERMEMBERMEMBERMEMBERMEMBERMEMBERMEMBERMEMBERMEMBERMEMBERMEMBER"),
          QueueWait = 1000 * office:index_of(Name,Queue),
          From ! {self(), room_full, QueueWait},
          room(Students, Capacity, Queue, Helping);

        %Non-empty queue but not yet in queue
        true ->
          io:format("PUTTING AT END"),
          %need to construct the queue appropriately with reverse
          NewQueue = office:putTail(Name,Queue),
          QueueWait = 1000 * office:index_of(Name,NewQueue),
          From ! {self(), room_full, QueueWait},
          room(Students, Capacity, NewQueue, Helping)
      end;

    % student entering, at capacity
    {From, enter, Name} ->
      io:format("The queue is ~s",Queue),
      IsFront = Name =:= office:first(Queue),
      IsMember = lists:member(Name,Queue),
      if
      %Non-empty queue but front of line
        IsFront,Helping =:= false ->
          io:format("~s FRONT.~nFRONT.~nFRONT.~nFRONT.~nFRONT.~nFRONT.~n", [Name]),
          room(Students, Capacity, Queue, Helping);

      %Queue is not empty and current student is in line
        IsMember ->
          %need to construct the queue appropriately with reverse
          QueueWait = 1000 * office:index_of(Name,Queue),
          From ! {self(), room_full, QueueWait},
          room(Students, Capacity, Queue, Helping);

      %Queue is not empty but student not yet in line
        true ->
          io:format("PUTTING AT END FOR REAL"),
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
      % make sure they are already in the room
      case lists:member(Name, Students) of
        true ->
          room(lists:delete(Name, Students), Capacity + 1, Queue, Helping);
        false ->
          From ! {self(), not_found},
          room(Students, Capacity, Queue, Helping)
      end;

    % student thanks
    {From, thanks, Name} ->
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
    {_, ok} ->
      io:format("~s recieving help.~n", [Name]),
      studentWork(Name),
      io:format("~s was helped.~n", [Name]),
      Office ! {self(), leave, Name},
      Office ! {self(), thanks, Name},
      io:format("~s left the office.~n~n", [Name]);

  % Instructor is busy
    {_, busy} ->
      io:format("~s asking for help.~n", [Name]),
      office:busyOffice(),
      io:format("~s wanted help but the instructor was busy, asking again.~n", [Name]),
      Office ! {self(), help_me, Name};

    % Office is full; sleep and try again.
    {_, room_full, SleepTime} ->
      io:format("~s could not enter and must wait ~B ms.~n", [Name, SleepTime]),
      timer:sleep(SleepTime),
      student(Office, Name)
  end.

busyOffice() ->
  io:format("Instructor is busy.~n~n"),
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
debugList([H]) -> io:format("~p~n",[H]);
debugList([H|T]) -> io:format("~p~n",[H]),
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