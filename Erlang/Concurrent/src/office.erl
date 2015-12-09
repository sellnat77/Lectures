-module(office).
-export([room/4, student/2, officedemo/0, busyOffice/0, checkFront/2, putTail/2,indexOf/2, index_of/2, index_of/3, debugList/1,checkEmpty/1]).

%Pattern matching to catch when queue is empty
room(Students, Capacity, [], Helping) ->
  io:format("Room called~n, current state is:~n"),
  io:format("Students:"),
  office:debugList(Students),
  receive
    {From, enter, Name} when Capacity > 0 ->
          io:format("~s admitted no queue~n",[Name]),
          From ! {self(), ok},
          room([Name|Students], Capacity - 1,[],Helping);

  % student entering, at capacity
    {From, enter, Name} ->
      %Queue is empty but at capacity
          io:format("~s not in queue must be placed in queue because at capacity but queue is emtpy~n",[Name]),
          %need to construct the queue appropriately with reverse
          QueueWait = 1000,
          From ! {self(), room_full, QueueWait},
          io:format("~n~n~nStarted new queue"),
          room([Students], Capacity, [Name],Helping)
  end;
room(Students, Capacity, Queue, Helping) ->
  io:format("~n~nRoom called with an existing queue~n, current state is:~n"),
  io:format("Students:"),
  office:debugList(Students),
  io:format("Queue:"),
  office:debugList(Queue),
  receive
    {From, enter, Name} when Capacity > 0 ->
      io:format("~n~n recieved message when there is capacity"),
      IsEmpty = office:checkEmpty(Queue),
      IsMemeber = lists:member(Name, Queue),
      IsFront = office:checkFront(Name,Queue),

      if
        %Non-empty queue but front of line
        IsFront ->
          io:format("~s admitted removing item from queue~n",[Name]),
          From ! {self(), ok},
          room([Name|Students], Capacity - 1,lists:delete(Name, Queue),Helping);

        %Non-empty queue but somewhere already inline
        IsMemeber ->
          io:format("~s Already in line~n",[Name]),
          QueueWait = 1000 * office:index_of(Name,Queue),
          From ! {self(), room_full, QueueWait},
          room([Students], Capacity, [Queue],Helping);

        %Non-empty queue but not yet in queue
        true ->
          io:format("~s ISEMPTY",[IsEmpty]),
          io:format("~s the queue is ",[Queue]),
          io:format("~s not in queue must be placed in queue the queue is already populated with some items~n",[Name]),
          %need to construct the queue appropriately with reverse
          NewQueue = office:putTail(Name,Queue),
          QueueWait = 1000 * office:index_of(Name,NewQueue),
          From ! {self(), room_full, QueueWait},
          io:format("Queue:"),
          office:debugList(NewQueue),
          room([Students], Capacity, [NewQueue],Helping)

      end;

    % student entering, at capacity
    {From, enter, Name} ->
      io:format("~n~n recieved message when there is no capacity"),
      IsFront = office:checkFront(Name,Queue),
      IsMemeber = lists:member(Name,Queue),
      if
      %Queue is empty but at capacity
      %Non-empty queue but front of line
        IsFront ->
          io:format("~s admitted removing item from queue~n",[Name]),
          From ! {self(), ok},
          room([Name|Students], Capacity - 1,lists:delete(Name, Queue),Helping);
        %Queue is not empty and current student is in line
        IsMemeber ->
          io:format("~s already in queue~n",[Name]),
          %need to construct the queue appropriately with reverse
          QueueWait = 1000 * office:index_of(Name,Queue),
          From ! {self(), room_full, QueueWait},
          room([Students], Capacity, [Queue],Helping);
        %Queue is not empty but student not yet in line
        true ->
          io:format("~s not in queue must be placed in existing queue and also at capacity~n",[Name]),
          %need to construct the queue appropriately with reverse
          NewQueue = office:putTail(Name,Queue),
          io:format("HIT?????"),
          Index = office:index_of(Name,NewQueue),
          io:format("The names index is ~B",[Index]),
          QueueWait = 1000 * Index,

          io:format("NEW HIT?????"),
          io:format("sending message that room is full and must wait for ~B",[QueueWait]),
          From ! {self(), room_full, QueueWait},
          io:format("Queue:"),
          office:debugList(NewQueue),
          room([Students], Capacity, [NewQueue],Helping)
      end;

    {From, help_me, Name} ->
      case Helping of
        true->
          io:format("helping true.~n"),
          From ! {self(), busy},
          room([Students],Capacity,[Queue],Helping);
        false->
          io:format("helping false.~n"),
          From ! {self(), ok},
          room([Students],Capacity,[Queue],true)
      end;

    % student leaving
    {From, leave, Name} ->
      % make sure they are already in the room
      case lists:member(Name, Students) of
        true ->
          io:format("~s Leaving.~n", [Name]),
          From ! {self(), ok},
          room(lists:delete(Name, Students), Capacity + 1,[Queue],false);
        false ->
          From ! {self(), not_found},
          io:format("~s not found.~n", [Name]),
          room([Students], Capacity,[Queue],false)
      end;

    % student thanks
    {From, thanks, Name} ->
      io:format("~s thanks you.~n", [Name]),
      room([Students],Capacity+1,Queue,false)
end.

studentWork(Name) ->
  SleepTime = rand:uniform(5000) + 5000,
  io:format("~p entered the Office and will work for ~B ms.~n", [Name, SleepTime]),
  timer:sleep(SleepTime).

student(Office, Name) ->
  io:format("~p called.~n", [Name]),
  timer:sleep(rand:uniform(3000)),
  Office ! {self(), enter, Name},
  receive
    % Success; can enter room.
    {_, ok} ->
      studentWork(Name),
      Office ! {self(), help_me, Name},
      io:format("~s ok without sleep.~n", [Name]),
      Office ! {self(), leave, Name},
      io:format("~s left the Office.~n", [Name]),
      Office ! {self(), thanks, Name};

  % Success; can enter room.
    {_, ok, SleepTime} ->

      io:format("~s ok with sleep.~n", [Name]),
      studentWork(Name),
      Office ! {self(), help_me, Name},
      Office ! {self(), thanks, Name},
      Office ! {self(), leave, Name},

      io:format("~s left the Office.~n", [Name]);

  % Instructor is busy
    {_, busy} ->
      office:busyOffice(),
      io:format("~s wanted help but the instructor was busy, asking again.~n", [Name]),
      Office ! {self(), help_me, Name},
      student(Office, Name);

    % Office is full; sleep and try again.
    {_, room_full, SleepTime} ->
      io:format("~s could not enter and must wait ~B ms.~n", [Name, SleepTime]),
      timer:sleep(SleepTime),
      student(Office, Name)
  end.

busyOffice() ->
  io:format("Instructor is busy.~n"),
  timer:sleep(1000).

checkFront(_,[]) -> true;
checkFront(Check,[H]) -> Check =:= H;
checkFront(Check,[H|_])-> Check =:= H.

putTail(Element, List) -> lists:reverse([Element |lists:reverse(List)]).

indexOf(_, []) -> 100000,
  io:format("NOT FOUND");
indexOf(Val,[Val|_]) -> 1,
  io:format("ONE ONE ONE");
indexOf(Val,[_|T]) -> 1+indexOf(Val,T),
  io:format("FINDING FINDING").

index_of(Value, List) ->
   index_of(Value, List, 1).
 index_of(V, [V|T], N) ->
   N;
 index_of(V, [_|T], N) ->
   index_of(V, T, N+1);
 index_of(_, [], _) ->
   false.

%index_of(_, [], _)  -> not_found;
%index_of(Item, [Item|_], Index) -> Index;
%index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

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