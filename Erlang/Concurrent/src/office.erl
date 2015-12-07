-module(office).
-export([room/4, student/2, officedemo/0, busyOffice/0, checkFront/2, putTail/2,indexOf/2]).

room(Students, Capacity, Queue, Helping) ->
  io:format("Room called~n"),
  receive
    {From, enter, Name} when Capacity > 0 ->
      case office:checkFront(Name,Queue) of
        %student is at front of queue or there is no queue
        true when Queue =:= []->
          io:format("Student admitted no queue~n"),
          From ! {self(), ok},
          room([Name|Students], Capacity - 1,Queue,Helping);
        true ->
          io:format("Student admitted removing item from queue~n"),
          From ! {self(), ok},
          room([Name|Students], Capacity - 1,lists:delete(Name, Queue),Helping);
        %student is not at front of queue
        false->
          io:format("Student not in queue must be placed in queue~n"),
          %need to construct the queue appropriately with reverse
          NewQueue = office:putTail(Name,Queue),
          QueueWait = 1000 * office:indexOf(Name,NewQueue),
          From ! {self(), room_full, QueueWait},
          room([Students], Capacity, [NewQueue],Helping)
      end;

    % student entering, at capacity
    {From, enter, Name} ->
      case lists:member(Name,Queue) of
      %student is in the queue
        true ->
          io:format("Student in queue, waiting~n"),
          QueueWait = 1000 * office:indexOf(Name,Queue),
          From ! {self(), ok, QueueWait},
          room([Students], Capacity, [Queue],Helping);
          %student is not in the queue
        false->
          io:format("Student not in queue must be placed in queue~n"),
          %need to construct the queue appropriately with reverse
          NewQueue = office:putTail(Name,Queue),
          QueueWait = 1000 * office:indexOf(Name,NewQueue),
          From ! {self(), room_full, QueueWait},
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
          io:format("~B Leaving.~n", Name),
          From ! {self(), ok},
          room(lists:delete(Name, Students), Capacity + 1,[Queue],false);
        false ->
          From ! {self(), not_found},
          io:format("~B not found.~n", Name),
          room([Students], Capacity,[Queue],false)
      end;

    % student thanks
    {From, thanks, Name} ->
      io:format("~B thanks you.~n", Name),
      room([Students],Capacity,Queue,false)
end.

studentWork(Name) ->
  SleepTime = rand:uniform(5000) + 5000,
  io:format("~s entered the Office and will work for ~B ms.~n", [Name, SleepTime]),
  timer:sleep(SleepTime).

student(Office, Name) ->
  timer:sleep(rand:uniform(3000)),
  Office ! {self(), enter, Name},
  receive
    % Success; can enter room.
    {_, ok} ->
      Office ! {self(), help_me, Name},
      studentWork(Name),
      io:format("~s ok without sleep.~n", [Name]),
      Office ! {self(), leave, Name},
      Office ! {self(), thanks, Name},
      io:format("~s left the Office.~n", [Name]);

  % Success; can enter room.
    {_, ok, SleepTime} ->
      Office ! {self(), help_me, Name},
      io:format("~s okwith sleep.~n", [Name]),
      studentWork(Name),
      Office ! {self(), leave, Name},
      Office ! {self(), thanks, Name},
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

indexOf(_, []) -> notFound;
indexOf(Val,[Val|_]) -> 1;
indexOf(Val,[_|T]) -> 1+indexOf(Val,[T]).

officedemo() ->
  R = spawn(office, room, [[], 3,[],false]), % start the room process with an empty list of students
  spawn(office, student, [R, "Ada"]),
  spawn(office, student, [R, "Barbara"]),
  spawn(office, student, [R, "Charlie"]),
  spawn(office, student, [R, "Donald"]),
  spawn(office, student, [R, "Elaine"]),
  spawn(office, student, [R, "Frank"]),
  spawn(office, student, [R, "George"]).