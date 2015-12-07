-module(office).
-export([room/4, student/2, officedemo/0, busyOffice/0, checkFront/2, putTail/2,indexOf/2, debugList/1,checkEmpty/1]).

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
          room([Students], Capacity, [Name],Helping)
  end;
room(Students, Capacity, Queue, Helping) ->
  io:format("Room called~n, current state is:~n"),
  io:format("Students:"),
  office:debugList(Students),
  io:format("Queue:"),
  office:debugList(Queue),
  receive
    {From, enter, Name} when Capacity > 0 ->
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
          QueueWait = 1000 * office:indexOf(Name,Queue),
          From ! {self(), room_full, QueueWait},
          room([Students], Capacity, [Queue],Helping);

        %Non-empty queue but not yet in queue
        true ->
          io:format("~s ISEMPTY",[IsEmpty]),
          io:format("~s the queue is ",[Queue]),
          io:format("~s not in queue must be placed in queue the queue is already populated with some items~n",[Name]),
          %need to construct the queue appropriately with reverse
          NewQueue = office:putTail(Name,Queue),
          QueueWait = 1000 * office:indexOf(Name,NewQueue),
          From ! {self(), room_full, QueueWait},
          io:format("Queue:"),
          office:debugList(NewQueue),
          room([Students], Capacity, [NewQueue],Helping)

      end;


      %case office:checkFront(Name,Queue) of
      %  %student is at front of queue or there is no queue
      %  true when Queue =:= []->
      %    io:format("Student admitted no queue~n"),
      %    From ! {self(), ok},
      %    room([Name|Students], Capacity - 1,[],Helping);
      %  true ->
      %    io:format("Student admitted removing item from queue~n"),
      %    From ! {self(), ok},
      %    room([Name|Students], Capacity - 1,lists:delete(Name, Queue),Helping);
      %  %student is not at front of queue
      %  false->
      %    io:format("Student not in queue must be placed in queue~n"),
      %    %need to construct the queue appropriately with reverse
      %    NewQueue = office:putTail(Name,Queue),
      %    QueueWait = 1000 * office:indexOf(Name,NewQueue),
      %    From ! {self(), room_full, QueueWait},
      %    room([Students], Capacity, [NewQueue],Helping)
      %end;

    % student entering, at capacity
    {From, enter, Name} ->
      IsEmpty = office:checkEmpty(Queue),
      IsMemeber = lists:member(Name,Queue),
      if
      %Queue is empty but at capacity
        IsEmpty ->
          io:format("~s not in queue must be placed in queue because at capacity but queue is emtpy~n",[Name]),
          %need to construct the queue appropriately with reverse
          NewQueue = office:putTail(Name,Queue),
          QueueWait = 1000 * office:indexOf(Name,NewQueue),
          From ! {self(), room_full, QueueWait},
          io:format("Queue:"),
          office:debugList(NewQueue),
          room([Students], Capacity, [NewQueue],Helping);
        %Queue is not empty and current student is in line
        IsMemeber ->
          io:format("~s already in queue~n",[Name]),
          %need to construct the queue appropriately with reverse
          QueueWait = 1000 * office:indexOf(Name,Queue),
          From ! {self(), room_full, QueueWait},
          room([Students], Capacity, [Queue],Helping);
        %Queue is not empty but student not yet in line
        true ->
          io:format("~s not in queue must be placed in queue and also at capacity~n",[Name]),
          %need to construct the queue appropriately with reverse
          NewQueue = office:putTail(Name,Queue),
          QueueWait = 1000 * office:indexOf(Name,NewQueue),
          From ! {self(), room_full, QueueWait},
          io:format("Queue:"),
          office:debugList(NewQueue),
          room([Students], Capacity, [NewQueue],Helping)
      end;


      %case lists:member(Name,Queue) of
      %%student is in the queue
      %  true ->
      %    io:format("Student in queue, waiting~n"),
      %    QueueWait = 1000 * office:indexOf(Name,Queue),
      %    From ! {self(), ok, QueueWait},
      %    room([Students], Capacity, [Queue],Helping);
      %    %student is not in the queue
      %  false->
      %    io:format("Student not in queue must be placed in queue~n"),
      %    %need to construct the queue appropriately with reverse
      %    NewQueue = office:putTail(Name,Queue),
      %    QueueWait = 1000 * office:indexOf(Name,NewQueue),
      %    From ! {self(), room_full, QueueWait},
      %    room([Students], Capacity, [NewQueue],Helping)
      %  end;

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
      room([Students],Capacity,Queue,false)
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
      Office ! {self(), help_me, Name},
      studentWork(Name),
      io:format("~s ok without sleep.~n", [Name]),
      Office ! {self(), leave, Name},
      Office ! {self(), thanks, Name},
      io:format("~s left the Office.~n", [Name]);

  % Success; can enter room.
    {_, ok, SleepTime} ->
      Office ! {self(), help_me, Name},
      io:format("~s ok with sleep.~n", [Name]),
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