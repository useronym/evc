# Erlang vector clock
This is a simple implementation of the [Vector Clock](http://en.wikipedia.org/wiki/Vector_clock) algorithm for Erlang.
It supports dynamic addition and removal of actors in the system at runtime.

It should also be noted that total ordering has been hacked on top of the regular VC implementation.
Each clock carries a timestamp that is used in the evc:compare/2 function. Compare operates as follows:
If A doesn't descend from B and B doesn't descend from A, compare timestamps instead. This compare function is
suitable for plugging into, for example, lists:sort/2.


## Usage
Actor A
```erlang
VC = evc:new(),
% A experiences an internal event
NewVC = evc:event(VC),
% A sends a message to B
B ! {NewVC, "I am a happy message!"}
```
Actor B
```erlang
VC = evc:new(),
receive
    {VCFromA, _Message} ->
        % B experiences internal event due to receiving a message
        InternalVC = evc:event(VC),
        % Merge the two VCs to get the new VC for B
        NewVC = evc:merge(InternalVC, VCFromA),
        % Now, the VC from A is "older" compared to the current VC in B
        true = evc:descends(NewVC, VCFromA)
end
```

#### Total ordering example
```erlang
M = evc:merge(evc:event(a, evc:new(a)), evc:event(b, evc:new(b))), % M is {1, 1} - 1 event in actor A and 1 event in actor B
A = evc:event(a, M),
timer:sleep(10),
B = evc:event(b, M), % A is {2, 1} and B is {1, 2}. Neither descends from either, however timestamp from B is newer
B2 = evc:event(b, B), % B2 is a direct descendant of B
timer:sleep(10),
C = evc:event(c, evc:new(c)), % C is completely unrelated to all the above clocks. Again, timestamp will be used.
C2 = evc:event(c, C),
[A, B, B2, C, C2] = lists:sort(fun evc:compare/2, [C2, B, A, C, B2]))
```
