# Erlang vector clock
This is a simple implementation of the [Vector Clock](http://en.wikipedia.org/wiki/Vector_clock) algorithm for Erlang. It supports dynamic addition and (not yet) removal of actors in the system at runtime.


![Image of Yaktocat](http://upload.wikimedia.org/wikipedia/commons/thumb/5/55/Vector_Clock.svg/725px-Vector_Clock.svg.png)


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
    {VCFromA, Message} ->
        % B experiences internal event due to receiving a message
        InternalVC = evc:event(VC),
        % Merge the two VCs to get the new VC for B
        NewVC = evc:merge(InternalVC, VCFromA),
        % Now, the VC from A is "older" compared to the current VC in B
        true = evc:older(VCFromA, NewVC)
end
```
