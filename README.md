# Some Mirage Deployment Scenarios (cf. _mirari run_)

A set of scenarios so I can clarify my thoughts about how we need to integrate Mirage, Mirari, Froc, Lwt, etc.

## Scenarios

### Hotplug event (data-space change)

Introduction of new "device" (typically logically via XenStore) potentially provokes app to use new device. E.g., higher performance network interface available; ARP change; existence of new, more appropriate storage.

_Does `libfable` just take care of all these cases?_

### Reconfiguration (configuration-space change)

Incoming reconfiguration to this instance triggers reconfiguration in other instances. E.g., interface change in A triggers routing change that must be propagated to routers B and C.

### Self-scaling (data-space change causes configuration-space change)

Increase/decrease in load causes need to change scale of app. 
Response is to create a new instance or determine which instance to destroy, and update OpenFlow or other network config.

## Proposal

### Terms

_Data-space_ handled by `Lwt` or other threading system. The state of a running unikernel.

_Configuration-space_ handled by `Froc` or other FRP system. Also a unikernel ultimately, but in a "manager" role.

__Assumption__: data-space unikernels have some communication channel back to the configuration-space.

_Behaviors_ or _Changeables_ (`Froc`), or _Signals_ (`React`)  are values that can change over time, but are defined at all times.

_Events_ (`Froc` and `React`) are defined only at particular instants in time, possibly (but not necessarily) with a different value at each instant. 

__NB__. `Froc` uses _signals_ to refer to events and behaviors when the distinction isn't important.

### Discussion

Need to communicate changes in data-space into the `Froc` computation. The `Froc` computation will then act on this, e.g., by spinning up a new VM.

How does the system, consisting of a set of unikernels including exactly (at least?) one Mirari unikernel (which itself encodes the transition function), transition from state _S(t)_ transition to state _S(t+1)_ following this update?

So we need mechanisms for:

1. an `'a Unikernel` to deliver a signal to the Mirari unikernel;
2. the Mirari unikernel to transform its state as a result of that signal; and
3. as a result of that transformation, the Mirari unikernel to transform the state of zero or more of the `'a Unikernels` it controls.

(1), assuming the Mirari unikernel itself uses `Lwt`, requires a way to emit a `Froc` signal from an `'a Lwt`, restarting the `'a Lwt`.

(2) requires the Mirari unikernel effectively to wrap up a `Froc` SA computation.

(3) requires the Mirari unikernel  to be able to construct, start and kill `'a Unikernel`s on the fly.


# Scratchpad

Need to communicate config changes into data-space, i.e., to transform the current data-space configuration (set of `'a Lwt`) as a result of a change in configuration-space. This is the direction that can be mocked up via fork plus transformation function per anil's suggestion.

Need to treat reconfigurations as atomic -- so avoid yield during reconfiguration (update behaviours and propagate) -- like having well-defined time steps in a simulation. (The _synchrony hypothesis_.)

So do we just wrap `Lwt` in a monad with some accessor functions?

This would tie together an `'a Lwt` with an `'a Behaviour`, would manage the big list of `'a Lwt` so that they can all happily co-schedule, and would lift the result when an `'a Lwt` terminates into its corresponding `'a Behaviour`.
