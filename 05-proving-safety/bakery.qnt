/*
https://lamport.azurewebsites.net/pubs/bakery.pdf
Changing the L* references since there are more possible
interleavings than if I just reuse the paper's.
Also, I do not allow for reentering the loop for simplicity

begin integer j;
    choosing [i] : = 1 ;
L1
    number[i] := 1 + maximum (number[l],..., number[N]);
    choosing[i] := 0;
    forj = 1 step l until N do
L2:     if choosing[j] != 0
            goto L2;
L3:     if number[j] != 0 and (number [j'], j) < (number[i],i)
            goto L3;
    end;
CS:
    critical section;
    number[i] := O;
DONE:
    noncritical section;
    goto L 1 ;
    end 
*/

module bakery {

    type ProcessState = str
    type ProcessId = int

    val num_processes: int = 3
    val possible_states: Set[ProcessState] = Set("L1", "L2", "L3", "CS", "DONE")
    val processes: Set[ProcessId] = 1.to(num_processes)

    var j: ProcessId -> int
    var choosing: ProcessId -> int
    var number: ProcessId -> int
    var process_state: ProcessId -> ProcessState
    var iteration: int

    pure def max(x: int, y: int): int = if (x > y) x else y

    def smaller(x: ProcessId, y: ProcessId): bool = or(
        and(number.get(x) == number.get(y), x < y),
        number.get(x) < number.get(y)
    )

    action init = all {
        j' = processes.mapBy(x => 1),
        choosing' = processes.mapBy(x => 1),
        number' = processes.mapBy(x => 0),
        process_state' = processes.mapBy(x => "L1"),
        iteration' = 0,
    }

    action step_l1(process_id: int) : bool =  {
        val current_number: int = number.get(process_id)
        val current_j: int = j.get(process_id)

        all {
            process_state.get(process_id) == "L1",
            iteration' = iteration+1,
            if (j.get(process_id) > num_processes) all {
                number'= number.set(process_id, 1 + current_number),
                choosing'=choosing.set(process_id, 0),
                process_state'=process_state.set(process_id, "L2"),
                j'=j.set(process_id, 1),
            } else {
                val target_number: int = number.get(j.get(process_id))
                all {
                    choosing'=choosing,
                    process_state'=process_state,
                    number'= number.set(process_id, max(target_number, current_number)),
                    j'=j.set(process_id, 1 + current_j)
                }
            }
        }
        
    }

    action step_l2(process_id: int): bool = {
        val current_j  = j.get(process_id)
        val current_choosing = choosing.get(process_id)
        val current_number = number.get(process_id)
        val number_to_compare = number.get(current_j)

        all {
            process_state.get(process_id) == "L2",
            iteration' = iteration + 1,
            if (current_j > num_processes) all {
                process_state'=process_state.set(process_id, "CS"),
                number'=number,
                choosing'=choosing,
                j'=j,

            } else if (choosing.get(current_j) != 0) all {
                process_state'=process_state, // stay at L2 frozen
                j'=j,
                number'=number,
                choosing'=choosing,

            } else all {
                process_state'=process_state.set(process_id, "L3"), //go to l3 on same iteration
                j'=j,
                number'=number,
                choosing'=choosing,

            }
        }
    }

    action step_l3(process_id: int): bool = {
        val current_j  = j.get(process_id)
        val current_choosing = choosing.get(process_id)
        val current_number = number.get(process_id)

        all {
            process_state.get(process_id) == "L3",
            iteration' = iteration + 1,
            if (and(number.get(current_j) != 0, smaller(current_j, process_id))) all {
                j'=j,
                number'=number,
                choosing'=choosing,
                process_state'=process_state,   //busy wait on l3
            } else all {
                j'=j.set(process_id, current_j+1),
                number'=number,
                process_state'=process_state.set(process_id, "L2"),
                choosing'=choosing,
            }
        }

    }

    action step_cs(process_id: int): bool = all {
        process_state.get(process_id) == "CS",
        number'=number.set(process_id, 0),
        choosing'=choosing,
        j'=j,
        process_state'=process_state.set(process_id, "DONE"),
        iteration' = iteration+1,
    }

    action step = all {
        nondet proc = oneOf(processes)
        any {
            step_l1(proc),
            step_l2(proc),
            step_l3(proc),
            step_cs(proc)
        }
    }

    def on_critical_session(): bool = filter(0.to(num_processes), key => process_state.get(key) == "CS").size() <= 1
    def all_done(): bool = process_state.keys().filter(key => process_state.get(key) == "DONE").size() == num_processes


    temporal mutex(): bool = always(on_critical_session)
    temporal termination(): bool = eventually(always(all_done))

}

