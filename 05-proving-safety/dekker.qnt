/* 
start:
    wants_to_enter[0]=true;
s1:
    while(wants_to_enter[1]==true) {
s2:
        if(turn!=0) {
s3:
            wants_to_enter[0]=false;
s4:
            while(turn!=0);
            wants_to_enter[0]=true;
        }
    }
cs:
    turn=1;
    wants_to_enter[0]=false;
done:
*/

module Dekker {

    type ProcessState = str
    type ProcessId = int

    var wants_to_enter: ProcessId -> bool
    var turn: int
    var process_state: ProcessId -> ProcessState
    
    val processes: Set[int] = 0.to(1)
    val possible_process_states: Set[ProcessState] = Set("start", "s1", "s2", "s3", "s4", "cs", "done")

    action init = all {
        nondet proc = processes.oneOf()
        val initial_state = Map(0 -> "start", 1-> "start")
        val initial_wants_to_enter = Map(0 -> false, 1 -> false)
        all {
            turn' = proc,
            wants_to_enter' = initial_wants_to_enter,
            process_state' = initial_state
        }
    }

    pure def other_process(process_id: int): int = (process_id + 1)%2

    action step_start(process_id: int, current_process_state: ProcessState): bool = all {
        current_process_state == "start",
        turn' = turn,
        process_state' = process_state.set(process_id, "s1"),
        wants_to_enter' = wants_to_enter.set(process_id, true),    
    }

    action step_s1(process_id: int, current_process_state: ProcessState): bool = all {
        current_process_state == "s1",
        turn' = turn,
        wants_to_enter'= wants_to_enter,
        if (wants_to_enter.get(other_process(process_id)) == true) {
            process_state'= process_state.set(process_id, "s2")
        } else {
            process_state'= process_state.set(process_id, "cs")
        }
    }

    action step_s2(process_id: int, current_process_state: ProcessState): bool = all {
        current_process_state == "s2",
        turn' = 0,
        wants_to_enter'= wants_to_enter,
        if (turn != process_id) {
            process_state'= process_state.set(process_id, "s3")
        } else {
            process_state'= process_state.set(process_id, "s1")
        }
    }

    action step_s3(process_id: int, current_process_state: ProcessState): bool = all {
        current_process_state == "s3",
        turn' = 0,
        process_state'=  process_state.set(process_id, "s4"),
        wants_to_enter'= wants_to_enter.set(process_id, false)
    }

    action step_s4(process_id: int, current_process_state: ProcessState): bool = all {
        current_process_state == "s4",
        turn' = turn,
        any {
            all {
                turn != process_id,
                wants_to_enter'= wants_to_enter,
                process_state' = process_state,
            },
            all {
                turn == process_id,
                wants_to_enter'=wants_to_enter.set(process_id, true),
                process_state'=process_state.set(process_id, "s1"),
            }
        }
    }

    action step_cs(process_id: int, current_process_state: ProcessState): bool = all {
        current_process_state == "cs",
        turn' = other_process(process_id),
        wants_to_enter' = wants_to_enter.set(process_id, false),
        process_state'=process_state.set(process_id, "done")
    }

    action step: bool = {
        nondet process_id = processes.oneOf()
        val current_process_state = get(process_state, process_id)
        any {
            step_start(process_id, current_process_state),
            step_s1(process_id, current_process_state),
            step_s2(process_id, current_process_state),
            step_s3(process_id, current_process_state),
            step_s4(process_id, current_process_state),
            step_cs(process_id, current_process_state)
        }
    }

    pure def mutual_exclusion(): bool = {
        not(
            and(
                process_state.get(0) == "cs", 
                process_state.get(1) == "cs"
            )
        )
    }

    pure def finishes_execution(): bool = {
        and(
            process_state.get(0) == "done", 
            process_state.get(1) == "done"
        ) 
    }
    
    temporal no_race_condition: bool = always(mutual_exclusion)
    temporal eventually_finishes: bool = eventually(finishes_execution)

}
